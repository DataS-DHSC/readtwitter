
#' Get tweets a user has liked
#' 
#' 
#' Endpoint documentation:
#' https://developer.twitter.com/en/docs/twitter-api/tweets/likes/api-reference/get-users-id-liked_tweets
#'  
#' @param token optional character string giving a bearer token used for 
#'   authorisation.#'
#' @param screen_name character string giving the twitter screen name to 
#'  download tweets from.
#' @param .n maximum number of liked tweets to return
#' @param ... values to be passed on the the API
#'
#' @return a list of JSONs returned from the API
#' @export
#'
user_liked_tweets <- function(token,
                              screen_name, 
                              ...,
                              .n = Inf,
                              .format = "parsed") {
  
  api_endpoint <- "/2/users/%s/liked_tweets"
  rate <- 75 / (15 * 60)
  min_page_size <- 10
  max_page_size <- 100

  checkmate::assert_string(token) 
  checkmate::assert_string(screen_name)
  checkmate::assert_number(.n, lower = min_page_size)
  
  # expansion needed to get full text of retweets, retweets with comments
  # and replies
  params <- list(...)
  
  if (is.null(params[["tweet.fields"]])) {
    params[["tweet.fields"]] <- paste(tweet_fields, collapse = ",")
  }
  
  if (is.null(params[["expansions"]])) {
    params[["expansions"]] <- "referenced_tweets.id"
  }
  
  if (identical(.format, "parsed")) .format <- parse_resps_tweets
  
  user_id <- api_user_id(token, screen_name)
  api <- sprintf(api_endpoint, user_id)  
  
  resps <- api_req_paginate(token, api, params, rate, .n, 
                            min_page_size, max_page_size) |>
    api_resps_parse(.format)
  
  
  return(resps)
  
}