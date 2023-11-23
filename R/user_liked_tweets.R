
#' Get tweets a user has liked
#' 
#' @param token optional character string giving a bearer token used for 
#'   authorisation.#'
#' @param screen_name character string giving the twitter screen name to 
#'  download tweets from.
#' @param n maximum number of liked tweets to return
#' @param ... values to be passed on the the API
#'
#' @return a list of JSONs returned from the API
#' @export
#'
user_liked_tweets <- function(token,
                              screen_name, 
                              n = 100, 
                              ...) {
  
  checkmate::assert_string(token) 
  checkmate::assert_string(screen_name)
  checkmate::assert_number(n, lower = 1)
  
  user_id <- api_user_id(token, screen_name)
  api <- sprintf("2/users/%s/liked_tweets", user_id)
  
  # expansion needed to get full text of retweets, retweets with comments
  # and replies
  params <- list(...)
  
  if (is.null(params["tweet.fields"])) {
    params["tweet.fields"] <- paste(tweet_fields, collapse = ",")
  }
  
  if (is.null(params["expansions"])) {
    params["expansions"] <- "referenced_tweets.id"
  }
  
  result <- api_paginate_cursor(token, api, params, n)
  
  return(result)
  
}