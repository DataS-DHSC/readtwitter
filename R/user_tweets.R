
#' Get tweets for specified screen name
#' 
#' Return a tweets from a specified screen name. The function
#' will return at most the last 3,200 tweets (this is a limit of the API) and 
#' these can be limited by either specifying the number of tweets to return, the 
#' time range that tweets fall in, or both. Note that the latest tweets are
#' returned first. Twitter rate limit the API call to 1,500 requests every 15
#' minutes and a maximum of 1000,000 tweets per month. 
#' 
#' Referenced tweets (retweets, retweets with comments, and replies) are
#' contained in the `includes` data object of each returned JSON. 
#'
#' Endpoint documentation:
#' https://developer.twitter.com/en/docs/twitter-api/tweets/timelines/api-reference/get-users-id-tweets
#'
#' @param token character string giving a bearer token used for 
#'   authorisation. 
#' @param screen_name character string giving the twitter screen name to 
#'  download tweets from.
#' @param ... values to be passed on the the API
#' @param .n optional integer giving the maximum number of tweets to download 
#'  (must be at least 5).
#' @param .start_date optional date if specified is the earliest dated tweets
#'  to include.
#' @param .end_date optional date if specified is the latest dated tweets to
#'  include.
#' @param .format optional one of "parsed"  (default), "body", or "raw"
#'
#' @return a list of JSONs returned from the API
#' @export
#'
user_tweets <- function(token,
                        screen_name, 
                        ...,
                        .n = NULL, 
                        .start_date = NULL, 
                        .end_date = NULL,
                        .format = "parsed") {
  
  api_endpoint <- "/2/users/%s/tweets"
  max_n <- 3200
  rate <- 1500 / (15 * 60)
  min_page_size <- 5
  max_page_size <- 100
  min_tweet_date <- as.Date("2010-11-06")
  
  checkmate::assert_string(token) 
  checkmate::assert_string(screen_name)
  checkmate::assert_number(.n, lower = min_page_size, 
                           upper = max_n, null.ok = TRUE)
  checkmate::assert_date(
    .start_date, lower = min_tweet_date, len = 1, null.ok = TRUE
  )
  checkmate::assert_date(
    .end_date, lower = min_tweet_date, upper = Sys.Date(),
    len = 1, null.ok = TRUE
  )
  
  if (!is.null(.start_date) && !is.null(.end_date) && .end_date <= .start_date) {
    stop("`end_date` must be after `start_date`")
  }
  
  # expansion needed to get full text of retweets, retweets with comments
  # and replies
  params <- list(...)
  
  if (is.null(.n)) {
    warning(
      sprintf(
        "API will only return latest %s tweets", format(max_n, big.mark = ",")
      )
    )
    .n <- max_n
  }
  
  if (is.null(params[["tweet.fields"]])) {
    params[["tweet.fields"]] <- paste(tweet_fields, collapse = ",")
  }
  
  if (is.null(params[["expansions"]])) {
    params[["expansions"]] <- "referenced_tweets.id"
  }

  if (!is.null(.start_date)) {
    params[["start_time"]] <- format(.start_date, "%Y-%m-%dT%H:%M:%SZ")
  }

  # as time is specified in API and date has a time of 00:00:00 add
  # one to the date so that all tweets from that day are included
  if (!is.null(.end_date)) {
    params[["end_time"]] <- format(.end_date + 1, "%Y-%m-%dT%H:%M:%SZ")
  }
  
  if (identical(.format, "parsed")) .format <- parse_resps_tweets
  
  user_id <- api_user_id(token, screen_name)
  api <- sprintf(api_endpoint, user_id)
  
  resps <- api_req_paginate(token, api, params, rate, .n,
                            min_page_size, max_page_size) |>
    api_resps_parse(.format)
    

  return(resps)
}
