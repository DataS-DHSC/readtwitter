
#' Get tweets for specified screen name
#' 
#' Return a data frame of tweets from a specified screen name. The function
#' will return at most the last 3,200 tweets (this is a limit of the API) and 
#' these can be limited by either specifying the number of tweets to return, the 
#' time range that tweets fall in, or both. Note that the latest tweets are
#' returned first. Twitter rate limit the API call to 15 requests every 15
#' minutes and a maximum of 10,000 tweets per month. 
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
#' @param n optional integer giving the maximum number of tweets to download.
#' @param start_date optional date if specified is the earliest dated tweets
#'  to include.
#' @param end_date optional date if specified is the latest dated tweets to
#'  include.
#' @param ... values to be passed on the the API
#'
#' @return a list of JSONs returned from the API
#' @export
#'
tweets_search_all <- function(token,
                              query,
                              ...,
                              .n = Inf, 
                              .start_date = NULL, 
                              .end_date = NULL,
                              .format = "parsed") {
  
  api_endpoint <- "/2/tweets/search/all"
  rate <- 300 / (15 * 60)
  min_page_size <- 10
  max_page_size <- 500
  min_tweet_date <- as.Date("2006-03-26")
  
  checkmate::assert_string(token) 
  checkmate::assert_string(query, max.chars = 1024)
  checkmate::assert_number(.n, lower = min_page_size)
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
  
  params[["query"]] <- query
  
  if (is.null(params[["tweet.fields"]])) {
    params[["tweet.fields"]] <- paste(tweet_fields, collapse = ",")
  }
  
  if (is.null(params[["expansions"]])) {
    params[["expansions"]] <- "referenced_tweets.id"
  }
  
  # need to put in the earliest date to prevent only the last
  # 30 days being searched
  if (!is.null(.start_date)) {
    params[["start_time"]] <- format(.start_date, "%Y-%m-%dT%H:%M:%SZ")
  } else {
    params[["start_time"]] <- format(min_tweet_date, "%Y-%m-%dT%H:%M:%SZ")
  }
  
  # as time is specified in API and date has a time of 00:00:00 add
  # one to the date so that all tweets from that day are included
  if (!is.null(.end_date)) {
    params[["end_time"]] <- format(.end_date + 1, "%Y-%m-%dT%H:%M:%SZ")
  }
  
  if (identical(.format, "parsed")) .format <- parse_resps_tweets
  
  api <- api_endpoint
  
  resps <- api_req_paginate(token, api, params, rate, .n,
                            min_page_size, max_page_size) |>
    api_resps_parse(.format)
  
  
  return(resps)
}
