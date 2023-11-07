
#' Get tweets for specified screen name
#' 
#' Return a data frame of tweets from a specified screen name. The function
#' will return at most the last 3,200 tweets (this is a limit of the API) and 
#' these can be limited by either specifying the number of tweets to return, the 
#' time range that tweets fall in, or both. Note that the latest tweets are
#' returned first. Twitter rate limit the API call to 15 requests every 15
#' minutes and a maximum of 10,000 tweets per month. 
#' 
#' Referenced tweets refers to retweets, retweets with comments, and replies. 
#' The API truncates the text on these tweets to 140 characters and so an
#' additional `referenced_tweets_text` field is added to the returned tibble.
#'
#' @param bearer_token character string giving the bearer token used for 
#'   authorisation.
#' @param screen_name character string giving the twitter handle to download
#'   tweets from.
#' @param n integer giving the maximum number of tweets to download.
#' @param start_date date if specified is the earliest dated tweets to include.
#' @param end_date date if specified is the latest dated tweets to include.
#'
#' @return a tibble of the tweet data.
#' @export
#' @import dplyr
#' @importFrom rlang .data
#'
get_tweets <- function(bearer_token, screen_name, 
                       n = 100, start_date = NULL, end_date = NULL) {
  
  #referenced_tweets <- referenced_tweets_id <- text <- referenced_tweets_text <- NULL
  
  # function input validation
  min_date <- as.Date("2010-11-06")
  
  checkmate::assert_string(bearer_token)
  checkmate::assert_string(screen_name)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_date(start_date, lower = min_date, len = 1, null.ok = TRUE)
  checkmate::assert_date(end_date, lower = min_date, upper = Sys.Date(),
                         len = 1, null.ok = TRUE)
  
  if (!is.null(start_date) && !is.null(end_date) && end_date <= start_date) {
    stop("`end_date` must be after `start_date`")
  }
  
  # list of fields to be returned by API
  tweet_fields = c(
    "attachments",
    "created_at",
    "entities",
    "in_reply_to_user_id",
    "public_metrics",
    "referenced_tweets",
    "source",
    "text"
  ) %>%
    paste0(collapse=",")
  
  # initialise an empty tibble to return if no tweets
  df_null <- tibble(
    text = character(),
    edit_history_tweet_ids = list(),
    id = character(),
    created_at = character(),
    referenced_tweets = list(),
    entities.annotations = list(),
    entities.mentions = list(),
    entities.urls = list(),
    entities.hashtags = list(),
    public_metrics.retweet_count = integer(),
    public_metrics.reply_count = integer(),
    public_metrics.like_count = integer(),
    public_metrics.quote_count = integer(),
    public_metrics.bookmark_count = integer(),
    public_metrics.impression_count = integer(),
    referenced_tweets_id =  character(),
    referenced_tweets_text = character()
  )
  
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))
  

  # convert User ID into Numerical ID needed for API
  response_raw <- 
    sprintf(
      'https://api.twitter.com/2/users/by/username/%s', screen_name
    ) %>% 
    httr::GET(
      httr::add_headers(.headers = headers),
      query = list("user.fields" = "id")
    ) 
  
  httr::stop_for_status(response_raw, "get user id")
  
  response <- response_raw %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
  num_id <- response$data$id
  
  # expansion needed to get full text of retweets, retweets with comments
  # and replies
  query_base <- list(
    tweet.fields = tweet_fields,
    expansions = "referenced_tweets.id"
  )
  
  if (!is.null(start_date)) {
    query_base <- c(
      query_base,
      start_time = format(start_date, "%Y-%m-%dT%H:%M:%SZ")
    )
  }
  
  # as time is specified in API and date has a time of 00:00:00 add
  # one to the date so that all tweets from that day are included
  if (!is.null(end_date)) {
    query_base <- c(
      query_base,
      end_time = format(end_date + 1, "%Y-%m-%dT%H:%M:%SZ")
    )    
  }
  
  df_list <- list(df_null)
  n_results <- 0
  next_token <- NULL
  
  # use a loop as passing token forward between 
  repeat {
    
    # work out how many tweets still need to be downloaded
    query_list <- c(
      query_base, 
      max_results = min(n - n_results, 100)
    )
    
    if (!is.null(next_token)) {
      query_list <- c(
        query_list, 
        pagination_token = next_token
      )
    }
    
    response_raw <- 
      sprintf(
        'https://api.twitter.com/2/users/%s/tweets', num_id
      ) %>% 
      httr::GET(
        httr::add_headers(.headers = headers),
        query = query_list
      )
    
    httr::stop_for_status(response_raw, "read tweets")
    
    response <- response_raw %>% 
      httr::content(as = "text") %>% 
      jsonlite::fromJSON(flatten = TRUE)
    
    if (response$meta$result_count == 0) {
      warning("No tweets returned on last API call.")
      break
    }
    
    # referenced_tweets is a list which includes the id of that tweet
    df <- response$data %>% 
      as_tibble() 
    
    # if there are referenced tweets add their text to the tibble
    if (!is.null(response$includes$tweets)) {
      df <- df %>% 
        tidyr::hoist(
          .data$referenced_tweets, referenced_tweets_id = "id", .remove = FALSE
        ) %>%
        tidyr::unnest(.data$referenced_tweets_id, keep_empty = TRUE)
      
      df_includes <- response$includes$tweets %>% 
        as_tibble() %>%
        mutate(
          referenced_tweets_id = id,
          referenced_tweets_text = .data$text
        ) %>%
        select(.data$referenced_tweets_id, .data$referenced_tweets_text)
      
      df <- df %>%
        left_join(
          df_includes,
          by = "referenced_tweets_id"
        )
    } else {
      df <- df %>%
        mutate(
          referenced_tweets_text = NA
        )
    }
     
    df_list <- list(df_list, df)

    next_token <- response$meta$next_token
    
    n_results <- n_results + response$meta$result_count
    
    if (is.null(next_token) || (n_results == n)) {
      if (n_results < n) {
        warning(
          sprintf("%i tweets requested but only %i returned.", n, n_results)
        )
      }
      break
    }
  }
  
  return(
    bind_rows(df_list)
  )
}

# # test code
# df <- get_tweets(
#   Sys.getenv("BEARER_TOKEN"),
#   "BBCLauraK",
#   n = 5,
#   start_date = as.Date("2023-07-16") - 4,
#   end_date = as.Date("2023-07-16")
# )

