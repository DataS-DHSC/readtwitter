


#' Title
#'
#' @param tweets_resp 
#'
#' @return formatted response
#' @export
#'
flatten_includes <- function(tweets_resp) {
  df <- tweets_resp$data |>
    dplyr::mutate(
      is_referenced_tweet = FALSE
    ) |>
    expand_tweet_types()
  
  if (!is.null(tweets_resp$includes_tweets)) {
    df_incl <- tweets_resp$includes_tweets |>
      dplyr::mutate(
        is_referenced_tweet = TRUE
      )  |>
      expand_tweet_types()
    
    df <- dplyr::bind_rows(df, df_incl)
  }
  
  return(df)  
}


# construct_search_queries <- function(screen_name
#                                      ) {
#   
#   user_query <- sprintf("(from:%s)", screen_name)
#   
#   
#   
#   checkmate::assert_string(screen_name)
#   
#   return(query_list)
# }

#' @keywords internal
expand_tweet_types <- function(df) {
  if ("referenced_tweets" %in% names(df)) {
    df <- df |>
      tidyr::hoist(
        referenced_tweets, 
        tweet_type = "type", 
        referenced_id = "id",
        .remove = FALSE, 
        .transform = toString
      ) |>
      dplyr::mutate(
        tweet_type = ifelse(tweet_type == "", "tweet", tweet_type),
        referenced_id = ifelse(referenced_id == "", "<NA>", referenced_id)
      )
  } else {
    df <- df |>
      dplyr::mutate(
        referenced_tweets = list("NULL"),
        tweet_type = "tweet",
        referenced_id = "<NA>"
      )
  }
  
  return(df)
}

