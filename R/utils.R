
#' Collapse down a returned list of tweets
#' 
#' Flatten a list of tweets by joining on the text of any
#' referenced tweets and any errors.
#'
#' @param tweets_resp response to be flattened
#'
#' @return data frame of tweets
#' @export
#'
flatten_referenced_tweets <- function(tweets_resp) {
  check_errors(tweets_resp$errors)
  
  if (is.null(tweets_resp$data)) {
    stop("Unable to flatten tweet as no data present")
  }
  
  df <- tweets_resp$data |>
    join_referenced_tweets(tweets_resp$includes_tweets, tweets_resp$errors)
  
  # add in any missing columns
  expansion_cols <- c(
    "text_retweeted", "text_quoted", "text_replied_to", "errors"
  )
  
  for (c in expansion_cols) {
    if (!(c %in% names(df))) df[[c]] <- NA_character_
  }
  
  return(df)
}


#' @keywords internal
build_search_queries <- function(screen_name,
                                 keywords,
                                 include_retweets,
                                 include_replies,
                                 include_quotes) {
  
  base_query <- sprintf("from:%s", screen_name)
  
  if (!include_retweets) base_query <- sprintf("%s -is:retweet", base_query)
  if (!include_replies) base_query <- sprintf("%s -is:reply", base_query)
  if (!include_quotes) base_query <- sprintf("%s -is:quote", base_query)
  
  # 5 characters for the two sets of parenthesise and the space
  #(<base query>) (<key words>)
  .max_query_len = 500 - 5
  
  max_query_len = .max_query_len - nchar(base_query)

  kw <- keywords |>
    trimws()
  
  # add quotes around any values with spaces or special terms
  escape_words <- "(\\s)|(\\bOR\\b)"
  kw <- ifelse(grepl(escape_words, kw, ignore.case = TRUE), sprintf('"%s"', kw), kw)  

  # longest single search term
  if (max(nchar(kw)) > max_query_len) {
    stop("Cannot generate query strings as keywords too long")
  }
  
  # add ORs between terms and one at end as simplifies logic  
  kw <- kw |>
    paste0(" OR ")
  
  mod_len <- nchar(" OR ")
  
  csum <- function(x) {
    cumsum(nchar(x)) - mod_len
  }

  query_list <- vector(
    mode = "list", 
    length = ceiling(max(csum(kw)) / max_query_len)
  )
  
  i <- 1
  while (length(kw) > 0) {
    idx <- csum(kw) <= max_query_len
    
    q_str <- paste(kw[idx], collapse = "")
    kw <- kw[!idx]
    
    query_list[[i]] <- sprintf(
      "(%s) (%s)", 
      base_query,
      substr(q_str, 1, nchar(q_str) - mod_len)
    )
    i <- i + 1
  }

  return(query_list)
}


#' @keywords internal
#' @importFrom dplyr mutate select group_by rename left_join join_by across 
#'   ungroup .data
#' @importFrom tidyr unnest_longer unnest_wider pivot_wider
expand_referenced_tweets <- function(df) {
  if ("referenced_tweets" %in% names(df)) {
    df <- df |>
      mutate(
        referenced_tweets_raw = .data$referenced_tweets
      ) |>
      unnest_longer(.data$referenced_tweets, keep_empty = TRUE) |> 
      unnest_wider(.data$referenced_tweets, names_sep = "_")
  } else {
    df <- df |>
      dplyr::mutate(
        referenced_tweets_id = NA_character_,
        referenced_tweets_type = NA_character_
      )
  }
  
  return(df)
}


#' @keywords internal
#' @importFrom dplyr left_join filter select rename distinct 
#'   join_by mutate .data
join_referenced_errors <- function(df, df_errors) {
  if (!is.null(df_errors)) {
    df <- df |>
      left_join(
        df_errors |>
          filter(.data$parameter == "referenced_tweets.id") |>
          select(.data$value, .data$detail) |>
          rename(referenced_tweets_id = .data$value, errors = .data$detail) |>
          distinct(),
        by = join_by(referenced_tweets_id)
      )
  } else {
    df <- df |>
      mutate(
        errors = NA_character_
      )
  } 
  
  return(df)
}


#' @keywords internal
#' 
#' @importFrom dplyr mutate select group_by rename left_join join_by across 
#'   ungroup
#' @importFrom tidyr unnest_longer unnest_wider pivot_wider
#' @importFrom dplyr any_of .data distinct
join_referenced_tweets <- function(df, df_ref, df_errors) {
  # check to see if any retweets, quote tweets or replies
  if (!is.null(df_ref)) {
    df <- df |>
      mutate(
        referenced_tweets_raw = .data$referenced_tweets
      ) |>
      unnest_longer(.data$referenced_tweets, keep_empty = TRUE) |>
      unnest_wider(.data$referenced_tweets, names_sep = "_") |>
      left_join(
        df_ref |>
          select(.data$id, .data$text) |>
          rename(
            referenced_tweets_id = .data$id, referenced_tweets_text = .data$text
          ),
        by = join_by(referenced_tweets_id)
      ) |>
      join_referenced_errors(df_errors) |>
      select(
        -.data$referenced_tweets_id
      ) |>
      group_by(
        across(
          -c(.data$referenced_tweets_type, .data$referenced_tweets_text, 
             .data$errors)
        )
      ) |>
      mutate(
        errors = paste(
          .data$errors[!is.na(.data$errors)], 
          collapse = " | "
        ),
        errors = ifelse(.data$errors == "", NA_character_, .data$errors)
      ) |>
      ungroup() |>
      distinct(id, .keep_all = TRUE) |>
      pivot_wider(
        names_from = .data$referenced_tweets_type, 
        values_from = .data$referenced_tweets_text,
        names_prefix = "text_"
      ) |>
      select(
        -any_of("text_NA")
      )
  } else {
    df <- df |>
      mutate(
        referenced_tweets_raw = list(NA)
      ) |>
      distinct(id, .keep_all = TRUE)
  }  
}


#' @keywords internal
check_errors <- function(df_errors) {
  if (!is.null(df_errors)) {
    unknown_errors <- setdiff(
      c("referenced_tweets.id"), 
      unique(df_errors$parameter)
    ) 
    
    if (length(unknown_errors) > 0) {
      warning(
        sprintf(
          "Unknown errors (%s)", paste(unknown_errors, collapse = ",")
        )
      )
    }
  }
}


#' Lookup usernames for user ids
#' 
#' @param df data frame of tweets
#' @param token character string giving a bearer token used for 
#'   authorisation.
#'   
#' @return data frame of tweets
#' @export
#' 
#' @importFrom dplyr left_join filter pull rename 
#'   join_by mutate .data
#'   
join_replied_to_usernames <- function(df, token) {
  if (!is.null(df$in_reply_to_user_id)) {
    user_ids <- df |>
      filter(.data$in_reply_to_user_id != "NA") |>
      pull("in_reply_to_user_id") |>
      unique()
    
    if (length(user_ids) > 0) {
      df_users <- users(token, user_ids, .format = "parsed") |>
        mutate(
          in_reply_to_username = paste0("@", .data$username)
        ) |>
        select(id, in_reply_to_username) |>
        rename(in_reply_to_user_id = .data$id)
      
      df <- df |>
        left_join(
          df_users,
          by = join_by(in_reply_to_user_id)
        )
    }
  }
  
  return(df)
}