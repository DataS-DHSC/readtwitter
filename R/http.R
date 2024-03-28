#' @keywords internal
api_req_construct <- function(token, 
                              api, 
                              params, 
                              rate) {
  
  req <- httr2::request("https://api.twitter.com") |>
    httr2::req_url_path_append(api) |>
    httr2::req_user_agent(getOption("HTTPUserAgent")) |>
    httr2::req_headers(
      Authorization = paste('Bearer', token),
      .redact = "Authorization") |>
    httr2::req_url_query(!!!params) 
  
  if (!is.null(rate)) {
    req <- req |> httr2::req_throttle(rate)
  }
    
  return(req)
}


#' @keywords internal
api_req_paginate <- function(token, 
                             api, 
                             params, 
                             rate,
                             n,
                             min_page_size,
                             max_page_size) {
  
  # will use a bespoke approach as currently
  # httr2::req_perform_iterative cannot handle number of results
  # returned if not in response (only number of pages)
  req <- api_req_construct(token, api, params, rate) |>
    httr2::req_url_query(
      max_results = max(min_page_size, min(n, max_page_size))
    )
  
  if (is.finite(n)) {
    pages <- ceiling(n / max_page_size)
  } else {
    pages <- 1
  }
  
  resp_list <- vector("list", pages)
  n_results <- 0
  next_cursor <- NULL
  i <- 1
  
  repeat {
    resp <- req |>
      httr2::req_perform()
    
    result_count <- httr2::resp_body_json(resp)$meta$result_count
    if (result_count > 0) {
      resp_list[[i]] <- resp
    }
    
    next_cursor <- httr2::resp_body_json(resp)$meta$next_token
    n_results <- n_results + result_count
    
    if (is.null(next_cursor) || (n_results >= n)) {
      break
    }
    
    # update request
    req <- req |>
      httr2::req_url_query(
        pagination_token = next_cursor,
        max_results = max(min_page_size, min(n - n_results, max_page_size))
      )
    
    i <- i + 1
    if (i > length(resp_list)) {
      length(resp_list) <- 2 * length(resp_list)
    }
  }
  
  if (is.finite(n) && (n_results < n)) {
    warning(
      sprintf("%d tweets requested but only %d returned.", n, n_results)
    )
  }
  
  return(resp_list)
}


#' @keywords internal
api_req_perform <- function(token, 
                            api, 
                            params, 
                            rate) {
  
  api_req_construct(token, api, params, rate) |>
    httr2::req_perform()
}


#' @keywords internal
api_resp_parse <- function(resp,
                           formatter) {
  
  if (is.function(formatter)) {
    resp <- resp |>
        formatter()
  } else if (identical(formatter, "body")) {
    resp <- resp |>
        httr2::resp_body_json()  
  } else if (!identical(formatter, "raw")) {
    stop(
      sprintf("Unrecognised output format '%s'", formatter)
    )
  } 
  
  return(resp)
}


#' @keywords internal
api_resps_parse <- function(resps,
                            formatter) {
  if (is.function(formatter)) {
    resps <- resps |>
      formatter()
  } else if (identical(formatter, "body")) {
    resps <- resps |>
      lapply(httr2::resp_body_json) 
  } else if (!identical(formatter, "raw")) {
    stop(
      sprintf("Unrecognised output format '%s'", formatter)
    )
  } 

  return(resps)
}
  