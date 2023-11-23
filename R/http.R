
#' @keywords internal
api_paginate_cursor <- function(token, api, params, n = Inf) {
  # maximum number of tweets is 3,200 and max page size is 100
  pages <- if (is.infinite(n)) 3200 / 100 else max(n %/% 100, 1)
  results <- vector("list", pages)
  
  n_results <- 0
  next_cursor <- NULL
  i <- 1
  
  # use a loop as passing token forward between 
  repeat {
    if (i > length(results)) {
      length(results) <- 2 * length(results)
    }
    
    params_i <- params
    
    # work out how many tweets still need to be downloaded
    # must be more than 5 and less than 100
    params_i$max_results <- max(5, min(n - n_results, 100))
    
    # add cursor if present
    if (!is.null(next_cursor)) {
      params_i$pagination_token <- next_cursor
    }
    
    json <- api_get(token, api, params = params_i) 
    
    results[[i]] <- json
    
    next_cursor <- json$meta$next_token
    
    n_results <- n_results + json$meta$result_count
    
    if (is.null(next_cursor) || (n_results >= n)) {
      if (n_results < n) {
        warning(
          sprintf("%i tweets requested but only %i returned.", n, n_results)
        )
      }
      break
    }

    i <- i + 1
  }
  
  return(results)
}

#' @keywords internal
api_get <- function(token, api, params = NULL, ...) {
  url <- paste0("https://api.twitter.com/", api)
  
  resp <- httr::GET(
    url,
    httr::add_headers(Authorization = paste('Bearer', token)),
    query = params, 
    ...
  )
  
  httr::stop_for_status(resp)
  
  if (is.null(resp$headers[["content-type"]]) ||
      !grepl("application/json", resp$headers[["content-type"]])) {
    stop("API did not return json")
  }
  
  resp <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  return(
    jsonlite::fromJSON(resp)
  )
}