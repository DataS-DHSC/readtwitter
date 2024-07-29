
#' Combine a list of formatted responses
#' 
#' @param resps list of formatted responses
#'
#' @return single formatted response 
#' @export
#'
bind_resps <- function(resps) {
  resps |>
    purrr::reduce(merge_resps)
}


#' @keywords internal
merge_resps <- function(x, y) {
  for (i in names(y)) {
    x_i <- purrr::pluck(x, i)
    y_i <- purrr::pluck(y, i)
    
    if (is.null(x_i)) {
      x[[i]] <- y_i
    } else if (is.data.frame(y_i)) {
      x[[i]] <- dplyr::bind_rows(x_i, y_i)
    } else if(is.list(y_i)) {
      x[[i]] <- merge_resps(x_i, y_i)
    } else {
      x[[i]] <- c(x_i, y_i)
    }
  }
  
  return(x)
}


#' @keywords internal
parse_resp_meta <- function(resp, .name = "data") {
  resp |>
    httr2::resp_body_json() |>
    purrr::pluck(.name) |>
    as.data.frame()
}


#' @keywords internal
parse_resps_tweets <- function(resps) {

  resp_bodies <- resps |>
    lapply(\(x) httr2::resp_body_json(x, simplifyVector = TRUE))

  resp_merged <- resp_bodies |>
    Filter(\(x) x$meta$result_count > 0, x = _) 
  
  if (length(resp_merged) > 0) {
    resp_merged <- bind_resps(resp_merged)
  }
  
  resp_merged[["meta"]] <- 
    purrr::map(
      resp_bodies, \(x) as.data.frame(x$meta)
    ) |> 
    dplyr::bind_rows()
  
  resp_merged <- resp_merged |>
    purrr::list_flatten()
  
  return(resp_merged)
}