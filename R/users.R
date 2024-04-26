
#' Get user details from user id
#' 
#' Endpoint documentation: 
#' https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users
#' 
#' @param token character string giving a bearer token used for 
#'   authorisation.
#' @param user_ids list of character strings giving the twitter user ids to lookup.
#' @param ... values to be passed on the the API
#' @param .format optional one of "parsed", "body" (default), or "raw"
#'
#' @return a list of JSONs returned from the API
#' @export
#'
users <- function(token,
                  user_ids, 
                  ...,
                  .format = "parsed") {
  
  api <- "2/users"
  rate <- 300 / (15 * 60)
  
  checkmate::assert_string(token) 
  checkmate::assert_character(user_ids)
  
  params <- list(...)
  
  if (identical(.format, "parsed")) {
    .format <- function(resps) {
      resps |>
        lapply(httr2::resp_body_json) |> 
        lapply(\(x) lapply(x$data, as.data.frame) |> dplyr::bind_rows()) |> 
        dplyr::bind_rows()
    }
  }
  
  resps <- user_ids |>
    .split_user_ids() |> 
    lapply(
      \(x) .users_req(token, api, params, rate, x)
    ) |>
    api_resps_parse(.format)

  return(resps)
}


#' @keywords internal
.users_req <- function(token, api, params, rate, user_ids_chunk) {
  params[["ids"]] <- user_ids_chunk
  return(
    api_req_perform(token, api, params, rate)
  )
}


#' @keywords internal
.split_user_ids <- function(user_ids) {
  # only up to 100 user ids in any call
  split(user_ids, ceiling(seq_along(user_ids) / 100)) |>
    unname() |>
    lapply(\(x) paste0(x, collapse = ","))
}
  
  