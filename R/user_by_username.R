
#' Get a variety of information about a user specified by their username
#' 
#' Endpoint documentation: 
#' https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-by-username-username
#' 
#' @param token optional character string giving a bearer token used for 
#'   authorisation.
#' @param screen_name character string giving the twitter screen name to 
#'  download tweets from.
#' @param ... values to be passed on the the API
#'
#' @return a list of JSONs returned from the API
#' @export
#'
user_by_username <- function(token,
                             screen_name, 
                             ...,
                             .format = "body") {
  
  checkmate::assert_string(token) 
  checkmate::assert_string(screen_name)
  
  params <- list(...)
  
  api <- sprintf("/2/users/by/username/%s", screen_name)
  
  if (identical(.format, "parsed")) .format <- parse_resp_meta 

  resp <- api_req_perform(token, api, params, NULL) |>
    api_resp_parse(.format)
  
  return(resp)
}


#' @keywords internal
api_user_id <- function(token, screen_name) {
  resp_body <- user_by_username(token, screen_name, user.fields = "id")
  return(resp_body$data$id)
}


