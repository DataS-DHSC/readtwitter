
#' @keywords internal
api_user_id <- function(token, screen_name) {
  
  api <- sprintf("2/users/by/username/%s", screen_name)
  params <- list("user.fields" = "id")
  
  json <- api_get(token, api, params) 
  
  return(json$data$id)
}