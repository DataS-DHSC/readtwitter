% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{users}
\alias{users}
\title{Get user details from user id}
\usage{
users(token, user_ids, ..., .format = "parsed")
}
\arguments{
\item{token}{character string giving a bearer token used for
authorisation.}

\item{user_ids}{list of character strings giving the twitter user ids to lookup.}

\item{...}{values to be passed on the the API}

\item{.format}{optional one of "parsed", "body" (default), or "raw"}
}
\value{
a list of JSONs returned from the API
}
\description{
Endpoint documentation:
https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users
}
