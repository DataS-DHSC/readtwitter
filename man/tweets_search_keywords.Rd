% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tweets_search_all.R
\name{tweets_search_keywords}
\alias{tweets_search_keywords}
\title{Keyword search of tweets for specified screen name}
\usage{
tweets_search_keywords(
  token,
  screen_name,
  keywords,
  ...,
  .include_retweets = TRUE,
  .include_replies = TRUE,
  .include_quotes = TRUE,
  .n = Inf,
  .start_date = NULL,
  .end_date = NULL,
  .format = "parsed"
)
}
\arguments{
\item{token}{character string giving a bearer token used for authorisation.}

\item{screen_name}{character string giving the twitter screen name to
download tweets from.}

\item{keywords}{character list of keywords to search for.}

\item{...}{values to be passed on the the API.}

\item{.include_retweets}{should retweets be included}

\item{.include_replies}{should replies be included}

\item{.include_quotes}{should quote tweets be included}

\item{.n}{optional integer giving the maximum number of tweets to download
(must be at least 5).}

\item{.start_date}{optional date if specified is the earliest dated tweets
to include.}

\item{.end_date}{optional date if specified is the latest dated tweets to
include.}

\item{.format}{optional one of "parsed"  (default), "body", or "raw"}
}
\value{
a formatted list of JSONs returned from the API
}
\description{
Keyword search of tweets for specified screen name
}
