
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *\*\* Package in development \*\**

# readtwitter package

The goal of this package is to access Twitter/X using its paid API,
identify the username of a given Twitter handle, and retrieve Tweets
from that user’s timeline.

Inputs required are: \* Your bearer token for access to the Twitter API;
\* The Twitter handle/screen name of he Twitter user whose timeline you
wish to scrape; \* Parameters for the Tweet retrieval.

The Tweets retrieved are specified by two parameters: \* a given number
of Tweets, which will return the most recent number; and/or \* a date,
which will return all Tweets falling between the given date and most
recent.

This code has been developed in-house and not extensively tested. If you
find any bugs or have suggestions of other functions to add, please
raise an issue in this repository.

## Installation

You can install the development version of ‘readtweets’ using:

``` r
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)
librarian::stock(DataS-DHSC/readtweets)
```
