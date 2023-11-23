
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *\*\* Package in development \*\**

# readtwitter package

This package can be used to access Twitter/X using its paid API 2.0.

The code if heavily influenced by the
[`rtweet`](https://github.com/ropensci/rtweet) package but makes use of
the API 2.0 endpoint so is compatible with the Basic level of [paid
subscription](https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api).
The hope is that the next release of `rtweet` will have support for API
2.0 endpoints making this package redundant.

This code has been developed in-house and not extensively tested. If you
find any bugs or have suggestions of other functions to add, please
raise an issue in this repository.

## Installation

You can install the development version of ‘readtwitter’ using:

``` r
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)
librarian::stock(DataS-DHSC/readtwitter)
```

## Licence

Unless stated otherwise, the codebase is released under the MIT License.
This covers both the codebase and any sample code in the documentation.

All other content is [© Crown
copyright](http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/)
and available under the terms of the [Open Government 3.0
licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
except where otherwise stated.
