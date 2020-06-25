
# rCOMPASS <img src="docs/images/logo.png" align="right" alt="" width="120" />

R Client for the COMPASS GraphQL API

This package provides access to the
[COMPASS](https://compass-.readthedocs.io/en/latest/index.html) GraphQL
API from R.

## Build Status

[![Travis-CI Build
Status](https://travis-ci.com/onertipaday/rcompass.svg?branch=master)](https://travis-ci.com/onertipaday/rcompass)

## Installation

The latest development version can be installed from github:

``` r
devtools::install_github("onertipaday/rcompass")
```

## Usage

``` r
#  Get all available compendia
library(rcompass)
get_available_compendia()
```
