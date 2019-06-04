# rcompass

R Client for the COMPASS GraphQL API

This package provides access to the [COMPASS](https://compass-.readthedocs.io/en/latest/index.html) GrapgQL API from R.

## Build Status

[![Travis-CI Build Status](https://travis-ci.com/onertipaday/rcompass.svg?branch=master)](https://travis-ci.com/onertipaday/rcompass)

## Installation
Installation from github requires the devtools package to be installed.

```R
devtools::install_github("onertipaday/rcompass")
```
## Usage

```R
#  Get all available compendia
library(rcompass)
get_compendia()
```
