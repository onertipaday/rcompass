base_url <- "http://compass.fmach.it/graphql/"

do_POST <- function(url, query, ...){
  temp <- httr::POST(
    url,
    body = list(query = query),
    encode = "json",
    ...)
  httr::stop_for_status(temp)
  temp
}

do_GET <- function(url, ...){
  temp <- httr::GET(
    url,
    ...)
  httr::stop_for_status(temp)
  temp
}

cont <- function(x) httr::content(x, as = 'text', encoding = "UTF-8")

parze <- function(x) jsonlite::fromJSON(x)
parze2 <- function(x) RJSONIO::fromJSON(x)
# parze3 <- function(x) rjson::fromJSON(x)
