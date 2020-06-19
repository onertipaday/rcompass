#' build_query
#'
#' @param query a string, the graphql query
#'
#' @return a list
#' @export
#'
#' @examples
#' my_query <- "{
#'         compendia {
#'             fullName,
#'             description
#'         }
#' }"
#' build_query(my_query)
build_query <- function(query){
  qry <- ghql::Query$new()
  qry$query('myquery', query)
  con <- ghql::GraphqlClient$new(url="http://compass.fmach.it/graphql/")
  jsonlite::fromJSON(con$exec(qry$queries$myquery),flatten = FALSE)$data
}


build_query2 <- function(query){
  qry <- ghql::Query$new()
  qry$query('myquery', query)
  con <- ghql::GraphqlClient$new(url="http://compass.fmach.it/graphql/")
  #jsonlite::fromJSON(con$exec(qry$queries$myquery))$data
  RJSONIO::fromJSON(con$exec(qry$queries$myquery))$data
}



# base_url <- "http://compass.fmach.it/graphql/"
#
# do_POST <- function(url, query, ...){
#   temp <- httr::POST(
#     url,
#     body = list(query = query),
#     encode = "json",
#     ...)
#   httr::stop_for_status(temp)
#   temp
# }
#
# cont <- function(x) httr::content(x, as = 'text', encoding = "UTF-8")
#
# parze <- function(x) jsonlite::fromJSON(x)
# parze2 <- function(x) RJSONIO::fromJSON(x)
