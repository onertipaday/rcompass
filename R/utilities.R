#' Process a GraphQL query from COMPASS API
#'
#' @param query A string, the GraphQL query
#'
#' @return A list
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
  RJSONIO::fromJSON(con$exec(qry$queries$myquery), nullValue=NA)$data
}
