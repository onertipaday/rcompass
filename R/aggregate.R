#' totalCounts for aggregated types
#'
#' @param compendium A string - the selected compendium
#' @param version A string - either (default) 'legacy' or '2.0'
#' @param aggregate_type A string - an aggregate type (either 'biofeatures','sampleSets',
#' 'platforms', 'samples', 'experiments')
#'
#' @return A numeric - the total count for the selected aggregate type
#' @export
#'
#' @examples
#'\dontrun{
#' totalCounts(version="1.0", aggregate_type="sampleSets")
#' totalCounts(version="1.0", aggregate_type="biofeatures")
#' totalCounts(aggregate_type = "platforms")
#' totalCounts(aggregate_type = "samples")
#' totalCounts(aggregate_type = "experiments")
#' }
totalCounts <- function(compendium="vespucci", version ="legacy", aggregate_type="biofeatures"){
#   my_query <- paste0('{
#   biofeatures(compendium:\"', compendium, '\", version:\"', version,'\") {
#     totalCount
#
#   }
# }')
  my_query <- paste0('{', aggregate_type, '(compendium:\"', compendium, '\", version:\"', version,'\") {
    totalCount
  }
}')
  build_query(my_query)[[1]]
}
