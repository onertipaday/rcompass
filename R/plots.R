#' get_available_plot_methods
#'
#' @param compendium A string - the selected compendium
#'
#' @return a list with plot methods for distribution, heatmap and network
#' @export
#'
#' @examples
#' get_available_plot_methods()
get_available_plot_methods <- function(compendium = "vespucci"){
  my_query <- paste0('{
  plotName(compendium:\"', compendium, '\"){
        distribution,
        heatmap,
        network
      }
  }')
  tmp <- build_query(my_query)$plotName
  list(distributions = sapply(tmp$distribution, unlist),
                                     heatmap = tmp$heatmap,
                                     network = tmp$network)
}


#' plot_heatmap
#'
#' @param compendium A string - the selected compendium
#' @param version A string ('legacy' as default)
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#'
#' @return An html page with the heatmap
#' @export
plot_heatmap  <- function(compendium = "vespucci",
                          version = "legacy",
                          biofeaturesNames=NULL,
                          samplesetNames=NULL){
  biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
  samplesetIds <- get_biofeature_id(name_In=samplesetNames)$id
  my_query <- paste0('{
  plotHeatmap(compendium:\"', compendium, '\",
    version:\"', version, '\",
    plotType: "module_heatmap_expression",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
        html
        }
  }')
  # build_query(my_query)$plotHeatmap
  build_query(my_query)$plotHeatmap
}


#' plot_network_coexpression
#'
#' @param compendium A string - the selected compendium
#' @param version A string ('legacy' as default)
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#'
#' @return An html page with the coexpression network
#' @export
plot_network_coexpression  <- function(compendium = "vespucci",
                          version = "legacy",
                          biofeaturesNames=NULL,
                          samplesetNames=NULL){
  biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
  samplesetIds <- get_biofeature_id(name_In=samplesetNames)$id
  my_query <- paste0('{
  plotHeatmap(compendium:\"', compendium, '\",
    version:\"', version, '\",
    plotType: "module_coexpression_network",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
        html
        }
  }')
  # build_query(my_query)$plotHeatmap
  build_query(my_query)$plotHeatmap
}


#' plot_heatmap
#'
#' @param compendium A string - the selected compendium
#' @param version A string ('legacy' as default)
#' @param biofeaturesNames A character vector (here gene_names)
#' @param biofeaturesIds A character vector - the biofeature ids
#'
#' @return An html page with the distribution
#' @export
plot_distribution_coexpressed_samplesets  <- function(compendium = "vespucci",
                          version = "legacy",
                          biofeaturesNames=NULL,
                          biofeaturesIds=NULL){
  my_query <- paste0('{
  plotHeatmap(compendium:\"', compendium, '\",
    version:\"', version, '\",
    plotType:"sample_sets_coexpression_distribution",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(biofeaturesIds, collapse = '","'),'\"]) {
        html
        }
  }')
  build_query(my_query)$plotHeatmap
}
