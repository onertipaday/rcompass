#' get_available_plot_methods
#'
#' @param compendium A string - the selected compendium
#'
#' @return A list with plot methods for distribution, heatmap and network
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


#' plot_module
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#' @param plot_type A string - the type of plot either 'heatmap', 'distributions' or 'network'
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#'
#' @return An html page with the heatmap
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#' 'VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030',
#' 'VIT_00s0505g00060','VIT_00s0873g00020','VIT_00s0904g00010')
#' mod_bf <- create_module(biofeaturesNames = gene_names)
#' my_plot_html <- plot_heatmap(plot_type = "heatmap",
#' biofeaturesNames = gene_names, normalization = "legacy")
#' tempDir <- tempfile()
#' dir.create(tempDir)
#' htmlFile <- file.path(tempDir, "plot_heatmap.html")
#' xml2::write_html(my_plot_html,file=htmlFile)
#' rstudioapi::viewer(htmlFile)
#' }
plot_module  <- function(compendium = "vespucci",
                         plot_type = "heatmap",
                          # version = "legacy",
                          # database = "vitis_vinifera",
                          normalization = NULL,
                          biofeaturesNames = NULL,
                          samplesetNames = NULL){
  if (is.null(normalization)) stop ("normalization has to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if (all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide either biofeaturesNames or samplesetsNames")
  if (is.null(biofeaturesNames)) {
    biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, top_n = 10)$id
    samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
  } else if (is.null(samplesetNames)){
    samplesetIds <-  get_samplesets_ranking(biofeaturesNames = biofeaturesNames, top_n = 10)$id
    biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
  } else {
    # biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, top_n = 10)$id
    # samplesetIds <-  get_samplesets_ranking(biofeaturesNames = biofeaturesNames, top_n = 10)$id
    biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
    samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
  }
  if(plot_type == "heatmap"){
    my_query <- paste0('{
  plotHeatmap(compendium:\"', compendium, '\",version:\"', version, '\", normalization:\"', normalization, '\", plotType: "module_heatmap_expression",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
        html
        }
  }')

  }
  else stop("plot_type should be either 'heatmap', 'distributions' or 'network'! ")


  build_query(my_query)
}

#' plot_heatmap
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#'
#' @return An html page with the heatmap
#' @export
#'
#' @examples
#'\dontrun{
#' my_plot_html <- plot_heatmap(QuickSearch_ABAcoreset)
#' tempDir <- tempfile()
#' dir.create(tempDir)
#' htmlFile <- file.path(tempDir, "plot_heatmap.html")
#' xml2::write_html(my_plot_html,file=htmlFile)
#' rstudioapi::viewer(htmlFile)
#' }
plot_heatmap  <- function(compendium = "vespucci",
                          # version = "legacy",
                          # database = "vitis_vinifera",
                          normalization = NULL,
                          biofeaturesNames = NULL,
                          samplesetNames = NULL){
  if (is.null(normalization)) stop ("Select either 'limma','tpm_sample' or legacy as normalization.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if (all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide either biofeaturesNames or samplesetsNames")
  if (is.null(biofeaturesNames)) {
    biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, top_n = 10)$id
  } else if (is.null(samplesetNames)){
    samplesetIds <-  get_samplesets_ranking(biofeaturesNames = biofeaturesNames, top_n = 10)$id
  } else {
    biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, top_n = 10)$id
    samplesetIds <-  get_samplesets_ranking(biofeaturesNames = biofeaturesNames, top_n = 10)$id
  }
  # biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
  # samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
  my_query <- paste0('{
  plotHeatmap(compendium:\"', compendium, '\",version:\"', version, '\",
    plotType: "module_heatmap_expression",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
        html
        }
  }')
  build_query(my_query)$plotHeatmap
}


#' plot_network_coexpression
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#'
#' @return An html page with the coexpression network
#' @export
plot_network_coexpression  <- function(compendium = "vespucci",
                                       normalization = NULL,
                                       biofeaturesNames=NULL,
                                       samplesetNames=NULL){
  if (is.null(normalization)) stop ("Select either 'limma','tpm_sample' or legacy as normalization.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if (all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide either biofeaturesNames or samplesetsNames")
  if (is.null(biofeaturesNames)) {
    biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, top_n = 10)$id
  } else if (is.null(samplesetNames)){
    samplesetIds <-  get_samplesets_ranking(biofeaturesNames = biofeaturesNames, top_n = 10)$id
  } else {
    biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, top_n = 10)$id
    samplesetIds <-  get_samplesets_ranking(biofeaturesNames = biofeaturesNames, top_n = 10)$id
  }
  biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
  samplesetIds <- get_biofeature_id(name_In=samplesetNames)$id
  my_query <- paste0('{
  plotNetwork(compendium:\"', compendium, '\",
    version:\"', version, '\",
    plotType: "module_coexpression_network",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
        html
        }
  }')
  build_query(my_query)$plotNetwork
}


# plot_network <- function(){}


#' plotDistribution
#'
#' @param type either 'html' (default) or 'json
#'
#' @return An html - the plot
#' @export
#'
#' @examples
#'\dontrun{
#' my_plot_html <- plotDistribution()
#' tempDir <- tempfile()
#' dir.create(tempDir)
#' htmlFile <- file.path(tempDir, "plotDistribution.html")
#' xml2::write_html(my_plot_html,file=htmlFile)
#' rstudioapi::viewer(htmlFile)
#'
#' junk=plotDistribution(type = "json")
#' RJSONIO::isValidJSON(junk, asText = T)
#' tmp=RJSONIO::fromJSON(junk)
#' }
plotDistribution <- function(type = "html"){
my_query <- paste0('{
  plotDistribution(compendium:"vespucci", version:"legacy",
    plotType:"sample_sets_coexpression_distribution",
    biofeaturesIds: ["QmlvRmVhdHVyZVR5cGU6MQ==","QmlvRmVhdHVyZVR5cGU6Mg=="]) {',
        type,'
  }
}')
build_query(my_query)$plotDistribution
  }
