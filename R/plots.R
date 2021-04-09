#' show all available plot methods
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


#' Plot a network from a model
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma','tpm' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param threshold A numeric
#' @param plot A logical - It return the graphics object
#'
#' @return Either a json, an html or a plotly htmlwidget
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#' 'VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030','VIT_00s0505g00060'
#' ,'VIT_00s0873g00020','VIT_00s0904g00010')
#' module_1 <- create_module(biofeaturesNames=gene_names, version = "legacy")
#' plot_module_network(module = module_1, plot = FALSE)
#'}
plot_module_network <- function(compendium = "vespucci",
                                module = NULL,
                                normalization = "legacy",
                                type = "json",
                                threshold = 0.7,
                                plot = TRUE){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm") version <- "2.0"
  else version <- "legacy"

  biofeaturesIds <- get_biofeature_id(name_In = rownames(module))$id
  samplesetIds <- get_sampleset_id(name_In = colnames(module))$id
  my_query <- paste0('{
      plotNetwork(compendium:\"', compendium, '\", version:\"', version, '\",
      threshold:', threshold, ', plotType:"module_coexpression_network",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                     type,'
      }
    }')
  output <- build_query(my_query)$plotNetwork
  if (plot) {
    my_data <- RJSONIO::fromJSON(output)$data
    my_layout <- RJSONIO::fromJSON(output)$layout
    fig <- plotly::plot_ly(x = my_data[[20]]$x, y = my_data[[20]]$y,
                           type = my_data[[20]]$type,
                           mode = "scatter")
    fig <- plotly::layout(fig, title = list(title = my_layout$title$text,
                                            titlefont = my_layout$title$font),
                          margin = my_layout$margin,
                          showlegend = my_layout$showlegend)
    fig
  }
  else output
}


#' Plot a distribution from a model
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma','tpm' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param plot A logical - it return the graphics object
#' @param plotType A string - see \code{\link{get_available_plot_methods}}
#' @param getRank A logical - if TRUE return the ranking
#'
#' @return Either a json, an html, a plotly htmlwidget or a data.frame with the ranking
#' @export
#'
#' @examples
#'\dontrun{
#'gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#''VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030','VIT_00s0505g00060',
#''VIT_00s0873g00020','VIT_00s0904g00010')
#' module_1 <- create_module(biofeaturesNames=gene_names, version = "legacy")
#' plot_module_distribution(module = module_1,
#' plotType = "biological_features_uncentered_correlation_distribution", plot = TRUE)
#' plot_module_distribution(module = module_1,
#' plotType = "sample_sets_magnitude_distribution", plot = TRUE)
#' plot_module_distribution(module = module_1,
#' plotType = "sample_sets_coexpression_distribution", plot = TRUE)
#'}
plot_module_distribution <- function(compendium = "vespucci",
                                module = NULL,
                                normalization = "legacy",
                                type = "json",
                                plot = TRUE,
                                plotType = "biological_features_uncentered_correlation_distribution",
                                getRank = FALSE){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm") version <- "2.0"
  else version <- "legacy"

  biofeaturesIds <- get_biofeature_id(name_In = rownames(module))$id
  samplesetIds <- get_sampleset_id(name_In = colnames(module))$id
  if(!getRank){
  my_query <- paste0('{
    plotDistribution(compendium:\"', compendium, '\", version:\"', version, '\",
    plotType:\"', plotType, '\",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                       type,'
      }
    }')
  #return(cat(my_query))
  output <- build_query(my_query)$plotDistribution
  if (plot) {
    my_data <- RJSONIO::fromJSON(output)$data
    my_layout <- RJSONIO::fromJSON(output)$layout
    fig1 <- plotly::plot_ly(x = my_data[[1]]$x, y = my_data[[1]]$y, type = my_data[[1]]$type,
                            mode = my_data[[1]]$mode )
    fig1 <- plotly::layout(fig1, xaxis = list(title = my_layout$xaxis2$title),
                           yaxis = list(title = my_layout$yaxis2$title), showlegend = FALSE)
    fig2 <- plotly::plot_ly(x = my_data[[2]]$x, y=my_data[[2]]$y, type = my_data[[2]]$type,
                            mode = my_data[[1]]$mode)
    fig2 <- plotly::layout(fig2, xaxis = list(title =  my_layout$xaxis$title),
                           yaxis = list(title =  my_layout$yaxis$title))
    plotly::subplot(fig2, fig1, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE)
  }
  else output
  } else {
    plot <- FALSE
    my_query <- paste0('{
    plotDistribution(compendium:\"', compendium, '\", version:\"', version, '\",
    plotType:\"', plotType, '\",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
                       ranking{
                         name
                         value
                       }
      }
    }')
    # output <- build_query(my_query)$plotDistribution$ranking
    as.data.frame(sapply(build_query(my_query)$plotDistribution$ranking,unlist))
  }
}

#' plot heatmap from a module
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma','tpm' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param plot A logical - it returns the graphics object
#' @param sorted A logical - it returns sorted index for both bf and ss
#' @param min A numeric (-6 default)
#' @param max A numeric (6 default)
#'
#' @return Either a json, an html, a plotly htmlwidget, or a list of sorted features
#' @export
#'
#' @examples
#'\dontrun{
#'gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110')
#' module_1 <- create_module(biofeaturesNames=gene_names, version = "legacy")
#' plot_module_heatmap(module = module_1, plot = TRUE)
#' plot_module_heatmap(module = module_1, sorted = TRUE, plot = FALSE)
#'}
plot_module_heatmap <- function(compendium = "vespucci",
                                module = NULL,
                                normalization = "legacy",
                                type = "json",
                                plot = TRUE,
                                min = -6,
                                max= 6,
                                sorted = FALSE){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm") version <- "2.0"
  else version <- "legacy"

  biofeaturesIds <- get_biofeature_id(name_In = rownames(module))$id
  samplesetIds <- get_sampleset_id(name_In = colnames(module))$id
  if(sorted){
    my_query <- paste0('{
      plotHeatmap(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:"module_heatmap_expression",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
                       sortedBiofeatures {
                         id
                       },
                       sortedSamplesets {
                         id
                       }
      }
    }')
    output <- build_query(my_query)$plotHeatmap
    return(list(sortedSamplesets = sapply(output$sortedSamplesets, unlist),
                sortedBiofeatures = sapply(output$sortedBiofeatures, unlist)))
  } else {
    my_query <- paste0('{
      plotHeatmap(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:"module_heatmap_expression",min:',min,',max:',max,',
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                       type,'
      }
    }')
  }
  output <- build_query(my_query)$plotHeatmap
  if (plot) {
    my_data <- RJSONIO::fromJSON(output)$data
    my_layout <- RJSONIO::fromJSON(output)$layout
    fig <- plotly::plot_ly(x = my_data[[2]]$x, y = my_data[[2]]$y, z = my_data[[2]]$z,
                           type = my_data[[1]]$type)
    fig <- plotly::layout(fig, xaxis = list(title = my_layout$xaxis$title),
                          yaxis = list(title = my_layout$yaxis$title))
    fig
  }
  else output
}

#' Show a plot using the rstudio viewer
#'
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param plotType A string - see \code{\link{get_available_plot_methods}}
#' @param normalization A string - either 'limma','tpm' or legacy as normalization
#' @param threshold A numeric - A Pearson correalation value
#' @param min A numeric (-6 default)
#' @param max A numeric (6 default)
#'
#' @return A plotly htmlwidget
#' @export
#'
view_plot <- function(module = NULL,
                      plotType = "biological_features_uncentered_correlation_distribution",
                      normalization = "legacy",
                      threshold = 0.7,
                      min = -6,
                      max = 6){
  if(is.null(module)) stop("Provide a module build by create_module()")
  if (plotType == "module_coexpression_network"){
    my_html <- plot_module_network(module = module,
                                   type = "html",
                                   normalization = normalization,
                                   threshold = threshold,
                                   plot = FALSE)
  } else if(plotType == "module_heatmap_expression"){
    my_html <- plot_module_heatmap(module = module,
                                   type = "html",
                                   normalization = normalization,
                                   min = min,
                                   max = max,
                                   plot = FALSE)
  } else if(plotType %in% c("biological_features_uncentered_correlation_distribution",
                            "sample_sets_magnitude_distribution",
                            "sample_sets_coexpression_distribution")){
    my_html <- plot_module_distribution(module = module,
                                   type = "html",
                                   plotType = plotType,
                                   normalization = normalization,
                                   plot = FALSE)

  } else stop("type has to be one of available plot methods.")
  h <- xml2::read_html(my_html)
  tmpDir <- tempfile()
  dir.create(tmpDir)
  htmlFile <- file.path(tmpDir, "viewer.html")
  xml2::write_html(h, tmpDir,file = htmlFile)
  # rstudioapi::viewer(htmlFile)
  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer("http://localhost:8100")
  else
    utils::browseURL("http://localhost:8100")
  viewer(htmlFile)
}
