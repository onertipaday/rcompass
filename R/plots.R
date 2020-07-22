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
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param plot A logical - it return the graphics object
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
                                plot = TRUE){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"

  biofeaturesIds <- get_biofeature_id(name_In = rownames(module))$id
  samplesetIds <- get_sampleset_id(name_In = colnames(module))$id
  my_query <- paste0('{
      plotNetwork(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:"module_coexpression_network",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                     type,'
      }
    }')

  # if(show_query) return(cat(my_query))
  # TODO!!!
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


# -----------------------------------------------------------------------------

#' plot distribution providing either biofeaturesNames of samplesetNames
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param type A string -  either 'html'  or 'json
#' @param plotType A string - see  \code{\link{get_available_plot_methods}}
#' @param useIds A logical - TRUE as default
#' @param plot A logical - it return the graphics object
#'
#' @return Eiher a json, an html or a plotly htmlwidget
#' @export
#'
#' @examples
#'\dontrun{
#' b_ids <- c("QmlvRmVhdHVyZVR5cGU6MQ==", "QmlvRmVhdHVyZVR5cGU6Mg==","QmlvRmVhdHVyZVR5cGU6Mw=="
#' ,"QmlvRmVhdHVyZVR5cGU6NA==", "QmlvRmVhdHVyZVR5cGU6NQ==")
#' s_ids <- c("U2FtcGxlU2V0VHlwZToxNDg=", "U2FtcGxlU2V0VHlwZToxNDk=", "U2FtcGxlU2V0VHlwZToxNTA=",
#'  "U2FtcGxlU2V0VHlwZToxNTE=", "U2FtcGxlU2V0VHlwZToxNTI=")
#' plotDistribution(biofeaturesNames = b_ids, samplesetNames = s_ids,
#' type = "json", useIds = TRUE, plot = TRUE)
#' my_plot_html <- plotDistribution(biofeaturesNames = b_ids, samplesetNames = s_ids,
#' type = "html", useIds = TRUE, plot = FALSE)
#' h <- xml2::read_html(my_plot_html)
#' tempDir <- tempfile()
#' dir.create(tempDir)
#' htmlFile <- file.path(tempDir, "plotDistribution.html")
#' xml2::write_html(h, tmpDir,file = htmlFile)
#' rstudioapi::viewer(htmlFile)
#' }
plotDistribution <- function(compendium = "vespucci",
                            biofeaturesNames = NULL,
                            samplesetNames = NULL,
                            normalization = "legacy",
                            plotType = "biological_features_uncentered_correlation_distribution",
                            useIds = TRUE,
                            type = "json",
                            plot = TRUE){
  if (is.null(normalization)) stop ("Normalization should to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if(is.null(biofeaturesNames) && is.null(samplesetNames)) stop("Provide either biofeatureNames or samplesetNames!")
  if(is.null(samplesetNames)  && !plotType == "biological_features_uncentered_correlation_distribution"){
    if(useIds)  biofeaturesIds <- biofeaturesNames
    else biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
    my_query <- paste0('{
      plotDistribution(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:\"', plotType, '\",
      biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"]) {',
                         type,'
      }
    }')
  }
  else if(is.null(biofeaturesNames) && !plotType == "sample_sets_coexpression_distribution"){
    if(useIds) samplesetIds <- samplesetNames
    else samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
    my_query <- paste0('{
      plotDistribution(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:\"', plotType, '\",
      samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {',
                         type,'
      }
    }')
  }
  else{
      if(useIds){
        biofeaturesIds <- biofeaturesNames
        samplesetIds <- samplesetNames
      }
      else {
        biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
        samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
      }
      my_query <- paste0('{
      plotDistribution(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:\"', plotType, '\",
      biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
      samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {',
                         type,'
      }
    }')
  }
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
  }


#' Plot a distribution from a model
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param plot A logical - it return the graphics object
#' @param plotType A string - see  \code{\link{get_available_plot_methods}}
#'
#' @return Either a json, an html or a plotly htmlwidget
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
                                plotType = "biological_features_uncentered_correlation_distribution"){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"

  biofeaturesIds <- get_biofeature_id(name_In = rownames(module))$id
  samplesetIds <- get_sampleset_id(name_In = colnames(module))$id
  my_query <- paste0('{
    plotDistribution(compendium:\"', compendium, '\", version:\"', version, '\",
    plotType:\"', plotType, '\",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                     type,'
      }
    }')
  # if(show_query) return(cat(my_query))
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
}

#' plot heatmap from a module
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param useIds A logical - TRUE as default
#' @param plot A logical - it return the graphics object
#'
#' @return Either a json, an html or a plotly htmlwidget
#' @export
#'
#' @examples
#'\dontrun{
#' my_bf_ids <- c("QmlvRmVhdHVyZVR5cGU6MQ==", "QmlvRmVhdHVyZVR5cGU6Mg==",
#' "QmlvRmVhdHVyZVR5cGU6Mw==", "QmlvRmVhdHVyZVR5cGU6NA==", "QmlvRmVhdHVyZVR5cGU6NQ==")
#' my_ss_ids <- c("U2FtcGxlU2V0VHlwZToxNDg=", "U2FtcGxlU2V0VHlwZToxNDk=",
#' "U2FtcGxlU2V0VHlwZToxNTA=", "U2FtcGxlU2V0VHlwZToxNTE=", "U2FtcGxlU2V0VHlwZToxNTI=")
#' plot_heatmap(biofeaturesNames = my_bf_ids, samplesetNames = my_ss_ids,
#' plot = TRUE, useIds = TRUE)
#'
#' gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#' 'VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030',
#' 'VIT_00s0505g00060','VIT_00s0873g00020','VIT_00s0904g00010')
#' module_1 <- create_module(biofeaturesNames=gene_names, version = "legacy")
#' sample_names <- get_sampleset_id(name_In = colnames(module_1))$name
#' test = plot_heatmap(biofeaturesNames = gene_names, samplesetNames = sample_names, useIds = F)
#' tmp=RJSONIO::fromJSON(test)[[1]]
#' #data = matrix(unlist(sapply(tmp[[2]]$z, unlist)),length(gene_names),length(sample_names))
#' data = sapply(tmp[[2]]$z, unlist)
#' mlen <- max(sapply(data,length))
#' out <- sapply(data,'[', 1:mlen)
#' colnames(out) <- gene_names
#' rownames(out) <- sample_names
#' plot_ly(y = colnames(out), x = sample_names,z= t(out), type = "heatmap") %>%
#' layout(margin = list(l=120))
#' p = plot_ly(x=tmp[[2]]$x, y=tmp[[2]]$y, z = data,
#' type = tmp[[1]]$type,
#' showscale = T) %>%
#' layout(margin = list(l=120))
#' p
#' # save the widget
#' #library(htmlwidgets)
#' #saveWidget(p, file=paste0( getwd(), "/plotlyHeatmap.html"))
#'}
plot_heatmap <- function(compendium = "vespucci",
               biofeaturesNames = NULL,
               samplesetNames = NULL,
               normalization = "legacy",
               type = "json",
               useIds = TRUE,
               plot = TRUE){
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if(all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide both biofeaturesNames and samplesetsNames")
  if(!useIds){
        biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
        samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
    }
    else {
      biofeaturesIds <- biofeaturesNames
      samplesetIds <- samplesetNames
    }
    my_query <- paste0('{
      plotHeatmap(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:"module_heatmap_expression",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                         type,'
      }
    }')

  # if(show_query) return(cat(my_query))
  output <- build_query(my_query)$plotHeatmap
  if (plot) {
    my_data <- RJSONIO::fromJSON(output)$data
    my_layout <- RJSONIO::fromJSON(output)$layout
    fig <- plotly::plot_ly(x = my_data[[2]]$x, y = my_data[[2]]$y, z = my_data[[2]]$z,
                           type = my_data[[1]]$type)
    fig <- plotly::layout(fig, xaxis = list(title = my_layout$xaxis$title),
                           yaxis = list(title = my_layout$yaxis$title))
  }
  else output
}

#' Plot a heatmap from a model
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param type  A string -  either 'html'  or 'json
#' @param plot A logical - it return the graphics object
#'
#' @return Either a json, an html or a plotly htmlwidget
#' @export
#'
#' @examples
#'\dontrun{
#' module_1 <- create_module(biofeaturesNames=gene_names, version = "legacy")
#' plot_module_heatmap(module = module_1, plot = TRUE)
#'}
plot_module_heatmap <- function(compendium = "vespucci",
                                module = NULL,
                                normalization = "legacy",
                                type = "json",
                                plot = TRUE){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"

  biofeaturesIds <- get_biofeature_id(name_In = rownames(module))$id
  samplesetIds <- get_sampleset_id(name_In = colnames(module))$id
  my_query <- paste0('{
      plotHeatmap(compendium:\"', compendium, '\", version:\"', version, '\",
      plotType:"module_heatmap_expression",
        biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
        samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                     type,'
      }
    }')

  # if(show_query) return(cat(my_query))
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
