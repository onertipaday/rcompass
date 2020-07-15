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

#' plot_network_coexpression
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (here gene_names)
#' @param samplesetNames A character vector - the sampleSets names
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param useIds A logical - TRUE as default
#' @param type  string -  either 'html'  or 'json
#'
#' @return Either a json, an html or a plotly htmlwidget
#' @export
plot_network_coexpression  <- function(compendium = "vespucci",
                                       normalization = "legacy",
                                       biofeaturesNames=NULL,
                                       samplesetNames=NULL,
                                       useIds = TRUE,
                                       type = "json"){
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
  if(useIds){
    biofeaturesIds <- biofeaturesNames
    samplesetIds <- samplesetNames
  }
  else {
    biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
    samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
  }
  # biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
  # samplesetIds <- get_biofeature_id(name_In=samplesetNames)$id
  my_query <- paste0('{
  plotNetwork(compendium:\"', compendium, '\",
    version:\"', version, '\",
    plotType: "module_coexpression_network",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {',
                     type,'
    }
  }')
  build_query(my_query)$plotNetwork
}


# -----------------------------------------------------------------------------

#' plotDistribution
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
#' type = "html", useIds = TRUE) %>% layout(xaxis = x, yaxis = y)
#' tempDir <- tempfile()
#' dir.create(tempDir)
#' htmlFile <- file.path(tempDir, "plotDistribution.html")
#' xml2::write_html(my_plot_html,file=htmlFile)
#' rstudioapi::viewer(htmlFile)
#' }
plotDistribution<- function(compendium = "vespucci",
                            biofeaturesNames = NULL,
                            samplesetNames = NULL,
                            normalization = "legacy",
                            plotType = "biological_features_uncentered_correlation_distribution",
                            useIds = TRUE,
                            type = "json",
                            plot = TRUE){
  if (is.null(normalization)) stop ("normalization should to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if(is.null(biofeaturesNames) && is.null(samplesetNames)) stop("Provide both biofeatureNames AND samplesetNames!")
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
#' @param plotType A string - the type of plot either 'heatmap', 'distributions' or 'network'
#' @param normalization A string - either 'limma','tpm_sample' or legacy as normalization
#' @param type  string -  either 'html'  or 'json
#' @param plotType A string - "module_heatmap_expression"
#' @param useIds A logical - TRUE as default
#'
#' @return A list
#' @export
#'
#' @examples
#'\dontrun{
#'library(plotly)
#'my_bf_ids <- c("QmlvRmVhdHVyZVR5cGU6MQ==", "QmlvRmVhdHVyZVR5cGU6Mg==",
#'"QmlvRmVhdHVyZVR5cGU6Mw==", "QmlvRmVhdHVyZVR5cGU6NA==", "QmlvRmVhdHVyZVR5cGU6NQ==")
#'my_ss_ids <- c("U2FtcGxlU2V0VHlwZToxNDg=", "U2FtcGxlU2V0VHlwZToxNDk=",
#'"U2FtcGxlU2V0VHlwZToxNTA=", "U2FtcGxlU2V0VHlwZToxNTE=", "U2FtcGxlU2V0VHlwZToxNTI=")
#'tmp <- plot_heatmap(biofeaturesNames = my_bf_ids, samplesetNames = my_ss_ids)
#' RJSONIO::isValidJSON(tmp, asText = T)
#' tmp=RJSONIO::fromJSON(tmp)[[1]]
#' data = matrix(unlist(sapply(tmp[[2]]$z, unlist)),5,5)
#' plot_ly(x=tmp[[2]]$x, y=tmp[[2]]$y, z = data,
#' type = tmp[[1]]$type,
#' colorscale= c( "rgb(0, 0, 0)", "rgb(100, 100, 100)"),
#' showscale = T) %>%
#' layout(margin = list(l=120))
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
               plotType = "module_heatmap_expression",
               type = "json",
               useIds = TRUE){
  if (is.null(normalization)) stop ("normalization has to be either 'limma','tpm_sample' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm_sample") version <- "latest"
  else version <- "legacy"
  if(useIds){
    biofeaturesIds <- biofeaturesNames
    samplesetIds <- samplesetNames
  }
  else {
    biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
    samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
  }
my_query <- paste0('{
  plotHeatmap(compendium:\"', compendium, '\", version:\"', version, '\",
  plotType:\"', plotType, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {',
                   type,'
  }
}')
build_query(my_query)$plotHeatmap

}
