#' create a module providing both biological features and sample sets
#'
#' @importFrom S4Vectors DataFrame
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string - either 'limma' (default),'tpm' or legacy as normalization
#' @param biofeaturesNames A character vector (gene_names)
#' @param samplesetNames A character vector (sampleset names)
#' @param sorted A logical (FALSE as default) - it returns a sorted index for both bf and ss
#' @param useIds A logical (FALSE as default) - It allows using biofeatureIds
#'
#' @return A SummarizedExperiment object
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110",
#' "VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030",
#' "VIT_00s0505g00060","VIT_00s0873g00020","VIT_00s0904g00010")
#' mod_bf <- create_module(biofeaturesNames = gene_names)
#' my_bf <- get_biofeature_id(id_In = gene_names, useIds = FALSE)
#' ss=c("GSE75498_OS_T0-13-vs-GSE75498_C_T0-21","harvest_4","harvest_5")
#' my_ss <- get_sampleset_id(id_In = ss, normalization = "limma", useIds = FALSE)
#' my_mod <- create_module(biofeaturesNames = my_bf$id, samplesetNames = my_ss$id,
#' normalization = "limma", useIds = TRUE)
#' pheatmap::pheatmap(na.omit(Biobase::exprs(my_mod)), col = RColorBrewer::brewer.pal(11,name="RdBu"))
#' }
create_module <- function(compendium = "vespucci",
                          normalization = "limma",
                          biofeaturesNames = NULL,
                          samplesetNames = NULL,
                          sorted = FALSE,
                          useIds = FALSE){
  if(all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide at least biofeaturesNames or samplesetsNames")
  else if (is.null(biofeaturesNames)) {
    return(create_module_ss(compendium = compendium,
                            normalization = normalization,
                            samplesetNames =  samplesetNames,
                            useIds = useIds))
  }
  else if (is.null(samplesetNames)) {
    return(create_module_bf(compendium = compendium,
                            normalization = normalization,
                            biofeaturesNames = biofeaturesNames,
                            useIds = useIds))
  }
  else {
    if(useIds){
      biofeaturesIds <- biofeaturesNames
      samplesetIds <- samplesetNames
    }
    else {
      biofeaturesIds <- get_biofeature_id(id_In = biofeaturesNames)$id
      samplesetIds <- get_sampleset_id(id_In = samplesetNames)$id
    }
    if(normalization == "legacy") version <- "legacy"
    else if(normalization %in% c("limma","tpm")) version <- "2.0"
    else stop("normalization HAS TO BE either legacy, limma or tpm.")
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
  }
  # cat(my_query)
  nv <- t(as.data.frame(sapply(build_query(my_query)$modules$normalizedValues, unlist)))
  rownames(nv) <- biofeaturesNames
  colnames(nv) <- samplesetNames
  ss_tmp <- get_sampleset_id(normalization = normalization,
                             id_In = colnames(nv) , useIds = T)
  ssData <- ss_tmp[match(colnames(nv),ss_tmp$id),]
  rownames(ssData) <- ssData$id
  bf_tmp <- get_biofeature_id(id_In = rownames(nv), useIds = T)
  bfData <- bf_tmp[match(rownames(nv),bf_tmp$id),]
  rownames(bfData) <- bfData$id
  out <- SummarizedExperiment::SummarizedExperiment(assays=list(counts=nv),
                       rowData=DataFrame(bfData[,2:3]),
                       colData=DataFrame(ssData[,2:3]))
  if(sorted){
    my_sorted_indexes <- plot_module_heatmap(module = out,
                                             normalization = normalization,
                                             plot = FALSE,
                                             sorted = TRUE)
    return(out[match(my_sorted_indexes$sortedBiofeatures,rownames(out)),
                       match(my_sorted_indexes$sortedSamplesets,colnames(out))])
  } else out

}


#' create a module based on provided biological features
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (gene_names)
#' @param normalization A string - either 'limma' (default),'tpm' or legacy as normalization
#' @param rank A string ('magnitude' as default)
#' @param top_n A numeric - an integer for selecting the top ranked samplesets
#' @param sorted A logical (FALSE as default) - it returns a sorted index for both bf and ss
#' @param useIds A logical (FALSE as default) - It allows using biofeatureIds
#'
#' @return A SummarizedExperiment object
#' @export
#'
#' @examples
#'\dontrun{
#' my_bf <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110")
#' tmp <- get_biofeature_id(id_In = my_bf, useIds = F)$id
#' my_mod <- create_module_bf(biofeaturesNames= tmp,
#' normalization = "limma", top_n = 15, useIds = T)
#' }
create_module_bf <- function(compendium = "vespucci",
                             biofeaturesNames=NULL,
                             normalization = "limma",
                             rank = "magnitude",
                             top_n = 50,
                             sorted = FALSE,
                             useIds = FALSE) {
  if(is.null(biofeaturesNames)) stop("You need to provide biofeaturesNames")
  if(useIds) biofeaturesIds <- biofeaturesNames
  else biofeaturesIds <- get_biofeature_id(id_In=biofeaturesNames, useIds = useIds)$id
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  samplesetIds <- get_samplesets_ranking(biofeaturesNames = biofeaturesIds, normalization = normalization, rank = rank, rankTarget = "samplesets", top_n = top_n, useIds = useIds)$id
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues,
      biofeatures {
          edges {
                node {
                  id
                 }
          }
      },
       sampleSets {
          edges {
                node {
                  id
                }
          }
        }
    }
  }')
  # cat(my_query,"\n")
  tmp <- build_query(my_query)$modules
  nv <- t(as.data.frame(sapply(tmp$normalizedValues, unlist)))
  rownames(nv) <- as.character(sapply(tmp$biofeatures, unlist))
  colnames(nv) <- as.character(sapply(tmp$sampleSets, unlist))
  ss_tmp <- get_sampleset_id(normalization = normalization,
                             id_In = colnames(nv), useIds = T)
  ssData <- ss_tmp[match(colnames(nv),ss_tmp$id),]
  rownames(ssData) <- ssData$id
  bf_tmp <- get_biofeature_id(id_In = rownames(nv), useIds = T)
  bfData <- bf_tmp[match(rownames(nv),bf_tmp$id),]
  rownames(bfData) <- bfData$id
  out <- SummarizedExperiment::SummarizedExperiment(assays=list(counts=nv),
                                             rowData=DataFrame(bfData[,2:3]),
                                             colData=DataFrame(ssData[,2:3]))
  if(sorted){
    my_sorted_indexes <- plot_module_heatmap(module = out,
                                             normalization = normalization,
                                             plot = FALSE,
                                             sorted = TRUE)
    return(out[match(my_sorted_indexes$sortedBiofeatures,rownames(out)),
               match(my_sorted_indexes$sortedSamplesets,colnames(out))])
  } else out
}

#' create a module based on provided sample sets
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string - either 'limma' (default),'tpm' or legacy as normalization
#' @param samplesetNames A character vector (sampleset names)
#' @param rank A string ('magnitude' as default) - use \code{\link{get_ranking}}
#' @param top_n A numeric - an integer for selecting the top ranked samplesets
#' @param sorted A logical (FALSE as default) - it returns a sorted index for both bf and ss
#' @param useIds A logical (FALSE as default) - It allows using samplesetIds
#'
#' @return A SummarizedExperiment object
#' @export
#'
#' @examples
#'\dontrun{
#' my_ids=c("U2FtcGxlU2V0VHlwZTo2NDE5","U2FtcGxlU2V0VHlwZToyMTg2OA==")
#' mod_ss <- create_module_ss(samplesetNames = my_ids,
#' normalization = "limma", top_n = 15, useIds = TRUE)
#' }
create_module_ss <- function(compendium = "vespucci",
                             samplesetNames = NULL,
                             normalization = "limma",
                             rank = "uncentered_correlation",
                             top_n = 50,
                             sorted = FALSE,
                             useIds = FALSE){
  if(is.null(samplesetNames)) stop("You need to provide samplesetNames")
  if(useIds) samplesetIds <- samplesetNames
  else samplesetIds <- get_sampleset_id(id_In = samplesetNames,
                                        normalization = normalization,
                                        useIds = useIds)$id
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetIds, normalization = normalization, rank = rank, rankTarget = "biofeatures", top_n = top_n, useIds = useIds)$id
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues,
      biofeatures {
          edges {
                node {
                  id
                 }
          }
      },
       sampleSets {
          edges {
                node {
                  id
                }
          }
        }
    }
  }')
  # cat(my_query, "\n")
  tmp <- build_query(my_query)$modules
  nv <- t(as.data.frame(sapply(tmp$normalizedValues, unlist)))
  rownames(nv) <- as.character(sapply(tmp$biofeatures, unlist))
  colnames(nv) <- as.character(sapply(tmp$sampleSets, unlist))
  ss_tmp <- get_sampleset_id(normalization = normalization,
                             id_In = colnames(nv), useIds = T)
  ssData <- ss_tmp[match(colnames(nv),ss_tmp$id),]
  rownames(ssData) <- ssData$id
  bf_tmp <- get_biofeature_id(id_In = rownames(nv), useIds = T)
  bfData <- bf_tmp[match(rownames(nv),bf_tmp$id),]
  rownames(bfData) <- bfData$id
  out <- SummarizedExperiment::SummarizedExperiment(assays=list(counts=nv),
                                             rowData=DataFrame(bfData[,2:3]),
                                             colData=DataFrame(ssData[,2:3]))
  if(sorted){
    my_sorted_indexes <- plot_module_heatmap(module = out,
                                             normalization = normalization,
                                             plot = FALSE,
                                             sorted = TRUE)
    return(out[match(my_sorted_indexes$sortedBiofeatures,rownames(out)),
               match(my_sorted_indexes$sortedSamplesets,colnames(out))])
  } else out
}

#' describe a module
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma' (default),'tpm' or legacy as normalization
#'
#' @return a list of three: "originalIds", "termShortName" and "samples"
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#' 'VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030','VIT_00s0505g00060'
#' ,'VIT_00s0873g00020','VIT_00s0904g00010')
#' module_1 <- create_module(biofeaturesNames=gene_names, normalization = "limma")
#' d_module <- describe_module(module = module_1, normalization = "limma")
#'}
describe_module <- function(compendium = "vespucci",
                                module = NULL,
                                normalization = "limma"){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm") version <- "2.0"
  else version <- "legacy"

  samplesetIds <- colnames(module)
  biofeaturesIds <- rownames(module)
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
    samplesDescriptionSummary {
                        category
                        details {
                            originalId
                            termShortName
                            samples {
                                id
                            }
                        }
    }
    }
  }')
  # cat(my_query, "\n")
  sds <- build_query(my_query)$modules$samplesDescriptionSummary
  sds

  categories <- purrr::modify_depth(sds,1,"category")
  details <- purrr::modify_depth(sds,1,"details") #lenght(details) 4
  my_det<- list()
  for (i in 1:length(details)){
    my_det[[i]] <- details[[i]]
  }


  originalId <- termShortName <- samples <- NULL
  oId <- list(); tSN <-list(); ss <- list()
  for (j in 1:length(my_det)){
    oId[[j]] <- unlist(rlist::list.map(my_det[[j]], originalId))
    tSN[[j]] <- unlist(rlist::list.map(my_det[[j]], termShortName))
    ss[[j]] <- rlist::list.map(my_det[[j]], samples)
  }
  tryCatch(
    {
      names(oId) <- unlist(categories)
      names(tSN) <- unlist(categories)
      names(ss) <- unlist(categories)
      return(list(originalIds = oId, termShortName = tSN, samples = ss))
    },
    error=function(error_message) {
      message(error_message)
      names(oId) <- unlist(categories)[1:3]
      names(tSN) <- unlist(categories)[1:3]
      names(ss) <- unlist(categories)[1:3]
      return(list(originalIds = oId, termShortName = tSN, samples = ss))
    }
  )
}


#' show the enrichment for ontology terms for both sampleSets and biofeatures
#'
#' @param compendium A string - the selected compendium
#' @param module A matrix or SummarizedExperiment with valid rownames (biofeatureNames) and colnames (samplesetsNames)
#' @param normalization A string - either 'limma' (default),'tpm' or legacy as normalization
#'
#' @return a list with two data.frame (PlantOntology and GeneOntology)
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#' 'VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030','VIT_00s0505g00060'
#' ,'VIT_00s0873g00020','VIT_00s0904g00010')
#' module_1 <- create_module(biofeaturesNames=gene_names, normalization = "limma")
#' enrich_module(module = module_1, normalization = "limma")
#'}
enrich_module <- function(compendium = "vespucci",
                            module = NULL,
                            normalization = "limma"){
  if (is.null(module)) stop ("Provide a module.")
  if (is.null(normalization)) stop ("Normalization has to be either 'limma','tpm' or 'legacy'.")
  else if (normalization == "limma" | normalization == "tpm") version <- "2.0"
  else version <- "legacy"

  samplesetIds <- colnames(module)
  biofeaturesIds <- rownames(module)
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      samplesetAnnotationEnrichment(corrPValueCutoff: 0.05) {
        ontology
        ontologyTerm {
          ontologyId
          description
          pValue
        }
      }
      biofeatureAnnotationEnrichment(corrPValueCutoff: 0.05) {
        ontology
        ontologyTerm {
          ontologyId
          description
          pValue
        }
      }
    }
  }')
  # cat(my_query, "\n")
  enrichment <- build_query(my_query)$modules
  samplesetAnnotationEnrichment <- lapply(enrichment$samplesetAnnotationEnrichment[[1]],unlist)
  PO <- tryCatch(data.frame(t(matrix(samplesetAnnotationEnrichment$ontologyTerm, nrow = 3,
                            ncol = length(samplesetAnnotationEnrichment$ontologyTerm)/3,
                            byrow = F))),
                 error = function(e) {return(GO = NULL)})
  if(!is.null(PO))  colnames(PO) <- c("ontologyId", "description", "pValue")
  else {}

  biofeatureAnnotationEnrichment <- lapply(enrichment$biofeatureAnnotationEnrichment[[1]],unlist)
  GO <- tryCatch(data.frame(t(matrix(biofeatureAnnotationEnrichment$ontologyTerm, nrow = 3,
                            ncol = length(biofeatureAnnotationEnrichment$ontologyTerm)/3,
                            byrow = F))),
                 error = function(e) {return(GO = NULL)})
  if(!is.null(GO))  colnames(GO) <- c("ontologyId", "description", "pValue")
  else {}

  list(PlantOntology = PO, GeneOntology = GO)
}
