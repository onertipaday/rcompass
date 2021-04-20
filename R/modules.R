#' create a module providing both biological features and sample sets
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string ('legacy' as default)
#' @param biofeaturesNames A character vector (gene_names)
#' @param samplesetNames A character vector (sampleset names)
#' @param useIds A logical (FALSE as default) - It allows using biofeatureIds
#'
#' @return A matrix - the module
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110",
#' "VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030",
#' "VIT_00s0505g00060","VIT_00s0873g00020","VIT_00s0904g00010")
#' mod_bf <- create_module(biofeaturesNames = gene_names)
#' my_bf <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' my_ss <- c("GSM671720.ch1-vs-GSM671719.ch1","GSM671721.ch1-vs-GSM671719.ch1"
#' ,"GSM671722.ch1-vs-GSM671719.ch1","GSM147672.ch1-vs-GSM147690.ch1")
#' my_mod <- create_module(biofeaturesNames = my_bf, samplesetNames = my_ss)
#' pheatmap::pheatmap(na.omit(my_mod), col = RColorBrewer::brewer.pal(11,name="RdBu"))
#' }
create_module <- function(compendium = "vespucci",
                          normalization = "legacy",
                          biofeaturesNames = NULL,
                          samplesetNames = NULL,
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
  nv
}


#' create a module based on provided biological features
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (gene_names)
#' @param normalization A string ('legacy' as default)
#' @param rank A string ('magnitude' as default)
#' @param useIds A logical (FALSE as default) - It allows using biofeatureIds
#'b
#' @return A matrix - the module
#' @export
#'
#' @examples
#'\dontrun{
#' my_bf <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110"
#' ,"VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060"
#' ,"VIT_00s0873g00020","VIT_00s0904g00010")
#' mod_bf <- create_module_bf(biofeaturesNames=my_bf, normalization = "legacy")
#' }
create_module_bf <- function(compendium = "vespucci",
                             biofeaturesNames=NULL,
                             normalization = "legacy",
                             rank = "magnitude",
                             useIds = FALSE) {
  if(is.null(biofeaturesNames)) stop("You need to provide biofeaturesNames")
  if(useIds) biofeaturesIds <- biofeaturesNames
  else biofeaturesIds <- get_biofeature_id(id_In=biofeaturesNames)$id
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")

  samplesetIds <- get_samplesets_ranking(biofeaturesNames = biofeaturesNames, normalization = normalization, rank = rank, rankTarget = "samplesets", useIds = useIds)$id
  # print(samplesetIds)
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
  tmp <- build_query(my_query)$modules
  nv <- t(as.data.frame(sapply(tmp$normalizedValues, unlist)))
  #rownames(nv) <- biofeaturesNames
  #colnames(nv) <- samplesetIds
  rownames(nv) <- as.character(sapply(tmp$biofeatures, unlist))
  colnames(nv) <- as.character(sapply(tmp$sampleSets, unlist))
  nv
}

#' create a module based on provided sample sets
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string ('legacy' as default)
#' @param samplesetNames A character vector (sampleset names)
#' @param rank A string ('magnitude' as default) - use \code{\link{get_ranking}}
#' @param useIds A logical (FALSE as default) - It allows using samplesetIds
#'
#' @return A matrix - the module
#' @export
#'
#' @examples
#'\dontrun{
#' my_ss <- c("GSM671720.ch1-vs-GSM671719.ch1","GSM671721.ch1-vs-GSM671719.ch1"
#' ,"GSM671722.ch1-vs-GSM671719.ch1","GSM147672.ch1-vs-GSM147690.ch1")
#' mod_ss <- create_module_ss(samplesetNames = my_ss)
#' }
create_module_ss <- function(compendium = "vespucci",
                             samplesetNames = NULL,
                             normalization = "legacy",
                             rank = "uncentered_correlation",
                             useIds = FALSE){
  if(is.null(samplesetNames)) stop("You need to provide samplesetNames")
  if(useIds) samplesetIds <- samplesetNames
  else samplesetIds <- get_sampleset_id(name_In=samplesetNames)$id
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")

  biofeaturesIds <- get_biofeature_ranking(samplesetNames = samplesetNames, normalization = normalization, rank = rank, rankTarget = "biofeatures", useIds = useIds)$id
  print(biofeaturesIds)
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
  tmp <- build_query(my_query)$modules
  nv <- t(as.data.frame(sapply(tmp$normalizedValues, unlist)))
  rownames(nv) <- as.character(sapply(tmp$biofeatures, unlist))
  colnames(nv) <- as.character(sapply(tmp$sampleSets, unlist))
  nv
}


#' Union of two modules
#'
#' @param mod1 first module to merge
#' @param mod2 second module to merge
#'
#' @return A matrix - the module
#' @export
#'
#' @examples
#'\dontrun{
#' my_ss <- c("GSM671720.ch1-vs-GSM671719.ch1","GSM671721.ch1-vs-GSM671719.ch1"
#' ,"GSM671722.ch1-vs-GSM671719.ch1","GSM147672.ch1-vs-GSM147690.ch1")
#' mod_ss <- create_module_ss(samplesetNames = my_ss, normalization = "legacy")
#' my_bf <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110"
#' ,"VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060"
#' ,"VIT_00s0873g00020","VIT_00s0904g00010")
#' mod_bf <- create_module_bf(biofeaturesNames= my_bf, normalization = "legacy")
#' mod_union <- merge_modules(mod1 = mod_ss, mod2 = mod_bf)
#' }
merge_modules <- function(mod1, mod2){
  create_module(biofeaturesNames = c(rownames(mod1),rownames(mod2)),
                samplesetNames = c(colnames(mod1),colnames(mod2)))
}


# -----------------------------------------------------------------------------
### TODO
#
#

# intersect_modules <- function(mod1, mod2){
#   create_module(biofeaturesNames = intersect(rownames(mod1),rownames(mod2)),
#                 samplesetNames = intersect(colnames(mod1),colnames(mod2)))
# }
#
# settdiff_modules <- function(mod1, mod2){
#   create_module(biofeaturesNames = setdiff(rownames(mod1),rownames(mod2)),
#                 samplesetNames = setdiff(colnames(mod1),colnames(mod2)))
# }
