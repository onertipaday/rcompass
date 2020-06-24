#' create module providing both biological features and sample sets
#'
#' @param compendium A string - the selected compendium
#' @param version A string ('legacy' as default)
#' @param biofeaturesNames A character vector (gene_names)
#' @param samplesetNames A character vector (sampleset names)
#'
#' @return A data.frame - the module
#' @export
#'
#' @examples
#'\dontrun{
#' gene_names <- c('VIT_00s0246g00220','VIT_00s0332g00060','VIT_00s0332g00110',
#' 'VIT_00s0332g00160','VIT_00s0396g00010','VIT_00s0505g00030',
#' 'VIT_00s0505g00060','VIT_00s0873g00020','VIT_00s0904g00010')
#' mod_bf <- create_module(biofeaturesNames = gene_names)
#' my_bf <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' my_ss <- c("GSM671720.ch1-vs-GSM671719.ch1","GSM671721.ch1-vs-GSM671719.ch1"
#' ,"GSM671722.ch1-vs-GSM671719.ch1","GSM147672.ch1-vs-GSM147690.ch1")
#' my_mod <- create_module(biofeaturesNames = my_bf, samplesetNames = my_ss)
#' pheatmap::pheatmap(my_mod, col = RColorBrewer::brewer.pal(11,name="RdBu"))
#' # compare to vespucci:
#' # url <- "http://vespucci.colombos.fmach.it/cws_data/export_data/colombos_20190828_nEwFDG.txt"
#' # vesp_test=readr::read_tsv(url)
#' }
#'
create_module <- function(compendium = "vespucci",
                          version = "legacy",
                          biofeaturesNames = NULL,
                          samplesetNames = NULL){
  if(all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide at least biofeaturesNames or samplesetsNames")
  else if (is.null(biofeaturesNames)) {
    return(create_module_ss(compendium = compendium,
                            version = version,
                            samplesetNames =  samplesetNames))
  }
  else if (is.null(samplesetNames)) {
    return(create_module_bf(compendium = compendium,
                            version = version,
                            biofeaturesNames = biofeaturesNames))
  }
  else {
    biofeaturesIds <- get_biofeature_id(name_In = biofeaturesNames)$id
    samplesetIds <- get_sampleset_id(name_In = samplesetNames)$id
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
  }
  nv <- t(as.data.frame(sapply(build_query(my_query)$modules$normalizedValues, unlist)))
  rownames(nv) <- biofeaturesNames
  colnames(nv) <- samplesetNames
  nv
}


#' create_module_bf
#'
#' @param compendium A string - the selected compendium
#' @param biofeaturesNames A character vector (gene_names)
#' @param version A string ('legacy' as default)
#' @param rank A string ('magnitude' as default)
#'b
#' @return A data.frame - the module
#' @export
#'
#' @examples
#' my_bf <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110"
#' ,"VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060"
#' ,"VIT_00s0873g00020","VIT_00s0904g00010")
#' mod_bf <- create_module_bf(biofeaturesNames=my_bf, version = "legacy")
create_module_bf <- function(compendium = "vespucci",
                             biofeaturesNames=NULL,
                             version = "legacy",
                             rank = "magnitude") {
  if(is.null(biofeaturesNames)) stop("You need to provide biofeaturesNames")
  biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
  # samplesetIds <- get_samplesets_ranking(compendium =  compendium,
  #                                        biofeaturesNames = NULL,
  #                                        biofeaturesIds = biofeaturesIds,
  #                                        version = version,
  #                                        rank = rank,
  #                                        top_n = top_n)$id
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"]), {
      normalizedValues
       sampleSets {
          edges {
                node {
                  name
                }
          }
        }
    }
  }')
  tmp <- build_query(my_query)$modules
  nv <- t(as.data.frame(sapply(tmp$normalizedValues, unlist)))
  rownames(nv) <- biofeaturesNames
  colnames(nv) <- as.character(sapply(tmp$sampleSets, unlist))
  nv
}


#' create_module_ss
#'
#' @param compendium A string - the selected compendium
#' @param version A string ('legacy' as default)
#' @param samplesetNames A character vector (sampleset names)
#' @param rank A string ('magnitude' as default) - use \code{\link{get_ranking}}
#'
#' @return A data.frame - the module
#' @export
#'
#' @examples
#' my_ss <- c("GSM671720.ch1-vs-GSM671719.ch1","GSM671721.ch1-vs-GSM671719.ch1"
#' ,"GSM671722.ch1-vs-GSM671719.ch1","GSM147672.ch1-vs-GSM147690.ch1")
#' mod_ss <- create_module_ss(samplesetNames = my_ss)
create_module_ss <- function(compendium = "vespucci",
                             samplesetNames = NULL,
                             version = "legacy",
                             rank = "uncentered_correlation"){
  if(is.null(samplesetNames)) stop("You need to provide samplesetNames")
  samplesetIds <- get_sampleset_id(name_In=samplesetNames)$id
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues
       biofeatures {
          edges {
                node {
                  name
                }
          }
        }
    }
  }')
  tmp <- build_query(my_query)$modules
  nv <- t(as.data.frame(sapply(tmp$normalizedValues, unlist)))
  rownames(nv) <- as.character(sapply(tmp$biofeatures, unlist))
  colnames(nv) <- samplesetNames
  nv
}


#' Union of two modules
#'
#' @param mod1 first module to merge
#' @param mod2 second module to merge
#'
#' @return A data.frame - the module
#' @export
#'
#' @examples
#' my_ss <- c("GSM671720.ch1-vs-GSM671719.ch1","GSM671721.ch1-vs-GSM671719.ch1"
#' ,"GSM671722.ch1-vs-GSM671719.ch1","GSM147672.ch1-vs-GSM147690.ch1")
#' mod_ss <- create_module_ss(samplesetNames = my_ss)
#' my_bf <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110"
#' ,"VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060"
#' ,"VIT_00s0873g00020","VIT_00s0904g00010")
#' mod_bf <- create_module_bf(biofeaturesNames= my_bf, version = "legacy")
#' mod_union <- merge_modules(mod1 = mod_ss, mod2 = mod_bf)
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
