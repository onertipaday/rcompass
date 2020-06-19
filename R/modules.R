#' create module providing both biological features and sample sets
#'
#' @param compendium string - the selected compendium
#' @param biofeaturesNames a character vector (here gene_names)
#' @param samplesetNames a character vector (here sampleset names)
#' @param version string ('legacy' as default)
#'
#' @return a list with three data.frame
#' @export
#'
#' @examples
#'\dontrun{
#' my_bf <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' my_ss <- c("GSE27180_48hours-1-vs-GSE27180_0h-2","E-MTAB-1514.US_03_3.ch1-vs-E-MTAB-1514.US_03_1.ch1")
#' my_mod <- create_module(biofeaturesNames=my_bf, samplesetNames=my_ss)
#' pheatmap::pheatmap(my_mod,col = RcolorBrewer::brewer.pal(11,name="RdBu"))
#' # compare to vespucci:
#' # url <- "http://vespucci.colombos.fmach.it/cws_data/export_data/colombos_20190828_nEwFDG.txt"
#' # vesp_test=readr::read_tsv(url)
#' }
#'
create_module <- function(compendium = "vespucci",
                          biofeaturesNames = NULL,
                          samplesetNames = NULL,
                          samplesetIds = c("U2FtcGxlU2V0VHlwZTo0OTY2","U2FtcGxlU2V0VHlwZToyNDgy","U2FtcGxlU2V0VHlwZTo4NzQ="), version = "legacy"){
  if(all(c(biofeaturesNames, samplesetNames) %in% NULL)) stop("You need to provide at least biofeaturesNames or samplesetsNames")
  else if (is.null(biofeaturesNames)) {
    return(create_module_ss(compendium = compendium, samplesetNames =  samplesetNames))
  }
  else if (is.null(samplesetNames)) {
    return(create_module_bf(compendium = compendium, biofeaturesNames = biofeaturesNames))
  }
  else {
    biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
    # samplesetIds <- get_sampleset_id(name_In=samplesetNames)$id
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues,
      sampleSets {
    }
  }')
  }
  build_query(my_query)$modules
}



#' create_module_bf
#'
#' @param compendium string - the selected compendium
#' @param biofeaturesNames a character vector (here gene_names)
#' @param version string ('legacy' as default)
#' @param rank string ('magnitude' as default)
#' @param top_n a numeric - an integer for selecting the top ranked sampleSets
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' gene_names <- c("VIT_00s0246g00220","VIT_00s0332g00060","VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060","VIT_00s0873g00020","VIT_00s0904g00010")
#' my_genes <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' mod1 <- create_module_bf(biofeaturesNames=my_genes)
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
    }
  }')
  #build_query(my_query)$modules
  tmp <- build_query2(my_query)$modules
  t(as.data.frame(sapply(tmp$normalizedValues, unlist), col.names = biofeaturesNames))
  # out <- build_query(my_query)$modules
  # data.frame(normalizedValues = out$normalizedValues,
  #            biofeatures = out$biofeatures$edges$node,
  #            sampleSets=out$sampleSets$edges$node)
}


#' create_module_ss
#'
#' @param compendium string - the selected compendium
#' @param version string ('legacy' as default)
#' @param samplesetNames a character vector - the sampleSets names
#' @param rank string ('magnitude' as default) - use \code{\link{get_ranking}}
#'
#' @return a data.frame - the module
#' @export
#'
#' @examples
#' my_ss <- c("E-MTAB-1514.US_30_2.ch1-vs-E-MTAB-1514.US_03_1.ch1","E-MTAB-1514.US_03_3.ch1-vs-E-MTAB-1514.US_03_1.ch1")
#' mod_ss <- create_module_ss(samplesetNames = my_ss)
create_module_ss <- function(compendium = "vespucci",
                             samplesetNames = NULL,
                             samplesetIds = NULL,
                             version = "legacy",
                             rank = "uncentered_correlation"){
  # if(is.null(samplesetNames)) stop("You need to provide samplesetNames")
  # samplesetIds <- get_biofeature_id(name_In=samplesetNames)$id
  # biofeaturesIds  <- get_biofeature_ranking(compendium =  compendium,
  #                                           samplesetIds = samplesetIds,
  #                                           version = version,
  #                                           rank =rank)
  # my_query <- paste0('{
  #   modules(compendium:\"', compendium, '\",
  #   biofeaturesIds:["', paste0(biofeaturesIds[[1]], collapse = '","'),'\"],
  #   samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
  #     normalizedValues
  #   }
  # }')


  my_query <- paste0('{
    modules(compendium:\"', compendium, '\", version:\"', version, '\", samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
  # tmp <- parze2(cont(do_POST(base_url, my_query)))$data$modules$normalizedValues
  # out <- as.data.frame(t(sapply(tmp, function(x) as.numeric(as.character(x)))))
  # rownames(out) <- biofeaturesIds[[2]]; colnames(out) <- samplesetNames
  # out
  build_query(my_query)$modules
}



# -----------------------------------------------------------------------------
### TODO
#
#
#' Union of two modules
#'
#' @param mod1 first module to merge
#' @param mod2 second module to merge
#'
#' @return
#' @export
#'
#' @examples
#' #mod1 <- create_module_ss()
#' #mod2 <- create_module_bf()
#' #mod12_union <- merge_modules(mod1,mod2)
merge_modules <- function(mod1, mod2){
  create_module(biofeaturesNames = c(rownames(mod1),rownames(mod2)),
                samplesetNames = c(colnames(mod1),colnames(mod2)))
}

intersect_modules <- function(mod1, mod2){
  create_module(biofeaturesNames = intersect(rownames(mod1),rownames(mod2)),
                samplesetNames = intersect(colnames(mod1),colnames(mod2)))
}

settdiff_modules <- function(mod1, mod2){
  create_module(biofeaturesNames = setdiff(rownames(mod1),rownames(mod2)),
                samplesetNames = setdiff(colnames(mod1),colnames(mod2)))
}
