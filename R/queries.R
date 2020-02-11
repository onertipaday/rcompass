#' Get all available compendia
#'
#' @return a list with each element containing name, fullName, descritpion and normaliaziotion for all available compendia.
#' @export
#'
#' @example
#' get_compendia()
get_compendia <- function(){
  my_query <- '{
    compendia {
  	  name
      fullName
      description
      normalization
	  }
  }'
  parze(cont(do_POST(base_url, my_query)))$data$compendia
}


#' Get compendium data sources
#'
#' @param compendium a character - the selected compendium
#'
#' @return a character vector containing the available data sources
#' @export
#'
#' @example
#' get_compendium_data_source()
get_compendium_data_source <- function(compendium="vitis_vinifera"){
  my_query <- '{
  dataSources(compendium: "vitis_vinifera") {
    edges {
      node {
        sourceName
      }
    }
  }
}'
  parze(cont(do_POST(base_url, my_query)))$data$dataSources$edges$node$sourceName
}


#' Get all available platforms for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame with 4 columns:
#' platformAccessId, platformName, description, dataSource, platformType
#' @export
#'
#' @example
#' get_platforms()
get_platforms <- function(compendium="vitis_vinifera"){
  my_query <- paste0('{
  platforms(compendium:\"', compendium, '\") {
        edges {
                node {
                        platformAccessId,
                        platformName,
                        description,
                        dataSource {
                                sourceName
                        },
                        platformType {
                                name
                        }
                }
        }
  }
}')
  parze(cont(do_POST(base_url, my_query)))$data$platforms$edges$node
}


#' get available platform types
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_platform_types()
get_platform_types <- function(compendium="vitis_vinifera"){
  my_query <- paste0('{
  platformTypes(compendium:\"', compendium, '\") {
    edges {
      node {
        name
      }
    }
  }
}')
  parze(cont(do_POST(base_url, my_query)))$data$platformTypes$edges$node
}


#' get_sample_info
#'
#' @param compendium
#' @param sampleName
#'
#' @return
#' @export
#'
#' @example
#' get_sample_info(sampleName = "GSM287866.ch1")
get_sample_info <-function(compendium="vitis_vinifera",
                           sampleName=NULL){
  if(missing(sampleName)){stop("Provide a sampleName.")}
    my_query <- paste0('{
      samples(compendium:\"', compendium, '\", sampleName:\"', sampleName,'\") {
      edges{
        node{
          id
          sampleName
          experiment{
            id
            experimentAccessId
            experimentName
          }
          platform{
            platformName
          }
          reporterPlatform{
            platformName
            platformAccessId
          }
        }
      }
    }

  }')
  parze(cont(do_POST(base_url, my_query)))$data$samples$edges$node
}

#' Get experiment Accessids and name
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#' @param sampleName a character - if NULL(default) returns all available experiments ids
#' for the selected compendium
#' @return a data.frame with experimentAccessId and EsperimentName
#' @export
#'
#' @examples
#' get_experiments(sampleName="GSM671721")
get_experiments <- function(compendium="vitis_vinifera",
                            sampleName=NULL){
  if(is.null(sampleName)){
    my_query <- paste0('{
      experiments(compendium:\"', compendium, '\") {
      edges {
        node {
          experimentAccessId,
          experimentName
        }
      }
    }
  }')}
  else{
    sampleId <- get_sample_info(sampleName = sampleName)
    my_query <- paste0('{
    experiments(compendium:\"', compendium, '\", id:\"', sampleId$id,'\") {
      edges {
        node {
          experimentAccessId,
          experimentName
        }
      }
    }
  }')
  }
  parze(cont(do_POST(base_url, my_query)))[[1]]$experiments$edges$node
}



#' get annotations for n samples from the selected compendium
#'
#' @param compendium a character - the selected compendium
#' @param n an integer: number of sample to retrieve (default 10)
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_sample_annotation(n=25)
get_sample_annotation <- function(compendium="vitis_vinifera", n=10){
    my_query <- paste0('{
    sampleAnnotations(compendium:\"', compendium, '\", first: ', n,') {
        edges {
          node {
            sample {
              id,
              sampleName
            },
            annotation {
              ontologyNode {
                originalId,
                ontology {
                  name
                }
              }
              value
            }
          }
        }
      }
    }')
  tmp <- parze(cont(do_POST(base_url, my_query)))$data$sampleAnnotations$edges$node
  data.frame(sampleName = tmp$sample$sampleName,
             sampleId = tmp$sample$id,
             ontology = tmp$annotation$ontologyNode)
}


#' Get all samples measuread with a given Platform
#'
#' Get all available samples for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#' @param platform_PlatformAccessId a character
#' @return a data.frame
#' @export
#'
get_samples <- function(compendium = "vitis_vinifera",
                        platformAccessId = "Vitis_Vinifera_Affy"){
  my_query <- paste0('{
 samples(compendium:\"', compendium, '\", platformAccessId:\"', platform_PlatformAccessId,'\") {
        edges {
                node {
                  id
                  sampleName
                  description
                  experiment {
                    id
                    organism
                    experimentAccessId
                    experimentName
                    scientificPaperRef
                    description
                  }
                  platform {
                    id
                    platformAccessId
                    platformName
                    description
                  }
                  reporterPlatform {
                    id
                    platformAccessId
                    platformName
                    description
                    }
                  }

                }
        }
}')
  parze(cont(do_POST(base_url, my_query)))$data$samples$edges$node
}

#' get_sampleset_id_by_name
#'
#' @param compendium a character - the selected compendium
#' @param name a character the sampleset of interest
#' @param ontology a character
#'
#' @return a character
#' @export
#'
#' @examples
#' get_sampleset_id_by_name()
get_sampleset_id_by_name <- function(compendium="vitis_vinifera",
                                name = "GSM786264.ch1-vs-GSM786258.ch1"){
  my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", name:\"', name, '\") {
  edges {
    node {
      id,
      name
    }
  }
 }
}')
  parze(cont(do_POST(base_url, my_query)))$data$sampleSets$edges$node$id
}

# TODO

#' Get all ontologies for
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame
#' @export
#'
get_ontologies <- function(compendium="vitis_vinifera"){
  my_query <- paste0('{

   ontology(compendium:\"', compendium, '\"){
    edges{
      node{
        id
        originalFilename
        name
        description
        creationDate
        isBiofeature
        isSample
      }
    }
  }
}')
  parze(cont(do_POST(base_url, my_query)))[[1]]$ontology$edges$node
}


# TODO - aggiungere i controlli se non vengono inserite le stringhe

#' get_biofeature_info
#'
#' @param compendium a character - the selected compendium
#' @param name a character the biofeature of interest
#' @param ontology a character
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_biofeature_info()
get_biofeature_info <- function(compendium="vitis_vinifera",
                                name = "VIT_00s0332g00060",
                                ontology="Gene ontology"){
  my_query <- paste0('{
    biofeatureAnnotations(compendium:\"', compendium, '\",
                        bioFeature_Name:\"', name, '\",
                        annotationValue_OntologyNode_Ontology_Name:\"',ontology, '\") {
    edges {
      node {
        annotationValue {
          ontologyNode {
            originalId,
            ontology {
              name
              id
            }
          }
        }
      }
    }
  }
}')
parze(cont(do_POST(base_url, my_query)))$data$biofeatureAnnotations$edges$node$annotationValue$ontologyNode
}


#' get biofeature sequence by name
#'
#' @param compendium a character - the selected compendium
#' @param name a character - the biofeature name
#' @param field the biofeature field of interest  (sequence as default)
#'
#' @return a character
#' @export
#'
get_biofeature_by_name <- function(compendium="vitis_vinifera",
                                   name="VIT_00s0332g00060",
                                   field="sequence"){
  my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\",
                        name:\"',name, '\") {
    edges {
      node {
        biofeaturevaluesSet(bioFeatureField_Name:\"', field, '\") {
    edges {
      node {
        name,
        id
        description
      }
    }
  }
}')
  tmp <- as.character(unlist(parze(cont(do_POST(base_url, my_query)))$data$biofeatures$edges))
  names(tmp) <- name
  tmp
}


#' get_biofeature_id
#'
#' @param compendium a character - the selected compendium
#' @param name a character - the biofeature name
#' @param field the biofeature field of interest  (sequence as default)
#'
#' @return a list
#' @export
#'
#' @examples
#' get_biofeature_id()
get_biofeature_id <- function(compendium="vitis_vinifera",
                                   name="VIT_00s0332g00060"){
  my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\",
                        name:\"',name, '\") {
    edges {
      node {
        name
        id
        description
            }
          }
        }
      }')
  tmp <- as.character(unlist(parze(cont(do_POST(base_url, my_query)))$data$biofeatures$edges))
  names(tmp) <- c("name", "id", "description")
  tmp
}

#' get all the biofeatures (genes here) which include the selected term within the annotations values
#'
#' @param compendium a character - the selected compendium
#' @param term a character
#'
#' @return a character vector
#' @export
#'
#' @example
#' get_biofeature_by_ann_term()
#'
get_biofeature_by_ann_term <- function(compendium="vitis_vinifera",
                                term="GO:0006260"){
  my_query <- paste0('{
  biofeatureAnnotations(compendium:\"', compendium, '\",
                        annotationValue_OntologyNode_OriginalId:\"',term, '\") {
    edges {
      node {
        bioFeature {
          name
        }
      }
    }
  }
  }')
  gsub("<br>",";", parze(cont(do_POST(base_url, my_query)))$data$biofeatureAnnotations$edges$node$bioFeature$name)
}


# TODO

# GET ontology structure
# '{
#   ontology(compendium:"vitis_vinifera", name:"Gene ontology") {
#     edges {
#       node {
#         structure
#       }
#     }
#   }
# }'


#' get samples related to a specific annotation term
#'
#' @param compendium a character - the selected compendium
#' @param term a character
#'
#' @return a data.frame
#' @export
#'
get_sample_by_ann_term <- function(compendium="vitis_vinifera",
                                term = "GROWTH.GREENHOUSE"){
  my_query <- paste0('{
  sampleAnnotations(compendium:\"', compendium, '\", annotationValue_OntologyNode_OriginalId:\"', term, '\") {
    edges {
      node {
        sample {
          sampleName,
          experiment {
            experimentAccessId
          }
        }
      }
    }
  }
}')
parze(cont(do_POST(base_url, my_query)))$data$sampleAnnotations$edges
}

#' create module with biological features and sample sets
#'
#' @param compendium a character - the selected compendium
#' @param biofeaturesIds a character vector
#' @param samplesetIds a character vector
#'
#' @return a data.frame
#'
#' @export
#'
#' @examples
#'\dontrun{
#' my_mod <- create_module(biofeatureNames=c("VIT_00s0332g00110","VIT_00s0332g00160",
#' "VIT_00s0396g00010","VIT_00s0505g00030"),
#' samplesetNames=c("E-MTAB-1514.US_30_2.ch1-vs-E-MTAB-1514.US_03_1.ch1",
#' "E-MTAB-1514.US_03_3.ch1-vs-E-MTAB-1514.US_03_1.ch1"))
#' pheatmap::pheatmap(my_module,col = RcolorBrewer::brewer.pal(11,name="RdBu"))
#' # compare to vespucci:
#' # url <- "http://vespucci.colombos.fmach.it/cws_data/export_data/colombos_20190828_nEwFDG.txt"
#' # vesp_test=readr::read_tsv(url)
#' }
#'
create_module <- function(compendium = "vitis_vinifera",
                          biofeatureNames = NULL,
                          samplesetNames = NULL){
  if(is.null(biofeatureNames) & is.null(samplesetNames)) stop("You need to provide at least biofeaturesNames or samplesetsNames")
  else if (is.null(biofeatureNames)) {
    out <- create_module_ss(compendium = compendium, samplesetNames =  samplesetNames)
  }
  else if (is.null(samplesetNames)) {
    out <- create_module_bf(compendium = compendium, biofeatureNames = biofeatureNames)
  }
  else {
    biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)[2]))
    samplesetIds <- sapply(samplesetNames,function(x) paste(get_sampleset_id_by_name(name=x)))
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
    tmp <- parze(cont(do_POST(base_url, my_query)))$data
    out <- data.frame(tmp$modules$normalizedValues, row.names = biofeatureNames)
    colnames(out) <- samplesetNames
    }
  out
}

# ---

get_samplesets_ranking <- function(compendium="vitis_vinifera",
                                   biofeatureNames=c("VIT_00s0332g00110","VIT_00s0332g00160",
                                                     "VIT_00s0396g00010","VIT_00s0505g00030"),
                                   normalization = "legacy",
                                   rank = "magnitude"){
  if(is.null(biofeatureNames)) stop("You need to provide biofeaturesNames")
  biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)[2]))
  rank_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    normalization:\"', normalization, '\",
    rank:\"', rank, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"]) {
      id
      name
      }
}')
  # id <- parze(cont(do_POST(base_url, rank_query)))$data$ranking$id
  tmp <- parze(cont(do_POST(base_url, rank_query)))$data$ranking
  # out <- data.frame(id=tmp$id,name=tmp$name,value=tmp$value)
  out <- data.frame(id=tmp$id,name=tmp$name)
}

# create_module_bf()
create_module_bf <- function(compendium="vitis_vinifera",
                          biofeatureNames=c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060","VIT_00s0873g00020","VIT_00s0904g00010"),
                          normalization = "legacy",
                          rank = "magnitude"){
  if(is.null(biofeatureNames)) stop("You need to provide biofeaturesNames")
  biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)[2]))
  samplesetIds <- get_samplesets_ranking(compendium =  compendium,
                                         biofeatureNames = biofeatureNames,
                                         normalization = normalization,
                                         rank =rank)
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds[[1]], collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
  tmp <- parze2(cont(do_POST(base_url, my_query)))$data$modules$normalizedValues
  out <- as.data.frame(t(sapply(tmp, function(x) as.numeric(as.character(x)))))
  rownames(out) <- biofeatureNames
  colnames(out) <- samplesetIds[[2]]
  out
}

# ---
# get_biofeature_ranking()
get_biofeature_ranking <- function(compendium="vitis_vinifera",
                                   samplesetIds=c("U2FtcGxlU2V0VHlwZToxMjYy",
                                                  "U2FtcGxlU2V0VHlwZToxMjYw"),
                                   normalization = "legacy",
                                   rank = "std"){
  if(is.null(samplesetIds)) stop("You need to provide samplesetIds")
  rank_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    normalization:\"', normalization, '\",
    rank:\"', rank, '\",
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
      id
      name
      }
}')
  tmp <- parze(cont(do_POST(base_url, rank_query)))$data$ranking
  out <- data.frame(id=tmp$id, name=tmp$name)
}

# create_module_ss()
create_module_ss <- function(compendium="vitis_vinifera",
                             samplesetNames=c("E-MTAB-1514.US_30_2.ch1-vs-E-MTAB-1514.US_03_1.ch1","E-MTAB-1514.US_03_3.ch1-vs-E-MTAB-1514.US_03_1.ch1"),
                             normalization = "legacy",
                             rank = "std"){
  if(is.null(samplesetNames)) stop("You need to provide samplesetNames")
  samplesetIds <- sapply(samplesetNames,function(x) paste(get_sampleset_id_by_name(name=x)))
  biofeaturesIds  <- get_biofeature_ranking(compendium =  compendium,
                                            samplesetIds = samplesetIds,
                                            normalization = normalization,
                                            rank =rank)
  my_query <- paste0('{
    modules(compendium:\"', compendium, '\",
    biofeaturesIds:["', paste0(biofeaturesIds[[1]], collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
  tmp <- parze2(cont(do_POST(base_url, my_query)))$data$modules$normalizedValues
  out <- as.data.frame(t(sapply(tmp, function(x) as.numeric(as.character(x)))))
  rownames(out) <- biofeaturesIds[[2]]; colnames(out) <- samplesetNames
  out
}

# ---


#' Union of two modules
#'
#' @param mod1
#' @param mod2
#'
#' @return
#' @export
#'
#' @examples
#' #mod1 <- create_module_ss()
#' #mod2 <- create_module_bf()
#' #mod12_union <- merge_modules(mod1,mod2)
merge_modules <- function(mod1, mod2){
  create_module(biofeatureNames = c(rownames(mod1),rownames(mod2)),
                samplesetNames = c(colnames(mod1),colnames(mod2)))
}

intersect_modules <- function(mod1, mod2){
  create_module(biofeatureNames = intersect(rownames(mod1),rownames(mod2)),
                samplesetNames = intersect(colnames(mod1),colnames(mod2)))
}

settdiff_modules <- function(mod1, mod2){
  create_module(biofeatureNames = setdiff(rownames(mod1),rownames(mod2)),
                samplesetNames = setdiff(colnames(mod1),colnames(mod2)))
}




