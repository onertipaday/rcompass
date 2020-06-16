#' Get all available compendia
#'
#' @return a list with each element containing name, fullName, description and normalization for all available compendia.
#' @export
#'
#' @example
#' get_available_compendia()
get_available_compendia <- function(){
  my_query <- '{
    compendia {
      name,
      fullName,
      description
      versions {
        versionNumber,
        databases {
          name,
          normalizations
        }
      }
    }
  }'
  # parze(cont(do_POST(base_url, my_query)))$data$compendia
  out <- build_query(my_query)$compendia
  list(info = c(name = out$name,
                fullName = out$fullName,
                description = out$description),
       versions=out$versions[[1]])
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
get_compendium_data_source <- function(compendium = "vespucci"){
  my_query <- '{
  dataSources(compendium: "vespucci") {
    edges {
      node {
        id,
        sourceName
      }
    }
  }
}'
  #parze(cont(do_POST(base_url, my_query)))$data$dataSources$edges$node$sourceName
  build_query(my_query)$dataSources$edges$node$sourceName
}


#' Get information about all available platforms for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame with five columns:
#' platformAccessId, platformName, description, dataSource, platformType
#'
#' @importFrom dplyr mutate
#' @export
#'
#' @example
#' info <- get_platform_information()
#' require(tidyverse)
#' info %>% count(platformType, dataSource)
get_platform_information <- function(compendium = "vespucci"){
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
  # parze(cont(do_POST(base_url, my_query)))$data$platforms$edges$node
  build_query(my_query)$platforms$edges$node %>%
    mutate(dataSource=dataSource$sourceName,
           platformType=platformType$name)
  # tmp <- build_query(my_query)$platforms$edges$node
  # data.frame( platformAccessId = tmp$platformAccessId,
  #             platformName = tmp$platformName,
  #             description = tmp$description,
  #             dataSource = tmp$dataSource$sourceName,
  #             platformType = tmp$platformType$name)
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
get_platform_types <- function(compendium = "vespucci"){
  my_query <- paste0('{
  platformTypes(compendium:\"', compendium, '\") {
    edges {
      node {
        name
      }
    }
  }
}')
  # parze(cont(do_POST(base_url, my_query)))$data$platformTypes$edges$node
  build_query(my_query)$platformTypes$edges$node
}


#' get_sample_info
#'
#' @param compendium
#' @param sampleName
#'
#' @return
#' @importFrom magrittr "%>%"
#' @export
#'
#' @example
#' get_sample_info(sampleName = "GSM287866.ch1")
get_sample_info <-function(compendium = "vespucci",
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
  # parze(cont(do_POST(base_url, my_query)))$data$samples$edges$node
  build_query(my_query)$samples$edges$node %>%
    dplyr::transmute( id,
                      sampleName,
                      experimentId = experiment$id,
      experimentAccessId = experiment$experimentAccessId,
      experimentName = experiment$experimentName,
      platformName = reporterPlatform$platformName,
      platformAccessId = reporterPlatform$platformAccessId)
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
#' get_experiments(sampleName="GSM671721.ch1")
get_experiments <- function(compendium = "vespucci",
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
  # parze(cont(do_POST(base_url, my_query)))[[1]]$experiments$edges$node
  build_query(my_query)$experiments$edges$node
}



#' get annotations for n samples from the selected compendium
#'
#' @param compendium a character - the selected compendium
#' @param n an integer: number of sample to retrieve (default 10)
#'
#' @return a data.frame
#' @importFrom dplyr transmute
#' @export
#'
#' @examples
#' get_sample_annotation(n=25)
get_sample_annotation <- function(compendium = "vespucci", n = 10){
    my_query <- paste0('{
    sampleAnnotations(compendium:\"', compendium, '\", first: ', n,') {
        edges {
          node {
            sample {
              id,
              sampleName
            },
            annotation
          }
        }
      }
    }')
  # tmp <- parze(cont(do_POST(base_url, my_query)))$data$sampleAnnotations$edges$node
    build_query(my_query)$sampleAnnotations$edges$node %>%
      transmute(sampleName = sample$sampleName,
             sampleId = sample$id,
             annotation)
  # tmp <- build_query(my_query)$sampleAnnotations$edges$node
  # data.frame(sampleName = tmp$sample$sampleName,
  #            sampleId = tmp$sample$id,
  #            annotation = tmp$annotation)
}

#' get_samples_by_gse
#'
#' @param compendium a character - the selected compendium
#' @param experiment_ExperimentAccessId a character - GSE (GEO Series (experiment) access id)
#'
#' @return a data.frame with three columns id, sampleName, description
#' @export
#'
#' @examples
#' get_samples_by_gse()
get_samples_by_gse <- function(compendium = "vespucci",
                               experiment_ExperimentAccessId="GSE54347"){
  my_query <- paste0('{
  samples(compendium:\"', compendium, '\", experiment_ExperimentAccessId:\"', experiment_ExperimentAccessId, '\") {
    edges {
      node {
      id,
      sampleName,
      description
        }
      }
    }
  }')
  # parze(cont(do_POST(base_url, my_query)))$data$sampleSets$edges$node$id
  build_query(my_query)$samples$edges$node
}

#' get_sample_by_gsm
#'
#' @param compendium a character - the selected compendium
#' @param sampleName_Icontains a character - GSM (GEO Sample access id)
#'
#' @return a data.frame with three columns: id, sampleName, description
#' @export
#'
#' @examples
#' get_sample_by_gsm()
get_sample_by_gsm <- function(compendium = "vespucci",
                              sampleName_Icontains="GSM1313535"){
  my_query <- paste0('{
  samples(compendium:\"', compendium, '\", sampleName_Icontains:\"', sampleName_Icontains, '\") {
    edges {
      node {
      id,
      sampleName,
      description
        }
      }
    }
  }')
  # parze(cont(do_POST(base_url, my_query)))$data$sampleSets$edges$node$id
  build_query(my_query)$samples$edges$node
}

#' Get all samples measuread with a given Platform
#'
#' Get all available samples for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#' @param platformAccessId a character
#' @return a data.frame
#' @export
#'
get_samples <- function(compendium = "vespucci",
                        platform_PlatformAccessId = "GPL11004"){
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
  # parze(cont(do_POST(base_url, my_query)))$data$samples$edges$node
  build_query(my_query)$samples$edges$node
}

#' get_sampleset_id
#'
#' @param compendium a character - the selected compendium
#' @param samples a character - unique id for the sampleset
#' @param name a character - the sampleset of interest
#'
#' @return a data.frame eith two columns: id and name
#' @export
#'
#' @examples
#' get_sampleset_id(name = "GSE27180_48hours-1-vs-GSE27180_0h-2")
#' get_sampleset_id(sample ="U2FtcGxlVHlwZTox")
get_sampleset_id <- function(compendium = "vespucci",
                                name = NULL,
                                samples = NULL){
  if(all(c(name, sample) %in% NULL)) stop("Enter either a name (name = 'GSE27180_48hours-1-vs-GSE27180_0h-2') or a sample id (e.g. sample = 'U2FtcGxlVHlwZTox')")
  if(is.null(name)){
    my_query <- paste0('query {
    sampleSets(compendium:\"', compendium, '\", samples:\"', samples, '\") {')
  }
  else if(is.null(samples)){
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", name:\"', name, '\") {')
  }
  my_query <- paste0(my_query,'
    edges {
      node {
        id,
        name
        }
      }
    }
  }')
  # parze(cont(do_POST(base_url, my_query)))$data$sampleSets$edges$node$id
  build_query(my_query)$sampleSets$edges$node
}

#' get_samples_annotation_triples
#'
#' @param compendium a character - the selected compendium
#' @param ids a character
#'
#' @return
#' @export
#'
#' @examples
get_samples_annotation_triples <- function(compendium = "vespucci",
                                           ids="QmlvRmVhdHVyZVR5cGU6MQ=="){
  my_query <- paste0('{
  annotationPrettyPrint(compendium:\"', compendium, '\", ids:\"', ids, '\") {
  rdfTriples
    }
  }')
  as.character(build_query(my_query)$annotationPrettyPrint$rdfTriples)
}


#' get_sparql_annotation_triples
#'
#' @param compendium a character - the selected compendium
#' @param ids a character
#'
#' @return
#' @export
#'
#' @examples
get_sparql_annotation_triples <- function(compendium = "vespucci",
                                           target="sample",
                                           query="SELECT ?s ?p ?o WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.obolibrary.org/obo/NCIT_C19157> . ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.obolibrary.org/obo/PO_0009010>}"){
  my_query <- paste0('{
  sparql(compendium:\"', compendium, '\", target:\"',target,'\", query:\"', query, '\") {
        rdfTriples
        }
  }')
  as.character(build_query(my_query)$sparql$rdfTriples)
}

# TODO - still not working


#' get_ontology_structure
#'
#' @param compendium a character - the selected compendium
#' @param name a character - the name of the ontolgy of interest
#'
#' @return a character
#' @export
#'
#' @examples
get_ontology_structure <- function(compendium='vespucci',
                                   name='Gene ontology'){
  my_query <- paste0('{
  ontology(compendium:\"', compendium, '\", name:\"', name, '\") {
  edges {
    node {
      structure
    }
  }
 }
}')
  # build_query(my_query)$ontology$edges$node$structure
  build_query(my_query)$ontology$edges$node
}



#' Get all ontologies for
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame
#' @export
#'
get_ontologies <- function(compendium = "vespucci"){
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
  # parze(cont(do_POST(base_url, my_query)))[[1]]$ontology$edges$node
  build_query(my_query)$ontology$edges$node
}


# TODO - aggiungere i controlli se non vengono inserite le stringhe

#' get_biofeature_annotations
#'
#' @param compendium a character - the selected compendium
#' @param name a character the biofeature of interest
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_biofeature_annotations(name = "VIT_00s0332g00060")
get_biofeature_annotations <- function(compendium = "vespucci",
                                name = "VIT_00s0332g00060"){
  my_query <- paste0('{
    biofeatureAnnotations(compendium:\"', compendium, '\", bioFeature_Name:\"', name, '\") {
    edges {
      node {
        id
        annotation
        bioFeature {
          		  id
        }
      }
    }
  }
}')
# parze(cont(do_POST(base_url, my_query)))$data$biofeatureAnnotations$edges$node$annotationValue$ontologyNode
build_query(my_query)$biofeatureAnnotations$edges$node
}


#' get biofeature sequence by name
#'
#' @param compendium a character - the selected compendium
#' @param name a character - the biofeature (here gene) name
#' @param field the biofeature field of interest ('sequence' as default)
#'
#' @return a data.frame with two columns: value for the requested field and unique biofeature id
#' @export
#'
#' @examples
#' get_biofeature_by_name(name = "VIT_00s0332g00060")
get_biofeature_by_name <- function(compendium = "vespucci",
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
                      value,
                      id
                    }
                  }
                }
              }
            }
          }
  }')
  # tmp <- as.character(unlist(parze(cont(do_POST(base_url, my_query)))$data$biofeatures$edges))
  # names(tmp) <- name
  # tmp
  build_query(my_query)$biofeatures$edges$node$biofeaturevaluesSet$edges[[1]]$node
}


#' get_biofeature_annotation_rdf
#'
#' @param compendium a character - the selected compendium
#' @param ids unique biologicafeature annotation from \code{\link{get_biofeature_by_name()}}
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_biofeature_annotation_rdf(ids="QmlvRmVhdHVyZVR5cGU6Mg==")
get_biofeature_annotation_rdf <- function(compendium = "vespucci",
                                   ids="QmlvRmVhdHVyZVR5cGU6Mg=="){
  my_query <- paste0('{
  annotationPrettyPrint(compendium:\"', compendium, '\", ids:\"',ids, '\") {
        rdfTriples
          }
  }')
  build_query(my_query)$annotationPrettyPrint$rdfTriples
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
get_biofeature_id <- function(compendium = "vespucci",
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
    build_query(my_query)$biofeatures$edges$node
}

# GET ontology structure
# '{
#   ontology(compendium:"vespucci", name:"Gene ontology") {
#     edges {
#       node {
#         structure
#       }
#     }
#   }
# }'


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
#' pheatmap::pheatmap(my_mod,col = RcolorBrewer::brewer.pal(11,name="RdBu"))
#' # compare to vespucci:
#' # url <- "http://vespucci.colombos.fmach.it/cws_data/export_data/colombos_20190828_nEwFDG.txt"
#' # vesp_test=readr::read_tsv(url)
#' }
#'
create_module <- function(compendium = "vespucci",
                          biofeatureNames = NULL,
                          samplesetNames = NULL){
  if(all(c(biofeatureNames, samplesetNames) %in% NULL)) stop("You need to provide at least biofeaturesNames or samplesetsNames")
  else if (is.null(biofeatureNames)) {
    out <- create_module_ss(compendium = compendium, samplesetNames =  samplesetNames)
  }
  else if (is.null(samplesetNames)) {
    out <- create_module_bf(compendium = compendium, biofeatureNames = biofeatureNames)
  }
  else {
    biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)[2]))
    samplesetIds <- sapply(samplesetNames,function(x) paste(get_sampleset_id(name=x)))
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

#' get_ranking
#'
#' @param compendium a character - the selected compendium
#'
#' @return a list
#' @export
#'
#' @examples
get_ranking <- function(compendium = "vespucci"){
  my_query <- paste0('{
  scoreRankMethods(compendium:\"', compendium, '\"){
        sampleSets,
        biologicalFeatures
      }
  }')
  build_query(my_query)$scoreRankMethods
}

get_samplesets_ranking <- function(compendium = "vespucci",
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
create_module_bf <- function(compendium = "vespucci",
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
get_biofeature_ranking <- function(compendium = "vespucci",
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
create_module_ss <- function(compendium = "vespucci",
                             samplesetNames=c("E-MTAB-1514.US_30_2.ch1-vs-E-MTAB-1514.US_03_1.ch1","E-MTAB-1514.US_03_3.ch1-vs-E-MTAB-1514.US_03_1.ch1"),
                             normalization = "legacy",
                             rank = "std"){
  if(is.null(samplesetNames)) stop("You need to provide samplesetNames")
  samplesetIds <- sapply(samplesetNames,function(x) paste(get_sampleset_id(name=x)))
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




