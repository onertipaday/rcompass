#' Get all available compendia
#'
#' @return a list
#' @export
#'
#' @examples
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
  out <- build_query(my_query)$compendia
  list(info = c(name = out$name,
                fullName = out$fullName,
                description = out$description),
       versions=out$versions[[1]])
}


#' Get compendium data sources
#'
#' @param compendium string - the selected compendium
#'
#' @return a character vector containing the available data sources
#' @export
#'
#' @examples
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
  build_query(my_query)$dataSources$edges$node$sourceName
}


#' Get information about all available platforms for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium string - the selected compendium
#'
#' @return a data.frame with five columns:
#' platformAccessId, platformName, description, dataSource, platformType
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
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
  # build_query(my_query)$platforms$edges$node %>%
  #   mutate(dataSource=dataSource$sourceName,
  #          platformType=platformType$name)
  tmp <- build_query(my_query)$platforms$edges$node
  data.frame(dataSource=tmp$dataSource$sourceName,
              platformType=tmp$platformType$name)
}


#' get available platform types
#'
#' @param compendium string - the selected compendium
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
  build_query(my_query)$platformTypes$edges$node
}


#' get_sample_info
#'
#' @param compendium string - the selected compendium
#' @param sampleName string - GEO Sample accession id (GSM)
#'
#' @return
#' @export
#'
#' @examples
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
  # build_query(my_query)$samples$edges$node %>%
  #   dplyr::transmute( id,
  #                     sampleName,
  #                     experimentId = experiment$id,
  #     experimentAccessId = experiment$experimentAccessId,
  #     experimentName = experiment$experimentName,
  #     platformName = reporterPlatform$platformName,
  #     platformAccessId = reporterPlatform$platformAccessId)
  tmp <- build_query(my_query)$samples$edges$node
  data.frame( id = tmp$id,
              sampleName = tmp$sampleName,
              experimentId = tmp$experiment$id,
              experimentAccessId = tmp$experiment$experimentAccessId,
              experimentName = tmp$experiment$experimentName,
              platformName = tmp$reporterPlatform$platformName,
              platformAccessId = tmp$reporterPlatform$platformAccessId)
}

#' Get experiment Accessids and name
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium string - the selected compendium
#' @param sampleName string - if NULL(default) returns all available experiments ids
#' for the selected compendium
#' @return a data.frame with experimentAccessId and EsperimentName
#' @export
#'
#' @examples
#' get_experiments()
#' get_experiments(sampleName="GSM671721.ch1")
get_experiments <- function(compendium = "vespucci",
                            sampleName=NULL){
  if(is.null(sampleName)){
    my_query <- paste0('{
      experiments(compendium:\"', compendium, '\") {')}
  else{
    sampleId <- get_sample_info(sampleName = sampleName)
    my_query <- paste0('{
    experiments(compendium:\"', compendium, '\", id:\"', sampleId$id,'\") {')
  }
  my_query <- paste0(my_query,'
        edges {
        node {
          experimentAccessId,
          experimentName
        }
      }
    }
  }')
  build_query(my_query)$experiments$edges$node
}


#' get annotations for n samples from the selected compendium
#'
#' @param compendium string - the selected compendium
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
    # build_query(my_query)$sampleAnnotations$edges$node %>%
    #   transmute(sampleName = sample$sampleName,
    #          sampleId = sample$id,
    #          annotation)
    tmp <- build_query(my_query)$sampleAnnotations$edges$node
    data.frame (sampleName = tmp$sample$sampleName,
                sampleId = tmp$sample$id,
                annotation = tmp$annotation)
}

#' get_samples_by_gse
#'
#' @param compendium string - the selected compendium
#' @param experiment_ExperimentAccessId string - GSE (GEO Series (experiment) access id)
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
  build_query(my_query)$samples$edges$node
}

#' get_sample_by_gsm
#'
#' @param compendium string - the selected compendium
#' @param sampleName_Icontains string - GSM (GEO Sample access id)
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
  build_query(my_query)$samples$edges$node
}

#' Get all samples measured with a given Platform
#'
#' Get all available samples for the selected compendium,
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium string - the selected compendium
#' @param platformAccessId string
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_samples()
get_samples <- function(compendium = "vespucci",
                        platform_PlatformAccessId = "GPL11004"){
  my_query <- paste0('{
 samples(compendium:\"', compendium, '\", platform_PlatformAccessId:\"', platform_PlatformAccessId,'\") {
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
  build_query(my_query)$samples$edges$node
}

#' get_sampleset_id
#'
#' @param compendium string - the selected compendium
#' @param name string - the sampleset of interest
#' @param name_In a character vector - the sampleset name
#' @param version string - either 'latest' or 'legacy'
#'
#' @return a data.frame with two columns: id and name
#' @export
#'
#' @examples
#' get_sampleset_id(name_In = "GSE27180_48hours-1-vs-GSE27180_0h-2")
#' get_sampleset_id(samples =c("U2FtcGxlVHlwZTox"))
get_sampleset_id <- function(compendium = "vespucci",
                             version = "latest",
                             samples = NULL,
                             name_In = NULL){
  if(all(c(samples, name_In) %in% NULL)) stop("Enter either a name_In (name_In = 'GSE27180_48hours-1-vs-GSE27180_0h-2') or a sample id (e.g. samples = 'U2FtcGxlVHlwZTox')")
  if(is.null(name_In)){
    my_query <- paste0('query {
    sampleSets(compendium:\"', compendium, '\", samples:\"', samples, '\") {')
  }
  else if(is.null(samples)){
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", name_In:\"', name_In, '\") {')
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
  build_query(my_query)$sampleSets$edges$node
}

#' get_samples_annotation_triples
#'
#' @param compendium string - the selected compendium
#' @param ids string
#'
#' @return
#' @export
#'
#' @examples
#' get_samples_annotation_triples()
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
#' @param compendium string - the selected compendium
#' @param ids string
#'
#' @return
#' @export
#'
#' @examples
#' get_sparql_annotation_triples()
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


#' get_available_normalization
#'
#' @param compendium string - the selected compendium
#' @param version string - either (default) 'latest' or '2.0'
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_available_normalization()
get_available_normalization <- function(compendium='vespucci',
                                   version='latest'){
  my_query <- paste0('{
  normalizations(compendium:\"', compendium, '\", version:\"', version, '\") {
  edges {
    node {
      name,
      date
    }
  }
 }
}')
  build_query(my_query)$normalizations$edges$node
}

#' get_ontology_structure
#'
#' @param compendium string - the selected compendium
#' @param name string - the name of the ontolgy of interest
#'
#' @return string
#' @export
#'
#' @examples
#' get_ontology_structure()
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
  build_query(my_query)$ontology$edges$node
}


#' Get all ontologies for
#'
#' @param compendium string - the selected compendium
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
  build_query(my_query)$ontology$edges$node
}


#' get_biofeature_annotations
#'
#' @param compendium string - the selected compendium
#' @param name string the biofeature of interest
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
build_query(my_query)$biofeatureAnnotations$edges$node
}


#' get biofeature sequence by name
#'
#' @param compendium string - the selected compendium
#' @param name string - the biofeature (here gene) name
#' @param field the biofeature field of interest ('sequence' as default)
#'
#' @return a data.frame with three columns: name,id and value
#' @export
#'
#' @examples
#' get_biofeature_by_name(name_In = "VIT_00s0332g00060")
#' get_biofeature_by_name(name_In = c("VIT_00s0332g00060", "VIT_00s0246g00220", "VIT_00s0332g00160"))
get_biofeature_by_name <- function(compendium = "vespucci",
                                   name_In=NULL,
                                   field="sequence"){
  if(is.null(name_In)) stop("Provide name_In (e.g. name_In = c('VIT_00s0332g00060', 'VIT_00s0332g00160')")
  my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\",
                        name_In:\"',paste0(name_In, collapse =","), '\") {
              edges {
                node {
                  biofeaturevaluesSet(bioFeatureField_Name:\"', field, '\") {
                  edges {
                    node {
                      id,
                      value
                    }
                  }
                }
              }
            }
          }
  }')
  tmp <- sapply(build_query(my_query)$biofeatures$edges$node$biofeaturevaluesSet$edges,
                unlist)
  data.frame(name = name_In,id = tmp[1,], value = tmp[2,])
}


#' get_biofeature_annotation_rdf
#'
#' @param compendium string - the selected compendium
#' @param ids unique biologicafeature annotation from \code{\link{get_biofeature_by_name}}
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_biofeature_annotation_rdf(ids="QmlvRmVhdHVyZVR5cGU6Mg==")
get_biofeature_annotation_rdf <- function(compendium = "vespucci",
                                   ids="QmlvRmVhdHVyZVR5cGU6Mg=="){
  if(is.null(ids)) stop("Provide ids (e.g. ids = 'QmlvRmVhdHVyZVR5cGU6Mg==')")
  my_query <- paste0('{
  annotationPrettyPrint(compendium:\"', compendium, '\", ids:\"',ids, '\") {
        rdfTriples
          }
  }')
  build_query(my_query)$annotationPrettyPrint$rdfTriples
}

#' get_biofeature_id
#'
#' @param compendium string - the selected compendium
#' @param name_In a character vector - the biofeature names
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' my_genes <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' get_biofeature_id(name_In = my_genes)
get_biofeature_id <- function(compendium = "vespucci",
                              name_In = NULL){
  if(is.null(name_In)) stop("Provide name_In (e.g. name_In = c('VIT_00s0332g00060', 'VIT_00s0332g00160')")
  my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\", name_In:\"',paste0(name_In, collapse =","), '\") {
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


#' get_ranking
#'
#' @param compendium string - the selected compendium
#'
#' @return a list with sampleSets and biologicalFeatures ranking available methods
#' @export
#'
#' @examples
#' get_ranking()
get_ranking <- function(compendium = "vespucci"){
  if(is.null(compendium)) stop("Provide the compendium id.")
  my_query <- paste0('{
  scoreRankMethods(compendium:\"', compendium, '\"){
        sampleSets,
        biologicalFeatures
      }
  }')
  build_query(my_query)$scoreRankMethods
}


#' get_samplesets_ranking
#'
#' @param compendium string - the selected compendium
#' @param rank string ('magnitude' as default)
#' @param version string ('legacy' as default)
#' @param biofeaturesNames a character vector (here gene_names)
#' @param biofeaturesIds a character vector - the biofeature ids
#' @param top_n a numeric - an integer for selecting the top ranked samplesets
#' @param rankTarget string ('sampleset' as default)
#'
#' @return a data.frame with four character vectors (id,name,type,value)
#' @export
#'
#' @examples
#' my_ids <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' get_samplesets_ranking(biofeaturesNames = my_ids, top_n = 10)
#' get_samplesets_ranking(biofeaturesIds=c("QmlvRmVhdHVyZVR5cGU6MQ==","QmlvRmVhdHVyZVR5cGU6Mg==", "QmlvRmVhdHVyZVR5cGU6Mw==","QmlvRmVhdHVyZVR5cGU6NA==","QmlvRmVhdHVyZVR5cGU6NQ=="), top_n = 10)
get_samplesets_ranking <- function(compendium = "vespucci",
                                   version = "legacy",
                                   rankTarget = "samplesets",
                                   rank = "magnitude",
                                   biofeaturesNames=NULL,
                                   biofeaturesIds=NULL,
                                   top_n = 50){
  if(is.null(biofeaturesNames) & is.null(biofeaturesIds)) stop("provide either biofeaturesNames XOR biofeatureIds")
  else if(is.null(biofeaturesIds)) {
    biofeaturesIds <- get_biofeature_id(name_In=biofeaturesNames)$id
  }
  my_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    version:\"', version, '\",
    rank:\"', rank, '\",
    rankTarget:\"', rankTarget, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"]) {
      id,
      name,
      value
      }
  }')
  as.data.frame(build_query(my_query)$ranking)[1:top_n,]
}


#' get_biofeature_ranking()
#'
#' @param compendium string - the selected compendium
#' @param samplesetIds a character vector (here gene_names)
#' @param samplesetNames a character vector (here sampleset names)
#' @param version string ('legacy' as default)
#' @param rank string ('magnitude' as default) - use \code{\link{get_ranking}}
#' @param top_n a numeric - an integer for selecting the top ranked samplesets
#' @param rankTarget string ('biofeature' as default)
#' for the available values
#'
#' @return a list with four character vectors (id,name,type,value)
#' @export
#'
#' @examples
#' junk <- get_biofeature_ranking(samplesetIds = c("U2FtcGxlU2V0VHlwZTo0OTY2", "U2FtcGxlU2V0VHlwZToyNDgy", "U2FtcGxlU2V0VHlwZTo4NzQ="), top_n = 10)
get_biofeature_ranking <- function(compendium = "vespucci",
                                   samplesetIds = NULL,
                                   samplesetNames = NULL,
                                   version = "legacy",
                                   rank = "uncentered_correlation",
                                   rankTarget = "biofeatures",
                                   top_n = 50){
  if(is.null(samplesetNames) & is.null(samplesetIds)) stop("provide either samplesetNames XOR samplesetIds")
  else if(is.null(samplesetIds)) {
    samplesetIds <- get_biofeature_id(name_In=samplesetNames)$id
  }
  my_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    version:\"', version, '\",
    rank:\"', rank, '\",
    rankTarget:\"', rankTarget, '\",
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
      id,
      name,
      value
      }
}')
  as.data.frame(build_query(my_query)$ranking)[1:top_n,]
}






