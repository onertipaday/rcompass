#' Get all available compendia in COMPASS
#'
#' @return a list with info e version information
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
  tmp <- build_query2(my_query)$compendia[[1]]
  list(info = c(name =  tmp$name,
                fullName =  tmp$fullName,
                description =  tmp$description),
       versions = sapply(tmp$versions, unlist))
}


#' Get compendium data sources
#'
#' @param compendium A string- the selected compendium
#'
#' @return A vector of character strings containing the available data sources
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
  tmp <- t(as.data.frame(sapply(build_query2(my_query)$dataSources$edges,unlist)))
  colnames(tmp) <-  c("id","sourceName"); rownames(tmp) <-  NULL
  tmp
}


#' Get information about all available platforms for the selected compendium,
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string- the selected compendium
#'
#' @return a data.frame with five columns: accessId, name, description, source, type
#'
#' @export
#'
#' @examples
#' info <- get_platform_information()
#' dplyr::count(info,type, source)
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
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$platforms$edges, unlist)))
  colnames(tmp) <-  c("accessId","name", "description","source","type")
  rownames(tmp) <-  NULL
  tmp

}


#' get available platform types
#'
#' @param compendium A string- the selected compendium
#'
#' @return A vector of character strings
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
  as.character(sapply(build_query2(my_query)$platformTypes$edges, unlist))
}


#' get_sample_info
#'
#' @param compendium A string- the selected compendium
#' @param sampleName A string- GEO Sample accession id (GSM)
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
            platformAccessId
            platformName
          }
          reporterPlatform{
            platformAccessId
            platformName
          }
        }
      }
    }

  }')
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("sampleId","sampleName",
                      "experimentId","experimentAccessId","experimentName",
                      "platformAccessId","platformName",
                      "reporterPlatformId","reporterPlatformName")
  rownames(tmp) <-  NULL
  tmp
}

#' Get experiment Accessids and name
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string- the selected compendium
#' @param sampleName A string- if NULL(default) returns all available experiments ids
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
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$experiments$edges, unlist)))
  colnames(tmp) <-  c("experimentAccessId","experimentName")
  rownames(tmp) <-  NULL
  tmp
}


#' get annotations for n samples from the selected compendium
#'
#' @param compendium A string- the selected compendium
#' @param n an integer: number of sample to retrieve (default 10)
#'
#' @return a data.frame with three columns: sampleId sampleName annotation
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
    tmp <- build_query(my_query)$sampleAnnotations$edges$node
    data.frame (sampleName = tmp$sample$sampleName,
                sampleId = tmp$sample$id,
                annotation = tmp$annotation)
    tmp <- as.data.frame(t(sapply(build_query2(my_query)$sampleAnnotations$edges, unlist)))
    colnames(tmp) <-  c("sampleId ","sampleName","annotation")
    rownames(tmp) <-  NULL
    tmp
}

#' get_samples_by_gse
#'
#' @param compendium A string- the selected compendium
#' @param experiment_ExperimentAccessId A string- GSE (GEO Series (experiment) access id)
#'
#' @return a data.frame with three columns id, name, description
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
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("id", "name", "description")
  rownames(tmp) <-  NULL
  tmp
}

#' get_sample_by_gsm
#'
#' @param compendium A string- the selected compendium
#' @param sampleName_Icontains A string- GSM (GEO Sample access id)
#'
#' @return a data.frame with three columns: id, name, description
#' @export
#'
#' @examples
#' get_sample_by_gsm(sampleName_Icontains="GSM1313535")
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
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("id", "name", "description")
  rownames(tmp) <-  NULL
  tmp
}

#' Get all samples measured with a given Platform
#'
#' Get all available samples for the selected compendium,
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string- the selected compendium
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
                    experimentAccessId
                    experimentName
                    description
                  }
                  platform {
                    id
                    platformAccessId
                    platformName
                  }
                  reporterPlatform {
                    id
                    platformAccessId
                    platformName
                    }
                  }

                }
        }
}')
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("id","name","description",
                      "experimentId","experimentAccessId","experimentName","experimentDescription",
                      "platformId","platformAccessId","platformName",
                      "reporterPlatformId","reporterPlatformAccessId","reporterPlatformName")
  rownames(tmp) <-  NULL
  tmp
}

#' get_sampleset_id
#'
#' @param compendium A string- the selected compendium
#' @param name A string- the sampleset of interest
#' @param name_In A vector of character strings - the sampleset name
#' @param version A string- either 'latest' or 'legacy'
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
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$sampleSets$edges, unlist)))
  colnames(tmp) <-  c("id","name"); rownames(tmp) <-  NULL
  tmp
}

#' get_samples_annotation_triples
#'
#' @param compendium A string- the selected compendium
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
  # as.character(build_query(my_query)$annotationPrettyPrint$rdfTriples)
  tmp <- as.data.frame(t(sapply(build_query2(my_query)$annotationPrettyPrint$rdfTriples, unlist)))
  colnames(tmp) <-  c("locusTag","Type","alternative"); rownames(tmp) <-  NULL
  tmp
}


#' get_sparql_annotation_triples
#'
#' @param compendium A string- the selected compendium
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
#' @param compendium A string- the selected compendium
#' @param version A string- either (default) 'latest' or '2.0'
#'
#' @return A vector of character strings with the available normalization methods
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
      name
    }
  }
 }
}')
  as.character(sapply(build_query2(my_query)$normalizations$edges, unlist))
}

#' get_ontology_structure
#'
#' @param compendium character - the selected compendium
#' @param name character - the name of the ontology of interest
#'
#' @return a character
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
  build_query2(my_query)$ontology$edges

  #TODO - it kills Rstudio!
}


#' Get all ontologies for
#'
#' @param compendium A string- the selected compendium
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
#' @param compendium A string- the selected compendium
#' @param name A stringthe biofeature of interest
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
#' @param compendium A string- the selected compendium
#' @param name A string- the biofeature (here gene) name
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
#' @param compendium A string- the selected compendium
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
#' @param compendium A string- the selected compendium
#' @param name_In A vector of character strings - the biofeature names
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
#' @param compendium A string- the selected compendium
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
  build_query2(my_query)$scoreRankMethods
}


#' get_samplesets_ranking
#'
#' @param compendium A string- the selected compendium
#' @param rank A string('magnitude' as default)
#' @param version A string('legacy' as default)
#' @param biofeaturesNames A vector of character strings (here gene_names)
#' @param biofeaturesIds A vector of character strings - the biofeature ids
#' @param top_n A numeric- an integer for selecting the top ranked samplesets
#' @param rankTarget A string('sampleset' as default)
#'
#' @return a data.frame with four character vectors (id,name,type,value)
#' @export
#'
#' @examples
#' my_ids <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' get_samplesets_ranking(biofeaturesNames = my_ids, top_n = 10)
#' get_samplesets_ranking(biofeaturesIds=c("QmlvRmVhdHVyZVR5cGU6MQ==","QmlvRmVhdHVyZVR5cGU6Mg==",
#'  "QmlvRmVhdHVyZVR5cGU6Mw==","QmlvRmVhdHVyZVR5cGU6NA==","QmlvRmVhdHVyZVR5cGU6NQ=="), top_n = 10)
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
#' @param compendium A string- the selected compendium
#' @param samplesetIds A vector of character strings (here gene_names)
#' @param samplesetNames A vector of character strings (here sampleset names)
#' @param version A string('legacy' as default)
#' @param rank A string('magnitude' as default) - use \code{\link{get_ranking}}
#' @param top_n A numeric- an integer for selecting the top ranked samplesets
#' @param rankTarget A string('biofeature' as default)
#' for the available values
#'
#' @return a list with four character vectors (id,name,type,value)
#' @export
#'
#' @examples
#' junk <- get_biofeature_ranking(samplesetIds = c("U2FtcGxlU2V0VHlwZTo0OTY2",
#' "U2FtcGxlU2V0VHlwZToyNDgy", "U2FtcGxlU2V0VHlwZTo4NzQ="), top_n = 10)
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






