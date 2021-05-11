#' get the current version of COMPASS
#'
#' @return A string
#' @export
#'
#' @examples
#' get_compass_version()
get_compass_version <- function(){
  build_query('{ version }')
}


#' Get all available compendia in COMPASS
#'
#' @return a list with info and versions
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
  tmp <- build_query(my_query)$compendia[[1]]
  info <- c(name =  tmp$name,
            fullName =  tmp$fullName,
            description =  tmp$description)
  versions <- list()
    for (i in 1:length(tmp$versions)) {
      versions[[i]] = paste(unlist(tmp$versions[[i]]), collapse =" ")
      }
  list(info = info, versions = unlist(versions))

}


#' Get compendium data sources
#'
#' @param compendium A string - the selected compendium
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
  tmp <- t(as.data.frame(sapply(build_query(my_query)$dataSources$edges,unlist)))
  colnames(tmp) <-  c("id","sourceName"); rownames(tmp) <-  NULL
  tmp
}


#' Get information about all available platforms for the selected compendium,
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string - the selected compendium
#'
#' @return A data.frame with five columns: accessId, name, description, source, type
#'
#' @export
#'
#' @examples
#' info <- get_platform_information()
#' dplyr::count(info, type, source)
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
  tmp <- as.data.frame(t(sapply(build_query(my_query)$platforms$edges, unlist)))
  colnames(tmp) <-  c("accessId","name", "description","source","type")
  rownames(tmp) <-  NULL
  tmp
}


#' get available platform types
#'
#' @param compendium A string - the selected compendium
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
  as.character(sapply(build_query(my_query)$platformTypes$edges, unlist))
}


#' get_samples_info
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string ('limma' as default)
#' @param id_In A character vector of sample ids
#' @param useIds A logical (FALSE as default) - It allows using sampleIds
#'
#' @return A data.frame
#' @export
#'
#' @examples
#'
#'\dontrun{
#' get_samples_info(id_In = c("U2FtcGxlU2V0VHlwZTo2NDE5","U2FtcGxlU2V0VHlwZTo2NDIx"),
#' useIds = TRUE)
#' get_samples_info(id_In = "GSM287866.ch1", useIds = FALSE)
#' }
get_samples_info <-function(compendium = "vespucci",
                            normalization = "limma",
                            id_In = NULL,
                            useIds = FALSE){
  if(missing(id_In))stop("Provide at least one sample id for the selected normalization.")
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(useIds){ my_query <- paste0('{
      samples(compendium:\"', compendium, '\", version:\"', version, '\",
    normalization:\"', normalization, '\", id_In:\"', paste0(id_In, collapse = ','), '\") {')

  } else {
    my_query <- paste0('{
      samples(compendium:\"', compendium, '\", version:\"', version, '\",
    normalization:\"', normalization, '\", sampleName_In:\"', paste0(id_In, collapse = ','), '\") {')
  }
    my_query <- paste0(my_query,'
      edges{
        node{
          id
          sampleName
          description
          experiment{
            id
            experimentAccessId
            experimentName
          }
          platform{
            id
            platformAccessId
            platformName
          }
        }
      }
    }

  }')
  # cat(my_query, "\n")
  tmp <- as.data.frame(t(sapply(build_query(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("sampleId","sampleName","sampleDescription",
                      "experimentId","experimentAccessId","experimentName",
                      "platformId","platformAccessId","platformName")
  rownames(tmp) <-  NULL
  tmp
}

#' Get experimentAccessid and experimentName from sampleName
#'
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string ('limma' as default)
#' @param sampleName A string - if NULL(default) returns all available experiments ids
#' for the selected compendium
#' @return A data.frame with experimentAccessId, esperimentName and description
#' @export
#'
#' @examples
#'\dontrun{
#' get_experiments()
#' get_experiments(sampleName="U2FtcGxlVHlwZTo2NDE5")
#' }
get_experiments <- function(compendium = "vespucci",
                            normalization = "limma",
                            sampleName = NULL){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(sampleName)){
    my_query <- paste0('{
      experiments(compendium:\"', compendium, '\", normalization:\"', normalization, '\") {')}
  else{
    tmp <- get_samples_info(id_In = sampleName, normalization = normalization)
    my_query <- paste0('{
    experiments(compendium:\"', compendium, '\",
    normalization:\"', normalization, '\", id:\"', tmp$experimentId,'\") {')
  }
  my_query <- paste0(my_query,'
        edges {
        node {
          experimentAccessId,
          experimentName
          description
        }
      }
    }
  }')
  # cat(my_query, "\n")
  tmp <- as.data.frame(t(sapply(build_query(my_query)$experiments$edges, unlist)))
  colnames(tmp) <-  c("experimentAccessId","experimentName","description")
  rownames(tmp) <-  NULL
  tmp
}


#' Get experimentAccessid and experimentName from sampleName
#'
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string ('limma' as default)
#' @param sampleSetId A string - if NULL(default) returns all available experiments ids
#' for the selected compendium
#' @return A data.frame with experimentAccessId, esperimentName and description
#' @export
#'
#' @examples
#'\dontrun{
#' ss=c("GSE75498_OS_T0-13-vs-GSE75498_C_T0-21","harvest_4","harvest_5")
#' my_ss <- get_sampleset_id(id_In = ss, normalization = "limma", useIds = FALSE)
#' get_experimentId_by_sampleSetId(sampleSetId = my_ss$id)
#' }
get_experimentId_by_sampleSetId <- function(compendium = "vespucci",
                            normalization = "limma",
                            sampleSetId = NULL){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(sampleSetId)){
    my_query <- paste0('{
      sampleSets(compendium:\"', compendium, '\", normalization:\"', normalization, '\") {')}
  else{
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", normalization:\"', normalization, '\", id_In:\"', paste0(sampleSetId, collapse =","), '\") {')
  }
  my_query <- paste0(my_query,'
    edges {
      node {
      id
      name
        normalizationdesignsampleSet {
          edges {
            node {
              sample {
                experiment {
                  id,
                  experimentAccessId
                }
              }
            }
          }
        }
      }
    }
    }
  }')
  #cat(my_query, "\n")
  tmp <- suppressMessages(bind_rows(lapply(build_query(my_query)$sampleSets$edges, unlist))[,1:4])
  colnames(tmp) <- c("sampleSetId","sampleSetName","experimentId","experimentAccessId")
  tmp
}

#' get annotations for either n samples or selected sampleName
#'
#' @param compendium A string - the selected compendium
#' @param n A numeric (integer): number of sample to retrieve (default 10)
#' @param sampleName A string - A sampleId or sampleName
#' @param useIds A logical (FALSE as default) - It allows using sampleIds
#' @param normalization A string - 'tpm' (as default), 'limma' or 'legacy'
#'
#' @return A data.frame with three columns: sampleId sampleName sampleAnnotation
#' @export
#'
#' @examples
#'\dontrun{
#' get_sample_annotation(n=5)
#' get_sample_annotation(sampleName = "U2FtcGxlVHlwZTo2NDE5", useIds = TRUE)
#' get_sample_annotation(sampleName = "GSM287866.ch1")
#' }
get_sample_annotation <- function(compendium = "vespucci",
                                  n = NULL,
                                  normalization = "limma",
                                  sampleName = NULL,
                                  useIds = FALSE){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(sampleName) && is.null(n)) stop("provide either n (an integer) XOR sampleName.")
  if(!is.null(n)){
    my_query <- paste0('{
      sampleAnnotations(compendium:\"', compendium, '\", normalization:\"', normalization,  '\", first: ', n,') {')}
  else{
    if(useIds) tmp <- sampleName
    else tmp <- get_samples_info(id_In = sampleName, normalization = normalization)$sampleId
    my_query <- paste0('{
    sampleAnnotations(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization,  '\", sampleId:\"', tmp,'\") {')
  }
  my_query <- paste0(my_query,'
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
    # cat(my_query,"\n")
    tmp <- build_query(my_query)$sampleAnnotations$edges$node
    data.frame (sampleName = tmp$sample$sampleName,
                sampleId = tmp$sample$id,
                annotation = tmp$annotation)
    tmp <- as.data.frame(t(sapply(build_query(my_query)$sampleAnnotations$edges, unlist)))
    colnames(tmp) <-  c("sampleId ","sampleName","sampleAnnotation")
    rownames(tmp) <-  NULL
    tmp
}

#' retrieve internal id, name (GSM), description from all samples with experiment id (GSE)
#'
#' @param compendium A string - the selected compendium
#' @param experimentAccessId A string - GSE (GEO Series (experiment) access id)
#' @param normalization A string - 'tpm' (as default), 'limma' or 'legacy'
#'
#' @return A data.frame with three columns: sampleId, sampleName, sampleDescription
#' @export
#'
#' @examples
#'\dontrun{
#' get_samples_by_gse(experimentAccessId = "GSE98923")
#' }
get_samples_by_gse <- function(compendium = "vespucci",
                               normalization = "legacy",
                               experimentAccessId = NULL){
  if(is.null(experimentAccessId)) stop("Provide experimentAccessId (e.g. GSE98923)")
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  my_query <- paste0('{
  samples(compendium:\"', compendium, '\",
  experiment_ExperimentAccessId:\"', experimentAccessId, '\", version:\"', version, '\", normalization:\"', normalization, '\") {
    edges {
      node {
      id,
      sampleName,
      description
        }
      }
    }
  }')
  # cat(my_query,"\n")
  tmp <- as.data.frame(t(sapply(build_query(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("sampleId", "sampleName", "sampleDescription")
  rownames(tmp) <-  NULL
  tmp
}

#' retrieve internal id, name (GSM), description for sample providing sampleName (GSM)
#'
#' @param compendium A string - the selected compendium
#' @param sampleName_In A character5 vector - GSMs (GEO Sample access ids)
#' @param normalization A string - 'tpm' (as default), 'limma' or 'legacy'
#'
#' @return A data.frame with three columns: sampleId, sampleName, sampleDescription
#' @export
#'
#' @examples
#'\dontrun{
#' get_sample_by_gsm(sampleName_In="GSM147672.ch1")
#' }
get_sample_by_gsm <- function(compendium = "vespucci",
                              normalization = "limma",
                              sampleName_In="GSM1313535"){
  if(missing(sampleName_In))stop("Provide at least one sample id for the selected normalization.")
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  my_query <- paste0('{
  samples(compendium:\"', compendium, '\", sampleName_In:\"', paste0(sampleName_In, collapse = ','), '\", version:\"', version, '\", normalization:\"', normalization, '\") {
    edges {
      node {
      id,
      sampleName,
      description
        }
      }
    }
  }')
  tmp <- as.data.frame(t(sapply(build_query(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("sampleId", "sampleName", "sampleDescription")
  rownames(tmp) <-  NULL
  tmp
}

#' Get all samples measured with a given platform
#'
#' Get all available samples for the selected compendium,
#' use \code{\link{get_available_compendia}} to check all the available compendia
#'
#' @param compendium A string - the selected compendium
#' @param platformAccessId A string - A GPL ID
#' @param allSamples A logical (FALSE default)
#'
#' @return A data.frame
#' @export
#'
#' @examples
#'\dontrun{
#' get_samples(platformAccessId = "GPL11004")
#' get_samples(allSamples = TRUE)
#' }
get_samples <- function(compendium = "vespucci",
                        allSamples = FALSE,
                        platformAccessId = NULL){
  if(allSamples & is.null(platformAccessId)){
  my_query <- paste0('{
      samples(compendium:\"', compendium, '\") {
      edges {
        node {
          sampleName,
          description
        }
      }
    }
  }')
  }
  else {}
  my_query <- paste0('{
 samples(compendium:\"', compendium, '\", platform_PlatformAccessId:\"', platformAccessId,'\") {
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
  tmp <- as.data.frame(t(sapply(build_query(my_query)$samples$edges, unlist)))
  colnames(tmp) <-  c("id","name","description",
                      "experimentId","experimentAccessId","experimentName","experimentDescription",
                      "platformId","platformAccessId","platformName",
                      "reporterPlatformId","reporterPlatformAccessId","reporterPlatformName")
  rownames(tmp) <-  NULL
  tmp
}

#' get_annotation_triples
#'
#' @param compendium A string - the selected compendium
#' @param ids A string - unique id of a biofeature, a sample, etc.
#'
#' @return A matrix
#' @export
#'
#' @examples
#' get_annotation_triples(ids = "U2FtcGxlVHlwZTozMjAw")
get_annotation_triples <- function(compendium = "vespucci", ids = NULL) {
  if(is.null(ids)) stop("You need to provide and id")
  my_query <- paste0('{
  annotationPrettyPrint(compendium:\"', compendium, '\", ids:["', paste0(ids, collapse = '","'),'\" ]) {
  rdfTriples
    }
  }')
  # cat(my_query, "\n")
  # build_query(my_query)$annotationPrettyPrint$rdfTriple
  matrix(sapply(build_query(my_query)$annotationPrettyPrint$rdfTriples, unlist), ncol = 3, byrow = TRUE)
}

#' get_sparql_annotation_triples
#'
#' @param compendium A string - the selected compendium
#' @param target A string - either 'sample' or 'biofeature'
#' @param query A string - sparql query
#' @param normalization A string - 'tpm' (as default), 'limma' or 'legacy'
#'
#' @return A data.frame with 3 columns
#' @export
#'
#' @examples
#' my_query=paste0('SELECT ?s ?p ?o WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
#' '<http://purl.obolibrary.org/obo/NCIT_C19157>',
#' '. ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
#' '<http://purl.obolibrary.org/obo/PO_0009010>}')
#' get_sparql_annotation_triples(normalization = "legacy", target = "sample", query = my_query)
get_sparql_annotation_triples <- function(compendium = "vespucci",
                                          normalization = "tpm",
                                           target = NULL,
                                           query = NULL){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(query) | is.null(target)) stop("Provide both a target ('sample' or 'biofeature') AND a proper sparql query!")

  my_query <- paste0('{
  sparql(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\", target:\"',target, '\", query:\"', query, '\") {
        rdfTriples
        }
  }')
  # cat(my_query,"\n")
  triples <- build_query(my_query)
  out <- as.data.frame(t(sapply(triples$sparql$rdfTriples,unlist)))
  # colnames(out) <- c("id", "name")
}

#' get_ids_from_alias
#'
#' @param compendium A string - the selected compendium
#' @param target A string - either sample or biofeature
#' @param alias_names A character vector - the aliases
#'
#' @return A character vector - the ids corresponding to the provided aliases
#' @export
#'
#' @examples
#' my_names <- c('Vv00s0125g00280','Vv00s0187g00140','Vv00s0246g00010',
#' 'Vv00s0246g00080','Vv00s0438g00020','Vv00s0246g00200','Q7M2G6','B9S8R7','B8XIJ8','D7SZ93')
#' get_ids_from_alias(target = "biofeature", alias_names = my_names)
get_ids_from_alias <- function(compendium = "vespucci",
                               target = "biofeature",
                               alias_names = NULL){
  if(is.null(alias_names)) stop("Provide aliases to be converted.")
  body <- "SELECT ?s ?p ?o WHERE { "
  for (i in 1:length(alias_names)){
    body = paste0(body, "{?s <http://purl.obolibrary.org/obo/NCIT_C41095> \'",alias_names[i],"\'} ")
  }
  my_query <- paste0( gsub("\\} \\{"," \\} UNION \\{",body), "}" )
  get_sparql_annotation_triples(compendium = compendium, target = target, query = my_query)[,1]
}


#' get_available_normalization
#'
#' @param compendium A string - the selected compendium
#' @param version A string - either '1.0' or 'legacy', '2.0' (default) or 'latest'
#'
#' @return A vector of character strings with the available normalization methods
#' @export
#'
#' @examples
#' get_available_normalization(version = "legacy")
get_available_normalization <- function(compendium = "vespucci",
                                        version = '2.0'){
  my_query <- paste0('{
  normalizations(compendium:\"', compendium, '\", version:\"', version, '\") {
  edges {
    node {
      name
    }
  }
 }
}')
  as.character(sapply(build_query(my_query)$normalizations$edges, unlist))
}

#' get_ontology_structure
#'
#' @param compendium character - the selected compendium
#' @param name_In character - the name of the ontology/ies of interest
#'
#' @return a character
#' @export
#'
#' @examples
#'\dontrun{
#' get_ontology_structure(name_In="Agronomy")
#' get_ontology_structure(name_In=c("Agronomy","Gene ontology"))
#' }
get_ontology_structure <- function(compendium = "vespucci",
                                   name_In = "Gene ontology"){
  my_query <- paste0('{
  ontology(compendium:\"', compendium, '\", name_In:\"', paste0(name_In, collapse =","), '\") {
  edges {
    node {
      structure
    }
  }
 }
}')
  RJSONIO::fromJSON(sapply(build_query(my_query)$ontology$edges, unlist))
  #CHECK!!! - it kills Rstudio!
}


#' Get all available ontologies for the selected compendium
#'
#' @param compendium A string - the selected compendium
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' get_ontologies()
get_ontologies <- function(compendium = "vespucci"){
  my_query <- paste0('{

   ontology(compendium:\"', compendium, '\"){
    edges{
      node{
        id
        name
      }
    }
  }
}')
  tmp <- as.data.frame(t(sapply(build_query(my_query)$ontology$edges, unlist)))
  colnames(tmp) <- c("id", "name")
  rownames(tmp) <- NULL
  tmp
}


#' get_OntologyNode
#'
#' @param compendium A string - the selected compendium
#' @param originalId A string - the ontology term
#' @param ontology_Name A string - the name of the selected ontology
#' @param normalization A string ('legacy' as default)
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' get_OntologyNode(normalization="tpm",originalId="PO_0009005",ontology_Name="Plant ontology")
get_OntologyNode <- function(compendium = "vespucci",
                             normalization = "tpm",
                             originalId =  "PO_0009005",
                             ontology_Name = "Plant ontology"){
  if(is.null(originalId)||is.null(ontology_Name)) stop('Provide both originalId and ontology_Name (e.g. originalId = "PO_0009005", ontology_Name = "Plant ontology"')
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  my_query <- paste0('{
    ontologyNode(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\", originalId:\"', originalId, '\",ontology_Name:\"',ontology_Name,'\") {
    edges {
      node {
        id,
        originalId,
        termShortName,
        json,
        ontology{
          id
        }
      }
    }
    }
  }')
  # cat(my_query,"\n")
  tmp <- build_query(my_query)$ontologyNode$edges
  # tmp <- as.data.frame(t(sapply(build_query(my_query)$sampleSets$edges, unlist)))
  #colnames(tmp) <-  c("id","name"); rownames(tmp) <-  NULL
  tmp
}


#' get_biofeature_annotations
#'
#' @param compendium A string - the selected compendium
#' @param bioFeature_Name_In A character vector with bioFeature names
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' get_biofeature_annotations(bioFeature_Name_In = c("VIT_00s0332g00060","VIT_00s0332g00070"))
get_biofeature_annotations <- function(compendium = "vespucci",
                                bioFeature_Name_In = NULL
                                ){
  if(is.null(bioFeature_Name_In)) stop("Provide a vector with bioFeatureNames!")
  my_query <- paste0('{
    biofeatureAnnotations(compendium:\"', compendium, '\", bioFeature_Name_In:\"',paste0(bioFeature_Name_In, collapse =","), '\") {
    edges {
      node {
        id
        annotation
        bioFeature {
          		  id
          		  name
        }
      }
    }
  }
}')
  tmp <- as.data.frame(t(sapply(build_query(my_query)$biofeatureAnnotations$edges, unlist)))
  colnames(tmp) <- c("annotationId", "bioFeatureAnnotation","bioFeatureId","bioFeatureName")
  rownames(tmp) <- NULL
  tmp
}


#' get_biofeature_by_name
#'
#' @param compendium A string - the selected compendium
#' @param field biofeature field of interest ('sequence' as default)
#' @param name_In A string - the biofeature (gene_name)
#'
#' @return A data.frame with three columns: name,id and value
#' @export
#'
#' @examples
#' get_biofeature_by_name(name_In = c("VIT_00s0332g00060", "VIT_00s0246g00220",
#'  "VIT_00s0332g00160"))
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
  # cat(my_query,"\n")
  tmp <- t(sapply(build_query(my_query)$biofeatures$edges, unlist))
  colnames(tmp) <- c("id", "value")
  rownames(tmp) <- NULL
  data.frame(name = name_In, tmp)
}


#' get_biofeature_id
#'
#' @param compendium A string - the selected compendium
#' @param id_In A vector of character strings - the biofeature id;
#' if NULL it returns all biofeature ids
#' @param useIds A logical (TRUE as default) - It allows using biofeature ids
#'
#' @return A data.frame with three columns: id, name, description
#' @export
#'
#' @examples
#' my_genes <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' get_biofeature_id(id_In = my_genes, useIds = FALSE)
#' my_ids <- c("QmlvRmVhdHVyZVZhbHVlVHlwZToyNzQzOQ==","QmlvRmVhdHVyZVZhbHVlVHlwZToyNzg5Mg==")
#' get_biofeature_id(id_In = my_ids, useIds = TRUE)
get_biofeature_id <- function(compendium = "vespucci",
                              id_In = NULL,
                              useIds = TRUE){
  if(is.null(id_In)){
    my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\") {
    edges {
      node {
        id
        name
        description
            }
          }
        }
      }')
    tmp <- t(sapply(build_query(my_query)$biofeatures$edges, unlist))
    colnames(tmp) <- c("id", "name", "description"); rownames(tmp) <- NULL
    as.data.frame(tmp)

  }
  else if(useIds){
  my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\", id_In:\"', paste0(id_In, collapse =","), '\") {
    edges {
      node {
        id
        name
        description
            }
          }
        }
      }')
  } else {
  my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\", name_In:\"', paste0(id_In, collapse =","), '\") {
    edges {
      node {
        id
        name
        description
            }
          }
        }
      }')
  }
  tmp <- t(sapply(build_query(my_query)$biofeatures$edges, unlist))
  colnames(tmp) <- c("id", "name", "description"); rownames(tmp) <- NULL
  as.data.frame(tmp)
}

#' get_sampleset_id
#'
#' @param compendium A string - the selected compendium
#' @param id_In A vector of character strings - either samplesetNames or sampleSetIds
#' @param useIds A logical (FALSE as default) - It allows using sampleSetIds
#' @param normalization A string - either 'limma' (default),'tpm' or legacy as normalization
#'
#' @return A data.frame with two columns: id and name
#' @export
#'
#' @examples
#' my_ss=c("GSE75498_OS_T0-13-vs-GSE75498_C_T0-21","harvest_4","harvest_5")
#' get_sampleset_id(id_In = my_ss, normalization = "limma", useIds = FALSE)
#' my_ids=c("U2FtcGxlU2V0VHlwZTo2NDE5","U2FtcGxlU2V0VHlwZToyMTg2OA==","U2FtcGxlU2V0VHlwZToyMTg2Nw==")
#' get_sampleset_id(id_In = my_ids, normalization = "limma", useIds = TRUE)
get_sampleset_id <- function(compendium = "vespucci",
                             normalization = "limma",
                             id_In = NULL,
                             useIds = FALSE){
  if(is.null(id_In)) stop("Provide id_In (e.g. id_In = 'GSM671720.ch1-vs-GSM671719.ch1','GSM671721.ch1-vs-GSM671719.ch1'")
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(useIds) {
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\", id_In:\"', paste0(id_In, collapse =","), '\") {
    edges {
      node {
        id,
        name,
        shortAnnotationDescription
        }
      }
    }
  }')
  }
  else {
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\", name_In:\"', paste0(id_In, collapse =","), '\") {
    edges {
      node {
        id,
        name,
        shortAnnotationDescription
        }
      }
    }
    }')
  }
  # cat(my_query,"\n")
  tmp <- as.data.frame(t(sapply(build_query(my_query)$sampleSets$edges, unlist)))
  colnames(tmp) <-  c("id","name","shortAnnotationDescription"); rownames(tmp) <-  NULL
  tmp
}

#' get_sampleset_by_sampleid
#'
#' @param compendium A string - the selected compendium
#' @param normalization A string ('legacy' as default)
#' @param samples A vector of character strings - samples id
#'
#' @return A data.frame with two columns: id and name
#' @export
#'
#' @examples
#' my_ids=c("U2FtcGxlVHlwZToxMzM4","U2FtcGxlU2V0VHlwZToxMzM3")
#' get_sampleset_by_sampleid(samples = my_ids, normalization = "tpm")
get_sampleset_by_sampleid <- function(compendium = "vespucci",
                             normalization = "tpm",
                             samples = NULL){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(samples)){
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\") {
    edges {
      node {
        id,
        name
        }
      }
    }
  }')
  }
  else{
    my_query <- paste0('{
    sampleSets(compendium:\"', compendium, '\", version:\"', version, '\", normalization:\"', normalization, '\", samples:["', paste0(samples, collapse = '","'),'\"]) {
    edges {
      node {
        id,
        name
        }
      }
    }
  }')
  }
  tmp <- as.data.frame(t(sapply(build_query(my_query)$sampleSets$edges, unlist)))
  colnames(tmp) <-  c("id","name")#; rownames(tmp) <-  NULL
  tmp
}



#' get_ranking
#'
#' @param compendium A string - the selected compendium
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
#' @param compendium A string - the selected compendium
#' @param rank A string ('magnitude' as default)
#' @param normalization A string ('legacy' as default)
#' @param biofeaturesNames A vector of character strings (here gene_names)
#' @param top_n A numeric - an integer for selecting the top ranked samplesets
#' @param rankTarget A string ('sampleset' as default)
#' @param useIds A logical - TRUE as default
#'
#' @return A data.frame with three columns id, name, value
#' @export
#'
#' @examples
#'\dontrun{
#' get_samplesets_ranking(biofeaturesNames=c("QmlvRmVhdHVyZVR5cGU6MQ==",
#' "QmlvRmVhdHVyZVR5cGU6Mg==","QmlvRmVhdHVyZVR5cGU6Mw==","QmlvRmVhdHVyZVR5cGU6NA==",
#'  "QmlvRmVhdHVyZVR5cGU6NQ=="), useIds = TRUE, top_n = 10)
#'  }
get_samplesets_ranking <- function(compendium = "vespucci",
                                   normalization = "legacy",
                                   rankTarget = "samplesets",
                                   rank = "magnitude",
                                   biofeaturesNames=NULL,
                                   top_n = 50,
                                   useIds = FALSE){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(biofeaturesNames)) stop("Provide biofeaturesNames")
  if(useIds) biofeaturesIds <- biofeaturesNames
  else biofeaturesIds <- get_biofeature_id(id_In=biofeaturesNames)$id
  my_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    version:\"', version, '\",
    normalization:\"', normalization, '\",
    rank:\"', rank, '\",
    rankTarget:\"', rankTarget, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"]) {
      id,
      name,
      value
      }
  }')
  as.data.frame(sapply(build_query(my_query)$ranking,unlist))[1:top_n,]
}


#' get_biofeature_ranking()
#'
#' @param compendium A string - the selected compendium
#' @param samplesetNames A vector of character strings (here sampleset names)
#' @param normalization A string ('legacy' as default)
#' @param rank A string ('magnitude' as default) - use \code{\link{get_ranking}}
#' @param top_n A numeric - an integer for selecting the top ranked samplesets
#' @param rankTarget A string ('biofeature' as default)
#' @param useIds A logical - TRUE as default
#'
#' @return A data.frame with three columns id, name, value
#' @export
#'
#' @examples
#'\dontrun{
#' my_ss <- c("U2FtcGxlU2V0VHlwZToyMTk4Nw==", "U2FtcGxlU2V0VHlwZToyMTk4OA==",
#' "U2FtcGxlU2V0VHlwZToyMTk4OQ==", "U2FtcGxlU2V0VHlwZToyMTk5MA==")
#' get_biofeature_ranking(samplesetNames = my_ss, top_n = 10,
#' normalization = "tpm", useIds = TRUE)
#' }
get_biofeature_ranking <- function(compendium = "vespucci",
                                   samplesetNames = NULL,
                                   normalization = "legacy",
                                   rank = "uncentered_correlation",
                                   rankTarget = "biofeatures",
                                   top_n = 50,
                                   useIds = TRUE){
  if(normalization == "legacy") version <- "legacy"
  else if(normalization %in% c("limma","tpm")) version <- "2.0"
  else stop("normalization HAS TO BE either legacy, limma or tpm.")
  if(is.null(samplesetNames)) stop("Provide samplesetNames")
  if(useIds) samplesetIds <- samplesetNames
  else samplesetIds <- get_sampleset_id(id_In = samplesetNames)$id
  my_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    version:\"', version, '\",
    normalization:\"', normalization, '\",
    rank:\"', rank, '\",
    rankTarget:\"', rankTarget, '\",
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
      id,
      name,
      value
      }
}')
  as.data.frame(sapply(build_query(my_query)$ranking,unlist))[1:top_n,]
}
