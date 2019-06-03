#' Get all available compendia
#'
#' @return a list with each element containing name, fullName, descritpion and normaliaziotion for all available compendia.
#' @export
#'
#' @examples
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
#' @examples
#' get_compendium_data_source()
get_compendium_data_source <- function(compendium="vitis_vinifera"){
  my_query <- '{
  dataSources(compendium: "vitis_vinifera") {
    edges {
      node {
        id,
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
#' @examples
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
        id,
        name,
        description
      }
    }
  }
}')
  parze(cont(do_POST(base_url, my_query)))$data$platformTypes$edges$node
}


#' Get available experiments information for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_experiments()
get_experiments <- function(compendium="vitis_vinifera"){
  my_query <- paste0('{
 experiments(compendium:\"', compendium, '\") {
    edges {
      node {
        organism,
        experimentAccessId,
        experimentName
      }
    }
  }
}')
  parze(cont(do_POST(base_url, my_query)))[[1]]$experiments$edges$node
}
#
# Le API della seguente funzione devono restituire le info per campione non per record: i.e.
# se indico n=10 devo ottenere le annotaioni per 10 campioni
#
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
}
  ')
  ontology <- parze(cont(do_POST(base_url, my_query)))$data$sampleAnnotations$edges$node$annotation$ontologyNode
  sampleName <- parze(cont(do_POST(base_url, my_query)))$data$sampleAnnotations$edges$node$sample$sampleName
  data.frame(sampleName = sampleName, ontology=ontology)
}


# TODO

#' Get all available samples for the selected compendium,
#' use \code{\link{get_compendia}} to check all the available compendia
#'
#' @param compendium a character - the selected compendium
#'
#' @return a data.frame
#' @export
#'
get_samples <- function(compendium="vitis_vinifera"){
  my_query <- paste0('{
 samples(compendium:\"', compendium, '\") {
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
                    comments

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

#' Title
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
            }
          }
        }
      }
    }
  }
}')
parze(cont(do_POST(base_url, my_query)))$data$biofeatureAnnotations$edges$node$annotationValue$ontologyNode
}



#' get biofeatures by annotation term
#'
#' @param compendium a character - the selected compendium
#' @param term a character
#'
#' @return a character vector
#' @export
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

#' create moduel with biological features and sample sets
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
create_module <- function(compendium="vitis_vinifera",
                          biofeaturesIds=c("QmlvRmVhdHVyZVR5cGU6NTIzMjU=","QmlvRmVhdHVyZVR5cGU6NTIzMjY="),
                          samplesetIds=c("U2FtcGxlU2V0VHlwZToxMjYw","U2FtcGxlU2V0VHlwZToxMjYx","U2FtcGxlU2V0VHlwZToxMjYy")){
  my_query <- paste0('{
  modules(compendium:"vitis_vinifera",
    biofeaturesIds:["QmlvRmVhdHVyZVR5cGU6NTIzMjU=","QmlvRmVhdHVyZVR5cGU6NTIzMjY="],
    samplesetIds:["U2FtcGxlU2V0VHlwZToxMjYw","U2FtcGxlU2V0VHlwZToxMjYx","U2FtcGxlU2V0VHlwZToxMjYy"]) {
    normalizedValues,
    sampleSets {
      edges {
        node {
          id,
          name,
          normalizationdesignsampleSet {
            edges {
              node {
                sample {
                  sampleName
                }
              }
            }
          }
        }
      }
    }
    biofeatures {
      edges {
        node {
          id,
          name
        }
      }
    }
  }
}')
  parze(cont(do_POST(base_url, my_query)))$data

}

