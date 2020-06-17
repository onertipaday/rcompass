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
#' @param compendium a character - the selected compendium
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
#' @param compendium a character - the selected compendium
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
  build_query(my_query)$platforms$edges$node %>%
    mutate(dataSource=dataSource$sourceName,
           platformType=platformType$name)
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
  build_query(my_query)$platformTypes$edges$node
}


#' get_sample_info
#'
#' @param compendium a character - the selected compendium
#' @param sampleName a character - GEO Sample accession id (GSM)
#'
#' @return
#' @importFrom magrittr "%>%"
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
    build_query(my_query)$sampleAnnotations$edges$node %>%
      transmute(sampleName = sample$sampleName,
             sampleId = sample$id,
             annotation)
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
  if(all(c(name, samples) %in% NULL)) stop("Enter either a name (name = 'GSE27180_48hours-1-vs-GSE27180_0h-2') or a sample id (e.g. samples = 'U2FtcGxlVHlwZTox')")
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
#' @param compendium a character - the selected compendium
#' @param ids a character
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
#' @param compendium a character - the selected compendium
#' @param version a character(default 'latest')
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
#' @param compendium a character - the selected compendium
#' @param name a character - the name of the ontolgy of interest
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
  build_query(my_query)$ontology$edges$node
}


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
#' @param name_In a character vector - the biofeature names
#' @param name a character - the biofeature name
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' get_biofeature_id(name="VIT_00s0332g00060")
#' get_biofeature_id(name_In = "VIT_00s0332g00060, VIT_00s0332g00160")
#' get_biofeature_id(name_In = c("VIT_00s0332g00060", "VIT_00s0332g00160"))
get_biofeature_id <- function(compendium = "vespucci",
                              name = "VIT_00s0332g00060",
                              name_In = NULL){
  if(is.null(name) & is.null(name_In)) stop("Provide either a name XOR a name_In")
  if(is.null(name_In)){
    my_query <- paste0('{
  biofeatures(compendium:\"', compendium, '\", name:\"',name, '\") {
    edges {
      node {
        name
        id
        description
            }
          }
        }
      }')
  }
  else {
  #   my_query <- paste0('{
  # biofeatures(compendium:\"', compendium, '\", name_In:\"',name_In, '\") {
  #   edges {
  #     node {
  #       name
  #       id
  #       description
  #           }
  #         }
  #       }
  #     }')
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
  }
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

#' get_ranking
#'
#' @param compendium a character - the selected compendium
#'
#' @return a list
#' @export
#'
#' @examples
#' get_ranking()
get_ranking <- function(compendium = "vespucci"){
  my_query <- paste0('{
  scoreRankMethods(compendium:\"', compendium, '\"){
        sampleSets,
        biologicalFeatures
      }
  }')
  build_query(my_query)$scoreRankMethods
}


#' get_available_plot_methods
#'
#' @param compendium a character - the selected compendium
#'
#' @return a list with plot methods for distribution, heatmap and network
#' @export
#'
#' @examples
#' get_available_plot_methods()
get_available_plot_methods <- function(compendium = "vespucci"){
  my_query <- paste0('{
  plotName(compendium:\"', compendium, '\"){
        distribution,
        heatmap,
        network
      }
  }')
  build_query(my_query)$plotName
}


#' create module with biological features and sample sets
#'
#' @param compendium a character - the selected compendium
#' @param biofeatureNames a character vector (here gene_names)
#' @param samplesetNames a character vector (here sampleset names)
#' @param version a character ('legacy' as default)
#'
#' @return a list with three data.frame
#' @export
#'
#' @examples
#'\dontrun{
#' my_mod <- create_module(biofeatureNames=c("VIT_00s0332g00110","VIT_00s0332g00160",
#' "VIT_00s0396g00010","VIT_00s0505g00030"),
#' samplesetNames=c("GSE27180_48hours-1-vs-GSE27180_0h-2",
#' "E-MTAB-1514.US_03_3.ch1-vs-E-MTAB-1514.US_03_1.ch1"))
#' pheatmap::pheatmap(my_mod,col = RcolorBrewer::brewer.pal(11,name="RdBu"))
#' # compare to vespucci:
#' # url <- "http://vespucci.colombos.fmach.it/cws_data/export_data/colombos_20190828_nEwFDG.txt"
#' # vesp_test=readr::read_tsv(url)
#' }
#'
create_module <- function(compendium = "vespucci",
                          biofeatureNames = NULL,
                          samplesetNames = NULL,
                          version = "legacy"){
  if(all(c(biofeatureNames, samplesetNames) %in% NULL)) stop("You need to provide at least biofeaturesNames or samplesetsNames")
  else if (is.null(biofeatureNames)) {
    out <- create_module_ss(compendium = compendium, samplesetNames =  samplesetNames)
  }
  else if (is.null(samplesetNames)) {
    out <- create_module_bf(compendium = compendium, biofeatureNames = biofeatureNames)
  }
  else {
    # biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)[2]))
    biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)$id))
    samplesetIds <- sapply(samplesetNames,function(x) paste(get_sampleset_id(name=x)))
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]), {
      normalizedValues,
      sampleSets {
          edges {
                node {
                  id
                  name
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
                  id
                  name
                }
          }
        }
    }
  }')
  #   tmp <- parze(cont(do_POST(base_url, my_query)))$data
  #   out <- data.frame(tmp$modules$normalizedValues, row.names = biofeatureNames)
  #   colnames(out) <- samplesetNames
     }
  # out
  build_query(my_query)$modules
}


#' create_module_bf
#'
#' @param compendium a character - the selected compendium
#' @param biofeatureNames a character vector (here gene_names)
#' @param normalization a character
#' @param rank a character ('magnitude' as default)
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' create_module_bf(biofeatureNames=c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060","VIT_00s0873g00020","VIT_00s0904g00010")
create_module_bf <- function(compendium = "vespucci",
                          biofeaturesNames=c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030","VIT_00s0505g00060","VIT_00s0873g00020","VIT_00s0904g00010"),
                          version = "legacy",
                          rank = "magnitude",
                          rankTarget = "samplesets"){
  if(is.null(biofeatureNames)) stop("You need to provide biofeaturesNames")
  biofeaturesIds <- sapply(biofeatureNames,function(x) paste(get_biofeature_id(name=x)$id))
  samplesetIds <- get_samplesets_ranking(compendium =  compendium,
                                         biofeaturesNames = biofeatureNames,
                                         version = version,
                                         rank =rank,
                                         rankTarget = rankTarget)
    my_query <- paste0('{
    modules(compendium:\"', compendium, '\",
    biofeaturesIds:["', paste0(biofeaturesIds, collapse = '","'),'\"],
    samplesetIds:["', paste0(samplesetIds[[1]], collapse = '","'),'\"]), {
      normalizedValues
    }
  }')
  build_query(my_query)$modules
}

#' get_samplesets_ranking
#'
#' @param compendium a character - the selected compendium
#' @param biofeatureNames a character vector (here gene_names)
#' @param rank a character ('magnitude' as default)
#' @param version a character ('legacy' as default)
#' @param rankTarget a character ('sampleset' as default)
#' @param biofeatureIds a character vector (here gene_ids)
#'
#' @return a list with four character vectors (id,name,type,value)
#' @export
#'
#' @examples
#' my_ids <- c("VIT_00s0332g00110","VIT_00s0332g00160","VIT_00s0396g00010","VIT_00s0505g00030")
#' get_samplesets_ranking(biofeaturesNames = my_ids)
#' get_samplesets_ranking(biofeaturesIds=c("QmlvRmVhdHVyZVR5cGU6MQ==","QmlvRmVhdHVyZVR5cGU6Mg==", "QmlvRmVhdHVyZVR5cGU6Mw==","QmlvRmVhdHVyZVR5cGU6NA==","QmlvRmVhdHVyZVR5cGU6NQ=="))
get_samplesets_ranking <- function(compendium = "vespucci",
                                   version = "legacy",
                                   rankTarget = "samplesets",
                                   rank = "magnitude",
                                   biofeaturesNames=NULL,
                                   biofeaturesIds=NULL,
                                   top_n = 50){
  if(is.null(biofeaturesNames) & is.null(biofeaturesIds)) stop("provide either biofeatureNames XOR biofeatureIds")
  else if(is.null(biofeaturesIds)) {
    biofeaturesIds <- get_biofeature_id(name = NULL, name_In=biofeaturesNames)$id
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
  } else {
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
  }

  build_query(my_query)$ranking
}



# get_biofeature_ranking()
#' Title
#'
#' @param compendium a character - the selected compendium
#' @param samplesetIds a character vector (here gene_names)
#' @param samplesetNames a character vector (here sampleset names)
#' @param version a character ('legacy' as default)
#' @param rank a character ('magnitude' as default) - use \code{\link{get_ranking()$sampleSets}}
#' for the available values
#'
#' @return a list with four character vectors (id,name,type,value)
#' @export
#'
#' @examples
#' get_biofeature_ranking()
get_biofeature_ranking <- function(compendium = "vespucci",
                                   samplesetIds = c("U2FtcGxlU2V0VHlwZToxMjYy",
                                                  "U2FtcGxlU2V0VHlwZToxMjYw"),
                                   samplesetNames = NULL,
                                   version = "legacy",
                                   rank = "std"){
  if(is.null(samplesetIds)) stop("You need to provide samplesetIds")
  my_query <- paste0('{
  ranking(compendium:\"', compendium, '\",
    version:\"', version, '\",
    rank:\"', rank, '\",
    samplesetIds:["', paste0(samplesetIds, collapse = '","'),'\"]) {
      id
      name
      value
      }
}')
  # tmp <- parze(cont(do_POST(base_url, rank_query)))$data$ranking
  # out <- data.frame(id=tmp$id, name=tmp$name)
  build_query(my_query)$ranking
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




