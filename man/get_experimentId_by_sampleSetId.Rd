% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queries.R
\name{get_experimentId_by_sampleSetId}
\alias{get_experimentId_by_sampleSetId}
\title{Get experimentAccessid and experimentName from sampleName}
\usage{
get_experimentId_by_sampleSetId(
  compendium = "vespucci",
  normalization = "limma",
  sampleSetId = NULL
)
}
\arguments{
\item{compendium}{A string - the selected compendium}

\item{normalization}{A string ('limma' as default)}

\item{sampleSetId}{A string - if NULL(default) returns all available experiments ids
for the selected compendium}
}
\value{
A data.frame with experimentAccessId, esperimentName and description
}
\description{
use \code{\link{get_available_compendia}} to check all the available compendia
}
\examples{
\dontrun{
ss=c("GSE75498_OS_T0-13-vs-GSE75498_C_T0-21","harvest_4","harvest_5")
my_ss <- get_sampleset_id(id_In = ss, normalization = "limma", useIds = FALSE)
get_experimentId_by_sampleSetId(sampleSetId = my_ss$id)
}
}
