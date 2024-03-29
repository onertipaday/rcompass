% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queries.R
\name{get_sampleset_id}
\alias{get_sampleset_id}
\title{get_sampleset_id}
\usage{
get_sampleset_id(
  compendium = "vespucci",
  normalization = "limma",
  id_In = NULL,
  useIds = FALSE
)
}
\arguments{
\item{compendium}{A string - the selected compendium}

\item{normalization}{A string - either 'limma' (default),'tpm' or legacy as normalization}

\item{id_In}{A vector of character strings - either samplesetNames or sampleSetIds}

\item{useIds}{A logical (FALSE as default) - It allows using sampleSetIds}
}
\value{
A data.frame with two columns: id and name
}
\description{
get_sampleset_id
}
\examples{
\dontrun{
my_ss=c("GSE75498_OS_T0-13-vs-GSE75498_C_T0-21","harvest_4","harvest_5")
get_sampleset_id(id_In = my_ss, normalization = "limma", useIds = FALSE)
my_ids=c("U2FtcGxlU2V0VHlwZTo2NDE5","U2FtcGxlU2V0VHlwZToyMTg2OA==","U2FtcGxlU2V0VHlwZToyMTg2Nw==")
get_sampleset_id(id_In = my_ids, normalization = "limma", useIds = TRUE)
}
}
