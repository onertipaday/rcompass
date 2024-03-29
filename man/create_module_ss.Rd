% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modules.R
\name{create_module_ss}
\alias{create_module_ss}
\title{create a module based on provided sample sets}
\usage{
create_module_ss(
  compendium = "vespucci",
  samplesetNames = NULL,
  normalization = "limma",
  rank = "uncentered_correlation",
  top_n = 50,
  sorted = FALSE,
  useIds = FALSE
)
}
\arguments{
\item{compendium}{A string - the selected compendium}

\item{samplesetNames}{A character vector (sampleset names)}

\item{normalization}{A string - either 'limma' (default),'tpm' or legacy as normalization}

\item{rank}{A string ('magnitude' as default) - use \code{\link{get_ranking}}}

\item{top_n}{A numeric - an integer for selecting the top ranked samplesets}

\item{sorted}{A logical (FALSE as default) - it returns a sorted index for both bf and ss}

\item{useIds}{A logical (FALSE as default) - It allows using samplesetIds}
}
\value{
A SummarizedExperiment object
}
\description{
create a module based on provided sample sets
}
\examples{
\dontrun{
my_ids=c("U2FtcGxlU2V0VHlwZTo2NDE5","U2FtcGxlU2V0VHlwZToyMTg2OA==")
mod_ss <- create_module_ss(samplesetNames = my_ids,
normalization = "limma", top_n = 15, useIds = TRUE)
}
}
