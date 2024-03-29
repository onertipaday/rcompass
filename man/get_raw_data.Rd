% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queries.R
\name{get_raw_data}
\alias{get_raw_data}
\title{get raw data for a particular sample}
\usage{
get_raw_data(compendium = "vespucci", sampleId = NULL, useIds = TRUE)
}
\arguments{
\item{compendium}{A string - the selected compendium}

\item{sampleId}{A string - either sampleId or sampleName}

\item{useIds}{A logical (TRUE as default) - It allows using sampleName}
}
\value{
A data.frame with five columns: biofeatures_id,
biofeatures_name, biofeatureReporters, values, valueType
}
\description{
get raw data for a particular sample
}
\examples{
\dontrun{
rd_01 <- get_raw_data(sampleId = "U2FtcGxlVHlwZToyMzQ=", useIds = TRUE) # MA
rd_02 <- get_raw_data(sampleId = "GSM1551306.ch1", useIds = FALSE) # RNA-seq
rd_03 <- get_raw_data(sampleId = "GSM287866.ch1", useIds = FALSE) # MA
}
}
