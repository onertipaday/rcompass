% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_annotation_network}
\alias{plot_annotation_network}
\title{plot rdf network}
\usage{
plot_annotation_network(
  compendium = "vespucci",
  ids = NULL,
  normalization = "limma",
  viewer = FALSE
)
}
\arguments{
\item{compendium}{A string - the selected compendium}

\item{ids}{A string - unique id of a biofeature, a sample, etc.}

\item{normalization}{A string - 'tpm' (as default), 'limma' or 'legacy'}

\item{viewer}{A logical - if TRUE will plot the html widget}
}
\value{
Either a json, an html or a plotly htmlwidget
}
\description{
plot rdf network
}
\examples{
\dontrun{
my_ids <- get_biofeature_id(id_In = c("U2FtcGxlU2V0VHlwZTo3MTA=","U2FtcGxlU2V0VHlwZToxMDI4",
 "U2FtcGxlU2V0VHlwZToxMDI5"))
plot_annotation_network(ids = my_ids$id, viewer = TRUE)
}
}
