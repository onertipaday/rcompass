.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("\nThis is 'rCOMPASS' version ", utils::packageVersion("rcompass"), " based on COMPASS version ", get_compass_version(), ".\n"))
}
