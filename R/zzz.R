.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("\nThis is 'rCOMPASS' version ", utils::packageVersion("rcompass"), " based on COMPASS version ", get_compass_version(), ".\n"))
  # packageStartupMessage('If you are new to rcompass, please consider reading the vignette through the command: vignette("rcompass").')
}
