.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("\nThis is 'rcompass' version", utils::packageVersion("rcompass"), "\n"))
  packageStartupMessage('If you are new to rcompass, please consider reading the vignette through the command: vignette("rcompass").')
}
