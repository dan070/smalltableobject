.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my package")
  packageStartupMessage(paste("libname=", libname))
  packageStartupMessage(paste("pkgname=", pkgname))

}

.onLoad <- function(libname, pkgname) {

}
