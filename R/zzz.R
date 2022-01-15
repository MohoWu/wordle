.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath('www',
                         system.file("img",
                                     package = "wordle"))
}
