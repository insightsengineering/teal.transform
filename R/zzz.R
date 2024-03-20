.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger("teal.transform")
  teal.logger::register_handlers("teal.transform")
  invisible()
}
