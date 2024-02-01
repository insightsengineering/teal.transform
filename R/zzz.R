.onLoad <- function(libname, pkgname) { # nolint
  teal.logger::register_logger("teal.transform")
  teal.logger::register_handlers("teal.transform")
}
