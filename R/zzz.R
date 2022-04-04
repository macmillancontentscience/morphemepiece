.onLoad <- function(libname, pkgname) { # nocov start
  morphemepiece_vocab <<- memoise::memoise(morphemepiece_vocab)
  morphemepiece_lookup <<- memoise::memoise(morphemepiece_lookup)
}  # nocov end
