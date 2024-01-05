.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname,pkgname) {

  .pkgenv$resources <- list()
  .pkgenv$tempdir <- tempdir()
  .pkgenv$outdir <- tempfile()
  dir.create(.pkgenv$outdir, showWarnings = FALSE)
  .pkgenv$verbose <- TRUE
  # .pkgenv$downloader <- .download_or_skip
  .pkgenv$testing <- FALSE
  .pkgenv$standard_args <- c("x", "name", "type", "mode", "outdir", "verbose")

 invisible()
}


mapme_options <- function(..., resources, outdir, tempdir, verbose, testing){

  if (!missing(resources)){
    if(!inherits(resources, "list") || is.null(names(resources))) {
      stop("resources must be a named list of sf objects.")
    }
    if(!any(sapply(resources, function(x) inherits(x, "sf")))){
      stop("single resources must be sf object of a vector or raster tileindex.")
    }
    .pkgenv$resources <- append(.pkgenv$resources, resources)
  }

  if (!missing(outdir)) {
    # cannot check because might be a remote bucket...
    .pkgenv$outdir <- outdir
  }

  if (!missing(tempdir)) {
    if(!dir.exists(tempdir)){
      stop("tempdir must point to an existing directory")
    }
    .pkgenv$tempdir <- tempdir
  }

  if (!missing(verbose)) {
    stopifnot(is.logical(verbose))
    .pkgenv$verbose <- verbose
  }

  # if(!missing(downloader)) {
  #   stopifnot(is.function(downloader))
  #   .pkgenv$downloader <- downloader
  # }

  if (!missing(testing)) {
    stopifnot(is.logical(testing))
    .pkgenv$testing <- testing
  }

  if (nargs() == 0) {
    return(list(
      outdir = .pkgenv$outdir,
      tempdir = .pkgenv$tempdir,
      verbose = .pkgenv$verbose,
      testing = .pkgenv$testing,
      resources = .pkgenv$resources
    ))
  }
}
