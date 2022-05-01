#' Initialization of a biodiversity portfolio object
#'
#' This function expects an \code{sf} object as its first argument that contains
#' only geometry of type \code{POLYGON} or \code{MULTIPOLYGON}. Each row of the
#' object is considered a single asset in the portfolio for which biodiversity
#' indicators will be calculated further down the processing chain. Some
#' preliminary checks are conducted, e.g. that the CRS of the object is
#' EPSG:4326 otherwise it will be transformed. Some portfolio wide parameters
#' such as the output directory for downloaded data sets, a temporal directory
#' for intermediate calculation of the number of cores available for the
#' indicator calculation can be set by the user to have more fine-control of
#' the workflow. However, these parameters are also set to sensible defaults
#' and thus can be omitted during portfolio initialization.
#'
#' @param x The sf object to be transformed to a portfolio
#' @param years Numeric vector for time frame of the analysis handed over as a
#'   vector of consecutive years
#' @param outdir Character indicating the directory where resources will be
#'   written to. If the directory does not exist, we will attempt to create it.
#'   The output director cannot be equal to the temporary directory. Defaults
#'   to the current working directory.
#' @param tmpdir Character vector to a directory used for intermediate files.
#'   Defaults to the output of \code{tempdir()}, e.g. the current R session's
#'   temporal directory. If a custom file path does not exist, we will attempt
#'   to create it. Cannot be equal to the output directory. Note, that for
#'   raster calculations we will set the temporal directory for the \code{terra}
#'   package here. Please make sure that enough disk space is available because
#'   some intermediate calculations can become quite large.
#' @param cores An integer value indicating the number of cores on the host
#'   machine available for indicator calculation. It defaults to
#'   \code{parallel::detectCores() - 1} cores, i.e. one core less than all
#'   available cores.
#' @param aria_bin A character vector to an aria2c executable for parallel
#'  downloads
#' @param verbose Logical, defaults to TRUE, indicating if progress information
#'   is printed.
#' @param add_resources Logical if existing resources in 'outdir' should be
#'   added to the portfolio. Defaults to TRUE. Setting it to FALSE can be of use
#'   e.g. if a previous download has terminated unexpectedly in order to resume.
#' @return The sf portfolio object `x` with amended attributes controlling the
#'   processing behavior further down the processing chain.
#' @keywords function
#' @export
init_portfolio <- function(x,
                           years,
                           outdir = getwd(),
                           tmpdir = tempdir(),
                           cores = parallel::detectCores() - 1,
                           add_resources = TRUE,
                           aria_bin = NULL,
                           verbose = TRUE) {
  if (outdir == tmpdir) {
    stop("Parameters outdir and tmpdir need to point to different directories.")
  }
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
  if (Sys.info()["sysname"] == "Windows" & cores > 1) {
    warning(paste("Parallel processing on Windows currently is not supported.",
      "Setting number of cores to 1",
      sep = " "
    ))
    cores <- 1
  }

  # deactivate progress bar if verbose is set to FALSE
  if (!verbose) pbapply::pboptions(type = "none")

  if (!is.null(aria_bin)) {
    aria_output <- try(system2(aria_bin, args = "--version", stdout = TRUE, stderr = FALSE), silent = TRUE)
    if (inherits(aria_output, "try-error") | !grepl("aria2 version", aria_output[1])) {
      warning(paste(
        "Argument 'aria_bin' does not point to a executable aria2 installation.",
        "The package will use R internal download utility."
      ))
      aria_bin <- NULL
    }
  }

  if (nrow(x) < 1) stop("x must contain at least one asset.")
  if (st_crs(x) != st_crs(4326)) {
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x <- st_transform(x, 4326)
  }
  # use tibble for prettier printing of the object
  x <- st_as_sf(tibble(x))
  # check for geometry types
  if (any(!unique(st_geometry_type(x)) %in% c("POLYGON"))) {
    stop("Some assests are not of type POLYGON. Please use sf::st_cast() to cast to POLYGON.")
  }
  # add a unique asset identifier
  if ("assetid" %in% names(x)) {
    message(
      paste("Found a column named 'assetid'.",
        " Overwritting its values with a unique identifier.",
        sep = ""
      )
    )
  }
  x$assetid <- 1:nrow(x)

  # check if resources already exist
  resources <- list()
  if (add_resources) {
    present_dirs <- list.dirs(outdir, full.names = FALSE)
    all_resources <- names(available_resources())
    present_resources <- present_dirs[which(present_dirs %in% all_resources)]
    resources <- lapply(present_resources, function(res) {
      list.files(file.path(outdir, res), pattern = ".gpkg$", full.names = TRUE)
    })
    names(resources) <- present_resources
    index <- unlist(lapply(resources, function(res) length(res) > 0))
    resources <- resources[index]
    if (length(resources) == 0) resources <- list()
  }

  # setting portfolio level attributes
  attr(x, "nitems") <- nrow(x)
  attr(x, "bbox") <- st_bbox(x)
  attr(x, "resources") <- resources
  attr(x, "years") <- years
  attr(x, "outdir") <- outdir
  attr(x, "tmpdir") <- tmpdir
  attr(x, "cores") <- cores
  attr(x, "verbose") <- verbose
  attr(x, "aria_bin") <- aria_bin
  attr(x, "testing") <- FALSE
  x
}

#' Writing a portfolio to disk
#'
#' The function is used to save a processes biodiversity portfolio to disk.
#' In order to ensure interoperability with other geospatial software the only
#' supported format is the GeoPackage. If any other format is chosen, the
#' function will automatically replace the supplied file extension with '.gpkg'.
#' The metadata of a portfolio together with the geometry will be written
#' to a table called 'metadata'. All available and supported indicators, which
#' are expected to be present as a nested list columns will be written to their
#' own respective tables. In order to allow re-joining the metadata with the
#' indicators, it is expected that a column called 'assetid' which uniquely
#' identifies all assets is present. Usually, users do not have to take care of
#' this since the usual \code{{mapme.biodiversity}} workflow will ensure that this
#' columns is present. Additional arguments to \code{st_write()} can be supplied.
#'
#' @param x A portfolio object processed with \code{{mapme.biodiversity}}
#' @param dsn A file path for the output file. Should end with \code{'.gpkg'}
#' @param overwrite A logical indicating if the output file should be overwritten
#'   if it exists
#' @param ... Additional arguments supplied to \code{st_write()}
#' @return \code{x}, invisibly
#' @keywords function
#' @export
write_portfolio <- function(x,
                            dsn,
                            overwrite = FALSE,
                            ...) {
  assetid <- NULL
  all_indicators <- names(available_indicators())
  present_indicators <- names(x)[which(names(x) %in% all_indicators)]

  if (length(present_indicators) == 0) {
    stop("No calculated indicators have been found. Cannot write as a portfolio.")
  }

  if (tools::file_ext(dsn) != "gpkg") {
    warning("Can only write portfolio as GPKG. Changing file extension.")
    filename <- paste0(strsplit(basename(dsn), split = "\\.")[[1]][1], ".gpkg")
    if (length(filename) != 1) stop("Please omit any additional dots ('.') from your filename.")
    dir <- dirname(dsn)
    dsn <- file.path(dir, filename)
  }

  if (file.exists(dsn) & overwrite == FALSE) {
    stop(sprintf("Output file %s exists and overwrite is FALSE.", dsn))
  }

  if (any(lapply(x, class)[present_indicators] != "list")) {
    warning("Some indicators are not nested list columns. Setting them to metadata.")
    index <- which(lapply(x, class)[present_indicators] != "list")
    present_indicators <- present_indicators[-index]
  }

  if (!"assetid" %in% names(x)) {
    stop("Column 'assetid' is missing.")
  }

  if (nrow(x) != length(unique(x$assetid))) {
    stop("Column 'assetid' does not uniquley identify assets.")
  }

  # separate metadata from data
  metadata <- dplyr::select(x, -tidyselect::all_of(present_indicators))
  data <- st_drop_geometry(dplyr::select(x, assetid, tidyselect::all_of(present_indicators)))

  # initiate GPKG with metadata (including geometries)
  st_write(metadata, dsn, "metadata", delete_dsn = overwrite, ...)

  # loop through the nested indicators and append as their own layers
  for (ind in present_indicators) {
    tmp <- dplyr::select(data, assetid, tidyselect::all_of(ind))
    tmp <- tidyr::unnest(tmp, tidyselect::all_of(ind))
    st_write(tmp, dsn, ind, append = TRUE, ...)
  }
  invisible(x)
}


#' Reading a portfolio object from disk
#'
#' This function can be used to read a portfolio object that was previously
#' written to disk via \code{write_portfolio()} back into R as an \code{sf} object.
#' It should specifically be directed against a GeoPackage which was the output
#' of \code{write_portfolio()}, otherwise the function is very likely to fail.
#' All available indicators will be read back into R as nested list columns
#' reflecting the output once \code{calc_indicators()} has been called.
#'
#' **Important Note**
#' Portfolio-wide attributes that were specified via \code{init_portfolio()} will
#' not be reconstructed. The reason is that users most likely exported to a
#' GeoPackage in order to share their data, thus the file is very likely to be
#' opened on a different machine / in a different working directory. Users can
#' simply apply \code{init_portfolio()} on the object to re-set these attributes.
#'
#'
#' @param file A character vector pointing to a GeoPackage that has been
#'   previously written to disk via \code{write_portfolio()}
#' @param ... Additional arguments supplied to \code{st_read()}
#' @return An sf object object with nested list columns for every indicator
#'   table found in the GeoPackage source file.
#' @keywords function
#' @export
#'
read_portfolio <- function(file, ...) {
  assetid <- NULL
  all_layers <- st_layers(file)
  if (!"metadata" %in% all_layers$name | all_layers$geomtype[[which(all_layers$name == "metadata")]] != "Polygon") {
    stop(sprintf(
      "Input file at '%s' does not seem to be a proper mapme.biodiversity portfolio file written with 'write_portfolio()'",
      file
    ))
  }

  metadata <- read_sf(file, layer = "metadata", ...)
  present_indicators <- all_layers$name[which(all_layers$name %in% names(available_indicators()))]
  if (length(present_indicators) == 0) {
    stop("Could not find any mapme.biodiversity indicator tables in the input file.")
  }

  for (ind in present_indicators) {
    tmp <- read_sf(file, layer = ind, ...)
    tmp <- tidyr::nest(tmp, data = !assetid)
    names(tmp)[2] <- ind
    metadata <- dplyr::left_join(metadata, tmp, by = "assetid")
  }

  dplyr::relocate(metadata, !!attributes(metadata)[["sf_column"]], .after = tidyselect::last_col())
}
