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
