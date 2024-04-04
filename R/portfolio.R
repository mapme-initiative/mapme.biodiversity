.check_portfolio <- function(x, verbose = mapme_options()[["verbose"]]) {
  stopifnot(inherits(x, "sf"))

  if (st_crs(x) != st_crs(4326)) {
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x <- st_transform(x, 4326)
  }
  if (any(!unique(st_geometry_type(x)) %in% c("POLYGON"))) {
    stop("Some assests are not of type POLYGON. Please use sf::st_cast() to cast to POLYGON.")
  }
  if (!inherits(x, "tibble")) {
    x <- st_as_sf(tibble::as_tibble(x))
  }
  if ("assetid" %in% names(x) && verbose) {
    message(
      paste("Found a column named 'assetid'.",
        " Overwritting its values with a unique identifier.",
        sep = ""
      )
    )
  }
  x$assetid <- 1:nrow(x)
  x
}

#' Writing a portfolio to disk
#'
#' `write_portfolio()` writes a processed biodiversity portfolio to disk.
#' In order to ensure interoperability with other geospatial software the only
#' supported format is the GeoPackage.
#' The metadata of a portfolio together with the geometry will be written
#' to a table called `'metadata'`. All calculated indicators, which
#' are expected to be present as nested list columns, will be written to their
#' own respective tables. In order to allow re-joining the metadata with the
#' indicators, it is expected that a column called `'assetid'` which uniquely
#' identifies all assets is present. Usually, users do not have to take care of
#' this since the usual `mapme.biodiversity` workflow will ensure that this
#' columns is present. Additional arguments to `st_write()` can be supplied.
#'
#' @param x A portfolio object processed with `mapme.biodiversity`
#' @param dsn A file path for the output file. Should end with `'.gpkg'`
#' @param overwrite A logical indicating if the output file should be overwritten
#'   if it exists
#' @param ... Additional arguments supplied to `st_write()`
#' @return `write_portfolio()` returns `x`, invisibly.
#' @name portfolio
#' @export
write_portfolio <- function(x,
                            dsn,
                            overwrite = FALSE,
                            ...) {
  assetid <- NULL
  stopifnot(inherits(x, "sf"))
  list_cols <- sapply(st_drop_geometry(x), is.list)
  present_indicators <- names(list_cols)[list_cols]


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
#' `read_portfolio()` is used to read a portfolio object that was previously
#' written to disk via `write_portfolio()` back into R as an `sf` object.
#' It should be directed against a GeoPackage which was the output
#' of `write_portfolio()`, otherwise the function is very likely to fail.
#' All available indicators will be read back into R as nested list columns
#' reflecting the output once `calc_indicators()` has been called.
#'
#' @param src A character vector pointing to a GeoPackage that has been
#'   previously written to disk via `write_portfolio()`
#' @param ... Additional arguments supplied to `st_read()`
#' @return `read_portfolio()` reutnrs an `sf` object object with nested list
#'   columns for every indicator table found in the GeoPackage source file.
#' @name portfolio
#' @export
#'
read_portfolio <- function(src, ...) {
  assetid <- NULL
  all_layers <- st_layers(src)
  if (!"metadata" %in% all_layers$name) {
    stop(sprintf(
      "Input file at '%s' does not seem to be a proper mapme.biodiversity portfolio file written with 'write_portfolio()'",
      src
    ))
  }

  metadata <- read_sf(src, layer = "metadata", ...)
  present_indicators <- all_layers$name[-which(all_layers$name == "metadata")]
  if (length(present_indicators) == 0) {
    stop("Could not find any mapme.biodiversity indicator tables in the input file.")
  }

  for (ind in present_indicators) {
    tmp <- read_sf(src, layer = ind, ...)
    tmp <- tidyr::nest(tmp, data = !assetid)
    names(tmp)[2] <- ind
    metadata <- dplyr::left_join(metadata, tmp, by = "assetid")
  }

  dplyr::relocate(metadata, !!attributes(metadata)[["sf_column"]], .after = tidyselect::last_col())
}
