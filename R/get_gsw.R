#' Global Surface Water Change
#'
#' The Global Surface Water dataset was developed by the European Commission's
#' Joint Research Centre in the framework of the Copernicus Programme. It maps
#' the location and temporal distribution of water surfaces at the global scale
#' over the past 3.8 decades and provides statistics on their extent and change.
#' It is provisioned as a global tiled raster resource available for all land
#' areas. The reported data represent aggregated observations between 1984 - 2021.
#'
#' The change in water occurrence intensity between the two periods is derived
#' from homologous pairs of months (i.e. same months containing valid
#' observations in both periods). The difference in the occurrence of surface
#' water was calculated for each homologous pair of months. The average of all
#' of these differences constitutes the Surface Water Occurrence change
#' intensity. The raster files have integer cell values between \code{[0, 200]}
#' where 0 represents surface water loss and 200 represents surface water gain.
#'
#' @name global_surface_water_change
#' @param version A character vector indicating the version of the GSW data set
#'   to make available.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution
#' mapping of global surface water and its long-term changes. Nature 540,
#' 418–422 (2016). https://doi.org/10.1038/nature20584
#' @source \url{https://global-surface-water.appspot.com/}
#' @include register.R
#' @export
get_global_surface_water_change <- function(version = "v1_4_2021") {
  stopifnot(version %in% .gsw_versions)

  function(x,
           name = "global_surface_water_change",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_gsw(x, statistic = "change", version = version)
  }
}
#' Global Surface Water Transitions
#'
#' The Global Surface Water dataset was developed by the European Commission's
#' Joint Research Centre in the framework of the Copernicus Programme. It maps
#' the location and temporal distribution of water surfaces at the global scale
#' over the past 3.8 decades and provides statistics on their extent and change.
#' It is provisioned as a global tiled raster resource available for all land
#' areas. The reported data represent aggregated observations between 1984 - 2021.
#'
#' GSW transition data contains information about the type of surface water
#' change for each pixel. The raster files have integer cell values between
#' \code{[0, 10]} that code for different transition classes:
#'
#' | Value | Transition Class      |
#' |-------|-----------------------|
#' | 1     | Permanent             |
#' | 2     | New Permanent         |
#' | 3     | Lost Permanent        |
#' | 4     | Seasonal              |
#' | 5     | New Seasonal          |
#' | 6     | Lost Seasonal         |
#' | 7     | Seasonal to Permanent |
#' | 8     | Permanent to Seasonal |
#' | 9     | Ephemeral Permanent   |
#' | 10    | Ephemeral Seasonal    |
#'
#' @name global_surface_water_transitions
#' @param version A character vector indicating the version of the GSW data set
#'   to make available.
#' @keywords resource
#' @returns  A character of file paths.
#' @references Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution
#' mapping of global surface water and its long-term changes. Nature 540,
#' 418–422 (2016). https://doi.org/10.1038/nature20584
#' @source \url{https://global-surface-water.appspot.com/}
#' @include register.R
#' @export
get_global_surface_water_transitions <- function(version = "v1_4_2021") {
  stopifnot(version %in% .gsw_versions)

  function(x,
           name = "global_surface_water_transitions",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_gsw(x, statistic = "transitions", version = version)
  }
}

#' Global Surface Water Seasonality
#'
#' The Global Surface Water dataset was developed by the European Commission's
#' Joint Research Centre in the framework of the Copernicus Programme. It maps
#' the location and temporal distribution of water surfaces at the global scale
#' over the past 3.8 decades and provides statistics on their extent and change.
#' It is provisioned as a global tiled raster resource available for all land
#' areas. The reported data represent aggregated observations between 1984 - 2021.
#'
#' GSW seasonality describes the intra-annual distribution of surface water for
#' each pixel. The raster files have integer cell values between \code{[0, 12]},
#' indicating how many months per year the pixel was classified as water.
#'
#' @name global_surface_water_seasonality
#' @param version A character vector indicating the version of the GSW data set
#'   to make available.
#' @keywords resource
#' @returns  A character of file paths.
#' @references Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution
#' mapping of global surface water and its long-term changes. Nature 540,
#' 418–422 (2016). https://doi.org/10.1038/nature20584
#' @source \url{https://global-surface-water.appspot.com/}
#' @include register.R
#' @export
get_global_surface_water_seasonality <- function(version = "v1_4_2021") {
  stopifnot(version %in% .gsw_versions)

  function(x,
           name = "global_surface_water_seasonality",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_gsw(x, statistic = "seasonality", version = version)
  }
}

#' Global Surface Water Recurrence
#'
#' The Global Surface Water dataset was developed by the European Commission's
#' Joint Research Centre in the framework of the Copernicus Programme. It maps
#' the location and temporal distribution of water surfaces at the global scale
#' over the past 3.8 decades and provides statistics on their extent and change.
#' It is provisioned as a global tiled raster resource available for all land
#' areas. The reported data represent aggregated observations between 1984 - 2021.
#'
#' Water Recurrence is a measurement of the degree of variability in the
#' presence of water from year to year. It describes the frequency with which
#' water returned to a particular location from one year to another, and is
#' expressed as a percentage. The raster files have integer cell values between
#' \code{[0, 100]}, where 100 represents that water reoccurs predictably every
#' year, whereas lower values indicate that water only occurs episodically.
#'
#' @name global_surface_water_recurrence
#' @param version A character vector indicating the version of the GSW data set
#'   to make available.
#' @keywords resource
#' @returns  A character of file paths.
#' @references Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution
#' mapping of global surface water and its long-term changes. Nature 540,
#' 418–422 (2016). https://doi.org/10.1038/nature20584
#' @source \url{https://global-surface-water.appspot.com/}
#' @include register.R
#' @export
get_global_surface_water_recurrence <- function(version = "v1_4_2021") {
  stopifnot(version %in% .gsw_versions)

  function(x,
           name = "global_surface_water_recurrence",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_gsw(x, statistic = "recurrence", version = version)
  }
}

#' Global Surface Water Occurrence
#'
#' The Global Surface Water dataset was developed by the European Commission's
#' Joint Research Centre in the framework of the Copernicus Programme. It maps
#' the location and temporal distribution of water surfaces at the global scale
#' over the past 3.8 decades and provides statistics on their extent and change.
#' It is provisioned as a global tiled raster resource available for all land
#' areas. The reported data represent aggregated observations between 1984 - 2021.
#'
#' GSW occurrence raw data comes in raster files with integer cell values
#' between \code{[0, 100]}. This value gives the percentage of the time that a
#' given pixel was classified as water during the entire observation period. So
#' a 0 denotes a pixel that was never classified as water, 100 denotes a pixel
#' with permanent water.
#'
#' @name global_surface_water_occurrence
#' @param version A character vector indicating the version of the GSW data set
#'   to make available.
#' @keywords resource
#' @returns  A character of file paths.
#' @references Pekel, JF., Cottam, A., Gorelick, N. et al. High-resolution
#' mapping of global surface water and its long-term changes. Nature 540,
#' 418–422 (2016). https://doi.org/10.1038/nature20584
#' @source \url{https://global-surface-water.appspot.com/}
#' @include register.R
#' @export
get_global_surface_water_occurrence <- function(version = "v1_4_2021") {
  stopifnot(version %in% .gsw_versions)

  function(x,
           name = "global_surface_water_occurrence",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .get_gsw(x, statistic = "occurrence", version = version)
  }
}

.get_gsw <- function(x, statistic = "occurrence", version = "v1_4_2021") {
  stopifnot(
    statistic %in% .gsw_statistics,
    version %in% .gsw_versions
  )

  # make the gsw grid and construct urls for intersecting tiles
  baseurl <- sprintf(
    "/vsicurl/https://storage.googleapis.com/global-surface-water/downloads2021/%s/%s",
    statistic, statistic
  )
  grid_gfc <- make_global_grid(
    xmin = -180, xmax = 170, dx = 10,
    ymin = -50, ymax = 80, dy = 10
  )
  tile_ids <- unique(unlist(st_intersects(x, grid_gfc)))
  if (length(tile_ids) == 0) {
    stop("The extent of the portfolio does not intersect with the GSW grid.",
      call. = FALSE
    )
  }
  ids <- sapply(tile_ids, function(n) .get_gsw_tile_id(grid_gfc[n, ]))
  urls <- sprintf("%s_%s%s.tif", baseurl, ids, version)

  fps <- grid_gfc[tile_ids, ]
  fps[["source"]] <- urls

  make_footprints(fps, what = "raster", co = c("-co", "COMPRESS=LZW", "-ot", "Byte"))
}

.get_gsw_tile_id <- function(tile) {
  min_x <- st_bbox(tile)[1]
  max_y <- st_bbox(tile)[4]
  ndigits_x <- nchar(abs(min_x))
  ndigits_y <- nchar(abs(max_y))
  formatstr_x <- paste0("%0", ndigits_x, "i")
  formatstr_y <- paste0("%0", ndigits_y, "i")
  if (min_x < 0) {
    min_x <- paste0(sprintf(formatstr_x, abs(min_x)), "W")
  } else {
    min_x <- paste0(sprintf(formatstr_x, abs(min_x)), "E")
  }
  if (max_y < 0) {
    max_y <- paste0(sprintf(formatstr_y, abs(max_y)), "S")
  } else {
    max_y <- paste0(sprintf(formatstr_y, abs(max_y)), "N")
  }
  paste0(min_x, "_", max_y)
}

.gsw_statistics <- c(
  "change",
  "occurrence",
  "recurrence",
  "seasonality",
  "transitions"
)

.gsw_versions <- c(
  "v1_4_2021"
)

.gsw_descr <- data.frame(
  name = .gsw_statistics,
  desc = c(
    "Global Surface Water - Change of water occurrence intensity",
    "Global Surface Water - Percentage of water occurrence",
    "Global Surface Water - Percentage of water recurrence",
    "Global Surface Water - Seasonality of water occurrrence",
    "Global Surface Water - Transition classes"
  )
)

for (gsw_statistic in .gsw_statistics) {
  resource_name <- paste0("global_surface_water_", gsw_statistic)
  register_resource(
    name = resource_name,
    description = .gsw_descr$desc[which(.gsw_descr$name == gsw_statistic)],
    licence = "https://www.copernicus.eu/en/access-data",
    source = "https://global-surface-water.appspot.com/download",
    type = "raster"
  )
}
