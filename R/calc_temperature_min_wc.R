#' Calculate minimum temperature statistics based on WorldClim
#'
#' This function allows to efficiently calculate minimum temperature statistics
#' from Worldclim for polygons. For each polygon, the desired statistic/s (min,
#' max, sum, mean, median, sd or var) is/are returned.
#' The required resources for this indicator are:
#'  - minimum temperature layer from [worldclim]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_worldclim}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name temperature_min_wc
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for minimum temperature statistics (in °C)
#' @examples
#' \dontshow{
#' mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
#' }
#' \dontrun{
#' library(sf)
#' library(mapme.biodiversity)
#'
#' outdir <- file.path(tempdir(), "mapme-data")
#' dir.create(outdir, showWarnings = FALSE)
#'
#'
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2018,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("worldclim_min_temperature") %>%
#'   calc_indicators("temperature_min_wc",
#'     stats_worldclim = c("mean", "median"),
#'     engine = "extract"
#'   ) %>%
#'   tidyr::unnest(temperature_min_wc)
#'
#' aoi
#' }
NULL

#' Calculate worldclim minimum temperature statistics
#'
#' Considering the 1km minimum temperature raster datasets from worldclim users
#' can specify which statistics among min, max, sum, mean, median, variance or
#' standard deviation to compute. Also, users can specify the functions i.e. zonal
#' from package terra, extract from package terra, or exactextract from exactextractr
#' as desired.
#'
#' @param x A single polygon for which to calculate the minimum temperature statistic
#' @param worldclim_min_temperature minimum temperature raster from which to compute statistics
#' @param stats_worldclim Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd

.calc_temperature_min_wc <- function(x,
                                     worldclim_min_temperature,
                                     engine = "extract",
                                     stats_worldclim = "mean",
                                     ...) {
  results <- .calc_worldclim(
    x = x,
    worldclim = worldclim_min_temperature,
    engine = engine,
    stats_worldclim = stats_worldclim
  )
  results
}

#' Helper function to compute worldclim statistics
#'
#' @param worldclim worldclim raster from which to compute statistics
#' @param x A single polygon for which to calculate the climatic statistic
#' @param stats_worldclim Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#'
#' @return A data-frame
#' @keywords internal
#' @include register.R
#' @noRd
.calc_worldclim <- function(x,
                            worldclim,
                            engine = "extract",
                            stats_worldclim = "mean") {
  if (is.null(worldclim)) {
    return(NA)
  }
  # set max value of 65535 to NA
  worldclim <- clamp(
    worldclim,
    lower = -Inf,
    upper = 65534,
    values = FALSE
  )

  layer <- strsplit(names(worldclim), "_")[[1]][3]

  results <- .select_engine(
    x = x,
    raster = worldclim,
    stats = stats_worldclim,
    engine = engine,
    name = layer,
    mode = "asset"
  )

  dates <- unlist(lapply(names(worldclim), function(x) strsplit(x, "_")[[1]][4]))
  dates <- paste0(tools::file_path_sans_ext(dates), "-01")
  results$date <- as.Date(dates, "%Y-%m-%d")
  results
}

register_indicator(
  name = "temperature_min_wc",
  resources = list(worldclim_min_temperature = "raster"),
  fun = .calc_temperature_min_wc,
  arguments = list(
    engine = "extract",
    stats_worldclim = "mean"
  ),
  processing_mode = "asset"
)
