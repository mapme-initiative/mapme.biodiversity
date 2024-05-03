#' Calculate minimum temperature statistics based on WorldClim
#'
#' This function allows to efficiently calculate minimum temperature statistics
#' from Worldclim for polygons. For each polygon, the desired statistic/s (min,
#' max, sum, mean, median, sd or var) is/are returned.
#'
#' The required resources for this indicator are:
#'  - minimum temperature layer from [worldclim_min_temperature]
#'
#' @name temperature_min_wc
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A function that returns a tibble with a column for minimum
#'   temperature statistics (in °C).
#' @include register.R
#' @export
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
#' mapme_options(
#'   outdir = outdir,
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_worldclim_min_temperature(years = 2018)) %>%
#'   calc_indicators(
#'     calc_temperature_min_wc(
#'       engine = "extract",
#'       stats = c("mean", "median")
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_temperature_min_wc <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           worldclim_min_temperature = NULL,
           name = "temperature_min_wc",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(worldclim_min_temperature)) {
      return(NULL)
    }

    results <- .calc_worldclim(
      x = x,
      worldclim = worldclim_min_temperature,
      engine = engine,
      stats = stats,
      unit = "C"
    )
    results
  }
}

register_indicator(
  name = "temperature_min_wc",
  description = "Statistics of WorldClim minimum temperature layer",
  resources = "worldclim_min_temperature"
)

#' Helper function to compute worldclim statistics
#'
#' @param worldclim worldclim raster from which to compute statistics
#' @param x A single polygon for which to calculate the climatic statistic
#' @param stats Function to be applied to compute statistics for polygons
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
                            stats = "mean",
                            unit = NULL) {
  stopifnot(!is.null(unit))
  if (is.null(worldclim)) {
    return(NULL)
  }
  # set max value of 65535 to NA
  worldclim <- clamp(
    worldclim,
    lower = -Inf,
    upper = 65534,
    values = FALSE
  )

  layer <- paste0("worldclim_", strsplit(names(worldclim), "_")[[1]][3])
  datetime <- unlist(lapply(names(worldclim), function(x) strsplit(x, "_")[[1]][4]))
  datetime <- as.Date(paste0(tools::file_path_sans_ext(datetime), "-01"))

  results <- select_engine(
    x = x,
    raster = worldclim,
    stats = stats,
    engine = engine,
    name = layer,
    mode = "asset"
  )

  results[["datetime"]] <- datetime

  results %>%
    tidyr::pivot_longer(-datetime, names_to = "variable", values_to = "value") %>%
    dplyr::mutate(unit = unit) %>%
    dplyr::select(datetime, variable, unit, value)
}
