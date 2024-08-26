#' Calculate Global Surface Water Time Series
#'
#' This function calculates the total area of the global surface water time
#' series data, separated by the following classes:
#'
#' - No Observation: It was not possible to determine whether a pixel was water
#' (this may be the case for frozen areas or during the polar night in extreme
#' latitudes).
#' - Permanent Water: Water was detected in twelve months per year or in a
#' combination of permanent and no observation.
#' - Seasonal Water: Water and no water was detected.
#' - No Water: No Water was detected.
#'
#' The required resources for this indicator are:
#'  - [gsw_time_series_resource]
#'
#' @name gsw_time_series_indicator
#' @keywords indicator
#' @format A function returning a tibble with time series of global surface
#' water data classes.
#' @include register.R
#' @export
#' @examples
#' \dontshow{
#' mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
#' outdir <- file.path(tempdir(), "mapme-data")
#' path_gsw_ts <- file.path(outdir, "gsw_time_series")
#' gsw_fnames_short <- dir(path_gsw_ts, pattern = ".tif$")
#' gsw_fnames_long <- sub("v5_", "VER5-0_yearlyClassification", gsw_fnames_short)
#'
#' invisible(file.copy(
#'    file.path(path_gsw_ts, gsw_fnames_short),
#'    file.path(path_gsw_ts, gsw_fnames_long)
#' ))
#' }
#' \dontrun{
#' library(mapme.biodiversity)
#' library(sf)
#'
#' outdir <- file.path(tempdir(), "mapme-data")
#' dir.create(outdir, showWarnings = FALSE)
#'
#' mapme_options(
#'   outdir = outdir,
#'   verbose = FALSE
#' )
#'
#' aoi <- read_sf(
#'   system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'               package = "mapme.biodiversity"
#' ))
#' aoi <- get_resources(aoi, get_gsw_time_series (years = 2000:2001))
#' aoi <- calc_indicators(aoi, calc_gsw_time_series())
#' aoi <- portfolio_long(aoi)
#'
#' aoi
#' }
calc_gsw_time_series <- function() {
  check_namespace("exactextractr")

  function(x = NULL,
           gsw_time_series,
           name = "gsw_timeseries",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {

    if (is.null(gsw_time_series)) {
      return(NULL)
    }

    gsw_time_series <- mask(
      gsw_time_series,
      x
    )

    gsw_time_series <- clamp(
      gsw_time_series,
      lower = 0,
      upper = 3,
      values = FALSE
    )

    idx_yr <- as.numeric(gregexpr("yearlyClassification", names(gsw_time_series))) + 20
    years_resource <- substr(names(gsw_time_series), idx_yr, idx_yr + 3)

    names(gsw_time_series) <- years_resource
    coverage_fractions <- exactextractr::exact_extract(gsw_time_series, x, "frac", coverage_area = TRUE)
    if(nlyr(gsw_time_series) == 1) {
      names(coverage_fractions) <- paste(names(coverage_fractions), years_resource, sep = ".")
    }
    x_total_area <- as.numeric(st_area(x)) / 10000

    results <- purrr::map_df(names(coverage_fractions), function(colname) {
      year <- substr(colname, nchar(colname) - 3, nchar(colname))
      variable <- switch(
        substr(colname, 6, 6),
        "0" = "no_observations",
        "1" = "not_water",
        "2" = "seasonal_water",
        "3" = "permanent_water"
      )
      tibble::tibble(
        datetime = as.POSIXct(paste0(year, "-01-01T00:00:00Z")),
        variable = variable,
        unit = "ha",
        value = coverage_fractions[[colname]] * x_total_area
      )
    })

    return(results)
  }
}

register_indicator(
  name = "gsw_time_series",
  description = "Global Surface Water - Yearly Time Series area estimation of water classes.",
  resources = "gsw_time_series"
)
