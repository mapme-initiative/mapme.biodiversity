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
#'  - [gsw_time_series]
#'
#' @name gsw_time_series
#' @keywords indicator
#' @param years Numeric vector of years to process between 1984 and 2021.
#' Default: `1984:2021`.
#' @format A function returning a tibble with time series of global surface
#' water data classes.
#' @export
calc_gsw_time_series <- function(years = 1984:2021) {
  check_namespace("exactextractr")
  available_years <- 1984:2021
  years <- check_available_years(years, available_years, "gsw_time_series")

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

    names(gsw_time_series) <- years
    coverage_fractions <- exactextractr::exact_extract(gsw_time_series, x, "frac", coverage_area = TRUE)
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
