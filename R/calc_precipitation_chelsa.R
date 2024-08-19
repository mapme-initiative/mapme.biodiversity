#' Calculate precipitation average based on CHELSA
#'
#' This functions allows to calculate averaged precipitation
#' from the CHELSA downscaled precipitation layers. Based on
#' user-selected years, monthly averages of precipitation are calculated.
#'
#' The required resources for this indicator are:
#'  - [chelsa]
#'
#' @name precipitation_chelsa
#' @param years A numeric vector indicating the years for which to calculate
#'   precipitation statistics.
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with variable
#'   precipitation and sum of precipitation (in mm/m^2) as value.
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
#'   get_resources(get_chelsa(years = 2010)) %>%
#'   calc_indicators(
#'     calc_precipitation_chelsa(
#'       years = 2010,
#'       engine = "extract"
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_precipitation_chelsa <- function(years = 1979:2018,
                                      engine = "extract") {
  engine <- check_engine(engine)
  avail_years <- 1979:2018
  years <- check_available_years(years, avail_years, "precipitation_chelsa")

  function(x,
           chelsa,
           name = "precipitation_chelsa",
           mode = "asset",
           aggregation = "mean",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(chelsa)) {
      return(NULL)
    }

    if (length(years) == 0) {
      return(NULL)
    }

    dates <- substr(names(chelsa), 11, 17)
    dates <- as.Date(paste0("01_", dates), "%d_%m_%Y")
    datetime <- as.POSIXct(paste0(dates, "T00:00:00Z"))

    layer_years <- as.numeric(substr(names(chelsa), 14, 17))
    chelsa <- chelsa[[which(layer_years %in% years)]]

    # extract zonal statistics
    stats <- select_engine(
      x = x,
      raster = chelsa / 100,
      stats = "mean",
      engine = engine,
      mode = "asset"
    )

    tibble(
      datetime = datetime,
      variable = "precipitation",
      unit = "mm/m^2",
      value = as.numeric(stats[["mean"]])
    )
  }
}


register_indicator(
  name = "precipitation_chelsa",
  description = "Statistics of CHELSA precipitation layer",
  resources = "chelsa"
)
