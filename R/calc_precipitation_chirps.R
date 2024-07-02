#' Calculate precipitation sums based on CHIRPS
#'
#' This functions allows to calculate precipitation sums based on the
#' CHIRPS rainfall estimates. Corresponding to the time-frame of the analysis
#' of the portfolio, monthly precipitation sums are calculated.
#'
#' The required resources for this indicator are:
#'  - [chirps]
#'
#' @name precipitation_chirps
#' @param years A numeric vector indicating the years for which to calculate
#'   precipitation statistics.
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with variable
#'   precipitation and sum of precipitation (in mm) as value.
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
#'   get_resources(get_chirps(years = 2010)) %>%
#'   calc_indicators(
#'     calc_precipitation_chirps(
#'       years = 2010,
#'       engine = "extract"
#'     )
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_precipitation_chirps <- function(years = 1981:2020,
                                      engine = "extract") {
  engine <- check_engine(engine)
  avail_years <- seq(1981, format(Sys.Date(), "%Y"))
  years <- check_available_years(years, avail_years, "precipitation_chirps")

  function(x,
           chirps,
           name = "precipitation_chirps",
           mode = "portfolio",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(chirps)) {
      return(NULL)
    }

    if (length(years) == 0) {
      return(NULL)
    }

    src_names <- names(chirps)
    # set values smaller 0 to NA
    chirps <- clamp(
      chirps,
      lower = 0,
      upper = Inf,
      values = FALSE
    )

    layer_years <- as.numeric(substr(src_names, 13, 17))
    target_chirps <- chirps[[which(layer_years %in% years)]]
    dates <- as.Date(paste0(substr(names(target_chirps), 13, 19), ".01"), "%Y.%m.%d")
    datetime <- as.POSIXct(paste0(dates, "T00:00:00Z"))

    # extract zonal statistics
    results <- select_engine(
      x = x,
      raster = target_chirps,
      stats = "sum",
      engine = engine,
      mode = "portfolio"
    )

    if (mode == "portfolio") {
      results <- purrr::map(1:length(results), function(i) {
        tibble(
          datetime = datetime,
          variable = "precipitation",
          unit = "mm",
          value = as.numeric(results[[i]][["sum"]])
        )
      })
    } else {
      results <- tibble(
        datetime = datetime,
        variable = "precipitation",
        unit = "mm",
        value = as.numeric(results[[1]][["sum"]])
      )
    }
    results
  }
}


register_indicator(
  name = "precipitation_chirps",
  description = "Statistics of CHIRPS precipitation layer",
  resources = "chirps"
)
