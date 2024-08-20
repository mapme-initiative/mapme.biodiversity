#' Calculate human footprint statistics
#'
#' Human footprint data measures the pressure imposed on the natural environment
#' by different dimensions of human actions. The theoretical maximum value,
#' representing the highest level of human pressure, is 50. This routine allows
#' to extract zonal statistics of the human footprint data.
#'
#' The required resources for this indicator are:
#'  - [humanfootprint_resource]
#'
#' @name humanfootprint_indicator
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble the `humanfootprint`
#' as variable and the associated value (unitless) per year.
#' @importFrom mapme.biodiversity check_engine check_stats select_engine
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_humanfootprint(years = 2010)) %>%
#'   calc_indicators(calc_humanfootprint(stats = "median")) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_humanfootprint <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(
      x,
      humanfootprint,
      name = "humanfootprint",
      mode = "asset",
      aggregation = "stat",
      verbose = mapme_options()[["verbose"]]) {
    if (is.null(humanfootprint)) {
      return(NULL)
    }

    if (all(unlist(global(noNA(humanfootprint), fun = "sum")) == 0)) {
      return(NULL)
    }

    x <- st_transform(x, st_crs(humanfootprint))
    result <- select_engine(
      x = x,
      raster = humanfootprint,
      stats = stats,
      engine = engine,
      name = "humanfootprint",
      mode = "asset"
    )
    years <- as.numeric(substring(names(humanfootprint), 4, 7))
    result[["datetime"]] <- as.POSIXct(paste0(years, "-01-01T00:00:00Z"))
    result <- tidyr::pivot_longer(result, cols = -datetime, names_to = "variable")
    result[["value"]] <- sapply(result[["value"]], function(value) {
      ifelse(is.infinite(value) || is.nan(value), NA, value)
    })
    result[["unit"]] <- "unitless"
    result[, c("datetime", "variable", "unit", "value")]
  }
}

register_indicator(
  name = "humanfootprint",
  description = "Statistics of the human footprint data set per polygon.",
  resources = "humanfootprint"
)
