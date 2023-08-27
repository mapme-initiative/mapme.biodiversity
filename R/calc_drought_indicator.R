#' Calculate drought indicator statistics
#'
#' This function allows to efficiently calculate the relative wetness in the
#' shallow groundwater section with regard to the the 1948-2012 reference period.
#' The values represent the wetness percentile a given area achieves at a given
#' point in time in regard to the reference period.
#' For each polygon, the desired statistic/s (mean, median or sd) is/are
#' returned. The required resources for this indicator are:
#'  - [nasa_grace]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_drought}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character "mean", "median" or "sd".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name drought_indicator
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for each specified stats and a column with the respective date.
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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2022,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("nasa_grace") %>%
#'   calc_indicators("drought_indicator",
#'     stats_drought = c("mean", "median"),
#'     engine = "extract"
#'   ) %>%
#'   tidyr::unnest(drought_indicator)
#'
#' aoi
#' }
NULL

#' Calculate drought indicator statistics
#'
#' Considering the 0.25 degrees drought indicator raster datasets users can specify
#' which statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param x A single polygon for which to calculate the drought statistic
#' @param nasa_grace The drought indicator raster resource from NASA GRACE
#' @param stats_drought Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "mean", "median" or "sd".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_drought_indicator <- function(x,
                                    nasa_grace,
                                    engine = "extract",
                                    stats_drought = "mean",
                                    verbose = TRUE,
                                    processing_mode = "portfolio",
                                    ...) {
  # check if input engines are correct
  if (is.null(nasa_grace)) {
    return(NA)
  }
  results <- .select_engine(
    x = x,
    raster = nasa_grace,
    stats = stats_drought,
    engine = engine,
    name = "wetness",
    mode = processing_mode
  )

  dates <- sub(".*(\\d{8}).*", "\\1", names(nasa_grace))
  dates <- as.Date(dates, format = "%Y%m%d")
  if (processing_mode == "portfolio") {
    results <- purrr::map(results, function(x) {
      x$date <- dates
      x
    })
  } else {
    results$date <- dates
  }
  results
}

register_indicator(
  name = "drought_indicator",
  resources = list(nasa_grace = "raster"),
  fun = .calc_drought_indicator,
  arguments = list(
    engine = "extract",
    stats_drought = "mean"
  ),
  processing_mode = "portfolio"
)
