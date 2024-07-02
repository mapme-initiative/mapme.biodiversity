#' Calculate Monthly Burned Area based on MODIS (MCD64A1)
#'
#' Calculates Monthly Burned Area based on the Terra and Aqua combined
#' MCD64A1 Version 6.1. which s a monthly, global gridded 500 meter (m)
#' product containing per-pixel burned-area information.
#'
#' The required resources for this indicator are:
#'  - [mcd64a1]
#'
#' @name burned_area
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with variable burned
#'   area and corresponding area (in ha) as values.
#' @references Giglio, L., C. Justice, L. Boschetti, D. Roy. MODIS/Terra+Aqua
#'   Burned Area Monthly L3 Global 500m SIN Grid V061. 2021, distributed by
#'   NASA EOSDIS Land Processes Distributed Active Archive Center.
#'   \doi{https://doi.org/10.5067/MODIS/MCD64A1.061}
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
#'   get_resources(get_mcd64a1(years = 2010)) %>%
#'   calc_indicators(calc_burned_area(engine = "extract")) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_burned_area <- function(engine = "extract") {
  engine <- check_engine(engine)

  function(x,
           mcd64a1 = NULL,
           name = "burned_area",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(mcd64a1)) {
      return(NULL)
    }

    x_proj <- st_transform(x, st_crs(mcd64a1))
    mcd64a1 <- terra::mask(mcd64a1, x_proj)
    mcd64a1 <- terra::subst(mcd64a1, from = c(-2, -1, 0, NA), to = 0, others = 1)
    arearaster <- cellSize(mcd64a1, mask = FALSE, unit = "ha")
    mcd64a1 <- mcd64a1 * arearaster

    stats <- select_engine(
      x = x_proj,
      raster = mcd64a1,
      stats = "sum",
      engine = engine,
      name = "burned_area",
      mode = "asset"
    )

    names(stats) <- "value"
    dates <- gsub("^.*?\\.A([0-9]+)\\..*$", "\\1", names(mcd64a1))
    dates <- as.POSIXct(paste0(as.Date(dates, "%Y%j"), "T00:00:00Z"))
    stats[["datetime"]] <- dates
    stats[["unit"]] <- "ha"
    stats[["variable"]] <- "burned_area"
    stats[, c("datetime", "variable", "unit", "value")]
  }
}

register_indicator(
  name = "burned_area",
  description = "Monthly burned area detected by MODIS satellites",
  resources = "mcd64a1"
)
