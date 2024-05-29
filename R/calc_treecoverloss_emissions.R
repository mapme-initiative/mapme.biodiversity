#' Calculate emission statistics
#'
#' This functions allows to efficiently calculate emission statistics for
#' areas of interest. For each year in the analysis timeframe, the forest losses
#' from Hansen et al. (2013) are overlayed with the respective emission layer
#' from Harris et al. (2021) and area-wise emission statistics are calculated
#' for each year.
#'
#' The required resources for this indicator are:
#'  - [gfw_treecover]
#'  - [gfw_lossyear]
#'  - [gfw_emissions]
#'
#' @name treecoverloss_emissions
#' @param years A numeric vector with the years for which to calculate emissions
#'   caused by treecover loss.
#' @param min_size The minimum size of a forest patch in ha.
#' @param min_cover The minimum threshold of stand density for a pixel to be
#'   considered forest in the year 2000.
#' @keywords indicator
#' @returns A function that returns a tibble with a column for years and
#'   emissions (in Mg).
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
#'   get_resources(
#'     get_gfw_treecover(version = "GFC-2023-v1.11"),
#'     get_gfw_lossyear(version = "GFC-2023-v1.11"),
#'     get_gfw_emissions()
#'   ) %>%
#'   calc_indicators(
#'     calc_treecoverloss_emissions(years = 2016:2017, min_size = 1, min_cover = 30)
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_treecoverloss_emissions <- function(years = 2000:2023,
                                         min_size = 10,
                                         min_cover = 35) {
  check_namespace("exactextractr")
  min_cover <- .gfw_check_min_cover(min_cover, "treecoverloss_emissions")
  min_size <- .gfw_check_min_size(min_size, "treecoverloss_emissions")
  years <- .gfw_check_years(years, "treecoverloss_emissions")

  function(x,
           gfw_treecover,
           gfw_lossyear,
           gfw_emissions,
           name = "treecoverloss_emissions",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    emissions <- NULL

    if (any(is.null(gfw_treecover), is.null(gfw_lossyear), is.null(gfw_emissions))) {
      return(NULL)
    }
    # mask gfw
    gfw_treecover <- terra::mask(gfw_treecover, x)
    # check if gfw_treecover only contains 0s, e.g. on the ocean
    if (.gfw_empty_raster(gfw_treecover, min_cover)) {
      return(NULL)
    }
    # prepare gfw rasters
    gfw <- .gfw_prep_rasters(x, gfw_treecover, gfw_lossyear, gfw_emissions, min_cover)

    # retrieves maximum lossyear value from layer name
    max_year <- as.numeric(
      gsub(
        ".*GFC-([0-9]+)-.*", "\\1",
        names(gfw_lossyear)
      )
    )

    if (max_year < min(years)) {
      return(NULL)
    }

    gfw_stats <- exactextractr::exact_extract(
      gfw, x, function(data, min_size) {
        # retain only forest pixels and set area to ha
        data <- .prep_gfw_data(data, min_size)
        emissions <- .sum_gfw(data, "emissions", max_year)

        if (all(emissions[["emissions"]] == 0)) {
          result <- tibble::tibble(
            years = years,
            emissions = 0
          )
          return(result)
        }

        emissions <- emissions[emissions[["years"]] %in% years, ]
        emissions[, c("years", "emissions")]
      },
      min_size = min_size, coverage_area = TRUE, summarize_df = TRUE
    )

    gfw_stats %>%
      dplyr::mutate(
        datetime = as.Date(paste0(years, "-01-01")),
        variable = "emissions",
        unit = "Mg",
        value = emissions
      ) %>%
      dplyr::select(datetime, variable, unit, value) %>%
      tibble::as_tibble()
  }
}


register_indicator(
  name = "treecoverloss_emissions",
  description = "Greenouse gas emissions cause by forest loss by year",
  resources = c(
    "gfw_treecover",
    "gfw_lossyear",
    "gfw_emissions"
  )
)
