#' Calculate treeloss statistics
#'
#' This functions allows to efficiently calculate the treecover and emissions
#' indicators in a single function call together. Since most of the pre-processing
#' operations for treecover and emissions are the same, it is more efficient
#' to calculate them in one run if users are actually interested in both statistics.
#' Otherwise users are advised to use the respective single indicator functions.
#'
#' The required resources for this indicator are:
#'  - [gfw_treecover]
#'  - [gfw_lossyear]
#'  - [gfw_emissions]
#'
#' @name treecover_area_and_emissions
#' @param years A numeric vector with the years for which to calculate treecover
#'   area and emissions.
#' @param min_size The minimum size of a forest patch in ha.
#' @param min_cover The minimum threshold of stand density for a pixel to be
#'   considered forest in the year 2000.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with variables treecover
#'   and emissions ind corresponding values (in ha and Mg) as value.
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
#'     calc_treecover_area_and_emissions(years = 2016:2017, min_size = 1, min_cover = 30)
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_treecover_area_and_emissions <- function(years = 2000:2023,
                                              min_size = 10,
                                              min_cover = 35) {
  check_namespace("exactextractr")
  check_namespace("landscapemetrics", error = FALSE)
  min_cover <- .gfw_check_min_cover(min_cover, "treecover_area_and_emissions")
  min_size <- .gfw_check_min_size(min_size, "treecover_area_and_emissions")
  years <- .gfw_check_years(years, "treecover_area_and_emissions")

  function(x,
           gfw_treecover,
           gfw_lossyear,
           gfw_emissions,
           name = "treecover_area_and_emissions",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    # handling of return value if resources are missing, e.g. no overlap
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
        losses <- .sum_gfw(data, "coverage_area", max_year)
        names(losses)[2] <- "loss"
        emissions <- .sum_gfw(data, "emissions")

        org_coverage <- sum(data[["coverage_area"]])

        if (all(losses[["loss"]] == 0)) {
          result <- tibble::tibble(
            years = years,
            emissions = 0,
            treecover = org_coverage,
          )
          return(result)
        }

        previous_losses <- sum(losses[["loss"]][losses[["years"]] < years[1]])

        if (previous_losses != 0) {
          org_coverage <- org_coverage - previous_losses
        }

        losses <- losses[losses[["years"]] %in% years, ]
        emissions <- emissions[emissions[["years"]] %in% years, ]
        losses[["treecover"]] <- org_coverage
        losses[["treecover"]] <- losses[["treecover"]] - cumsum(losses[["loss"]])
        losses[["emissions"]] <- emissions[["emissions"]]
        losses[, c("years", "emissions", "treecover")]
      },
      min_size = min_size, coverage_area = TRUE, summarize_df = TRUE
    )

    gfw_stats %>%
      tidyr::pivot_longer(-years, names_to = "variable") %>%
      dplyr::mutate(
        datetime = as.POSIXct(paste0(years, "-01-01T00:00:00Z")),
        unit = ifelse(variable == "treecover", "ha", "Mg")
      ) %>%
      dplyr::select(datetime, variable, unit, value)
  }
}


register_indicator(
  name = "treecover_area_and_emissions",
  description = "Area of forest cover and greenhouse gas emssions caused by forest loss by year",
  resources = c(
    "gfw_treecover",
    "gfw_lossyear",
    "gfw_emissions"
  )
)
