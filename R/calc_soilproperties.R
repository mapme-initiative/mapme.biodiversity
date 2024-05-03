#' Calculate Zonal Soil Properties
#'
#' This indicator allows the extraction of zonal statistics for resource layers
#' previously downloaded from SoilGrids, thus in total supporting the calculation
#' of zonal statistics for 10 different soil properties at 6 different depths for
#' a total of 4 different model outputs (stat). Zonal statistics will be calculated
#' for all SoilGrid layers that have been previously made available vie \code{get_resources()}.
#'
#' The required resource for this indicator is:
#'  - [soilgrids]
#'
#' @name soilproperties
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A tibble with a column for the SoilGrid layer, the depth and the model
#'   output statistic as well as additional columns for all zonal statistics
#'   specified via \code{stats_soil}
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
#'     get_soilgrids(
#'       layers = c("clay", "silt"),
#'       depths = c("0-5cm", "5-15cm"),
#'       stats = "mean"
#'     )
#'   ) %>%
#'   calc_indicators(
#'     calc_soilproperties(engine = "extract", stats = c("mean", "median"))
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_soilproperties <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           soilgrids = NULL,
           name = "soilproperties",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    layer <- stat <- conversion_factor <- conventional_units <- NULL
    # check if input engines are correct
    if (is.null(soilgrids)) {
      return(NULL)
    }
    results <- select_engine(
      x = x,
      raster = soilgrids,
      stats = stats,
      engine = engine,
      mode = "asset"
    )

    layers <- gsub("-", "_", gsub(".tif", "", names(soilgrids)))
    results[["variable"]] <- layers
    results[["layer"]] <- purrr::map_chr(layers, function(lyr) strsplit(lyr, "_")[[1]][1])
    results <- tidyr::pivot_longer(results, -c(variable, layer), names_to = "stat", values_to = "value")

    conv_df <- purrr::map(.sg_layers, tibble::as_tibble) %>% purrr::list_rbind()
    conv_df[["layer"]] <- names(.sg_layers)

    results <- tibble::tibble(merge(results, conv_df))

    results %>%
      dplyr::mutate(
        value = value / conversion_factor,
        variable = paste0(variable, "_", stat),
        datetime = as.Date("2017-02-01")
      ) %>%
      dplyr::select(datetime, variable, unit = conventional_units, value = value)
  }
}

register_indicator(
  name = "soilproperties",
  description = "Statistics of SoilGrids layers",
  resources = "soilgrids"
)
