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
#'   tidyr::unnest(soilproperties)
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
           verbose = mapme_options()[["verbose"]]) {
    # check if input engines are correct
    if (is.null(soilgrids)) {
      return(NA)
    }
    results <- select_engine(
      x = x,
      raster = soilgrids,
      stats = stats,
      engine = engine,
      mode = "asset"
    )

    parameters <- gsub(".tif", "", names(soilgrids))
    parameters <- lapply(parameters, function(param) {
      splitted <- strsplit(param, "_")[[1]]
      names(splitted) <- c("layer", "depth", "stat")
      splitted
    })

    results$layer <- sapply(parameters, function(para) para["layer"])
    results$depth <- sapply(parameters, function(para) para["depth"])
    results$stat <- sapply(parameters, function(para) para["stat"])

    # conversion to conventional units
    conv_df <- lapply(.sg_layers, function(y) as.data.frame(y))
    conv_df <- do.call(rbind, conv_df)["conversion_factor"]
    conv_df$layer <- row.names(conv_df)
    results <- tibble(merge(results, conv_df))
    # apply conversion factor
    for (stat in stats) results[[stat]] <- results[[stat]] / results[["conversion_factor"]]
    # select cols in right order
    as_tibble(results[, c("layer", "depth", "stat", stats)])
  }
}

register_indicator(
  name = "soilproperties",
  description = "Statistics of SoilGrids layers",
  resources = "soilgrids"
)
