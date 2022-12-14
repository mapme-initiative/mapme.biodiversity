#' Calculate Zonal Soil Properties
#'
#' This indicator allows the extraction of zonal statistics for resource layers
#' previously downloaded from SoilGrids, thus in total supporting the calculation
#' of zonal statistics for 10 different soil properties at 6 different depths for
#' a total of 4 different model outputs (stat). Zonal statistics will be calculated
#' for all SoilGrid layers that have been previously made available vie \code{get_resources()}.
#' The required resource for this indicator is:
#'  - [soilgrids]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_soil}{Function to be applied to compute statistics for polygons either
#'   single or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name soilproperties
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for the SoilGrid layer, the depth and the model
#'   output statistic as well as additional columns for all zonal statistics
#'   specified via \code{stats_soil}
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#'
#' temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#' if(!file.exists(temp_loc)){
#' dir.create(temp_loc)
#' resource_dir <- system.file("res", package = "mapme.biodiversity")
#' file.copy(resource_dir, temp_loc, recursive = TRUE)
#' }
#'
#' (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'                         package = "mapme.biodiversity") %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2022,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("soilgrids",
#'     layers = c("clay", "silt"), depths = c("0-5cm", "5-15cm"), stats = "mean"
#'   ) %>%
#'   calc_indicators("soilproperties", stats_soil = c("mean", "median"), engine = "extract") %>%
#'   tidyr::unnest(soilproperties)))
NULL

.calc_soilproperties <- function(shp,
                                 soilgrids,
                                 engine = "zonal",
                                 stats_soil = "mean",
                                 rundir = tempdir(),
                                 verbose = TRUE,
                                 todisk = FALSE,
                                 ...) {
  # check if input engines are correct
  if (is.null(soilgrids)) {
    return(NA)
  }
  # check if intermediate raster should be written to disk
  if (ncell(soilgrids) > 1024 * 1024) todisk <- TRUE
  # check if input engine is correctly specified
  available_engines <- c("zonal", "extract", "exactextract")
  .check_engine(available_engines, engine)
  # check if only supoorted stats have been specified
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  .check_stats(available_stats, stats_soil)

  if (engine == "extract") {
    extractor <- .soil_extract
  }
  if (engine == "exactextract") {
    extractor <- .soil_exactextractr
  }
  if (engine == "zonal") {
    extractor <- .soil_zonal
  }

  extractor(
    shp = shp,
    soilgrids = soilgrids,
    stats = stats_soil,
    todisk = todisk,
    rundir = rundir
  )
}

.soil_zonal <- function(shp = NULL,
                        soilgrids,
                        stats = "mean",
                        todisk = FALSE,
                        rundir = tempdir(),
                        ...) {
  shp_v <- vect(shp)
  parameters <- gsub(".tif", "", names(soilgrids))
  parameters <- lapply(parameters, function(param) {
    splitted <- strsplit(param, "_")[[1]]
    names(splitted) <- c("layer", "depth", "stat")
    splitted
  })

  soilgrids_mask <- terra::mask(soilgrids,
    shp_v,
    filename =  ifelse(todisk, file.path(rundir, "soilgrids.tif"), ""),
    overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp_v,
    soilgrids_mask,
    field = 1:nrow(shp_v),
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    overwrite = TRUE
  )
  results <- lapply(stats, function(stat) {
    out <- terra::zonal(soilgrids_mask,
      p_raster,
      fun = stat,
      na.rm = T
    )
    as.numeric(out[-1])
  })

  names(results) <- stats
  results <- as.data.frame(results)
  results$layer <- sapply(parameters, function(para) para["layer"])
  results$depth <- sapply(parameters, function(para) para["depth"])
  results$stat <- sapply(parameters, function(para) para["stat"])
  conv_df <- lapply(.sg_layers, function(y) as.data.frame(y))
  conv_df <- do.call(rbind, conv_df)["conversion_factor"]
  conv_df$layer <- row.names(conv_df)
  results <- merge(results, conv_df)
  for (stat in stats) results[[stat]] <- results[[stat]] / results[["conversion_factor"]]
  as_tibble(results[, c("layer", "depth", "stat", stats)])
}

.soil_extract <- function(shp = NULL,
                          soilgrids = NULL,
                          stats = "mean",
                          todisk = todisk,
                          rundir = tempdir(),
                          ...) {
  shp_v <- vect(shp)
  parameters <- gsub(".tif", "", names(soilgrids))
  parameters <- lapply(parameters, function(param) {
    splitted <- strsplit(param, "_")[[1]]
    names(splitted) <- c("layer", "depth", "stat")
    splitted
  })

  soilgrids_mask <- terra::mask(soilgrids,
    shp_v,
    filename =  ifelse(todisk, file.path(rundir, "soilgrids.tif"), ""),
    overwrite = TRUE
  )
  results <- lapply(stats, function(stat) {
    out <- terra::extract(soilgrids_mask,
      shp_v,
      fun = stat,
      na.rm = T
    )
    as.numeric(out[-1])
  })

  names(results) <- stats
  results <- as.data.frame(results)
  results$layer <- sapply(parameters, function(para) para["layer"])
  results$depth <- sapply(parameters, function(para) para["depth"])
  results$stat <- sapply(parameters, function(para) para["stat"])
  conv_df <- lapply(.sg_layers, function(y) as.data.frame(y))
  conv_df <- do.call(rbind, conv_df)["conversion_factor"]
  conv_df$layer <- row.names(conv_df)
  results <- merge(results, conv_df)
  for (stat in stats) results[[stat]] <- results[[stat]] / results[["conversion_factor"]]
  as_tibble(results[, c("layer", "depth", "stat", stats)])
}

.soil_exactextractr <- function(soilgrids = NULL,
                                shp = NULL,
                                stats = "mean",
                                todisk = todisk,
                                rundir = tempdir(),
                                ...) {
  if(!requireNamespace("exactextractr", quietly = TRUE)){
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }
  parameters <- gsub(".tif", "", names(soilgrids))
  parameters <- lapply(parameters, function(param) {
    splitted <- strsplit(param, "_")[[1]]
    names(splitted) <- c("layer", "depth", "stat")
    splitted
  })

  results <- lapply(stats, function(stat) {
    if (stat %in% c("sd", "var")) {
      out <- exactextractr::exact_extract(
        soilgrids,
        shp,
        fun = ifelse(stat == "sd", "stdev", "variance")
      )
    } else {
      out <- exactextractr::exact_extract(
        soilgrids,
        shp,
        fun = stat
      )
    }
    as.numeric(out)
  })

  names(results) <- stats
  results <- as.data.frame(results)
  results$layer <- sapply(parameters, function(para) para["layer"])
  results$depth <- sapply(parameters, function(para) para["depth"])
  results$stat <- sapply(parameters, function(para) para["stat"])
  conv_df <- lapply(.sg_layers, function(y) as.data.frame(y))
  conv_df <- do.call(rbind, conv_df)["conversion_factor"]
  conv_df$layer <- row.names(conv_df)
  results <- merge(results, conv_df)
  for (stat in stats) results[[stat]] <- results[[stat]] / results[["conversion_factor"]]
  as_tibble(results[, c("layer", "depth", "stat", stats)])
}
