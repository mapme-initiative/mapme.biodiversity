#' Calculate minimum temperature statistics based on WorldClim
#'
#' This function allows to efficiently calculate minimum temperature statistics
#' from Worldclim for polygons. For each polygon, the desired statistic/s (min,
#' max, sum, mean, median, sd or var) is/are returned.
#' The required resources for this indicator are:
#'  - minimum temperature layer from [worldclim]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_worldclim}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name temperature_min_wc
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for minimum temperature statistics (in °C)
#' @examples
#' library(sf)
#' library(mapme.biodiversity)
#'
#' temp_loc <- file.path(tempdir(), "mapme.biodiversity")
#' if (!file.exists(temp_loc)) {
#'   dir.create(temp_loc)
#'   resource_dir <- system.file("res", package = "mapme.biodiversity")
#'   file.copy(resource_dir, temp_loc, recursive = TRUE)
#' }
#'
#' (try(aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2018,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("worldclim_min_temperature") %>%
#'   calc_indicators("temperature_min_wc",
#'     stats_worldclim = c("mean", "median"),
#'     engine = "extract"
#'   ) %>%
#'   tidyr::unnest(temperature_min_wc)))
NULL

#' Calculate worldclim minimum temperature statistics
#'
#' Considering the 1km minimum temperature raster datasets from worldclim users
#' can specify which statistics among min, max, sum, mean, median, variance or
#' standard deviation to compute. Also, users can specify the functions i.e. zonal
#' from package terra, extract from package terra, or exactextract from exactextractr
#' as desired.
#'
#' @param shp A single polygon for which to calculate the minimum temperature statistic
#' @param worldclim_min_temperature minimum temperature raster from which to compute statistics
#' @param stats_worldclim Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @noRd

.calc_temperature_min_wc <- function(shp,
                                     worldclim_min_temperature,
                                     engine = "extract",
                                     stats_worldclim = "mean",
                                     rundir = tempdir(),
                                     verbose = TRUE,
                                     todisk = FALSE,
                                     ...) {
  results <- .calc_worldclim(
    shp = shp,
    worldclim = worldclim_min_temperature,
    engine = engine,
    stats_worldclim = stats_worldclim,
    rundir = rundir,
    verbose = verbose,
    todisk = todisk
  )
  results
}

#' Helper function to compute worldclim statistics
#'
#' @param worldclim worldclim raster from which to compute statistics
#' @param shp A single polygon for which to calculate the climatic statistic
#' @param stats_worldclim Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.calc_worldclim <- function(shp,
                            worldclim,
                            engine = "extract",
                            stats_worldclim = "mean",
                            rundir = tempdir(),
                            verbose = TRUE,
                            todisk = FALSE,
                            ...) {
  if (is.null(worldclim)) {
    return(NA)
  }
  # check if intermediate raster should be written to disk
  if (ncell(worldclim) > 1024 * 1024) todisk <- TRUE
  # check if input engine is correctly specified
  available_engines <- c("zonal", "extract", "exactextract")
  .check_engine(available_engines, engine)
  # check if only supoorted stats have been specified
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  .check_stats(available_stats, stats_worldclim)

  # set max value of 65535 to NA
  worldclim <- clamp(worldclim,
    lower = -Inf, upper = 65534, values = FALSE,
    filename = ifelse(todisk, file.path(rundir, "worldclim.tif"), ""),
    overwrite = TRUE,
    datatype = "INT1U",
    filetype = "GTiff"
  )

  if (engine == "extract") {
    .comp_worldclim_extract(
      shp = shp,
      worldclim = worldclim,
      stats = stats_worldclim
    )
  } else if (engine == "exactextract") {
    .comp_worldclim_exact_extract(
      shp = shp,
      worldclim = worldclim,
      stats = stats_worldclim
    )
  } else {
    .comp_worldclim_zonal(
      worldclim = worldclim,
      shp = shp,
      stats = stats_worldclim,
      todisk = todisk,
      rundir = rundir
    )
  }
}


#' Helper function to compute statistics using routines from terra zonal
#'
#' @param worldclim worldclim raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_worldclim_zonal <- function(worldclim = NULL,
                                  shp = NULL,
                                  stats = "mean",
                                  todisk = FALSE,
                                  rundir = tempdir(),
                                  ...) {
  shp_v <- vect(shp)
  worldclim <- terra::mask(worldclim,
    shp_v,
    filename =  ifelse(todisk, file.path(rundir, "worldclim.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp_v,
    worldclim,
    field = 1:nrow(shp_v),
    touches = TRUE,
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )
  layer <- strsplit(names(worldclim), "_")[[1]][3]
  results <- lapply(1:length(stats), function(j) {
    out <- terra::zonal(
      worldclim,
      p_raster,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(worldclim = unlist(out[-1]))
    names(out) <- paste0(layer, "_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(worldclim)
  date_name <- unlist(lapply(layer_names, function(x) strsplit(x, "_")[[1]][4]))
  date_name <- paste0(tools::file_path_sans_ext(date_name), "-01")
  results$date <- as.Date(date_name, "%Y-%m-%d")
  results
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param worldclim worldclim raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_worldclim_extract <- function(worldclim = NULL,
                                    shp = NULL,
                                    stats = "mean",
                                    ...) {
  shp_v <- vect(shp)
  layer <- strsplit(names(worldclim), "_")[[1]][3]
  results <- lapply(1:length(stats), function(j) {
    out <- terra::extract(
      worldclim,
      shp_v,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(worldclim = unlist(out[-1]))
    names(out) <- paste0(layer, "_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(worldclim)
  date_name <- unlist(lapply(layer_names, function(x) strsplit(x, "_")[[1]][4]))
  date_name <- paste0(tools::file_path_sans_ext(date_name), "-01")
  results$date <- as.Date(date_name, "%Y-%m-%d")
  results
}

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param worldclim worldclim raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd


.comp_worldclim_exact_extract <- function(worldclim = NULL,
                                          shp = NULL,
                                          stats = "mean",
                                          ...) {
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }
  layer <- strsplit(names(worldclim), "_")[[1]][3]
  results <- lapply(1:length(stats), function(j) {
    if (stats[j] %in% c("sd", "var")) {
      out <- exactextractr::exact_extract(
        worldclim,
        shp,
        fun = ifelse(stats[j] == "sd", "stdev", "variance")
      )
    } else {
      out <- exactextractr::exact_extract(
        worldclim,
        shp,
        fun = stats[j]
      )
    }
    out <- tibble(worldclim = unlist(out))
    names(out) <- paste0(layer, "_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(worldclim)
  date_name <- unlist(lapply(layer_names, function(x) strsplit(x, "_")[[1]][4]))
  date_name <- paste0(tools::file_path_sans_ext(date_name), "-01")
  results$date <- as.Date(date_name, "%Y-%m-%d")
  results
}
