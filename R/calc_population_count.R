#' Calculate population count statistics
#'
#' WorldPop, which was initiated in 2013, offers easy access to spatial demographic
#' datasets, claiming to use peer-reviewed and fully transparent methods to create
#' global mosaics for the years 2000 to 2020. This function allows to efficiently
#' calculate population count statistics (e.g. total number of population) for
#' polygons. For each polygon, the desired statistic/s (min, max, sum, mean,
#' median, sd or var) is/are returned.
#' The required resources for this indicator are:
#'  - [worldpop]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_popcount}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name population_count
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for population count statistics
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
#'     years = 2000:2010,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("worldpop") %>%
#'   calc_indicators("population_count", stats_popcount = c("sum", "median"), engine = "extract") %>%
#'   tidyr::unnest(population_count)))
NULL

#' Calculate population count statistics
#'
#' Considering the 1km WorldPop Unconstrained Global Mosaics raster datasets users
#' can specify which statistics among min, max, sum, mean, median, standard deviation
#' or var to compute. Also, users can specify the functions i.e. zonal from package
#' terra, extract from package terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the population count statistic
#' @param worldpop The population count raster resource from worldPop
#' @param stats_popcount Function to be applied to compute statistics for polygons
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

.calc_population_count <- function(shp,
                                   worldpop,
                                   engine = "extract",
                                   stats_popcount = "sum",
                                   rundir = tempdir(),
                                   verbose = TRUE,
                                   todisk = FALSE,
                                   ...) {
  if (is.null(worldpop)) {
    return(NA)
  }
  # check if intermediate raster should be written to disk
  if (ncell(worldpop) > 1024 * 1024) todisk <- TRUE
  # check if input engine is correctly specified
  available_engines <- c("zonal", "extract", "exactextract")
  .check_engine(available_engines, engine)
  # check if only supoorted stats have been specified
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  .check_stats(available_stats, stats_popcount)

  # set max value of 65535 to NA
  worldpop <- clamp(worldpop,
                    lower = -Inf, upper = 65534, values = FALSE,
                    filename = ifelse(todisk, file.path(rundir, "worldpop.tif"), ""),
                    overwrite = TRUE,
                    datatype = "INT1U",
                    filetype = "GTiff"
  )

  if (engine == "extract") {
    .comp_worldpop_extract(
      shp = shp,
      worldpop = worldpop,
      stats = stats_popcount
    )
  } else if (engine == "exactextract") {
    .comp_worldpop_exact_extract(
      shp = shp,
      worldpop = worldpop,
      stats = stats_popcount
    )
  } else {
    .comp_worldpop_zonal(
      worldpop = worldpop,
      shp = shp,
      stats = stats_popcount,
      todisk = todisk,
      rundir = rundir
    )
  }
}

#' Helper function to compute statistics using routines from terra zonal
#'
#' @param worldpop population count raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_worldpop_zonal <- function(worldpop = NULL,
                                 shp = NULL,
                                 stats = "sum",
                                 todisk = FALSE,
                                 rundir = tempdir(),
                                 ...) {
  shp_v <- vect(shp)
  worldpop <- terra::mask(worldpop,
                          shp_v,
                          filename =  ifelse(todisk, file.path(rundir, "worldpop.tif"), ""),
                          datatype = "INT1U",
                          overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp_v,
                               worldpop,
                               field = 1:nrow(shp_v),
                               touches = TRUE,
                               filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
                               datatype = "INT1U",
                               overwrite = TRUE
  )

  results <- lapply(1:length(stats), function(j) {
    out <- terra::zonal(
      worldpop,
      p_raster,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(population = unlist(out[-1]))
    names(out) <- paste0("popcount_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(worldpop)
  year_name <- unlist(lapply(layer_names, function(x) strsplit(x, "_")[[1]][2]))
  results$year <- year_name
  results
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param worldpop population count raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_worldpop_extract <- function(worldpop = NULL,
                                   shp = NULL,
                                   stats = "sum",
                                   ...) {
  shp_v <- vect(shp)
  results <- lapply(1:length(stats), function(j) {
    out <- terra::extract(
      worldpop,
      shp_v,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(population = unlist(out[-1]))
    names(out) <- paste0("popcount_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(worldpop)
  year_name <- unlist(lapply(layer_names, function(x) strsplit(x, "_")[[1]][2]))
  results$year <- year_name
  results
}

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param worldpop population count raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_worldpop_exact_extract <- function(worldpop = NULL,
                                         shp = NULL,
                                         stats = "sum",
                                         ...) {
  if(!requireNamespace("exactextractr", quietly = TRUE)){
    stop(paste(
      "Needs package 'exactextractr' to be installed.",
      "Consider installing with 'install.packages('exactextractr')"
    ))
  }
  results <- lapply(1:length(stats), function(j) {
    if (stats[j] %in% c("sd", "var")) {
      out <- exactextractr::exact_extract(
        worldpop,
        shp,
        fun = ifelse(stats[j] == "sd", "stdev", "variance")
      )
    } else {
      out <- exactextractr::exact_extract(
        worldpop,
        shp,
        fun = stats[j]
      )
    }
    out <- tibble(population = unlist(out))
    names(out) <- paste0("popcount_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(worldpop)
  year_name <- unlist(lapply(layer_names, function(x) strsplit(x, "_")[[1]][2]))
  results$year <- year_name
  results
}
