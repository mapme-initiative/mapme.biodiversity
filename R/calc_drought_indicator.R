#' Calculate drought indicator statistics
#'
#' This function allows to efficiently calculate the relative wetness in the
#' shallow groundwater section with regard to the the 1948-2012 reference period.
#' The values represent the wetness percentile a given area achieves at a given
#' point in time in regard to the reference period.
#' For each polygon, the desired statistic/s (mean, median or sd) is/are
#' returned. The required resources for this indicator are:
#'  - [nasagrace]
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
#'   get_resources("nasagrace") %>%
#'   calc_indicators("drought_indicator", stats_drought = c("mean", "median"), engine = "extract") %>%
#'   tidyr::unnest(drought_indicator)))
NULL

#' Calculate drought indicator statistics
#'
#' Considering the 0.25 degrees drought indicator raster datasets users can specify
#' which statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the drought statistic
#' @param nasagrace The drought indicator raster resource from NASA GRACE
#' @param stats_drought Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "mean", "median" or "sd".
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

.calc_drought_indicator <- function(shp,
                                    nasagrace,
                                    engine = "extract",
                                    stats_drought = "mean",
                                    rundir = tempdir(),
                                    verbose = TRUE,
                                    todisk = FALSE,
                                    processing_mode = "portfolio",
                                    ...) {

  # check if input engines are correct
  if (is.null(nasagrace)) {
    return(NA)
  }
  # check if intermediate raster should be written to disk
  if (ncell(nasagrace) > 1024 * 1024) todisk <- TRUE
  # check if input engine is correctly specified
  available_engines <- c("zonal", "extract", "exactextract")
  .check_engine(available_engines, engine)
  # check if only supoorted stats have been specified
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  .check_stats(available_stats, stats_drought)

  if (engine == "extract") {
    extractor <- .comp_drought_extract
  }
  if (engine == "exactextract") {
    extractor <- .comp_drought_exact_extract
  }
  if (engine == "zonal") {
    extractor <- .comp_drought_zonal
  }


  if (processing_mode == "asset") {
    results <- extractor(
      nasagrace = nasagrace,
      shp = shp,
      stats = stats_drought,
      todisk = todisk,
      rundir = rundir
    )
  }

  if (processing_mode == "portfolio") {
    cores <- attributes(shp)$cores
    results <- parallel::mclapply(1:nrow(shp), function(i) {
      out <- extractor(
        nasagrace = nasagrace,
        shp = shp[i, ],
        stats = stats_drought,
        todisk = todisk,
        rundir = rundir
      )
    }, mc.cores = cores)
  }

  results
}

#' Helper function to compute statistics using routines from terra zonal
#'
#' @param nasagrace drought indicator raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_drought_zonal <- function(nasagrace = NULL,
                                shp = NULL,
                                stats = "mean",
                                todisk = FALSE,
                                rundir = tempdir(),
                                ...) {
  shp_v <- vect(shp)
  nasagrace <- terra::mask(nasagrace,
    shp_v,
    filename =  ifelse(todisk, file.path(rundir, "nasagrace.tif"), ""),
    overwrite = TRUE
  )

  p_raster <- terra::rasterize(shp_v,
    nasagrace,
    field = 1:nrow(shp_v),
    touches = TRUE,
    filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    overwrite = TRUE
  )

  results <- lapply(1:length(stats), function(j) {
    out <- terra::zonal(
      nasagrace,
      p_raster,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(wetness = as.numeric(out[-1]))
    names(out) <- paste0("wetness_", stats[j])
    out
  })

  results <- tibble(do.call(cbind, results))
  bn <- names(nasagrace)
  time_frame <- sub(".*(\\d{8}).*", "\\1", bn)
  time_frame <- as.Date(time_frame, format = "%Y%m%d")
  results$date <- time_frame
  results
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param nasagrace drought indicator raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_drought_extract <- function(shp = NULL,
                                  nasagrace = NULL,
                                  stats = "mean",
                                  ...) {
  shp_v <- vect(shp)
  results <- lapply(1:length(stats), function(j) {
    out <- terra::extract(
      nasagrace,
      shp_v,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(wetness = as.numeric(out[-1]))
    names(out) <- paste0("wetness_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  bn <- names(nasagrace)
  time_frame <- sub(".*(\\d{8}).*", "\\1", bn)
  time_frame <- as.Date(time_frame, format = "%Y%m%d")
  results$date <- time_frame
  results
}

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param nasagrace drought indicator raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_drought_exact_extract <- function(shp = NULL,
                                        nasagrace = NULL,
                                        stats = "mean",
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
        nasagrace,
        shp,
        fun = ifelse(stats[j] == "sd", "stdev", "variance")
      )
    } else {
      out <- exactextractr::exact_extract(
        nasagrace,
        shp,
        fun = stats[j]
      )
    }
    out <- tibble(wetness = as.numeric(out))
    names(out) <- paste0("wetness_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  bn <- names(nasagrace)
  time_frame <- sub(".*(\\d{8}).*", "\\1", bn)
  time_frame <- as.Date(time_frame, format = "%Y%m%d")
  results$date <- time_frame
  results
}
