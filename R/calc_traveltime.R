#' Calculate accessibility statistics
#'
#' Accessibility is the ease with which larger cities can be reached from a
#' certain location. This function allows to efficiently calculate accessibility
#' statistics (i.e. travel time to nearby major cities) for polygons. For each
#' polygon, the desired statistic/s (mean, median or sd) is/are returned.
#' The required resources for this indicator are:
#'  - [nelson_et_al]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{stats_accessibility}{Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name traveltime
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for accessibility statistics (in minutes)
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
#'   get_resources("nelson_et_al",
#'     range_traveltime = c("5k_10k", "100k_200k", "500k_1mio", "1mio_5mio")
#'   ) %>%
#'   calc_indicators("traveltime", stats_accessibility = c("min", "max"), engine = "extract") %>%
#'   tidyr::unnest(traveltime)))
NULL

#' Calculate accessibility to major cities' statistics
#'
#' Considering the 1km travel time raster datasets users can specify which
#' statistics among mean, median or standard deviation to compute. Also, users
#' can specify the functions i.e. zonal from package terra, extract from package
#' terra, or exactextract from exactextractr as desired.
#'
#' @param shp A single polygon for which to calculate the accessibility statistic
#' @param nelson_et_al The nelson_et_al raster resource (Weiß et al. (2018))
#' @param stats_accessibility Function to be applied to compute statistics for polygons
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

.calc_traveltime <- function(shp,
                             nelson_et_al,
                             engine = "extract",
                             stats_accessibility = "mean",
                             rundir = tempdir(),
                             verbose = TRUE,
                             todisk = FALSE,
                             ...) {
  if (is.null(nelson_et_al)) {
    return(NA)
  }
  # check if intermediate raster should be written to disk
  if (ncell(nelson_et_al) > 1024 * 1024) todisk <- TRUE
  # check if input engine is correctly specified
  available_engines <- c("zonal", "extract", "exactextract")
  .check_engine(available_engines, engine)
  # check if only supoorted stats have been specified
  available_stats <- c("mean", "median", "sd", "min", "max", "sum", "var")
  .check_stats(available_stats, stats_accessibility)

  # set max value of 65535 to NA
  nelson_et_al <- clamp(nelson_et_al,
                        lower = -Inf, upper = 65534, values = FALSE,
                        filename = ifelse(todisk, file.path(rundir, "traveltime.tif"), ""),
                        overwrite = TRUE,
                        datatype = "INT1U",
                        filetype = "GTiff"
  )

  if (engine == "extract") {
    .comp_traveltime_extract(
      shp = shp,
      nelson_et_al = nelson_et_al,
      stats = stats_accessibility
    )
  } else if (engine == "exactextract") {
    .comp_traveltime_exact_extract(
      shp = shp,
      nelson_et_al = nelson_et_al,
      stats = stats_accessibility
    )
  } else {
    .comp_traveltime_zonal(
      nelson_et_al = nelson_et_al,
      shp = shp,
      stats = stats_accessibility,
      todisk = todisk,
      rundir = rundir
    )
  }
}

#' Helper function to compute statistics using routines from terra zonal
#'
#' @param nelson_et_al nelson_et_al raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_traveltime_zonal <- function(nelson_et_al = NULL,
                                   shp = NULL,
                                   stats = "mean",
                                   todisk = FALSE,
                                   rundir = tempdir(),
                                   ...) {
  shp_v <- vect(shp)
  nelson_et_al <- terra::mask(nelson_et_al,
                              shp_v,
                              filename =  ifelse(todisk, file.path(rundir, "traveltime.tif"), ""),
                              datatype = "INT1U",
                              overwrite = TRUE
  )
  p_raster <- terra::rasterize(shp_v,
                               nelson_et_al,
                               field = 1:nrow(shp_v),
                               touches = TRUE,
                               filename =  ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
                               datatype = "INT1U",
                               overwrite = TRUE
  )

  shp_v <- vect(shp)
  results <- lapply(1:length(stats), function(j) {
    out <- terra::zonal(
      nelson_et_al,
      p_raster,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(minutes = unlist(out[-1]))
    names(out) <- paste0("minutes_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(nelson_et_al)
  distance_name <- unlist(lapply(layer_names, function(x) strsplit(x, "-|.tif")[[1]][2]))
  results$distance <- distance_name
  results
}

#' Helper function to compute statistics using routines from terra extract
#'
#' @param nelson_et_al nelson_et_al raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_traveltime_extract <- function(shp = NULL,
                                     nelson_et_al = NULL,
                                     stats = "mean",
                                     ...) {
  shp_v <- vect(shp)
  results <- lapply(1:length(stats), function(j) {
    out <- terra::extract(
      nelson_et_al,
      shp_v,
      fun = stats[j],
      na.rm = T
    )
    out <- tibble(minutes = unlist(out[-1]))
    names(out) <- paste0("minutes_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(nelson_et_al)
  distance_name <- unlist(lapply(layer_names, function(x) strsplit(x, "-|.tif")[[1]][2]))
  results$distance <- distance_name
  results
}

#' Helper function to compute statistics using routines from exactextractr
#'
#' @param nelson_et_al nelson_et_al raster from which to compute statistics
#'
#' @return A data-frame
#' @keywords internal
#' @noRd

.comp_traveltime_exact_extract <- function(shp = NULL,
                                           nelson_et_al = NULL,
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
        nelson_et_al,
        shp,
        fun = ifelse(stats[j] == "sd", "stdev", "variance")
      )
    } else {
      out <- exactextractr::exact_extract(
        nelson_et_al,
        shp,
        fun = stats[j]
      )
    }
    out <- tibble(minutes = unlist(out))
    names(out) <- paste0("minutes_", stats[j])
    out
  })
  results <- tibble(do.call(cbind, results))
  layer_names <- names(nelson_et_al)
  distance_name <- unlist(lapply(layer_names, function(x) strsplit(x, "-|.tif")[[1]][2]))
  results$distance <- distance_name
  results
}
