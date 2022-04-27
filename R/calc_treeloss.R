#' Calculate treeloss statistics
#'
#' This functions allows to efficiently calculate the treecover and emissions
#' indicators in a single function call together. Since most of the pre-processing
#' operations for treecover and emissions are the same, it is more efficient
#' to calculate them in one run if users are actually interested in both statistics.
#' Otherwise users are advised to use the respective single indicator functions.
#' The required resources for this indicator are:
#'  - [treecover2000]
#'  - [lossyear]
#'  - [greenhouse].
#'
#' The following arguments can be set:
#' \describe{
#'   \item{min_size}{The minum size of a forest patch to be considered as forest in ha.}
#'   \item{min_cover}{The minimum cover percentage per pixel to be considered as forest.}
#' }
#'
#' @name treeloss
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years, treecover (in ha), and emissions (in Mg CO2)
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
#' (aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg", package = "mapme.biodiversity") %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2016:2017,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources(
#'     resources = c("treecover2000", "lossyear", "greenhouse"),
#'     vers_treecover = "GFC-2020-v1.8", vers_lossyear = "GFC-2020-v1.8"
#'   ) %>%
#'   calc_indicators("treeloss", min_size = 1, min_cover = 30) %>%
#'   tidyr::unnest(treeloss))
NULL


#' Calculate treecover are and greenhouse gas emissions
#'
#' This is a joint indicator calculating both, treecover and greenhouse
#' gas emissions per treecover loss. It actually combines the indicators for
#' treecover and emissions into a single function since the computations for
#' both indicators are very similar. If a user is interested only in one of
#' these two indicators consider applying the respective functions.
#'
#' @param shp A single polygon for which to calculate the tree cover statistic
#' @param treecover2000 The treecover 2000 resource from GFW
#' @param lossyear The lossyear resource from GFW
#' @param greenhouse The greenhouse emission layer from GFW
#' @param min_size The minimum size of a forest patch in ha.
#' @param min_cover The minimum threshold of stand density for a pixel to be
#'   considered forest in the year 2000.
#' @param rundir A directory where intermediate files are written to.
#' @param verbose A directory where intermediate files are written to.
#' @param todisk Logical indicating whether or not temporary raster files shall
#'   be written to disk
#' @param ... additional arguments
#' @return A tibble
#' @importFrom stringr str_sub
#' @keywords internal
#' @noRd
.calc_treeloss <- function(shp,
                           treecover2000,
                           lossyear,
                           greenhouse,
                           min_size = 10,
                           min_cover = 35,
                           rundir = tempdir(),
                           verbose = TRUE,
                           todisk = FALSE,
                           ...) {

  # initial argument checks
  # handling of return value if resources are missing, e.g. no overlap
  if (any(is.null(treecover2000), is.null(lossyear), is.null(greenhouse))) {
    return(NA)
  }
  # retrieve years from portfolio
  years <- attributes(shp)$years

  if (any(years < 2000)) {
    warning(paste("Cannot calculate treeloss statistics ",
      "for years smaller than 2000.",
      sep = ""
    ))
    years <- years[years >= 2000]
    if (length(years) == 0) {
      return(
        tibble(
          years = NA,
          treecover = NA,
          emissions = NA
        )
      )
    }
  }
  if (ncell(treecover2000) > 1024 * 1024) todisk <- TRUE
  # check if treecover2000 only contains 0s, e.g. on the ocean
  minmax_treecover2000 <- unique(as.vector(minmax(treecover2000)))
  if (length(minmax_treecover2000) == 1) {
    if (minmax_treecover2000 == 0 | is.nan(minmax_treecover2000)) {
      return(
        tibble(
          years = years,
          treecover = rep(0, length(years)),
          emissions = rep(0, length(years))
        )
      )
    }
  }

  # check additional arguments
  min_cover_msg <- paste("Argument 'min_cover' for indicator 'treeloss' ",
    "must be a numeric value between 0 and 100.",
    sep = ""
  )
  if (is.numeric(min_cover)) {
    min_cover <- as.integer(round(min_cover))
  } else {
    stop(min_cover_msg, call. = FALSE)
  }
  if (min_cover < 0 || min_cover > 100) {
    stop(min_cover_msg, call. = FALSE)
  }

  min_size_msg <- paste("Argument 'min_size' for indicator 'treeloss' ",
    "must be a numeric value greater 0.",
    sep = ""
  )
  if (is.numeric(min_size)) {
    min_size <- as.integer(round(min_size))
  } else {
    stop(min_size_msg, call. = FALSE)
  }
  if (min_size <= 0) stop(min_size_msg, call. = FALSE)

  #------------------------------------------------------------------------------
  # start calculation if everything is set up correctly
  # retrieve an area raster
  arearaster <- cellSize(
    treecover2000,
    unit = "ha",
    filename = ifelse(todisk, file.path(rundir, "arearaster.tif"), ""),
    datatype = "FLT4S",
    overwrite = TRUE
  )
  # rasterize the polygon
  polyraster <- rasterize(
    vect(shp), treecover2000,
    field = 1, touches = TRUE,
    filename = ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # mask treecover2000
  treecover2000 <- mask(
    treecover2000, polyraster,
    filename =  ifelse(todisk, file.path(rundir, "treecover2000.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # mask lossyear
  lossyear <- mask(
    lossyear, polyraster,
    filename =  ifelse(todisk, file.path(rundir, "lossyear.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # resample greenhouse if extent doesnt match
  if (ncell(greenhouse) != ncell(treecover2000)) {
    greenhouse <- resample(
      greenhouse, treecover2000,
      method = "bilinear",
      filename =  ifelse(todisk, file.path(rundir, "greenhouse.tif"), ""),
      datatype = "FLT4S",
      overwrite = TRUE
    )
  }
  # mask greenhouse
  greenhouse <- mask(
    greenhouse, polyraster,
    filename =  ifelse(todisk, file.path(rundir, "greenhouse.tif"), ""),
    datatype = "FLT4S",
    overwrite = TRUE
  )

  # binarize the treecover2000 layer based on min_cover argument
  binary_treecover2000 <- classify(
    treecover2000,
    rcl = matrix(c(0, min_cover, 0, min_cover, 100, 1), ncol = 3, byrow = TRUE),
    include.lowest = TRUE,
    filename = ifelse(todisk, file.path(rundir, "binary_treecover2000.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )
  # retrieve patches of comprehensive forest areas
  patched <- patches(
    binary_treecover2000,
    directions = 4, zeroAsNA = TRUE,
    filename = ifelse(todisk, file.path(rundir, "patched.tif"), ""),
    datatype = "INT4U",
    overwrite = TRUE
  )

  unique_vals <- unique(as.vector(minmax(patched)))
  if (length(unique_vals) == 1) {
    if (is.nan(unique_vals)) {
      return(
        tibble(
          years = years,
          treecover = rep(0, length(years)),
          emissions = rep(0, length(years))
        )
      )
    }
  }
  # get the sizes of the patches
  patchsizes <- zonal(
    arearaster, patched, sum,
    as.raster = TRUE,
    filename = ifelse(todisk, file.path(rundir, "patchsizes.tif"), ""),
    datatype = "FLT4S",
    overwrite = TRUE
  )
  # remove patches smaller than threshold
  binary_treecover2000 <- ifel(
    patchsizes < min_size, 0, binary_treecover2000,
    filename = ifelse(todisk, file.path(rundir, "binary_treecover2000.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # return 0 if binary treecover2000 only consits of 0 or nan
  minmax_treecover2000 <- unique(as.vector(minmax(binary_treecover2000)))
  if (length(minmax_treecover2000) == 1) {
    if (minmax_treecover2000 == 0 | is.nan(minmax_treecover2000)) {
      return(
        tibble(
          years = years,
          treecover = rep(0, length(years)),
          emissions = rep(0, length(years))
        )
      )
    }
  }

  # set no loss occurrences to NA
  lossyear <- ifel(
    lossyear == 0, NA, lossyear,
    filename = ifelse(todisk, file.path(rundir, "lossyear.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # exclude non-tree pixels from lossyear layer
  lossyear <- mask(
    lossyear, binary_treecover2000,
    filename = ifelse(todisk, file.path(rundir, "lossyear.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # get forest cover statistics for each year
  yearly_loss_values <- lapply(years, function(y) {
    y <- y - 2000
    current_treecover2000 <- ifel(
      lossyear <= y, NA, binary_treecover2000,
      filename = ifelse(todisk, file.path(rundir, "current_treecover2000.tif"), ""),
      datatype = "INT1U",
      overwrite = TRUE
    )
    current_arearaster <- mask(
      arearaster, current_treecover2000,
      maskvalues = c(NA, 0),
      filename = ifelse(todisk, file.path(rundir, "current_area.tif"), ""),
      datatype = "FLT4S",
      overwrite = TRUE
    )
    ha_sum_treecover2000 <- zonal(
      current_arearaster,
      polyraster, sum,
      na.rm = TRUE
    )[2]
    ha_sum_treecover2000 <- as.numeric(ha_sum_treecover2000)

    current_losslayer <- ifel(
      lossyear == y, 1, NA,
      filename = ifelse(todisk, file.path(rundir, "current_losses.tif"), ""),
      datatype = "INT1U",
      overwrite = TRUE
    )
    current_greenhouse <- mask(
      greenhouse, current_losslayer,
      filename = ifelse(todisk, file.path(rundir, "current_emissions.tif"), ""),
      datatype = "FLT4S",
      overwrite = TRUE
    )
    # terra engine
    emissions_sum <- zonal(current_greenhouse, polyraster, sum, na.rm = TRUE)[2]
    emissions_sum <- as.numeric(emissions_sum)
    tibble(treecover = ha_sum_treecover2000, emissions = emissions_sum)
  })

  # memory clean up
  rm(arearaster, binary_treecover2000, patchsizes, patched, polyraster)
  # return a data-frame
  out <- do.call(rbind, yearly_loss_values)
  out$years <- years
  out[, c(3, 2, 1)]
}
