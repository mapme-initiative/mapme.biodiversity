#' Calculate treeloss statistics
#'
#' This functions allows to efficiently calculate the treecover and emissions
#' indicators in a single function call together. Since most of the pre-processing
#' operations for treecover and emissions are the same, it is more efficient
#' to calculate them in one run if users are actually interested in both statistics.
#' Otherwise users are advised to use the respective single indicator functions.
#' The required resources for this indicator are:
#'  - [gfw_treecover]
#'  - [gfw_lossyear]
#'  - [gfw_emissions].
#'
#' The following arguments can be set:
#' \describe{
#'   \item{min_size}{The minimum size of a forest patch to be considered as forest in ha.}
#'   \item{min_cover}{The minimum cover percentage per pixel to be considered as forest.}
#' }
#'
#' @name treecover_area_and_emissions
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years, treecover (in ha), and emissions (in Mg CO2)
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
#'     years = 2016:2017,
#'     outdir = file.path(temp_loc, "res"),
#'     tmpdir = tempdir(),
#'     add_resources = FALSE,
#'     cores = 1,
#'     verbose = FALSE
#'   ) %>%
#'   get_resources(
#'     resources = c("gfw_treecover", "gfw_lossyear", "gfw_emissions"),
#'     vers_treecover = "GFC-2020-v1.8", vers_lossyear = "GFC-2020-v1.8"
#'   ) %>%
#'   calc_indicators("treecover_area_and_emissions", min_size = 1, min_cover = 30) %>%
#'   tidyr::unnest(treecover_area_and_emissions)))
NULL


#' Calculate treecover area and greenhouse gas emissions
#'
#' This is a joint indicator calculating both, treecover and greenhouse
#' gas emissions per treecover loss. It actually combines the indicators for
#' treecover and emissions into a single function since the computations for
#' both indicators are very similar. If a user is interested only in one of
#' these two indicators consider applying the respective functions.
#'
#' @param shp A single polygon for which to calculate the tree cover statistic
#' @param gfw_treecover The treecover 2000 resource from GFW
#' @param gfw_lossyear The lossyear resource from GFW
#' @param gfw_emissions The greenhouse emission layer from GFW
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
.calc_treecover_area_and_emissions <- function(shp,
                                               gfw_treecover,
                                               gfw_lossyear,
                                               gfw_emissions,
                                               min_size = 10,
                                               min_cover = 35,
                                               rundir = tempdir(),
                                               verbose = TRUE,
                                               todisk = FALSE,
                                               ...) {

  # initial argument checks
  # handling of return value if resources are missing, e.g. no overlap
  if (any(is.null(gfw_treecover), is.null(gfw_lossyear), is.null(gfw_emissions))) {
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
  if (ncell(gfw_treecover) > 1024 * 1024) todisk <- TRUE
  # check if gfw_treecover only contains 0s, e.g. on the ocean
  minmax_gfw_treecover <- unique(as.vector(minmax(gfw_treecover)))
  if (length(minmax_gfw_treecover) == 1) {
    if (minmax_gfw_treecover == 0 | is.nan(minmax_gfw_treecover)) {
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
    gfw_treecover,
    unit = "ha",
    filename = ifelse(todisk, file.path(rundir, "arearaster.tif"), ""),
    datatype = "FLT4S",
    overwrite = TRUE
  )
  # rasterize the polygon
  polyraster <- rasterize(
    vect(shp), gfw_treecover,
    field = 1, touches = TRUE,
    filename = ifelse(todisk, file.path(rundir, "polygon.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # mask gfw_treecover
  gfw_treecover <- mask(
    gfw_treecover, polyraster,
    filename =  ifelse(todisk, file.path(rundir, "gfw_treecover.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # mask lossyear
  gfw_lossyear <- mask(
    gfw_lossyear, polyraster,
    filename =  ifelse(todisk, file.path(rundir, "gfw_lossyear.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # resample greenhouse if extent doesnt match
  if (ncell(gfw_emissions) != ncell(gfw_treecover)) {
    gfw_emissions <- resample(
      gfw_emissions, gfw_treecover,
      method = "bilinear",
      filename =  ifelse(todisk, file.path(rundir, "gfw_emissions.tif"), ""),
      datatype = "FLT4S",
      overwrite = TRUE
    )
  }
  # mask greenhouse
  gfw_emissions <- mask(
    gfw_emissions, polyraster,
    filename =  ifelse(todisk, file.path(rundir, "gfw_emissions.tif"), ""),
    datatype = "FLT4S",
    overwrite = TRUE
  )

  # binarize the gfw_treecover layer based on min_cover argument
  binary_gfw_treecover <- classify(
    gfw_treecover,
    rcl = matrix(c(0, min_cover, 0, min_cover, 100, 1), ncol = 3, byrow = TRUE),
    include.lowest = TRUE,
    filename = ifelse(todisk, file.path(rundir, "binary_gfw_treecover.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )
  # retrieve patches of comprehensive forest areas
  patched <- patches(
    binary_gfw_treecover,
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
  binary_gfw_treecover <- ifel(
    patchsizes < min_size, 0, binary_gfw_treecover,
    filename = ifelse(todisk, file.path(rundir, "binary_gfw_treecover.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # return 0 if binary gfw_treecover only consits of 0 or nan
  minmax_gfw_treecover <- unique(as.vector(minmax(binary_gfw_treecover)))
  if (length(minmax_gfw_treecover) == 1) {
    if (minmax_gfw_treecover == 0 | is.nan(minmax_gfw_treecover)) {
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
  gfw_lossyear <- ifel(
    gfw_lossyear == 0, NA, gfw_lossyear,
    filename = ifelse(todisk, file.path(rundir, "gfw_lossyear.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # exclude non-tree pixels from lossyear layer
  gfw_lossyear <- mask(
    gfw_lossyear, binary_gfw_treecover,
    filename = ifelse(todisk, file.path(rundir, "gfw_lossyear.tif"), ""),
    datatype = "INT1U",
    overwrite = TRUE
  )

  # get forest cover statistics for each year
  yearly_loss_values <- lapply(years, function(y) {
    y <- y - 2000
    current_gfw_treecover <- ifel(
      gfw_lossyear <= y, 0, binary_gfw_treecover,
      filename = ifelse(todisk, file.path(rundir, "current_gfw_treecover.tif"), ""),
      datatype = "INT1U",
      overwrite = TRUE
    )
    current_arearaster <- mask(
      arearaster, current_gfw_treecover,
      maskvalues = c(NA, 0),
      filename = ifelse(todisk, file.path(rundir, "current_area.tif"), ""),
      datatype = "FLT4S",
      overwrite = TRUE
    )
    ha_sum_gfw_treecover <- zonal(
      current_arearaster,
      polyraster, sum,
      na.rm = TRUE
    )[2]
    ha_sum_gfw_treecover <- as.numeric(ha_sum_gfw_treecover)

    current_losslayer <- ifel(
      gfw_lossyear == y, 1, 0,
      filename = ifelse(todisk, file.path(rundir, "current_losses.tif"), ""),
      datatype = "INT1U",
      overwrite = TRUE
    )
    current_gfw_emissions <- mask(
      gfw_emissions, current_losslayer, maskvalues = 0,
      filename = ifelse(todisk, file.path(rundir, "current_emissions.tif"), ""),
      datatype = "FLT4S",
      overwrite = TRUE
    )
    # terra engine
    emissions_sum <- zonal(current_gfw_emissions, polyraster, sum, na.rm = TRUE)[2]
    emissions_sum <- as.numeric(emissions_sum)
    tibble(treecover = ha_sum_gfw_treecover, emissions = emissions_sum)
  })

  # memory clean up
  rm(arearaster, binary_gfw_treecover, patchsizes, patched, polyraster)
  # return a data-frame
  out <- do.call(rbind, yearly_loss_values)
  out$years <- years
  out[, c(3, 2, 1)]
}
