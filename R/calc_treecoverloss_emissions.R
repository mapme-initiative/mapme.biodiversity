#' Calculate emission statistics
#'
#' This functions allows to efficiently calculate emission statistics for
#' areas of interest. For each year in the analysis timeframe, the forest losses
#' from Hansen et al. (2013) are overlayed with the respective emission layer
#' from Harris et al. (2021) and area-wise emission statistics are calculated
#' for each year.
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
#' @name treecoverloss_emissions
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years and emissions (in Mg)
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
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   init_portfolio(
#'     years = 2016:2017,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources(
#'     resources = c("gfw_treecover", "gfw_lossyear", "gfw_emissions"),
#'     vers_treecover = "GFC-2021-v1.9", vers_lossyear = "GFC-2021-v1.9"
#'   ) %>%
#'   calc_indicators("treecoverloss_emissions", min_size = 1, min_cover = 30) %>%
#'   tidyr::unnest(treecoverloss_emissions)
#'
#' aoi
#' }
NULL

#' Calculate emissions statistics
#'
#' @param x A single polygon for which to calculate the tree cover statistic
#' @param gfw_treecover The treecover 2000 resource from GFW
#' @param gfw_lossyear The lossyear resource from GFW
#' @param gfw_emissions The greenhouse emission layer from GFW
#' @param min_size The minimum size of a forest patch in ha.
#' @param min_cover The minimum threshold of stand density for a pixel to be
#'   considered forest in the year 2000.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @importFrom stringr str_sub
#' @keywords internal
#' @include register.R
#' @noRd
.calc_treecoverloss_emissions <- function(x,
                                          gfw_treecover,
                                          gfw_lossyear,
                                          gfw_emissions,
                                          min_size = 10,
                                          min_cover = 35,
                                          verbose = TRUE,
                                          ...) {
  # initial argument checks
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop("R package 'exactextractr' required. Please install via 'install.packages('exactextractr')'")
  }
  # handling of return value if resources are missing, e.g. no overlap
  if (any(is.null(gfw_treecover), is.null(gfw_lossyear), is.null(gfw_emissions))) {
    return(NA)
  }
  # retrieve years from portfolio
  years <- attributes(x)$years
  if (any(years < 2000)) {
    warning(paste("Cannot calculate emissions statistics ",
      "for years smaller than 2000.",
      sep = ""
    ))
    years <- years[years >= 2000]
    if (length(years) == 0) {
      return(tibble::tibble(years = NA, emissions = NA))
    }
  }

  # check if gfw_treecover only contains 0s, e.g. on the ocean
  minmax_gfw_treecover <- unique(as.vector(terra::minmax(gfw_treecover)))
  if (length(minmax_gfw_treecover) == 1) {
    if (minmax_gfw_treecover == 0 || is.nan(minmax_gfw_treecover)) {
      return(tibble::tibble(years = years, emissions = 0))
    }
  }

  # check additional arguments
  min_cover_msg <- paste("Argument 'min_cover' for indicator 'emissions' ",
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

  min_size_msg <- paste("Argument 'min_size' for indicator 'emissions' ",
    "must be a numeric value greater 0.",
    sep = ""
  )
  if (!is.numeric(min_size) || min_size <= 0) stop(min_size_msg, call. = FALSE)

  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
  # start calculation if everything is set up correctly
  # mask gfw_treecover
  gfw_treecover <- terra::mask(gfw_treecover, x)

  # binarize the gfw_treecover layer based on min_cover argument
  binary_gfw_treecover <- terra::classify(gfw_treecover,
    rcl = matrix(c(
      NA, NA, 0,
      0, min_cover, 0,
      min_cover, 100, 1
    ), ncol = 3, byrow = TRUE),
    include.lowest = TRUE
  )

  # resample greenhouse if extent doesnt match
  if (terra::ncell(gfw_emissions) != terra::ncell(gfw_treecover)) {
    gfw_emissions <- terra::resample(
      gfw_emissions, gfw_treecover,
      method = "bilinear"
    )
  }

  gfw_emissions <- terra::mask(gfw_emissions, binary_gfw_treecover)

  # create patches
  if (!requireNamespace("landscapemetrics", quietly = TRUE)) {
    message("Consider running `install.packages('landscapemetrics') to improve performance of GFW routines.")
    patched <- terra::patches(binary_gfw_treecover, directions = 4, zeroAsNA = TRUE)
  } else {
    patched <- landscapemetrics::get_patches(binary_gfw_treecover, class = 1, direction = 4)[[1]][[1]]
  }
  # mask lossyear
  gfw_lossyear <- terra::mask(gfw_lossyear, binary_gfw_treecover)
  gfw_lossyear <- terra::ifel(gfw_lossyear == 0, NA, gfw_lossyear)


  gfw <- c(binary_gfw_treecover, gfw_lossyear, patched, gfw_emissions)
  names(gfw) <- c("treecover", "lossyear", "patches", "emissions")

  gfw_stats <- exactextractr::exact_extract(
    gfw, x, function(data, min_size) {
      # retain only forest pixels and set area to ha
      data <- .prep_gfw_data(data, min_size)
      emissions <- .sum_gfw(data, "emissions")

      if (all(emissions[["emissions"]] == 0)) {
        result <- tibble::tibble(
          years = years,
          emissions = 0
        )
        return(result)
      }

      emissions <- emissions[emissions[["years"]] %in% years, ]
      emissions[, c("years", "emissions")]
    },
    min_size = min_size, coverage_area = TRUE, summarize_df = TRUE
  )

  rm(gfw, binary_gfw_treecover, gfw_lossyear, patched, gfw_emissions)
  gc()

  tibble::as_tibble(gfw_stats)
}


register_indicator(
  name = "treecoverloss_emissions",
  resources = list(
    gfw_treecover = "raster",
    gfw_lossyear = "raster",
    gfw_emissions = "raster"
  ),
  fun = .calc_treecoverloss_emissions,
  arguments = list(
    min_size = 10,
    min_cover = 35
  ),
  processing_mode = "asset"
)
