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
      return(tibble(years = NA, emissions = NA))
    }
  }

  # check if gfw_treecover only contains 0s, e.g. on the ocean
  minmax_gfw_treecover <- unique(as.vector(minmax(gfw_treecover)))
  if (length(minmax_gfw_treecover) == 1) {
    if (minmax_gfw_treecover == 0 | is.nan(minmax_gfw_treecover)) {
      return(tibble(years = years, emissions = rep(0, length(years))))
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
  if (!is.numeric(min_size) | min_size <= 0) stop(min_size_msg, call. = FALSE)

  #------------------------------------------------------------------------------
  # start calculation if everything is set up correctly
  # retrieve an area raster
  arearaster <- cellSize(
    gfw_treecover,
    unit = "ha"
  )
  # rasterize the polygon
  polyraster <- rasterize(
    vect(x), gfw_treecover,
    field = 1, touches = TRUE
  )
  # mask gfw_treecover
  gfw_treecover <- mask(
    gfw_treecover, polyraster
  )

  # mask lossyear
  gfw_lossyear <- mask(
    gfw_lossyear, polyraster
  )

  # resample greenhouse if extent doesnt match
  if (ncell(gfw_emissions) != ncell(gfw_treecover)) {
    gfw_emissions <- resample(
      gfw_emissions, gfw_treecover,
      method = "bilinear"
    )
  }
  # mask greenhouse
  gfw_emissions <- mask(
    gfw_emissions, polyraster
  )
  # binarize the gfw_treecover layer based on min_cover argument
  binary_gfw_treecover <- classify(
    gfw_treecover,
    rcl = matrix(c(0, min_cover, 0, min_cover, 100, 1), ncol = 3, byrow = TRUE)
  )
  # retrieve patches of comprehensive forest areas
  patched <- patches(
    binary_gfw_treecover,
    directions = 4,
    zeroAsNA = TRUE
  )

  unique_vals <- unique(as.vector(minmax(patched)))
  if (length(unique_vals) == 1) {
    if (is.nan(unique_vals)) {
      return(tibble(years = years, emissions = rep(0, length(years))))
    }
  }
  # get the sizes of the patches
  patchsizes <- zonal(
    arearaster, patched, sum,
    as.raster = TRUE
  )
  # remove patches smaller than threshold
  binary_gfw_treecover <- ifel(
    patchsizes < min_size, 0, binary_gfw_treecover
  )

  # return 0 if binary gfw_treecover only consits of 0 or nan
  minmax_gfw_treecover <- unique(as.vector(minmax(binary_gfw_treecover)))
  if (length(minmax_gfw_treecover) == 1) {
    if (minmax_gfw_treecover == 0 | is.nan(minmax_gfw_treecover)) {
      return(
        tibble(
          years = years,
          emissions = rep(0, length(years))
        )
      )
    }
  }
  # set no loss occurrences to NA
  gfw_lossyear <- ifel(
    gfw_lossyear == 0, NA, gfw_lossyear
  )

  # exclude non-tree pixels from lossyear layer
  gfw_lossyear <- mask(
    gfw_lossyear, binary_gfw_treecover
  )
  # get forest cover statistics for each year
  yearly_emission_values <- lapply(years, function(y) {
    y <- y - 2000
    current_gfw_emissions <- ifel(
      gfw_lossyear == y, gfw_emissions, 0
    )
    # terra engine
    emissions_sum <- zonal(current_gfw_emissions, polyraster, sum, na.rm = TRUE)[2]
    as.numeric(emissions_sum)
  })

  # memory clean up
  rm(arearaster, binary_gfw_treecover, patchsizes, patched, polyraster)
  # return a data-frame
  tibble(years = years, emissions = as.vector(unlist(yearly_emission_values)))
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
