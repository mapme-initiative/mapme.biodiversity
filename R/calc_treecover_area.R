#' Calculate treecover statistics
#'
#' This functions allows to efficiently calculate treecover statistics for
#' polygons. For each year in the analysis timeframe, the forest losses in
#' preceding and the current years are subtracted from the treecover in the
#' year 2000 and actual treecover figures within the polygon are returned.
#' The required resources for this indicator are:
#'  - [gfw_treecover]
#'  - [gfw_lossyear]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{min_size}{The minimum size of a forest patch to be considered as forest in ha.}
#'   \item{min_cover}{The minimum cover percentage per pixel to be considered as forest.}
#' }
#'
#' @name treecover_area
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years and treecover (in ha)
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
#'     resources = c("gfw_treecover", "gfw_lossyear"),
#'     vers_treecover = "GFC-2022-v1.10", vers_lossyear = "GFC-2022-v1.10"
#'   ) %>%
#'   calc_indicators("treecover_area", min_size = 1, min_cover = 30) %>%
#'   tidyr::unnest(treecover_area)
#'
#' aoi
#' }
NULL

#' Calculate tree cover per year based on GFW data sets
#'
#' Considering the 2000 GFW forest cover density layer users can specify
#' a cover threshold above which a pixel is considered to be covered by forest.
#' Additionally, users can specify a minimum size of a comprehensive patch of
#' forest pixels. Patches below this threshold will not be considered as forest
#' area.
#'
#' @param x A single polygon for which to calculate the tree cover statistic
#' @param gfw_treecover The treecover 2000 resource from GFW
#' @param gfw_lossyear The lossyear resource from GFW
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
.calc_treecover_area <- function(x,
                                 gfw_treecover,
                                 gfw_lossyear,
                                 min_size = 10,
                                 min_cover = 35,
                                 verbose = TRUE,
                                 ...) {
  # initial argument checks
  .check_namespace("exactextractr")
  # check additional arguments
  .gfw_check_min_cover(min_cover, "treecover_area")
  .gfw_check_min_size(min_size, "treecover_area")

  # handling of return value if resources are missing, e.g. no overlap
  if (any(is.null(gfw_treecover), is.null(gfw_lossyear))) {
    return(NA)
  }
  # retrieve years from portfolio
  years <- attributes(x)$years
  years <- .gfw_check_years(years, "treecover_area")
  if (length(years) == 0) {
    return(tibble::tibble(years = NA, treecover = NA))
  }
  # check if gfw_treecover only contains 0s, e.g. on the ocean
  if (.gfw_empty_raster(gfw_treecover)) {
    return(tibble::tibble(years = years, treecover = 0))
  }

  # prepare gfw rasters
  gfw <- .gfw_prep_rasters(x, gfw_treecover, gfw_lossyear, min_cover)

  # apply extraction routine
  gfw_stats <- exactextractr::exact_extract(
    gfw, x, function(data, min_size) {
      # retain only forest pixels and set area to ha
      data <- .prep_gfw_data(data, min_size)
      losses <- .sum_gfw(data, "coverage_area")
      names(losses)[2] <- "loss"
      org_coverage <- sum(data[["coverage_area"]])

      if (all(losses[["loss"]] == 0)) {
        result <- data.frame(
          years = years,
          treecover = org_coverage
        )
        return(result)
      }

      previous_losses <- sum(losses[["loss"]][losses[["years"]] < years[1]])

      if (previous_losses != 0) {
        org_coverage <- org_coverage - previous_losses
      }

      losses <- losses[losses[["years"]] %in% years, ]
      losses[["treecover"]] <- org_coverage
      losses[["treecover"]] <- losses[["treecover"]] - cumsum(losses[["loss"]])
      losses[, c("years", "treecover")]
    },
    min_size = min_size, coverage_area = TRUE, summarize_df = TRUE
  )

  rm(gfw)
  gc()
  tibble::as_tibble(gfw_stats)
}


.prep_gfw_data <- function(data, min_size) {
  # retain only forest pixels and set area to ha
  data <- data[data[["treecover"]] == 1, ]
  data[["coverage_area"]] <- data[["coverage_area"]] / 10000 # to ha

  # exclude patches smaller than threshold
  patch_sizes <- by(data[["coverage_area"]], data[["patches"]], sum)
  keep_patches <- names(patch_sizes)[which(patch_sizes > min_size)]
  keep_patches <- as.numeric(keep_patches)

  data <- data[data[["patches"]] %in% keep_patches, ]
}


.sum_gfw <- function(data, what = "coverage_area") {
  # calculate loss area by year
  df <- data.frame(years = 2000:2022, var = 0)
  names(df)[2] <- what
  my_sum <- by(data[[what]], data[["lossyear"]], sum, na.rm = TRUE)
  sum_years <- as.numeric(names(my_sum))
  sum_years <- sum_years + 2000

  index <- match(sum_years, df[["years"]])
  df[[what]][index] <- as.numeric(my_sum)

  df
}

.gfw_check_years <- function(years, indicator) {
  if (any(years < 2000)) {
    msg <- paste("Cannot calculate %s statistics ",
      "for years smaller than 2000.",
      sep = ""
    )
    msg <- sprintf(msg, indicator)
    warning(msg)
  }
  years[years >= 2000]
}

.gfw_empty_raster <- function(gfw) {
  minmax <- unique(as.vector(terra::minmax(gfw)))
  if (length(minmax) > 1) {
    return(FALSE)
  }
  if (minmax == 0 || is.nan(minmax)) {
    return(TRUE)
  }
}

.gfw_check_min_cover <- function(min_cover, indicator) {
  msg <- paste("Argument 'min_cover' for indicator '%s' ",
    "must be a numeric value between 0 and 100.",
    sep = ""
  )
  msg <- sprintf(msg, indicator)
  if (length(min_cover) != 1) stop(msg, call. = FALSE)
  if (!is.numeric(min_cover)) stop(msg, call. = FALSE)
  if (min_cover < 0 || min_cover > 100) {
    stop(msg, call. = FALSE)
  }
}

.gfw_check_min_size <- function(min_size, indicator) {
  msg <- paste(
    "Argument 'min_size' for indicator '%s' must be ",
    "a numeric value greater 0.",
    sep = ""
  )
  msg <- sprintf(msg, indicator)
  if (!is.numeric(min_size) || min_size <= 0) stop(msg, call. = FALSE)
}

.gfw_calc_patches <- function(gfw) {
  if (!requireNamespace("landscapemetrics", quietly = TRUE)) {
    msg <- paste("Could not load package 'landscapemetrics'.\n",
      "Consider installing it for better performance with:\n",
      "install.packages('landscapemetrics)'",
      sep = ""
    )
    message(msg)
    patched <- terra::patches(gfw, directions = 4, zeroAsNA = TRUE)
  } else {
    patched <- landscapemetrics::get_patches(gfw, class = 1, direction = 4)[[1]][[1]]
  }
  patched
}


.gfw_prep_rasters <- function(x, treecover, lossyear, cover) {
  # mask gfw_treecover
  treecover <- terra::mask(treecover, x)
  # binarize the treecover layer based on mcover argument
  binary_treecover <- terra::classify(treecover,
    rcl = matrix(c(
      NA, NA, 0,
      0, cover, 0,
      cover, 100, 1
    ), ncol = 3, byrow = TRUE),
    include.lowest = TRUE
  )

  # mask lossyear
  lossyear <- terra::mask(lossyear, binary_treecover)
  lossyear <- terra::ifel(lossyear == 0, NA, lossyear)
  # create patches
  patched <- .gfw_calc_patches(binary_treecover)

  # prepare return raster
  gfw <- c(binary_treecover, lossyear, patched)
  names(gfw) <- c("treecover", "lossyear", "patches")
  gfw
}


register_indicator(
  name = "treecover_area",
  resources = list(
    gfw_treecover = "raster",
    gfw_lossyear = "raster"
  ),
  fun = .calc_treecover_area,
  arguments = list(
    min_size = 10,
    min_cover = 35
  ),
  processing_mode = "asset"
)
