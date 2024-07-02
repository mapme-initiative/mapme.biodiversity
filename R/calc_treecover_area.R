#' Calculate treecover statistics
#'
#' This functions allows to efficiently calculate treecover statistics for
#' polygons. For each year in the analysis timeframe, the forest losses in
#' preceding and the current years are subtracted from the treecover in the
#' year 2000 and actual treecover figures within the polygon are returned.
#'
#' The required resources for this indicator are:
#'  - [gfw_treecover]
#'  - [gfw_lossyear]
#'
#' @name treecover_area
#' @param years A numeric vector with the years for which to calculate treecover
#'   area.
#' @param min_size The minimum size of a forest patch to be considered as forest in ha.
#' @param min_cover The minimum cover percentage per pixel to be considered as forest.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with variable treecover
#'   and corresponding area (in ha) as value.
#' @include register.R
#' @export
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
#' mapme_options(
#'   outdir = outdir,
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(
#'     get_gfw_treecover(version = "GFC-2023-v1.11"),
#'     get_gfw_lossyear(version = "GFC-2023-v1.11")
#'   ) %>%
#'   calc_indicators(calc_treecover_area(years = 2016:2017, min_size = 1, min_cover = 30)) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_treecover_area <- function(years = 2000:2023,
                                min_size = 10,
                                min_cover = 35) {
  check_namespace("exactextractr")
  min_cover <- .gfw_check_min_cover(min_cover, "treecover_area")
  min_size <- .gfw_check_min_size(min_size, "treecover_area")
  years <- .gfw_check_years(years, "treecover_area")

  function(x = NULL,
           gfw_treecover = NULL,
           gfw_lossyear = NULL,
           name = "treecover_area",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    treecover <- NULL
    # handling of return value if resources are missing, e.g. no overlap
    if (any(is.null(gfw_treecover), is.null(gfw_lossyear))) {
      return(NULL)
    }
    # mask gfw
    gfw_treecover <- terra::mask(gfw_treecover, x)
    # check if gfw_treecover only contains 0s, e.g. on the ocean
    if (.gfw_empty_raster(gfw_treecover, min_cover)) {
      return(NULL)
    }
    # prepare gfw rasters
    gfw <- .gfw_prep_rasters(x, gfw_treecover, gfw_lossyear, cover = min_cover)

    # retrieves maximum lossyear value from layer name
    max_year <- as.numeric(
      gsub(
        ".*GFC-([0-9]+)-.*", "\\1",
        names(gfw_lossyear)
      )
    )

    if (max_year < min(years)) {
      return(NULL)
    }

    # apply extraction routine
    gfw_stats <- exactextractr::exact_extract(
      gfw, x, function(data, min_size) {
        # retain only forest pixels and set area to ha
        data <- .prep_gfw_data(data, min_size)
        losses <- .sum_gfw(data, "coverage_area", max_year)
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

    gfw_stats %>%
      dplyr::mutate(
        datetime = as.POSIXct(paste0(years, "-01-01T00:00:00Z")),
        variable = "treecover",
        unit = "ha",
        value = treecover
      ) %>%
      dplyr::select(datetime, variable, unit, value) %>%
      tibble::as_tibble()
  }
}


.prep_gfw_data <- function(data, min_size) {
  # retain only forest pixels and set area to ha
  data <- data[data[["treecover"]] == 1, ]
  data[["coverage_area"]] <- data[["coverage_area"]] / 10000 # to ha

  # exclude patches smaller than threshold
  patch_sizes <- by(data[["coverage_area"]], data[["patches"]], sum)
  keep_patches <- names(patch_sizes)[which(patch_sizes > min_size)]
  keep_patches <- as.numeric(keep_patches)

  data[data[["patches"]] %in% keep_patches, ]
}


.sum_gfw <- function(data, what = "coverage_area", max_year = 2023) {
  # calculate loss area by year
  df <- data.frame(years = 2000:max_year, var = 0)
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
  years <- years[years >= 2000]
  if (length(years) == 0) {
    stop("GFW not available for the selected time range.")
  }
  return(years)
}

.gfw_empty_raster <- function(gfw, min_cover) {
  minmax <- unique(as.vector(terra::minmax(gfw)))
  if (length(minmax) > 1 && max(minmax) >= min_cover) {
    return(FALSE)
  }
  TRUE
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
  min_cover
}

.gfw_check_min_size <- function(min_size, indicator) {
  msg <- paste(
    "Argument 'min_size' for indicator '%s' must be ",
    "a numeric value greater 0.",
    sep = ""
  )
  msg <- sprintf(msg, indicator)
  if (length(min_size) != 1) stop(msg, call. = FALSE)
  if (!is.numeric(min_size) || min_size <= 0) stop(msg, call. = FALSE)
  min_size
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


.gfw_prep_rasters <- function(x, treecover, lossyear, emissions, cover) {
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

  if (!missing(emissions)) {
    if (terra::ncell(emissions) != terra::ncell(treecover)) {
      emissions <- terra::resample(
        emissions, treecover,
        method = "bilinear"
      )
    }
    emissions <- terra::mask(emissions, lossyear)
    gfw <- c(binary_treecover, lossyear, patched, emissions)
    names(gfw) <- c("treecover", "lossyear", "patches", "emissions")
  } else {
    # prepare return raster
    gfw <- c(binary_treecover, lossyear, patched)
    names(gfw) <- c("treecover", "lossyear", "patches")
  }
  gfw
}


register_indicator(
  name = "treecover_area",
  description = "Area of forest cover by year",
  resources = c(
    "gfw_treecover",
    "gfw_lossyear"
  )
)
