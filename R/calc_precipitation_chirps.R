#' Calculate precipitation statistics based on CHIRPS
#'
#' This functions allows to calculate precipitation statistics based on the
#' CHIRPS rainfall estimates. Corresponding to the time-frame of the analysis
#' of the portfolio, monthly precipitation statistics are calculated. These include
#' the total rainfall amount, rainfall anomaly against the 1981-2010 climate normal,
#' and the Standardized Precipitation Index (SPI) which is available for scales
#' between 1 and 48 months. Th function needs the \code{SPEI} package to be
#' installed.
#' The required resources for this indicator are:
#'  - [chirps]
#'
#' The following arguments can be set:
#' \describe{
#'   \item{scales_spi}{An integer vector indicating the scales for which to calculate the SPI.}
#'   \item{spi_previous_year}{An integer specifying how many previous years to include in
#'   order to fit the SPI. Defaults to 8 years.}
#'   \item{engine}{The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.}
#' }
#'
#' @name precipitation_chirps
#' @docType data
#' @keywords indicator
#' @format A tibble with a column for years, months, absolute rainfall (in mm), rainfall
#'   anomaly (in mm) and one or more columns per selected time-scale for SPI (dimensionless).
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
#'     years = 2010,
#'     outdir = outdir,
#'     tmpdir = tempdir(),
#'     verbose = FALSE
#'   ) %>%
#'   get_resources("chirps") %>%
#'   calc_indicators("precipitation_chirps",
#'     engine = "exactextract",
#'     scales_spi = 3,
#'     spi_prev_years = 8
#'   ) %>%
#'   tidyr::unnest(precipitation_chirps)
#'
#' aoi
#' }
NULL

#' Calculate precipitation statistics based on CHIRPS
#'
#' @param x A single polygon for which to calculate the tree cover statistic
#' @param chirps The CHIRPS resource
#' @param scales_spi Integers specifying time-scales for SPI
#' @param spi_prec_years Integer specyfing how many previous years to include in
#'   order to fit the SPI. Defaults to 8.
#' @param verbose A directory where intermediate files are written to.
#' @param ... additional arguments
#' @return A tibble
#' @keywords internal
#' @include register.R
#' @noRd
.calc_precipitation_chirps <- function(x,
                                       chirps,
                                       scales_spi = 3,
                                       spi_prev_years = 8,
                                       engine = "extract",
                                       rundir = tempdir(),
                                       verbose = TRUE,
                                       processing_mode = "portfolio",
                                       ...) {
  if (!requireNamespace("SPEI", quietly = TRUE) & !is.null(scales_spi)) {
    stop("R package 'SPEI' required. Please install via 'install.packages('SPEI'')'")
  }
  # initial argument checks
  # retrieve years from portfolio
  # handling of return value if resources are missing, e.g. no overlap
  if (is.null(chirps)) {
    return(NA)
  }
  years <- attributes(x)$years

  if (!is.null(scales_spi)) {
    if (any(scales_spi < 0) | any(scales_spi > 48)) {
      stop("Values of 'scales_spi' for SPI calculation must be integers between 0 and 48.")
    }
  }
  if (any(years < 1981)) {
    warning(paste("Cannot calculate precipitation statistics ",
      "for years smaller than 1981",
      sep = ""
    ))
    years <- years[years >= 1981]
    if (length(years) == 0) {
      return(NA)
    }
  }

  src_names <- names(chirps)
  # set values smaller 0 to NA
  chirps <- clamp(
    chirps,
    lower = 0,
    upper = Inf,
    values = FALSE
  )

  layer_years <- as.numeric(substr(src_names, 13, 17))
  climate_chirps <- chirps[[which(layer_years %in% 1981:2010)]]
  target_chirps <- chirps[[which(layer_years %in% years)]]

  # calculate long-term monthly average
  layer_names <- names(climate_chirps)
  layer_months <- as.numeric(substr(layer_names, 18, 19))
  # chirps[chirps < 0] = NA
  climate_chirps <- lapply(1:12, function(i) {
    app(
      climate_chirps[[layer_months == i]],
      fun = "mean"
    )
  })
  climate_chirps <- do.call(c, climate_chirps)
  names(climate_chirps) <- c(1:12)
  anomaly_chirps <- target_chirps - climate_chirps

  # calculate SPI
  if (!is.null(scales_spi)) {
    spi_chirps <- lapply(scales_spi, function(scale) {
      target_years_spi <- years[1] - spi_prev_years
      target_years_spi <- target_years_spi:years[length(years)]
      target_spi <- chirps[[which(layer_years %in% target_years_spi)]]
      spi_chirps <- app(
        target_spi,
        scale = scale,
        fun = function(x, scale) {
          SPEI::spi(x, scale = scale, na.rm = TRUE, verbose = FALSE)$fitted
        }
      )
      names(spi_chirps) <- names(target_spi)
      spi_chirps[[names(target_chirps)]]
    })
    names(spi_chirps) <- paste0("spi_", scales_spi)
  } else {
    spi_chirps <- NULL
  }

  # extract zonal statistics
  results_absolute <- .select_engine(
    x = x,
    raster = target_chirps,
    stats = "mean",
    engine = engine,
    mode = processing_mode
  )

  results_anomaly <- .select_engine(
    x = x,
    raster = anomaly_chirps,
    stats = "mean",
    engine = engine,
    mode = processing_mode
  )

  if (!is.null(spi_chirps)) {
    results_spi <- purrr::map(1:nrow(x), function(i) {
      results_x <- purrr::lmap(spi_chirps, function(spi) {
        result <- .select_engine(
          x = x[i, ],
          raster = spi[[1]],
          stats = "mean",
          engine = engine,
          name = names(spi),
          mode = "asset"
        )
        names(result) <- names(spi)
        result
      })
      dplyr::bind_cols(results_x)
    })
    if (processing_mode == "asset") results_spi <- results_spi[[1]]
  }

  dates <- as.Date(paste0(substr(names(target_chirps), 13, 19), ".01"), "%Y.%m.%d")

  if (processing_mode == "portfolio") {
    results <- purrr::map(1:nrow(x), function(i) {
      result <- tibble(
        dates = dates,
        absolute = as.numeric(results_absolute[[i]]$mean),
        anomaly = as.numeric(results_anomaly[[i]]$mean)
      )
      if (!is.null(scales_spi)) {
        result <- dplyr::bind_cols(result, results_spi[[i]])
      }
    })
  } else {
    results <- tibble(
      dates = dates,
      absolute = as.numeric(results_absolute$mean),
      anomaly = as.numeric(results_anomaly$mean)
    )
    if (!is.null(scales_spi)) {
      results <- dplyr::bind_cols(results, results_spi)
    }
  }
  results
}


register_indicator(
  name = "precipitation_chirps",
  resources = list(chirps = "raster"),
  fun = .calc_precipitation_chirps,
  arguments = list(
    scales_spi = 3,
    spi_prev_years = 8,
    engine = "extract"
  ),
  processing_mode = "portfolio"
)
