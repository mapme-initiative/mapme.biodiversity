#' Calculate precipitation statistics based on CHIRPS
#'
#' This functions allows to calculate precipitation statistics based on the
#' CHIRPS rainfall estimates. Corresponding to the time-frame of the analysis
#' of the portfolio, monthly precipitation statistics are calculated. These include
#' the total rainfall amount, rainfall anomaly against the 1981-2010 climate normal,
#' and the Standardized Precipitation Index (SPI) which is available for scales
#' between 1 and 48 months. Th function needs the \code{SPEI} package to be
#' installed.
#'
#' The required resources for this indicator are:
#'  - [chirps]
#'
#' @name precipitation_chirps
#' @param years A numeric vector indicating the years for which to calculate
#'   precipitation statistics.
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param scales_spi Integers specifying time-scales for SPI
#' @param spi_prev_years Integer specifying how many previous years to include in
#'   order to fit the SPI. Defaults to 8.
#' @keywords indicator
#' @returns A function that returns a tibble with a column for years, months,
#'   absolute rainfall (in mm), rainfall anomaly (in mm) and one or more columns
#'   per selected time-scale for SPI (dimensionless).
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
#'   get_resources(get_chirps()) %>%
#'   calc_indicators(
#'     calc_precipitation_chirps(
#'       years = 2010,
#'       engine = "extract",
#'       scales_spi = 3,
#'       spi_prev_years = 8
#'     )
#'   ) %>%
#'   tidyr::unnest(precipitation_chirps)
#'
#' aoi
#' }
calc_precipitation_chirps <- function(years = 1981:2020,
                                      engine = "extract",
                                      scales_spi = 3,
                                      spi_prev_years = 8) {
  check_namespace("SPEI")
  engine <- check_engine(engine)

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
  }

  function(x,
           chirps,
           name = "precipitation_chirps",
           mode = "portfolio",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(chirps)) {
      return(NA)
    }

    if (length(years) == 0) {
      return(NA)
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
    results_absolute <- select_engine(
      x = x,
      raster = target_chirps,
      stats = "mean",
      engine = engine,
      mode = mode
    )

    results_anomaly <- select_engine(
      x = x,
      raster = anomaly_chirps,
      stats = "mean",
      engine = engine,
      mode = mode
    )

    if (!is.null(spi_chirps)) {
      results_spi <- purrr::map(1:nrow(x), function(i) {
        results_x <- purrr::lmap(spi_chirps, function(spi) {
          result <- select_engine(
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
      if (mode == "asset") results_spi <- results_spi[[1]]
    }

    dates <- as.Date(paste0(substr(names(target_chirps), 13, 19), ".01"), "%Y.%m.%d")

    if (mode == "portfolio") {
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
}


register_indicator(
  name = "precipitation_chirps",
  description = "Statistics of CHIRPS precipitation layer",
  resources = "chirps"
)
