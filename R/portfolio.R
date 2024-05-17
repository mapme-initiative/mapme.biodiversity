#' Portfolio methods
#'
#' `write_portfolio()` writes a processed biodiversity portfolio to disk.
#' In order to ensure interoperability with other geospatial software, the
#' data has to be transformed to either long- or wide-format. With the long
#' format, geometries will be repeated thus possibly resulting in
#' a large file size. With the wide-format, indicators will be added
#' to the portfolio as columns without repeating the geometries, but potentially
#' resulting in a large number of columns
#'
#' @param x A portfolio object processed with `mapme.biodiversity`.
#' @param dsn A file path for the output file.
#' @param format A character indicating if data should be written in long or
#'   wide format.
#' @param overwrite A logical indicating if the output file should be
#'   overwritten if it exists.
#' @param ... Additional arguments supplied to `st_write()`
#' @return `write_portfolio()` returns `dsn`, invisibly.
#' @name portfolio
#' @export
write_portfolio <- function(x,
                            dsn,
                            format = c("long", "wide"),
                            overwrite = FALSE,
                            ...) {
  x <- .check_portfolio(x)
  format <- match.arg(format)

  if (length(.indicators_col(x)) == 0) {
    stop("No calculated indicators have been found")
  }

  if (file.exists(dsn) & !overwrite) {
    stop(sprintf("Output file %s exists and overwrite is FALSE.", dsn))
  }

  transformer <- switch(format,
    long = portfolio_long,
    wide = portfolio_wide
  )

  data <- transformer(x)

  st_write(data, dsn, delete_dsn = overwrite, ...)
  invisible(dsn)
}

#' Transform portfolio to long
#'
#' `portfolio_long()` transforms a portfolio to long-format, potentially
#' dropping geometries in the process.
#'
#' @param indicators If NULL (the default), all indicator columns will be detected
#'   and transformed automatically. If a character vector is supplied, only
#'   those indicators will be transformed.
#' @param drop_geoms A logical, indicating if geometries should be dropped.
#'
#' @return `portfolio_long()` returns the portfolio object in long-format.
#' @name portfolio
#' @export
#'
portfolio_long <- function(x, indicators = NULL, drop_geoms = FALSE) {
  .check_portfolio(x)
  if (is.null(indicators)) {
    indicators <- names(.indicators_col(x))
  }
  stopifnot(all(indicators %in% names(x)))

  is_null <- sapply(indicators, function(ind) {
    all(sapply(x[[ind]], is.null))
  })

  if (all(is_null)) {
    warning("All indicator columns contained 'NULL'.")
    return(x[, -which(names(x) %in% indicators)])
  }

  if (any(is_null)) {
    x <- x[, -.indicators_col(x)[is_null]]
    indicators <- names(.indicators_col(x))
  }

  if (drop_geoms) x <- st_drop_geometry(x)

  ind_cols <- c("indicator", "datetime", "variable", "unit", "value")
  all_cols <- names(x)
  other_cols <- all_cols[-which(all_cols %in% indicators)]

  x_long <- purrr::map(indicators, function(ind) {
    indicator <- tidyr::unnest(x, cols = {{ ind }})
    indicator["indicator"] <- ind
    indicator <- indicator[, c(other_cols, ind_cols)]
  }) %>%
    purrr::list_rbind()

  if (!drop_geoms) {
    x_long <- .geom_last(st_as_sf(x_long))
  }
  x_long
}

#' Transform portfolio to wide
#'
#' `portfolio_wide()` transforms a portfolio to wide-format, potentially
#' dropping geometries in the process.
#'
#' @return `portfolio_wide()` returns the portfolio object in wide-format.
#' @name portfolio
#' @export
#'
portfolio_wide <- function(x, indicators = NULL, drop_geoms = FALSE) {
  assetid <- NULL
  .check_portfolio(x)
  if (is.null(indicators)) {
    indicators <- names(.indicators_col(x))
  }
  stopifnot(all(indicators %in% names(x)))

  is_null <- sapply(indicators, function(ind) {
    all(sapply(x[[ind]], is.null))
  })

  if (all(is_null)) {
    warning("All indicator columns contained 'NULL'.")
    return(x[, -which(names(x) %in% indicators)])
  }

  if (any(is_null)) {
    x <- x[, -.indicators_col(x)[is_null]]
    indicators <- names(.indicators_col(x))
  }

  if (drop_geoms) x <- st_drop_geometry(x)

  indicators_wide <- purrr::map(indicators, function(indicator) {
    indicator_assets <- x[[indicator]]
    purrr::map(indicator_assets, function(asset) {
      asset[["indicator"]] <- indicator
      tidyr::pivot_wider(
        asset,
        names_from = c(indicator, datetime, variable, unit),
        names_sep = "_", values_from = value
      )
    }) %>%
      purrr::list_rbind()
  }) %>%
    purrr::list_cbind() %>%
    dplyr::mutate(assetid = x[["assetid"]])

  x_wide <- x %>%
    dplyr::select(-{{ indicators }}) %>%
    dplyr::left_join(indicators_wide, by = dplyr::join_by(assetid))

  if (!drop_geoms) {
    x_wide <- .geom_last(st_as_sf(x_wide))
  }
  x_wide
}

.check_portfolio <- function(x, verbose = mapme_options()[["verbose"]]) {
  stopifnot(inherits(x, "sf"))

  if (st_crs(x) != st_crs(4326)) {
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x <- st_transform(x, 4326)
  }
  if (any(!unique(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Only assets of type 'POLYGON' and 'MULTIPOLYGON' are supported.")
  }
  if (!inherits(x, "tibble")) {
    x <- st_as_sf(tibble::as_tibble(x))
  }
  if ("assetid" %in% names(x) && verbose) {
    msg <- paste("Found a column named 'assetid'.",
      "Overwritting its values with a unique identifier.",
      sep = " "
    )
    message(msg)
  }
  x[["assetid"]] <- 1:nrow(x)
  x
}

.indicators_col <- function(x) {
  inds <- which(sapply(x, is.list))
  is_sf <- inherits(x, "sf")
  if (is_sf && length(inds) == 1) {
    msg <- paste("No calculated indicators have been found.",
      "Cannot write as a portfolio.",
      sep = " "
    )
    stop(msg)
  }
  if (is_sf) {
    inds[-which(names(inds) == attributes(x)[["sf_column"]])]
  } else {
    inds
  }
}
