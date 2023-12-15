#' Compute specific indicators
#'
#' With \code{calc_indicators()} specific biodiversity indicators
#' can be calculated. A requirement is that the resources that
#' are mandatory inputs for the requested indicators are available
#' locally. Multiple indicators and their respective additional
#' arguments can be supplied. You can check available indicators and
#' their requirement via \code{available_indicators()}, but
#' the function will also gracefully inform you about any misspecifications.
#' @param x A biodiversity portfolio object constructed via
#'   \code{init_portfolio()}
#' @param indicators A character vector indicating the requested indicators. All
#'   specified indicators must be supported by the package. You can use
#'   \code{available_indicators()} to get more information, e.g. additional
#'   required arguments and their default values, about the supported indicators
#' @param ... Additional arguments required for the requested indicators. Check
#'  \code{available_indicators()} to learn more about the supported indicators
#'  and their arguments.
#' @return The sf portfolio object \code{x} with additional nested list column per
#'   requested indicator.
#' @keywords function
#' @export
calc_indicators <- function(x, indicators, ...) {
  # check if the requested resource is supported
  req_resources <- .check_requested_indicator(indicators)
  # check if any of the requested resources is already locally available
  existing_resources <- names(attributes(x)[["resources"]])
  .check_existing_resources(
    existing_resources, req_resources,
    needed = TRUE
  )
  for (indicator in indicators) x <- .get_single_indicator(x, indicator, ...)
  x
}


#' Calculation of an indicator
#'
#' This functions let's users calculate on or more biodiversity indicators for
#'   a portfolio.
#' @param x A sf object returned by init_portfolio().
#' @param indicator A variable length character vector with the indicators to
#'   calculate.
#' @param ... Additional arguments required by the requested indicators.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr relocate last_col
#' @importFrom tidyr nest
.get_single_indicator <- function(x, indicator, ...) {
  i <- NULL
  # get arguments from function call and portfolio object
  args <- list(...)
  atts <- attributes(x)

  # retrieve the selected indicator
  selected_indicator <- available_indicators(indicator)
  # get processing mode
  processing_mode <- selected_indicator[[indicator]][["processing_mode"]]
  # matching the specified arguments to the required arguments
  params <- .check_resource_arguments(selected_indicator, args)
  # append parameters
  params[["verbose"]] <- atts[["verbose"]]
  fun <- selected_indicator[[indicator]][["fun"]]
  avail_resources <- atts[["resources"]]
  req_resources <- selected_indicator[[indicator]][["resources"]]

  processor <- switch(
    processing_mode,
    asset = .asset_processor,
    portfolio = .portfolio_processor,
    stop(sprintf("Processing mode '%s' is not supported.", processing_mode)))

  results <- processor(x, fun, avail_resources, req_resources, params)
  # bind the asset results
  results <- .bind_assets(results)
  # nest the results
  results <- nest(results, !!indicator := !.id)
  # attach results
  x[indicator] <- results[indicator]
  # sent sf column to back and return
  x <- relocate(x, !!attributes(x)[["sf_column"]], .after = last_col())
  x
}

.asset_processor <- function(
    x,
    fun,
    avail_resources,
    req_resources,
    params){


  n <- nrow(x)
  p <- progressr::progressor(steps = n)

  furrr::future_map(seq_len(n), function(i) {
    p()
    resources <- .prep_resources(x[i, ], avail_resources, req_resources)
    result <- .compute(x[i, ], resources, fun, params)
    .check_single_asset(result, i)
  }, .options = furrr::furrr_options(seed = TRUE))
}

.portfolio_processor <- function(
    x,
    fun,
    avail_resources,
    req_resources,
    params ){

  resources <- .prep_resources(x, avail_resources, req_resources)
  results <- .compute(x, resources, fun, params)
  if (!inherits(results, "list")) {
    stop("Expected output for processing mode 'portfolio' is a list.")
  }
  results <- purrr::imap(results, function(r, i) .check_single_asset(r, i))
  results
}

.compute <- function(x, resources, fun, args) {
  args <- append(args, resources)
  args[["x"]] <- x
  try(do.call(what = fun, args = args), silent = TRUE)
}

#' @importFrom utils str
.check_single_asset <- function(obj, i){

  if (inherits(obj, "try-error")) {
    warning(sprintf("At asset %s an error occured. Returning NA.\n", i), obj)
    return(NA)
  }

  if (!inherits(obj, "tbl_df")) {
    warning(sprintf("At asset %s a non-tibble object was returned. Returning NA.\n", i), str(obj))
    return(NA)
  }

  if (nrow(obj) == 0) {
    warning(sprintf("At asset %s a 0-length tibble was returned. Returning NA.", i))
    return(NA)
  }
  obj
}

.bind_assets <- function(results) {
  # bind results to data.frame
  index_tbl <- purrr::map_lgl(results, function(x) inherits(x, c("tbl_df", "data.frame")))

  # case all assets returned tibbles
  if (all(index_tbl)) {
    return(dplyr::bind_rows(results, .id = ".id"))
  }

  # case all assets returned NA
  if (all(!index_tbl)) {
    return(
      tibble::tibble(
        .id = as.character(1:length(results)),
        value = rep(NA, length(results))
      )
    )
  }

  # case some assets returned NA
  if (any(index_tbl) & any(!index_tbl)) {
    colnames <- names(results[[which(index_tbl)[1]]])
    fill_values <- lapply(1:length(colnames), function(x) {
      return(NA)
    })
    fill_values <- tibble::as_tibble(data.frame(fill_values))
    names(fill_values) <- colnames
    for (i in which(!index_tbl)) results[[i]] <- fill_values
    return(tibble::tibble(dplyr::bind_rows(results, .id = ".id")))
  }
}
