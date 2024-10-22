#' Species threat abatement and restoration (STAR) zonal statistics
#'
#' Calculates zonal statistics of the STAR resources raster layers.
#'
#' For threat abatement, low values (towards 0) represent locations with
#' low levels of threat for all present species, while higher values (>5000),
#' represent areas with large number of species under significant threats.
#'
#' For restoration, lower values (towards 0) represent areas where restoration efforts are
#' less likely to contribute to lowering the threat level on the present species,
#' while higher values (>5000) indicate areas where restoration efforts are
#' very likley to result in lowering the threat levels of the present species.
#'
#' Be referred to the documentation of the STAR resource and the original
#' research article for details on interpreting its values.
#'
#' The required resources for this indicator are:
#'  - [star]
#'
#' @name star_stats
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum" "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble with IUCN layers with
#'   specified statistics as variable and respective species richness (count)
#'   as value.
#' @export
calc_star <- function(engine = "extract", stats = "mean") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(
      x,
      star,
      name = "star",
      mode = "asset",
      aggregation = "stat",
      verbose = mapme_options()[["verbose"]]) {
    if (is.null(star)) {
      return(NULL)
    }

    if (all(unlist(global(noNA(star), fun = "sum")) == 0)) {
      return(NULL)
    }

    x <- st_transform(x, st_crs(star))

    result <- select_engine(
      x = x,
      raster = star,
      stats = stats,
      engine = engine,
      name = "",
      mode = "asset"
    )

    result[["variable"]] <- names(star)
    result <- tidyr::pivot_longer(result, -variable, names_to = "stats")
    result[["variable"]] <- paste0(tolower(result[["variable"]]), result[["stats"]])
    result[["value"]][is.na(result[["value"]])] <- 0
    result[["datetime"]] <- as.POSIXct("2019-01-01T00:00:00Z")
    result[["unit"]] <- "unitless"
    result[, c("datetime", "variable", "unit", "value")]
  }
}

register_indicator(
  name = "star",
  description = "Species threat abatement and restoration (STAR) zonal statistics",
  resources = "star"
)
