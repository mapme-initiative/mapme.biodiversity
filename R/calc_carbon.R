#' Calculate carbon statistics
#'
#' These functions allow to calculated statistics based on the harmonized
#' carbon layers for 2010 and 2018 by Noon et al. (2022).
#'
#' The required resources for these indicators are:
#'  - [carbon_resources]
#'
#' @details
#' Irrecoverable carbon is the amount of carbon that, if lost today, could not
#' be recovered until 2050. It can be calculated for above- and below-ground
#' carbon, the total amount of carbon, or for all layers.
#'
#' @name carbon_indicators
#' @param type One of "total", "soil", "biomass", "all". Determines
#'     for which data layer the statistics are calculated.
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons either
#'   one or multiple inputs as character. Supported statistics are: "mean",
#'   "median", "sd", "min", "max", "sum", and "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble with `(type)_carbon_(stat)`
#'   as variable and the respective statistic (in Mg) as value.
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
#' aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(
#'     get_man_carbon(),
#'     get_vul_carbon(),
#'     get_irr_carbon()
#'   ) %>%
#'   calc_indicators(
#'     calc_man_carbon(stats = "sum"),
#'     calc_vul_carbon(stats = "sum"),
#'     calc_irr_carbon(stats = "sum")
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_irr_carbon <- function(type = c("total", "soil", "biomass", "all"),
                            engine = "extract",
                            stats = "mean") {
  type <- match.arg(type)
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(
      x,
      irr_carbon,
      name = "irr_carbon",
      mode = "asset",
      aggregation = "stat",
      verbose = mapme_options()[["verbose"]]) {
    .calc_carbon_stats(
      x,
      layer = irr_carbon,
      which_layer = type,
      stats = stats,
      engine = engine,
      name = "irr_carbon",
      mode = "asset"
    )
  }
}

register_indicator(
  name = "irr_carbon",
  description = "Statistics of irrecoverable carbon per polygon.",
  resources = "irr_carbon"
)

#' @details
#' Manageable carbon is the amount of carbon that, in principle, is manageable
#' by human activities, e.g. its release to the atmosphere can be prevented.
#' It can be calculated for above- and below-ground carbon, the total amount
#' of carbon, or for all layers.
#'
#' @name carbon_indicators
#' @keywords indicator
#' @export
calc_man_carbon <- function(type = c("total", "soil", "biomass", "all"),
                            engine = "extract",
                            stats = "mean") {
  type <- match.arg(type)
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(
      x,
      man_carbon,
      name = "man_carbon",
      mode = "asset",
      aggregation = "stat",
      verbose = mapme_options()[["verbose"]]) {
    .calc_carbon_stats(
      x,
      layer = man_carbon,
      which_layer = type,
      stats = stats,
      engine = engine,
      name = "man_carbon",
      mode = "asset"
    )
  }
}

register_indicator(
  name = "man_carbon",
  description = "Statistics of manageable carbon per polygon.",
  resources = "man_carbon"
)

#' @details
#' Vulnerable carbon is the amount of carbon that would be released in a typical
#' land conversion activity.
#' It can be calculated for above- and below-ground carbon, the total amount
#' of carbon, or for all layers.
#'
#' @name carbon_indicators
#' @keywords indicator
#' @export
calc_vul_carbon <- function(type = c("total", "soil", "biomass", "all"),
                            engine = "extract",
                            stats = "mean") {
  type <- match.arg(type)
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(
      x,
      vul_carbon,
      name = "vul_carbon",
      mode = "asset",
      aggregation = "stat",
      verbose = mapme_options()[["verbose"]]) {
    .calc_carbon_stats(
      x,
      layer = vul_carbon,
      which_layer = type,
      stats = stats,
      engine = engine,
      name = "vul_carbon",
      mode = "asset"
    )
  }
}

register_indicator(
  name = "vul_carbon",
  description = "Statistics of vulnerable carbon per polygon.",
  resources = "vul_carbon"
)

.calc_carbon_stats <- function(
    x,
    layer,
    which_layer = c("total", "soil", "biomass", "all"),
    stats = "mean",
    engine = "extract",
    name = NULL,
    mode = "asset") {
  stat <- NULL
  if (is.null(layer)) {
    return(NULL)
  }

  if (which_layer == "all") which_layer <- c("total", "soil", "biomass")
  names(layer) <- tolower(names(layer))
  layer <- layer[[grep(paste(which_layer, collapse = "|"), names(layer))]]

  if (all(unlist(global(noNA(layer), fun = "sum")) == 0)) {
    return(NULL)
  }

  area_r <- terra::cellSize(layer, unit = "ha")
  result <- select_engine(
    x = x,
    raster = layer * area_r,
    stats = stats,
    engine = engine,
    name = name,
    mode = mode
  )

  type <- sapply(strsplit(names(layer), "_"), function(x) x[3])
  year <- as.numeric(gsub("\\D", "", names(layer)))

  result[["type"]] <- type
  result[["year"]] <- year
  result <- tidyr::pivot_longer(result, cols = -c(type, year), names_to = "variable")
  result <- dplyr::mutate(result,
    stat = strsplit(variable, "_")[[1]][3],
    variable = paste(name, type, stats, sep = "_"),
    datetime = as.POSIXct(paste0(year, "-01-01T00:00:00Z")),
    unit = "Mg",
    value = sapply(value, function(x) ifelse(is.infinite(x) || is.nan(x), NA, x))
  )
  result[, c("datetime", "variable", "unit", "value")]
}
