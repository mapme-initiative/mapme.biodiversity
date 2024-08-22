#' Calculate areal statistics for IBPES Biomes
#'
#' This indicator calculates the areal distribution of different biome classes
#' within an asset based on the IBPES biomes dataset.
#'
#' The required resources for this indicator are:
#'  - [ipbes_biomes]
#'
#' @name ipbes_biome_stats
#' @keywords indicator
#' @returns A function that returns an indicator tibble with the biome class
#' as variable and the respective area (in ha) as value.
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
#'   get_resources(get_ipbes_biomes()) %>%
#'   calc_indicators(calc_ipbes_biomes()) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_ipbes_biomes <- function() {
  function(
      x,
      ipbes_biomes,
      name = "ipbes_biomes",
      mode = "portfolio",
      aggregation = "sum",
      verbose = mapme_options()[["verbose"]]) {
    if (is.null(ipbes_biomes)) {
      return(NULL)
    }
    if (st_crs(x) != st_crs(ipbes_biomes)) {
      x <- st_transform(x, st_crs(ipbes_biomes))
    }

    results <- lapply(1:nrow(x), function(i) {
      exactextractr::exact_extract(ipbes_biomes, x[i, ], fun = function(data, class_df) {
        data[["coverage_area"]] <- data[["coverage_area"]] / 10000
        class_area <- by(data[["coverage_area"]], data[["value"]], sum)
        result <- tibble::tibble(
          variable = as.numeric(names(class_area)),
          unit = "ha",
          value = as.numeric(class_area)
        )
        result[["variable"]] <- class_df[["name"]][result[["variable"]]]
        result[["variable"]] <- gsub(" ", "_", result[["variable"]])
        result[["datetime"]] <- as.POSIXct("2019-01-01T00:00:00Z")
        result[, c("datetime", "variable", "unit", "value")]
      }, class_df = .ipbes_biome_classes, coverage_area = TRUE, summarize_df = TRUE)
    })

    results
  }
}

register_indicator(
  name = "ipbes_biomes",
  description = "Area distibution of IBPES biomes within a polygon.",
  resources = "ipbes_biomes"
)

.ipbes_biome_classes <- data.frame(
  class = c(1:15),
  name = c(
    "tropical and subtropical dry and humid forests",
    "temperate and boreal forests and woodland",
    "mediterranean forests woodlands and scrub",
    "tundra and high mountain habitats",
    "tropical and subtropical savannas and grasslands",
    "temperate grassland",
    "deserts and xeric shrubland",
    "wetlands",
    "urban and semi-urban areas",
    "cultivated areas",
    "cyrosphere",
    "aquaculture areas",
    "inland surface waters and water bodies",
    "shelf ecosystems",
    "open ocean pelagic systems"
  )
)
