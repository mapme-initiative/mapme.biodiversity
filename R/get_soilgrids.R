#' SoilGrids data layers
#'
#' SoilGrids is a project combining global observation data with
#' machine learning to map the spatial distribution of soil properties across
#' the globe. It is produced at a spatial resolution of 250 meters and each
#' parameter is mapped at different depths. In order to be able to assess
#' prediction uncertainty, besides the mean and median prediction, the 0.05 and
#' 0.95 percentile predictions are available.
#' The following parameters are available:
#' \describe{
#'   \item{bdod}{Bulk density of the fine earth fraction (kg/dm3)}
#'   \item{cec}{Cation Exchange Capacity of the soil (cmol(c)/kg)}
#'   \item{cfvo}{Volumetric fraction of coarse fragments > 2 mm (cm3/100cm3 (volPerc))}
#'   \item{clay}{Proportion of clay particles < 0.002 mm in the fine earth fraction (g/100g)}
#'   \item{nitrogen}{Total nitrogen (g/kg)}
#'   \item{phh2o}{Soil pH (pH)}
#'   \item{sand}{Proportion of sand particles > 0.05 mm in the fine earth fraction (g/100g)}
#'   \item{silt}{Proportion of silt particles >= 0.002 mm and <= 0.05 mm in the fine earth fraction (g/100g)}
#'   \item{soc}{Soil organic carbon content in the fine earth fraction (g/kg)}
#'   \item{ocd}{Organic carbon density (kg/m3)}
#'   \item{ocs}{Organic carbon stocks (kg/m²)}
#' }
#'
#' Except for \code{ocs}, which is only available for a depth of \code{"0-30cm"},
#' all other parameters are available at the following depths:
#' - "0-5cm"
#' - "5-15cm"
#' - "15-30cm"
#' - "30-60cm"
#' - "60-100cm"
#' - "100-200cm"
#'
#' Each parameter and depth is available for the following statistics:
#' - "Q0.05"
#' - "Q0.50"
#' - "mean"
#' - "Q0.95"
#' @name soilgrids
#' @param layers A character vector indicating the layers to download from
#'   soilgrids
#' @param depths A character vector indicating the depths to download
#' @param stats A character vector indicating the statistics to download.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M.,
#'   Kempen, B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil
#'   information for the globe with quantified spatial uncertainty, SOIL, 7,
#'   217–240, 2021. \doi{https://doi.org/10.5194/soil-7-217-2021}
#' @source \url{https://www.isric.org/explore/soilgrids}
#' @include register.R
#' @export
get_soilgrids <- function(layers, depths, stats) {
  if (any(missing(layers), missing(depths), missing(stats))) {
    stop(
      paste("For downloading data from soilgrid a valid layer, a valid ",
        "depth range and a valid statistic have to be specified.",
        sep = ""
      ),
      call. = FALSE
    )
  }

  if (any(!layers %in% names(.sg_layers))) {
    na_layers <- layers[which(!layers %in% .sg_layers)]
    stop(sprintf(
      paste("The selected layer(s) '%s' is/are not available. ",
        "Please choose one of: %s.",
        sep = ""
      ),
      paste(na_layers, sep = ", "), paste(names(.sg_layers), collapse = ", ")
    ), call. = FALSE)
  }

  if (any(!depths %in% .sg_depths)) {
    na_depths <- depths[which(!depths %in% .sg_depths)]
    stop(
      sprintf(
        paste("The selected depth range(s) '%s' is/are not available. ",
          "Please choose one of: %s.",
          sep = ""
        ),
        na_depths, paste(.sg_depths, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (any(!stats %in% .sg_stats)) {
    na_stats <- stats[which(!stats %in% .sg_stats)]
    stop(
      sprintf(
        paste("The selected predictions statistic(s) '%s' is not available. ", "
              Please choose one of: %s.", sep = ""),
        na_stats, paste(.sg_stats, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  function(x,
           name = "soilgrids",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    grid <- expand.grid(layers, depths, stats)
    names(grid) <- c("layer", "depth", "stat")

    urls <- purrr::pmap_chr(grid, function(layer, depth, stat) {
      if (layer != "ocs" & depth == "0-30cm") {
        message("Depth '0-30cm' is only available of layer 'ocs'.")
        return(NA)
      }

      if (layer == "ocs" & depth != "0-30cm") {
        message("Layer 'ocs' is only available at depth '0-30cm'.")
        return(NA)
      }
      baseurl <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
      datalayer <- sprintf("%s/%s_%s_%s.vrt", layer, layer, depth, stat)
      paste0(baseurl, datalayer)
    })

    urls <- na.omit(urls)
    filenames <- basename(urls)
    bbox <- c(xmin = -19949750, ymin = -6147500, xmax = 19861750, ymax = 8361000)
    fps <- st_as_sf(st_as_sfc(st_bbox(bbox, crs = .sg_wkt)))
    fps[["source"]] <- urls
    make_footprints(fps, filenames = filenames, what = "raster")
  }
}


.sg_layers <- list(
  bdod = list(
    description = "Bulk density of the fine earth fraction",
    mapped_units = "cg/cm3",
    conversion_factor = 100,
    conventional_units = "kg/dm3"
  ),
  cec = list(
    description = "Cation Exchange Capacity of the soil",
    mapped_units = "mmol(c)/kg",
    conversion_factor = 10,
    conventional_units = "cmol(c)/kg"
  ),
  cfvo = list(
    description = "Volumetric fraction of coarse fragments (> 2 mm)",
    mapped_units = "cm3/dm3 (volPerc)",
    conversion_factor = 10,
    conventional_units = "cm3/100cm3 (volPerc)"
  ),
  clay = list(
    description = paste("Proportion of clay particles (< 0.002 mm) ",
      "in the fine earth fraction",
      sep = ""
    ),
    mapped_units = "g/kg",
    conversion_factor = 10,
    conventional_units = "g/100g (Perc)"
  ),
  nitrogen = list(
    description = "Total nitrogen (N)",
    mapped_units = "cg/kg",
    conversion_factor = 100,
    conventional_units = "g/kg"
  ),
  phh2o = list(
    description = "Soil pH",
    mapped_units = "pHx10",
    conversion_factor = 10,
    conventional_units = "pH"
  ),
  sand = list(
    description = paste("Proportion of sand particles (> 0.05 mm) ",
      "in the fine earth fraction",
      sep = ""
    ),
    mapped_units = "g/kg",
    conversion_factor = 10,
    conventional_units = "g/100g (Perc)"
  ),
  silt = list(
    description = paste("Proportion of silt particles (>= 0.002 mm ",
      "and <= 0.05 mm) in the fine earth fraction",
      sep = ""
    ),
    mapped_units = "g/kg",
    conversion_factor = 10,
    conventional_units = "g/100g (Perc)"
  ),
  soc = list(
    description = "Soil organic carbon content in the fine earth fraction",
    mapped_units = "dg/kg",
    conversion_factor = 10,
    conventional_units = "g/kg"
  ),
  ocd = list(
    description = "Organic carbon density",
    mapped_units = "hg/m3",
    conversion_factor = 10,
    conventional_units = "kg/m3"
  ),
  ocs = list(
    description = "Organic carbon stocks",
    mapped_units = "t/ha",
    conversion_factor = 10,
    conventional_units = "kg/m2"
  )
)

.sg_depths <- c(
  "0-5cm", "5-15cm", "15-30cm", "30-60cm",
  "60-100cm", "100-200cm", "0-30cm"
)
.sg_stats <- c("Q0.05", "Q0.5", "mean", "Q0.95")

.sg_wkt <- 'PROJCRS["Interrupted_Goode_Homolosine",
    BASEGEOGCRS["GCS_WGS_1984 ellipse",
        DATUM["World Geodetic System 1984",
            ELLIPSOID["WGS 84",6378137,298.257223563,
                LENGTHUNIT["metre",1]],
            ID["EPSG",6326]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["Degree",0.0174532925199433]]],
    CONVERSION["unnamed",
        METHOD["Interrupted Goode Homolosine"]],
    CS[Cartesian,2],
        AXIS["(E)",east,
            ORDER[1],
            LENGTHUNIT["metre",1,
                ID["EPSG",9001]]],
        AXIS["(N)",north,
            ORDER[2],
            LENGTHUNIT["metre",1,
                ID["EPSG",9001]]]]'

register_resource(
  name = "soilgrids",
  description = "ISRIC - Modelled global soil property layers",
  licence = "CC-BY 4.0",
  source = "https://www.isric.org/explore/soilgrids",
  type = "raster"
)
