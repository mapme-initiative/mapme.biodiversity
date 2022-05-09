#' Soildgrids data layers
#'
#' Soilgrids is a project combining global observation data with
#' machine learning to map the spatial distribution of soil properties across
#' the globe. It is produced at a spatial resolution of 250 meters and each
#' parameters is mapped at different depths. In order to be able to assess
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
#' Users can specify the following arguments:
#' \describe{
#'   \item{layer}{The soil parameter as a single charachter}
#'   \item{depth}{The requested depth as a single chrachter}
#'   \item{stat}{The predicted statistic as a single charachter}
#'   }
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
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for all land areas.
#' @references Hengl T, Mendes de Jesus J, Heuvelink GBM, Ruiperez Gonzalez M,
#' Kilibarda M, et al. (2017) SoilGrids250m: Global gridded soil information
#' based on machine learning. PLOS ONE 12(2): e0169748.
#' \doi{https://doi.org/10.1371/journal.pone.0169748}
#' @source \url{https://www.isric.org/explore/soilgrids}
NULL

#' Helper function to download any soilgrids data layer
#'
#' This function constructs for a given data layer, depth and stat a data layer
#' for the extent of a portfolio and projects the original interrupted  Goode's
#' homolosine projection to Lat/Lon for further processing in the package.
#' Depending on the size of a portfolio this process requires a lot of time. If
#' the portfolio extents of interrupted areas of the homolosine projection (e.g.
#' over the oceans) gdal issues some error messages but proceeds anyway. This is
#' not an issue for the scope of the soil grid indicators because they only make
#' sense for land masses.
#'
#' @param x A sf portfolio object
#' @param layers A charchter vector indicating the layers to download from
#'   soilgrids
#' @param depths A charachter vector indicating the depths to download
#' @param stats A chrachter vector indicating the statistics to download.
#' @param rundir The directory where temporary and final results are written to.
#' @param verbose A logical controlling the verbosity.
#'
#' @return A charchter vector of the final filenames
#' @keywords internal
#' @importFrom stringr str_replace
#' @noRd
.get_soilgrids <- function(x,
                           layers,
                           depths,
                           stats,
                           rundir = tempdir(),
                           verbose,
                           ...) {
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
    stop(sprintf(
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


  filenames <- list()
  for (layer in layers) {
    for (depth in depths) {
      for (stat in stats) {
        if (layer != "ocs" & depth == "0-30cm") {
          message("Depth '0-30cm' is only available of layer 'ocs'.")
          next
        }

        if (layer == "ocs" & depth != "0-30cm") {
          message("Layer 'ocs' is only available at depth '0-30cm'.")
          next
        }

        baseurl <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
        datalayer <- sprintf("%s/%s_%s_%s.vrt", layer, layer, depth, stat)
        filename <- file.path(rundir, str_replace(basename(datalayer), "vrt", "tif"))
        if (attr(x, "testing")) {
          filenames <- append(filenames, basename(filename))
          next
        }

        if (!file.exists(filename)) {
          if (verbose) {
            message(
              sprintf(
                paste("Starting to download data for layer '%s', depth '%s', and stat '%s'.",
                  " This may take a while...",
                  sep = ""
                ),
                layer, depth, stat
              )
            )
          }
          soilgrid_source <- rast(file.path(baseurl, datalayer))
          x_bbox <- st_as_sf(st_as_sfc(st_bbox(x)))
          x_proj <- st_transform(x_bbox, crs(soilgrid_source))
          soilgrid_cropped <- crop(soilgrid_source, x_proj,
            filename = file.path(rundir, "soillayer_cropped.tif"),
            datatype = "INT2U", overwrite = TRUE
          )
          suppressWarnings(
            project(soilgrid_cropped, "EPSG:4326",
              filename = filename,
              datatype = "INT2U", overwrite = TRUE
            )
          )
          file.remove(file.path(rundir, "soillayer_cropped.tif"))
        } else {
          if (verbose) {
            message(sprintf("Output file %s exists. Skipping re-download. Please delete if spatial extent has changed.", basename(filename)))
          }
        }
        filenames <- append(filenames, filename)
      }
    }
  }
  unlist(filenames)
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
