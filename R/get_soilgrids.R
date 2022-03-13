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
#' @param layer A charchter vector indicating the layer to download from
#'   soilgrids
#' @param depth A charachter vector indicating the depth to download
#' @param stat A chrachter vector indicating the statistic to download.
#' @param rundir The directory where temporary and final results are written to.
#' @param verbose A logical controlling the verbosity.
#'
#' @return A charchter vector of the final filenames
#' @keywords internal
#' @importFrom stringr str_replace
.get_soilgrids <- function(x,
                           layer,
                           depth,
                           stat,
                           rundir = tempdir(),
                           verbose = TRUE) {
  if (any(missing(layer), missing(depth), missing(stat))) {
    stop(
      paste("For downloading data from soilgrid a valid layer, a valid ",
        "depth range and a valid statistic have to be specified.",
        sep = ""
      )
    )
  }

  if (!layer %in% names(.sg_layers)) {
    stop(
      sprintf(
        paste("The selected layer '%s' is not available. ",
          "Please choose one of: %s.",
          sep = ""
        ),
        layer, paste(names(.sg_layers), collapse = ", ")
      )
    )
  }

  if (!depth %in% .sg_depths) {
    stop(
      sprintf(
        paste("The selected depth range '%s' is not available. ",
          "Please choose one of: %s.",
          sep = ""
        ),
        depth, paste(.sg_depths, collapse = ", ")
      )
    )
  }

  if (!stat %in% .sg_stats) {
    stop(
      sprintf(
        paste("The selected predictions statistic '%s' is not available. ", "
              Please choose one of: %s.", sep = ""),
        stat, paste(.sg_stats, collapse = ", ")
      )
    )
  }
  message(
    sprintf(
      paste("Starting to download data for layer '%s'.",
        " This may take a while...",
        sep = ""
      ),
      layer
    )
  )

  baseurl <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
  datalayer <- sprintf("%s/%s_%s_%s.vrt", layer, layer, depth, stat)
  filename <- str_replace(basename(datalayer), "vrt", "tif")
  if (file.exists(file.path(rundir, filename))) {
    message(
      sprintf(
        "File %s exists in output directory. Skipping download.",
        filename
      )
    )
    return(file.path(rundir, filename))
  }
  prjstring <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"
  srcprj <- st_crs(prjstring)
  x %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(srcprj) %>%
    st_bbox() %>%
    as.numeric() -> bbox
  bbox <- paste(c(bbox[1], bbox[4], bbox[3], bbox[2]), collapse = " ")

  command_to_local_vrt <- sprintf(
    "gdal_translate -tr 250 250 -projwin %s %s %s",
    bbox, file.path(baseurl, datalayer),
    file.path(rundir, basename(datalayer))
  )
  system(command_to_local_vrt, intern = TRUE)

  command_to_wgs84 <- sprintf(
    "gdalwarp -s_srs '%s' -t_srs EPSG:4326  %s %s ",
    prjstring,
    file.path(rundir, basename(datalayer)),
    file.path(rundir, paste0("proj_", basename(datalayer)))
  )
  system(command_to_wgs84, intern = TRUE)

  command_to_gtiff <- sprintf(
    paste("gdal_translate -co TILED=YES -co COMPRESS=DEFLATE ",
      "-co PREDICTOR=2 -co BIGTIFF=YES %s %s",
      sep = ""
    ),
    file.path(rundir, paste0("proj_", basename(datalayer))),
    file.path(rundir, filename)
  )
  system(command_to_gtiff)

  file.remove(
    grep(list.files(rundir, full.names = TRUE),
      pattern = ".tif$", invert = TRUE, value = TRUE
    )
  )
  file.path(rundir, filename)
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
    conversion_factor = 100,
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
.sg_stats <- c("Q0.05", "Q0.50", "mean", "Q0.95")

.get_bdod <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "bdod", depth, stat, rundir, verbose)
}

.get_cec <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "cec", depth, stat, rundir, verbose)
}

.get_cfvo <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "cfvo", depth, stat, rundir, verbose)
}

.get_clay <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "clay", depth, stat, rundir, verbose)
}

.get_nitrogen <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "nitrogen", depth, stat, rundir, verbose)
}

.get_phh2o <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "phh2o", depth, stat, rundir, verbose)
}

.get_sand <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "sand", depth, stat, rundir, verbose)
}
.get_silt <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "silt", depth, stat, rundir, verbose)
}

.get_soc <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "soc", depth, stat, rundir, verbose)
}

.get_ocd <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth == "0-30cm") {
    stop(paste("Depth '0-30cm' is not available for this layer. ",
      "Please choose another depth.",
      sep = ""
    ))
  }
  .get_soilgrids(x, "ocd", depth, stat, rundir, verbose)
}

.get_ocs <- function(x, depth, stat, rundir = tempdir(), verbose = TRUE) {
  if (depth != "0-30cm") {
    message(paste("Layer 'ocs' is only available for depth '0-30cm'.",
      "Setting to this value.",
      sep = ""
    ))
    depth <- "0-30cm"
  }
  .get_soilgrids(x, "ocs", depth, stat, rundir, verbose)
}
