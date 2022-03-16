#' Downloads Global Mangrove Extent Polygon
#'
#' The polygons represent the mangrove, which is tropical coastal vegetation
#' and considered the most significant part of the marine ecosystem. This
#' resource is available for the period 1996- 2016 from Global Mangrove Watch
#' (GMW), providing geospatial information about global mangrove extent.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name Global_Mangrove_Extent
#' @keywords internal
#'

.get_mangrove <- function(x,
                          rundir = tempdir(),
                          verbose = TRUE) {
  target_years <- attributes(x)$years
  available_years <- c(1996, 2007:2010, 2015, 2016)
  target_years <- .check_available_years(
    target_years, available_years, "mangroveextent"
  )
  urls <- unlist(sapply(target_years, function(year) .get_mangrove_url(year)))
  filenames <- file.path(
    rundir,
    basename(paste0("gmw-extent_", target_years, ".zip"))
  )
  if (any(file.exists(filenames))) {
    message("Skipping existing files in output directory.")
  }
  # start download in a temporal directory within tmpdir
  .download_or_skip(urls, filenames, verbose)

  # unzip and convert shp to gpkg
  message("Translating shapefiles to GeoPackages. This may take a while....")
  sapply(filenames, function(zip) .unzip_mangrove(zip, rundir))

  # return paths to the gpkg
  list.files(rundir, full.names = T, pattern = ".gpkg")
}


#' Helper function to correctly unzip a mangrove layer zip file
#'
#'
#' @param zip_files A charchter vector with potentially multiple zip files
#' @param rundir The directory to where the files are unzipped
#'
#' @return Nothing.
#' @keywords internal
.unzip_mangrove <- function(zip, rundir) {
  bn <- basename(zip)
  if (bn == "gmw-extent_2007.zip") {
    gpkg <- file.path(rundir, "gmw-extent_2007.gpkg")
    if (file.exists(gpkg)) {
      return(gpkg)
    }
    utils::unzip(
      zipfile = file.path(rundir, basename(paste0("gmw-extent_2007.zip"))),
      exdir = rundir,
    )
    shp <- file.path(rundir, "GMW_2007_v2.0.shp")
    write_sf(shp, file.path(rundir, basename("gmw-extent_2007.gpkg")))
    command <- sprintf("ogr2ogr %s %s", gpkg, shp)
    system(command)
    d_files <- list.files(rundir, full.names = T)
    unlink(grep("gmw-extent*", d_files, value = T, invert = T),
      recursive = T, force = T
    )
  } else {
    year <- gsub(".*?([0-9]+).*", "\\1", bn)
    gpkg <- file.path(rundir, paste0("gmw-extent_", year, ".gpkg"))
    if (file.exists(gpkg)) {
      return(gpkg)
    }
    utils::unzip(
      zipfile = file.path(
        rundir,
        basename(paste0("gmw-extent_", year, ".zip"))
      ),
      exdir = rundir
    )
    shp <- paste0(
      rundir, "/GMW_001_GlobalMangroveWatch_", year,
      "/01_Data/GMW_", year, "_v2.shp"
    )
    command <- sprintf("ogr2ogr %s %s", gpkg, shp)
    system(command)
    d_files <- list.files(rundir, full.names = T)
    unlink(grep("gmw-extent*", d_files, value = T, invert = T),
      recursive = T, force = T
    )
  }
}


#' A helper function to construct correct mangrove layer urls
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector
#' @keywords internal
.get_mangrove_url <- function(target_year) {
  available_years <- c(1996, 2007:2010, 2015, 2016)
  if (target_year %in% available_years) {
    paste0("https://wcmc.io/GMW_", target_year)
  } else {
    warning(
      sprintf(
        "Mangove extent not available for target year %s", target_year
      )
    )
    NULL
  }
}
