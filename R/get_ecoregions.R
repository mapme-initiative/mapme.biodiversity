#' Downloads Terrestrial Ecoregions of the World (TEOW) Polygon
#'
#' The polygons represent the ecoregions, defined as relatively large units of land
#' or inland water sharing a large majority of biodiversity. The datasets is made
#' available from World Wildlife Fund (WWF). There are 867 terrestrial ecoregions,
#' further classified into 14 different terrestrial biomes.
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @name Terrestrial_Ecoregions_of_the_World
#' @keywords internal
#'

.get_ecoregions <- function(x,
                            rundir = tempdir(),
                            verbose = TRUE) {

  # get url
  url <- "https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip"

  # start download in a temporal directory within rundir
  downloads = tryCatch({
    download.file(url, file.path(rundir, basename(paste0("TEOW_global.zip"))), quiet = TRUE)
  },
  error = function(e) e,
  warning = function(e) e
  )
  if(inherits(downloads, "error")) stop(downloads)
  # unzip
  .UnzipAndRemove(file.path(rundir,"TEOW_global.zip"), rundir)
  # load shp
  shp <- read_sf(file.path(rundir, "official/wwf_terr_ecos.shp"))
  # write as gpkg
  st_write(shp, file.path(rundir, "wwf_terr_ecos.gpkg"))
  # remove all except desired layers
  all_files <- list.files(rundir, full.names = T)
  unlink(grep(paste0("wwf_terr_ecos.gpkg"), all_files, value = T, invert = T), recursive = T, force = T)
  # return paths to the gpkg
  list.files(rundir, full.names = T)
}
