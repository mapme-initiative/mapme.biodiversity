#' Terrestrial Ecoregions of the World (TEOW) Polygon
#'
#' This resource is part of the publication by Olson et al. (2004)
#' "Terrestrial Ecosystems of the World (TEOW) from WWF-US (Olson)". It
#' depicts 867 terrestrial ecoregions around the world classified into 14
#' different terrestrial biomes such as forests, grasslands, or deserts.
#' The polygons represent the ecoregions, defined as relatively large units of
#' land or inland water sharing a large majority of biodiversity. The datasets
#' is made available from World Wildlife Fund (WWF) for the year 2001.
#'
#' @name teow
#' @docType data
#' @keywords resource
#' @format Global terrestrial polygon resource depicting ecoregions.
#' @references Olson, D. M., Dinerstein, E., Wikramanayake, E. D., Burgess, N. D.,
#' Powell, G. V. N., Underwood, E. C., D’Amico, J. A., Itoua, I., Strand, H. E.,
#' Morrison, J. C., Loucks, C. J., Allnutt, T. F., Ricketts, T. H., Kura, Y., Lamoreux,
#' J. F., Wettengel, W. W., Hedao, P., Kassem, K. R. 2001. Terrestrial ecoregions of
#' the world: a new map of life on Earth. Bioscience 51(11):933-938.
#' @source \url{https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
NULL


#' Downloads Terrestrial Ecoregions of the World (TEOW) Polygon
#'
#' @param x An sf object returned by init_portfolio
#' @param rundir A directory where intermediate files are written to.
#' @param verbose Logical controlling verbosity.
#' @importFrom utils unzip
#' @keywords internal
#' @include register.R
#' @noRd
.get_teow <- function(x,
                      rundir = tempdir(),
                      verbose = TRUE) {

  filename <- file.path(rundir, "wwf_terr_ecos.gpkg")
  # check if output file exists
  if (file.exists(filename)) {
    return(filename)
  }

  # get url
  url <- paste("https://files.worldwildlife.org/wwfcmsprod/files/",
    "Publication/file/6kcchn7e3u_official_teow.zip",
    sep = ""
  )
  # start download in a temporal directory within rundir
  downloads <- tryCatch(
    {
      download.file(url,
        file.path(rundir, basename(paste0("TEOW_global.zip"))),
        quiet = TRUE
      )
    },
    error = function(e) e,
    warning = function(e) e
  )
  if (inherits(downloads, "error")) stop(downloads)
  # unzip
  .unzip_and_remove(file.path(rundir, "TEOW_global.zip"), rundir)
  # load shp
  shp <- read_sf(file.path(rundir, "official/wwf_terr_ecos.shp"))
  # write as gpkg
  write_sf(shp, file.path(rundir, "wwf_terr_ecos.gpkg"))
  # remove all except desired layers
  all_files <- list.files(rundir, full.names = T)
  unlink(grep(paste0("wwf_terr_ecos.gpkg"), all_files, value = T, invert = T),
    recursive = T, force = T
  )
  # return paths to the gpkg
  filename
}

register_resource(
  name = "teow",
  type = "vector",
  source = "https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world",
  fun = .get_teow,
  arguments <- list()
)
