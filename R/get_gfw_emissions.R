#' Forest greenhouse gas emissions
#'
#' This resource is part of the publication by Harris et al. (2021)
#' "Global maps of twenty-first century forest carbon fluxes.". It
#' represents "the greenhouse gas
#' emissions arising from stand-replacing forest disturbances that occurred in
#' each modelled year (megagrams CO2 emissions/ha, between 2001 and 2020).
#' Emissions include all relevant ecosystem carbon pools (aboveground biomass,
#' belowground biomass, dead wood, litter, soil) and greenhouse gases (CO2, CH4,
#' N2O)." The area unit that is downloaded here corresponds to the
#' "megagrams of CO2 emissions/pixel" layer, in order to support the calculation
#' of area-wise emissions.
#'
#' There are no arguments users need to specify. However, users should note
#' that the spatial extent for this dataset does not totally cover the same
#' extent as the \code{treecover2000} and \code{lossyear} resources by Hansen
#' et al. (2013). A missing value (NA) will be inserted for greenhouse gas
#' emissions for areas where no data is available.
#'
#' @name gfw_emissions
#' @docType data
#' @keywords resource
#' @format A global tiled raster resource available for all land areas.
#' @references Harris, N.L., Gibbs, D.A., Baccini, A. et al. Global maps of
#' twenty-first century forest carbon fluxes. Nat. Clim. Chang. 11, 234–240
#' (2021). https://doi.org/10.1038/s41558-020-00976-6
#' @source \url{https://data.globalforestwatch.org/datasets/gfw::forest-greenhouse-gas-emissions/about}
NULL


#' Get greenhouse layer
#'
#' @param x An sf object returned by init_portfolio
#' @param verbose Logical controlling verbosity.
#' @param rundir A directory where intermediate files are written to.
#' @keywords internal
#' @noRd
.get_gfw_emissions <- function(x,
                               verbose = TRUE,
                               rundir = tempdir()) {

  index_file <- system.file("extdata", "greenhouse_index.geosjon", package = "mapme.biodiversity")
  spatialindex <- st_read(index_file, quiet = TRUE)
  tile_ids <- unique(unlist(st_intersects(x, spatialindex)))
  tile_ids <- spatialindex$tile_id[tile_ids]
  urls <- as.character(
    spatialindex$Mg_CO2e_px_download[spatialindex$tile_id %in% tile_ids]
  )
  filenames <- file.path(
    rundir,
    sprintf("gfw_forest_carbon_gross_emissions_Mg_CO2e_px_%s.tif", tile_ids)
  )
  if (attr(x, "testing")) {
    return(basename(filenames))
  }
  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls, filenames, verbose, check_existence = FALSE, aria_bin = aria_bin)
  # return all paths to the downloaded files
  filenames
}
