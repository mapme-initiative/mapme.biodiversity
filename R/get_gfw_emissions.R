#' Forest greenhouse gas emissions
#'
#' This resource is part of the publication by Harris et al. (2021)
#' "Global maps of twenty-first century forest carbon fluxes.". It
#' represents "the greenhouse gas
#' emissions arising from stand-replacing forest disturbances that occurred in
#' each modelled year (megagrams CO2 emissions/ha, between 2001 and 2023).
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
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Harris, N.L., Gibbs, D.A., Baccini, A. et al. Global maps of
#' twenty-first century forest carbon fluxes. Nat. Clim. Chang. 11, 234–240
#' (2021). https://doi.org/10.1038/s41558-020-00976-6
#' @source \url{https://data.globalforestwatch.org/datasets/gfw::forest-greenhouse-gas-emissions/about}
#' @include register.R
#' @export
get_gfw_emissions <- function() {
  function(x,
           name = "gfw_emissions",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    index_file <- system.file("extdata", "greenhouse_index.geosjon", package = "mapme.biodiversity")
    spatialindex <- st_read(index_file, quiet = TRUE, check_ring_dir = TRUE)

    row_ids <- unique(unlist(st_intersects(x, spatialindex)))
    tile_str <- spatialindex$tile_id[row_ids]

    urls <- as.character(spatialindex$Mg_CO2e_px_download[row_ids])
    urls <- paste0("/vsicurl/", urls)
    filenames <- sprintf("gfw_forest_carbon_gross_emissions_Mg_CO2e_px_%s.tif", tile_str)

    fps <- spatialindex[row_ids, "geometry"]
    fps[["source"]] <- urls
    make_footprints(fps, filenames,
      what = "raster",
      co = c("-co", "INTERLEAVE=BAND", "-co", "COMPRESS=DEFLATE")
    )
  }
}

register_resource(
  name = "gfw_emissions",
  description = "Global Forest Watch - CO2 Emssions caused by forest cover loss",
  licence = "CC-BY 4.0",
  source = "https://data.globalforestwatch.org/datasets/gfw::forest-greenhouse-gas-emissions/about",
  type = "raster"
)
