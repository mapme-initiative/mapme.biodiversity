#' Carbon Layers
#'
#' These resources are from the publication by Noon et al. (2022) "Mapping the
#' irrecoverable carbon in Earth’s ecosystems". This publication differentiates
#' between 3 different kinds of carbon with varying degrees of manageability
#' by humans. All three layers are available for above and below ground
#' carbon, as well as a layer combining the two.
#'
#' It may be required to increase the timeout option to successfully download
#' theses layers from their source location via e.g. `options(timeout = 600)`.
#'
#' @details
#' Irrecoverable carbon is defined as the amount of carbon, that, if lost today,
#' cannot be recovered until mid 21st century (so within 30 years, considering
#' the publication date).
#'
#' @name carbon_resources
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Noon, M.L., Goldstein, A., Ledezma, J.C. et al. Mapping the
#'   irrecoverable carbon in Earth’s ecosystems. Nat Sustain 5, 37–46 (2022).
#'   https://doi.org/10.1038/s41893-021-00803-6
#' @source \url{https://zenodo.org/records/4091029}
#' @include register.R
#' @export
get_irr_carbon <- function() {
  if (is.null(mapme_options()[["outdir"]])) {
    warning(paste(
      "Carbon layers must be downloaded from the source location",
      "irrespective if `outdir` was specified or not."
    ))
  }

  function(x,
           name = "irr_carbon",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .fetch_carbon("Irrecoverable", outdir)
  }
}

register_resource(
  name = "irr_carbon",
  description = "Amount of carbon irrecoverably lost by a typical land use conversion event until mid-century.",
  licence = "CC NC 4.0",
  source = "https://zenodo.org/records/4091029",
  type = "raster"
)

#' @details
#' Vulnerable carbon is defined as the amount of carbon that would be lost in a
#' hypothetical but typical conversion event (without including information
#' of the probability of such an event to be actually occurring).
#'
#' @name carbon_resources
#' @include register.R
#' @export
get_vul_carbon <- function() {
  if (is.null(mapme_options()[["outdir"]])) {
    warning(paste(
      "Carbon layers must be downloaded from the source location",
      "irrespective if `outdir` was specified or not."
    ))
  }

  function(x,
           name = "vul_carbon",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .fetch_carbon("Vulnerable", outdir)
  }
}

register_resource(
  name = "vul_carbon",
  description = "Amount of carbon that is vulnerable to a typical land use conversion event.",
  licence = "CC NC 4.0",
  source = "https://zenodo.org/records/4091029",
  type = "raster"
)


#' @details
#' Manageable carbon is defined as all land areas, expect cyrosols, because
#' carbon loss is driven by direct land-use conversion which could be halted or
#' because climate change impacts affecting the area can potentially be directly
#' mitigated through adaptive management.
#'
#' @name carbon_resources
#' @include register.R
#' @export
get_man_carbon <- function() {
  if (is.null(mapme_options()[["outdir"]])) {
    warning(paste(
      "Carbon layers must be downloaded from the source location",
      "irrespective if `outdir` was specified or not."
    ))
  }

  function(x,
           name = "man_carbon",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    .fetch_carbon("Manageable", outdir)
  }
}

register_resource(
  name = "man_carbon",
  description = "Amount of carbon that is manageable by humans.",
  licence = "CC NC 4.0",
  source = "https://zenodo.org/records/4091029",
  type = "raster"
)


#' @importFrom utils unzip download.file
.fetch_carbon <- function(layer = NULL, outdir = NULL) {
  tifs <- c(
    "%s_C_Biomass_%s.tif",
    "%s_C_Soil_%s.tif",
    "%s_C_Total_%s.tif"
  )
  tifs <- c(sprintf(tifs, layer, 2010), sprintf(tifs, layer, 2018))
  has_outdir <- !is.null(outdir)
  tmpdir <- tempfile()
  dir.create(tmpdir)

  if (has_outdir) {
    tifs <- file.path(outdir, tifs)
  } else {
    tifs <- file.path(tmpdir, tifs)
  }

  is_available <- purrr::map_lgl(tifs, spds_exists, what = "raster")
  if (all(is_available)) {
    return(make_footprints(tifs, what = "raster"))
  }

  srcs <- .get_goldstein_url(sprintf("^%s_Carbon_\\d{4}\\.zip$", layer))
  srcs$filename <- file.path(tmpdir, srcs[["filename"]])

  purrr::walk2(srcs$url, srcs$filename, function(src, dst) {
    download.file(src, dst, mode = ifelse(Sys.info()["sysname"] == "Windows", "wb", "w"))
    unzip(dst, junkpaths = TRUE, exdir = tmpdir)
  })

  tifs <- list.files(tmpdir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)
  tifs <- grep(layer, tifs, value = TRUE)
  make_footprints(
    tifs,
    what = "raster",
    co = c("-of", "COG", "-co", "COMPRESS=LZW", "-ot", "UInt16")
  )
}


#' @noRd
#' @importFrom httr2 request req_perform req_retry resp_status resp_body_json
.get_goldstein_url <- function(layer) {
  baseurl <- "https://zenodo.org/api/records/4091029"
  is_transient <- function(resp) resp_status(resp) %in% c(429, 503, 504)
  rsp <- req_perform(req_retry(request(baseurl),
    max_seconds = 10,
    is_transient = is_transient
  ))
  cnt <- resp_body_json(rsp)
  files_df <- lapply(cnt[["files"]], function(x) data.frame(filename = x[["key"]], url = x[["links"]][["self"]]))
  files_df <- do.call(rbind, files_df)
  files_df <- files_df[grep(layer, files_df[["filename"]]), ]
  files_df[grep(".zip$", files_df[["filename"]]), ]
}
