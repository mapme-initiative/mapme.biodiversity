#' @keywords internal
#' @include register.R
#' @noRd
.get_cci <- function(x,
                     rundir = tempdir(),
                     verbose = TRUE) {


  target_years <- attributes(x)$years
  available_years <- c(1992:2015)
  target_years <- check_available_years(
    target_years, available_years, "esalandcover"
  )

  items <- try(rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
                 rstac::stac_search(
                   collection = "esa-cci-lc",
                   bbox = as.numeric(st_bbox(x)),
                   limit = NULL
                 ) %>%
                 rstac::post_request() %>%
                 rstac::items_fetch())

  fps <- rstac::items_bbox(items)
  fps <- purrr::map_dfr(fps, function(bbox){
    class(bbox) <- "bbox"
    bbox <- st_as_sf(st_as_sfc(bbox))
    st_crs(bbox) <- st_crs("EPSG:4326")
    st_geometry(bbox) <- "geometry"
    bbox
  })

  bare_urls <- rstac::assets_url(items, asset_names = "lccs_class")
  signed_urls <- items %>%
    rstac::items_sign(rstac::sign_planetary_computer()) %>%
    rstac::assets_url(asset_names = "lccs_class")

  fps[["source"]] <-  paste0("/vsicurl/", signed_urls)
  fps[["filename"]] <- basename(bare_urls)
  fps <- fps[grep("v2.0.7", fps[["source"]]), ]
  fps <- fps[grep(paste(target_years, collapse = "|"), fps[["source"]]), ]
  fps[ ,c("source", "filename")]

}

register_resource(
  name = "cci_lcc",
  type = "raster",
  source = "Microsoft Planetary Computer",
  fun = .get_cci,
  arguments = list()
)


#' @keywords internal
#' @include register.R
#' @noRd
.calc_cci <- function(x,
                      cci_lcc,
                      verbose = TRUE,
                      ...) {

  class_df <- NULL

  exactextractr::exact_extract(cci_lcc, x, function(data){

    coverage_area <- data[["coverage_area"]] / 10000
    total_area <- sum(coverage_area)
    data <- data[,-ncol(data)] |> as.data.frame()
    cci_names <- names(data)


    results <- purrr::imap_dfr(data, function(x, name, coverage, total, classes){
      year <- as.numeric(strsplit(name, "-")[[1]][8])

      stats <- by(coverage, x, sum)

      stats <- tibble::tibble(
        coverage = as.numeric(stats),
        value = as.numeric(dimnames(stats)[[1]]))

      stats <- merge(stats, .cci_classes, all = TRUE)
      stats[is.na(stats)] <- 0
      stats[["year"]] <- year
      stats[["percentage"]] <-  stats[["coverage"]] / total * 100
      stats[["total"]] <- total
      stats <- stats[stats[["class"]] != "other", ]
      stats[c("year", "class", "coverage", "percentage", "total")]
    }, coverage = coverage_area, total = total_area, classes = class_df)

    tibble::as_tibble(results)

  }, summarize_df = TRUE, coverage_area = TRUE)


}


.cci_classes <- data.frame(
  value = seq(0, 220, 10),
  class = c(
    "no data",
    "rainfed cropland",
    "irrigated cropland",
    "mosaic cropland (>50%) / natural vegetation (<50%)",
    "mosaic natural vegetation (>50%) / cropland (<50%)",
    "tree cover, broadleaved, evergreen, closed to open (>15%)",
    "tree cover, broadleaved, deciduous, closed to open (>15%)",
    "tree cover, needleleaved, evergreen, closed to open (>15)",
    "tree cover, needleleaved, deciduous, closed to open (>15%)",
    "tree cover, mixed leaf type (broadleaved and needleleaved)",
    "mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
    "mosaic herbaceous cover (>50%) / tree and shrub (<50%)",
    "shrubland",
    "grassland",
    "lichens and mosses",
    "sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
    "tree cover, flooded, fresh or brakish water",
    "tree cover, flooded, saline water",
    "shrub or herbaceous cover, flooded, fresh/saline/brakish water",
    "urban areas",
    "bare areas",
    "water bodies",
    "permanent snow and ice"
  )
)



register_indicator(
  name = "cci_landcover",
  resources = list(cci_lcc = "raster"),
  fun = .calc_cci,
  arguments = list(),
  processing_mode = "asset"
)

