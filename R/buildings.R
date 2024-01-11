#' @keywords internal
#' @include register.R
#' @noRd
.get_buildings <- function(
    x,
    rundir = tempdir(),
    outdir = tempdir(),
    verbose = TRUE){

  world <- geodata::world(path = tempdir(), quiet = TRUE)
  world <- st_as_sf(world)
  world <- st_make_valid(world)

  codes <- unique(world$GID_0[unlist(st_intersects(x, world))])

  base <- "/vsicurl/https://data.source.coop/vida/google-microsoft-open-buildings/flageobuf/by_country/country_iso=%s/%s.fgb"
  urls <- sprintf(base, codes, codes)
  fps <- make_footprints(urls, what = "vector")
  fps[["filename"]] <- basename(urls)
  fps
}

register_resource(
  name = "buildings",
  type = "vector",
  source = "source.coop",
  fun = .get_buildings,
  arguments = list()
)

#' @keywords internal
#' @include register.R
#' @noRd
.calc_building_count <- function(
    x,
    buildings,
    verbose = TRUE,
    ...){

  buildings <- do.call(rbind, buildings)
  buildings <- buildings[unlist(st_contains(x, buildings)), ]
  if(nrow(buildings) == 0) return(tibble(count = 0, google = 0, microsoft = 0))
  counts <- table(buildings$bf_source)
  n <- nrow(buildings)
  tibble(count = n,
         google = ifelse("google" %in% names(counts), counts[["google"]], 0),
         microsoft = ifelse("microsoft" %in% names(counts), counts[["microsoft"]], 0)
         )
}

register_indicator(
  name = "building_count",
  resources = list(buildings = "vector"),
  fun = .calc_building_count,
  arguments = list(),
  processing_mode = "asset"
)

