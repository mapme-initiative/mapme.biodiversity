devtools::load_all()

aoi <- st_read(system.file("extdata/burundi.gpkg", package = "mapme.biodiversity"))
aoi <- init_portfolio(aoi, 2000:2020, outdir = NULL)
get_res <- function(obj) purrr::list_rbind(attributes(obj)$resources) |> st_as_sf()

gfw <- get_resources(aoi, c("gfw_lossyear", "gfw_treecover"))
get_res(gfw)

wp <- get_resources(aoi, "worldpop")
get_res(wp)

wc <- get_resources(aoi, c("worldclim_max_temperature", "worldclim_min_temperature", "worldclim_precipitation"))
get_res(wc)

ged <- get_resources(aoi, "ucdp_ged")
get_res(ged)

teow <- get_resources(aoi, "teow")
get_res(teow)

sg <- get_resources(aoi, "soilgrids", layers = c("clay", "sand", "silt"), stats= "mean", depth = "0-5cm")
get_res(sg)

tt <- get_resources(aoi, "nelson_et_al")
get_res(tt)

dem <- get_resources(aoi, "nasa_srtm")
get_res(dem)

grace <- get_resources(aoi, "nasa_grace")
get_res(grace)

gmw <- get_resources(aoi, "gmw")
get_res(gmw)

fritz <- get_resources(aoi, "fritz_et_al")
get_res(fritz)

esa <- get_resources(aoi, "esalandcover")
get_res(esa)

chirps <- get_resources(aoi, "chirps")
get_res(chirps)
