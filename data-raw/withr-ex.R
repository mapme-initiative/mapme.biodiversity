remotes::install_github("boettiger-lab/earthdatalogin")
library(terra)
library(sf)
file <- "vrt://NETCDF:/vsicurl/https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/MUR-JPL-L4-GLOB-v4.1/20200101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc:analysed_sst?a_srs=OGC:CRS84&a_ullr=-180,90,180,-90&projwin=-93,49,-76,41"
token <- earthdatalogin::edl_set_token(format = "header", set_env_var = F, prompt_for_netrc = FALSE) # dont set the token

# try reading directly
try(x <- rast(file)) # fails
gdal_config = c(GDAL_HTTP_HEADERS = token)
withr::with_envvar(gdal_config, x <- rast(file)) # succeeds

# test round-trip without specifying file extensions
tmp1 <- tempfile()
tmp2 <- tempfile(fileext = ".tif")
sf::gdal_utils("translate", file, tmp1)
sf::gdal_utils("translate", tmp1, tmp2)

x1 <- rast(tmp1)
x2 <- rast(tmp2)
r <- c(x, x1, x2)
plot(r)
names(r)
# we don't keep the names with the roundtrip
# but can read from the original file info
info <- gdal_utils("info", file, options = "-json") |> jsonlite::parse_json()
info$files[[1]] |> basename()



# let's try it with mapme
devtools::load_all()
token <- earthdatalogin::edl_set_token(format = "header", set_env_var = FALSE, prompt_for_netrc = FALSE)
nc_res <- function(x, rundir = tempdir(),
                   outdir = tempdir(),
                   verbose = TRUE) {
  file <- "vrt://NETCDF:/vsicurl/https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/MUR-JPL-L4-GLOB-v4.1/20200101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc:analysed_sst?a_srs=OGC:CRS84&a_ullr=-180,90,180,-90&projwin=-93,49,-76,41"
  fps <- make_footprints(file, what = "raster")
  fps[["destination"]] <- file.path(outdir, "20200101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")
  fps
}

register_resource(name = "nc-test", type = "raster", fun = nc_res)

outdir <- file.path(tempdir(), "mapme")
dir.create(outdir, showWarnings = FALSE)
aoi <- st_read("../portfolio_tc_donors.gpkg")[1, ]
aoi <- init_portfolio(aoi, years = 2010:2015, outdir = outdir)

aoi2 <- get_resources(aoi, "nc-test", download = TRUE, gdal_config = gdal_http_opts(GDAL_HTTP_HEADERS = token))
(f1 <- attributes(aoi2)$resources$`nc-test`$location)
attributes(attributes(aoi2)$resources$`nc-test`)$gdal_config

aoi3 <- get_resources(aoi, "nc-test", download = F, gdal_config = gdal_config)
(f2 <- attributes(aoi3)$resources$`nc-test`$location)
attributes(attributes(aoi3)$resources$`nc-test`)$gdal_config

r1 <- rast(f1)
r2 <- rast(f2)
plot(c(r1, r2))


