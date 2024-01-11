devtools::load_all()
library(terra)
library(sf)
library(withr)

x <- st_read(system.file("extdata/sierra_de_neiba_478140.gpkg", package = "mapme.biodiversity"))
# Azure Blob Storage
# CPL_CURL_VERBOSE="YES"
# we are going to use the connection string env var with defaults set by the
# azurite docker image
account <- "devstoreaccount1"
key <- "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="
endpoint <- "http://127.0.0.1:10000/"

# dont set env vars globally, rather we run functions unsing withr
gdal_config_az <- gdal_az_opts(c(
  gdal_cloud_opts(),
  AZURE_STORAGE_CONNECTION_STRING = sprintf("DefaultEndpointsProtocol=http;AccountName=%s;AccountKey=%s;BlobEndpoint=%s%s;", account, key, endpoint, account)
))

# resource function mock-up, returns a footprint with source and destination
gfw <- function(dst) {
  src <- "/vsicurl/https://storage.googleapis.com/earthenginepartners-hansen/GFC-2022-v1.10/Hansen_GFC-2022-v1.10_treecover2000_20N_080W.tif"
  fp <- make_footprints(src, what = "raster")
  fp["destination"] <- dst
  fp[ ,c("source", "destination")]
}

# now we call the function, giving us the footprint
fp <- .call_resource_fun(gfw, list(dst = "/vsiaz/mapme/gfw/Hansen_GFC-2022-v1.10_treecover2000_20N_080W.tif"), name = "gfw")
# we fire gdalwarp here for raster
.get_spds(fp$source, fp$destination, "raster", gdal_config_az)
# for .read_raster to work we need a location column
# this is now the location in the bucket
fp["location"] <- fp[["destination"]]

# without the correct env vars set, reading fails
try(r <- .read_raster(x, fp))
# calling this with the right env vars succeeds
with_envvar(gdal_config_az, r <- .read_raster(x, fp))
r

# now we do the same with a vector resource, this is a mock-up of the TEOW
teow <- function(dst) {
  src <- "/vsizip//vsicurl/https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip/official/wwf_terr_ecos.shp"
  fp <- make_footprints(src, what = "vector")
  fp["destination"] <-  dst
  fp[ ,c("source", "destination")]
}

# same as above, but it will fire ogr2ogr
fp <- .call_resource_fun(teow, list(dst = "/vsiaz/mapme/teow/wwf_terr_ecos.gpkg"), name = "teow")
.get_spds(fp$source, fp$destination, "vector", gdal_config_az)
fp["location"] <- fp[["destination"]]

# reading fails without the env vars set
try(v <- .read_vector(x, fp))
with_envvar(gdal_config_az, v <- .read_vector(x, fp))
v


# the same examples but using the minio S3 storage
# CPL_CURL_VERBOSE="YES"
gdal_config_s3 <- gdal_s3_opts(c(
  gdal_cloud_opts(),
  AWS_ACCESS_KEY_ID = "miniouser",
  AWS_SECRET_ACCESS_KEY = "miniopass",
  AWS_S3_ENDPOINT = "127.0.0.1:9000",
  AWS_VIRTUAL_HOSTING = "FALSE",
  AWS_HTTPS = "FALSE"))

# fetching a raster resource, note we changed the driver to /vsis3/
fp <- .call_resource_fun(gfw, list(dst = "/vsis3/mapme/gfw/Hansen_GFC-2022-v1.10_treecover2000_20N_080W.tif"), name = "gfw")
.get_spds(fp$source, fp$destination, "raster", gdal_config_s3)
fp["location"] <- fp[["destination"]]

try(r <- .read_raster(x, fp))
with_envvar(gdal_config_s3, r <- .read_raster(x, fp))
r

# fetching a vector resource, note we changed the driver to /vsis3/
fp <- .call_resource_fun(teow, list(dst = "/vsis3/mapme/teow/wwf_terr_ecos.gpkg"), name = "teow")
.get_spds(fp$source, fp$destination, "vector", gdal_config_s3)
fp["location"] <- fp[["destination"]]

try(v <- .read_vector(x, fp))
with_envvar(gdal_config_s3, v <- .read_vector(x, fp))
v


#.get_spds also allows to translate between two buckets, both of which
# need identification
# internally it is doing a roundtrip with the source being written to disk,
# then it is written to the destination, each requiring the correct env vars
# to be set to access the buckets

# from azure to minio
fp$source <- "/vsiaz/mapme/gfw/Hansen_GFC-2022-v1.10_treecover2000_20N_080W.tif"
fp$destination <- "/vsis3/mapme/gfw2/fromazure.tif"
.get_spds(fp$source, fp$destination, "raster", gdal_config_s3, gdal_config_az)
fp["location"] <- fp[["destination"]]
with_envvar(gdal_config_s3, r <- .read_raster(x, fp))
r

# from minio to azure
fp$source <- "/vsis3/mapme/gfw/Hansen_GFC-2022-v1.10_treecover2000_20N_080W.tif"
fp$destination <- "/vsiaz/mapme/gfw2/fromminio.tif"
.get_spds(fp$source, fp$destination, "raster", gdal_config_az, gdal_config_s3)
fp["location"] <- fp[["destination"]]
with_envvar(gdal_config_az, r <- .read_raster(x, fp))
v

# .get_spds returns TRUE/FALSE if the translation was successful/unsuccessful
# Here we test each configuration of potential errors

# from one bucket to disk: source does not exist
src <- "/vsis3/mapme/teow/wwf_terr_ecos2.gpkg"
dest <- tempfile(fileext = ".tif")
.get_spds(src, dest, "raster", gdal_config_s3)

# from disk to a bucket: wrong configuration for destination
# note, the file will first be downloaded before GDAL tries to write
# to the destination, we should include a check if we can write there
src <- system.file("tif/geomatrix.tif", package = "sf")
dest <- "/vsis3/mapme/fails.tif"
gdal_config_s3["AWS_ACCESS_KEY_ID"] <- "nouser"
.get_spds(src, dest, "raster", gdal_config_s3)

# between to buckets: source does not exist
src <- "/vsis3/mapme/teow/wwf_terr_ecos2.gpkg"
dest <- "/vsiaz/mapme/teow/wwf_terr_ecos2.gpkg"
gdal_config_s3["AWS_ACCESS_KEY_ID"] <- "miniouser"
.get_spds(src, dest, "raster", gdal_config_az, gdal_config_s3)
# betweem to buckets: destination bucket doe not exist
src <- "/vsis3/mapme/teow/wwf_terr_ecos.gpkg"
dest <- "/vsiaz/mapme2/teow/wwf_terr_ecos2.gpkg"
.get_spds(src, dest, "raster", gdal_config_az, gdal_config_s3)
