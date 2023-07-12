library(aws.s3)
library(wdpar)
library(mapme.biodiversity)
library(sf)
library(tidyverse)
# Launch locally on Windows
# system2("trials\minio.exe server C:\minio --console-address :9090")
# Environment variables
Sys.setenv("AWS_ACCESS_KEY_ID" = "minioadmin",
           "AWS_SECRET_ACCESS_KEY" = "minioadmin",
           "AWS_S3_ENDPOINT"= "localhost:9000",
           "AWS_SESSION_TOKEN" = "",
           "AWS_HTTPS" = "FALSE",
           "AWS_VIRTUAL_HOSTING"="FALSE")

# Create bucket and load it with data
put_bucket("myuser", region = "", use_https = FALSE)

bucketlist(region = "", use_https = FALSE)

my_files <- list.files("trials/mapme_senegal", recursive = TRUE, full.name = TRUE)
my_dests <- my_files %>%
  str_replace("^trials/mapme_senegal/", "mapme/")

map2(my_files, my_dests, put_object, bucket = "myuser",
     region = "", use_https = FALSE)

# create an object of s3 bucket with aws.s3
my_s3_portfolio <- get_bucket("myuser", prefix = "mapme", region = "",
                           use_https = FALSE)

# Three functions that handle portfolios from s3
list.dirs <- function(x, ...) {
  if(class(x) == "s3_bucket") {
    unname(purrr::map_chr(x, "Key")) |>
      stringr::str_extract("/(.*)/") |>
      stringr::str_remove_all("/") |>
      unique()
  } else {
    base::list.dirs()
  }
}

list.files <- function(x, pattern = ".*", ...) {
  if (class(x) == "s3_bucket") {
    bucket <- stringr::str_remove(x[["Contents"]][["Bucket"]],
                                  "/mapme") |>
      paste0("/")
    out <- unname(purrr::map_chr(x, "Key")) |>
      stringr::str_subset(pattern)
    paste0("/vsis3/", bucket, out)
  } else {
    base::list.files(x, ...)
  }
}


file.path <- function(x, y, resource = NULL, ...) {
  if (class(x) == "s3_bucket" & length(x) > 0) {
    get_bucket(bucket = stringr::str_remove(x[["Contents"]][["Bucket"]],
                                            "/mapme"),
               prefix = paste0("mapme/", y),
               region = "",
               use_https = FALSE)
  } else if (class(x) == "s3_bucket" & length(x) == 0) {
    paste("/vsis3", attr(x, "Name"), attr(x, "Prefix"), y,
          sep = "/")
  } else {
    base::file.path()
  }
}

dir.exists <- function(paths) {
  if (class(paths) == "s3_bucket") TRUE
  else base::dir.exists(paths)
}

dir.create <- function(x, ...) {
  if (class(attr(aoi_pf, "outdir")) == "s3_bucket") {
    # nothing
  } else {
    dir.create(x, ...)
  }
}

# Load polygons
parc_saloum <- wdpa_read("trials/WDPA/WDPA_Jun2023_SEN-shapefile.zip") %>%
  filter(st_geometry_type(.) == "MULTIPOLYGON") %>%
  filter(NAME == "Delta du Saloum" & DESIG == "Parc National") %>%
  st_cast("POLYGON")

mapme_senegal <- init_portfolio(x = parc_saloum,
                                outdir = my_s3_portfolio,
                                years = 2000:2020,
                                add_resources = TRUE)

# This doesn't work: apparently, due to vsis3 not working
mapme_senegal <- calc_indicators(mapme_senegal, "mangroves_area")


mapme_senegal2 <- get_resources(mapme_senegal,
                                resources = "gfw_lossyear")


# Create an area of interest using the test data included in the
# mapme.biodiversity package

aoi <- system.file("extdata", "sierra_de_neiba_478140.gpkg",
                   package = "mapme.biodiversity") %>%
  read_sf() %>% st_cast("POLYGON")

# Create bucket in s3 for the portfolio data
put_bucket("myportfolio", region = "", use_https = FALSE)
#> [1] TRUE

# Check that the portfolio exists
bucketlist(region = "", use_https = FALSE)
#> Bucket             CreationDate
#> 1 myportfolio 2023-07-12T14:28:51.713Z


mypf <- get_bucket("myportfolio", prefix = "mapme", region = "",
                   use_https = FALSE)

aoi_pf <- init_portfolio(x = aoi,
                         outdir = mypf,
                         tmpdir = "trials",
                         years = 2000:2020,
                         add_resources = TRUE)

aoi_pf <- get_resources(aoi_pf, "gfw_lossyear")


my_textpf <- get_bucket_df("myportfolio",  region = "",
                              use_https = FALSE)

