library(sf)
library(dplyr)
library(tidyr)
library(mapme.biodiversity)
library(rnaturalearth)

loc <- tempfile()
dir.create(loc)
bur <- ne_countries(country = "Burundi", returnclass = "sf")
mapme_options(outdir = loc)

get_resources(bur, get_ucdp_ged(version = "22.1"))
ged <- prep_resources(bur)[["ucdp_ged"]][[1]]
ged <- ged %>% filter(year %in% c(1991, 1992))
ged <- select(
  ged,
  starts_with("deaths_"),
  type_of_violence,
  date_prec,
  where_prec,
  date_start
)

dir.create("inst/res/ucdp_ged/", showWarnings = F)
st_write(ged, dsn = "inst/res/ucdp_ged/ged221-csv.gpkg")
st_write(select(bur, sov_a3), dsn = "inst/extdata/burundi.gpkg", delete_dsn = T)
