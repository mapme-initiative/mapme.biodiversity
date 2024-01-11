library(sf)
library(dplyr)
library(tidyr)
library(mapme.biodiversity)
library(rnaturalearth)

loc <- tempfile()
dir.create(loc)
bur <- ne_countries(country = "Burundi", returnclass = "sf")
attr(bur, "testing") <- FALSE
ged <- mapme.biodiversity:::.get_ucdp_ged(bur, version_ged = "22.1", rundir = loc)
ged <- st_read(ged, wkt_filter = st_as_text(st_as_sfc(st_bbox(bur))))
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
st_write(ged, dsn = "inst/res/ucdp_ged/ged221-csv.gpkg", delete_dsn = T)
st_write(select(bur, sov_a3), dsn = "inst/extdata/burundi.gpkg", delete_dsn = T)
