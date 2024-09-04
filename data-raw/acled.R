library(sf)
library(dplyr)
library(tidyr)
library(mapme.biodiversity)
library(rnaturalearth)

bur <- ne_countries(country = "Burundi", returnclass = "sf")
dir.create("inst/res/acled", recursive = TRUE, showWarnings = FALSE)

set.seed(1231)
n <- 25
acled <- st_as_sf(st_sample(bur, n))
acled$description <- "This is simulated data and does not represent real events."
acled$year <- 2000
index <- round(sample(1:365, n, replace = TRUE))
acled$event_date <- seq(as.Date("2000-01-01"),
  as.Date("2000-12-31"),
  by = "1 day"
)[index]
acled$time_precision <- sample(1:3, n, replace = TRUE)
acled$geo_precision <- sample(1:3, n, replace = TRUE)
disorders <- c("Demonstrations", "Political violence", "Strategic developments")
acled$disorder_type <- sample(disorders, n, replace = TRUE)
events <- c(
  "Battles", "Explosions/Remote violence", "Protests", "Riots",
  "Strategic developments", "Violence against civilians"
)
acled$event_type <- sample(events, n, replace = TRUE)
sub_events <- c(
  "Government regains territory", "Non-state actor overtakes territory",
  "Armed clash", "Excessive force against protesters", "Protest with intervention",
  "Peaceful protest", "Violent demonstration", "Mob violence",
  "Chemical weapon", "Air/drone strike", "Suicide bomb",
  "Shelling/artillery/missile attack", "Remote explosive/landmine/IED",
  "Grenade", "Sexual violence", "Attack", "Abduction/forced disappearance",
  "Agreement", "Arrests", "Change to group/activity", "Disrupted weapons use",
  "Headquarters or base established", "Looting/property destruction",
  "Non-violent transfer of territory", "Other"
)
acled$sub_event_type <- sample(sub_events, n, replace = TRUE)
acled$fatalities <- round(runif(n, 1, 50))
acled <- .geom_last(acled)
st_write(acled, "inst/res/acled/acled_events_2000.gpkg", delete_dsn = TRUE)
