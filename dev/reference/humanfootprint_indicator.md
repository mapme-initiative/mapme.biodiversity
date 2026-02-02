# Calculate human footprint statistics

Human footprint data measures the pressure imposed on the natural
environment by different dimensions of human actions. The theoretical
maximum value, representing the highest level of human pressure, is 50.
This routine allows to extract zonal statistics of the human footprint
data.

## Usage

``` r
calc_humanfootprint(engine = "extract", stats = "mean")
```

## Arguments

- engine:

  The preferred processing functions from either one of "zonal",
  "extract" or "exactextract" as character.

- stats:

  Function to be applied to compute statistics for polygons either one
  or multiple inputs as character. Supported statistics are: "mean",
  "median", "sd", "min", "max", "sum" "var".

## Value

A function that returns an indicator tibble the `humanfootprint` as
variable and the associated value (unitless) per year.

## Details

The required resources for this indicator are:

- [humanfootprint_resource](https://mapme-initiative.github.io/mapme.biodiversity/dev/reference/humanfootprint_resource.md)

## Examples

``` r
# \dontrun{
library(sf)
library(mapme.biodiversity)

outdir <- file.path(tempdir(), "mapme-data")
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = FALSE
)

aoi <- system.file("extdata", "shell_beach_protected_area_41057_B.gpkg",
  package = "mapme.biodiversity"
) %>%
  read_sf() %>%
  get_resources(get_humanfootprint(years = 2010)) %>%
  calc_indicators(calc_humanfootprint(stats = "median")) %>%
  portfolio_long()
#> Waiting 5s for retry backoff ■■■■■■■                         
#> Waiting 5s for retry backoff ■■■■■■■■■■■■                    
#> Waiting 5s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 14s for retry backoff ■■■■■■■                         
#> Waiting 14s for retry backoff ■■■■■■■■■■■■■■                  
#> Waiting 14s for retry backoff ■■■■■■■■■■■■■■■■■■■■■           
#> Waiting 14s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■     
#> Waiting 14s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Error in req_perform(req_retry(req, max_seconds = 15, is_transient = is_transient)) : 
#>   HTTP 403 Forbidden.
#> Error in .check_footprints(resource, resource_name): Download for resource humanfootprint failed.
#> Returning unmodified portfolio.

aoi
#> Error: object 'aoi' not found
# }
```
