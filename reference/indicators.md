# Register or list indicators in mapme.biodiversity

`register_indicator()` is used to register a new indicator function with
base information to the package's internal environment used to inform
users about available indicators. Note, registering a custom indicator
will only have effect for the current R session.

`available_indicators()` returns a tibble of registered indicators with
basic information such as the required resources.

## Usage

``` r
register_indicator(name = NULL, description = NULL, resources = NULL)

available_indicators(indicators = NULL)
```

## Arguments

- name:

  A character vector indicating the name of the indicator.

- description:

  A character vector with a basic description

- resources:

  A character vector of the required resources that need to be available
  to calculate the indicator. The names must correspond with already
  registered resources.

- indicators:

  If `NULL` returns a list of all registered indicators (default).
  Otherwise only the ones specified.

## Value

`register_indicator()` is called for the side-effect of registering an
indicator

[`available_resources()`](https://mapme-initiative.github.io/mapme.biodiversity/reference/resources.md)
returns a tibble listing available indicators.

## Examples

``` r
# \dontrun{
register_indicator(
  name = "treecover_area",
  description = "Area of forest cover by year",
  resources = c(
    "gfw_treecover",
    "gfw_lossyear"
  )
)
# }
available_indicators()
#> # A tibble: 41 × 3
#>    name                          description                           resources
#>    <chr>                         <chr>                                 <list>   
#>  1 biodiversity_intactness_index Averaged biodiversity intactness ind… <tibble> 
#>  2 biome                         Areal statistics of biomes from TEOW  <tibble> 
#>  3 burned_area                   Monthly burned area detected by MODI… <tibble> 
#>  4 deforestation_drivers         Areal statistics of deforestation dr… <tibble> 
#>  5 drought_indicator             Relative wetness statistics based on… <tibble> 
#>  6 ecoregion                     Areal statistics of ecoregions based… <tibble> 
#>  7 elevation                     Statistics of elevation based on NAS… <tibble> 
#>  8 exposed_population_acled      Number of people exposed to conflict… <tibble> 
#>  9 exposed_population_ucdp       Number of people exposed to conflict… <tibble> 
#> 10 fatalities_acled              Number of fatalities by event type b… <tibble> 
#> # ℹ 31 more rows
```
