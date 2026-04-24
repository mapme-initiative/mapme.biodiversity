# Register or list resources in mapme.biodiversity

`register_resource()` is used to register a new resource function with
base information to the package's internal environment used to inform
users about available resources. Note, registering a custom resource
will only have effect for the current R session.

`available_resources()` returns a tibble of registered resources with
basic information such as the source and the licence.

## Usage

``` r
register_resource(
  name = NULL,
  description = NULL,
  licence = NULL,
  source = NULL,
  type = NULL
)

available_resources(resources = NULL)
```

## Arguments

- name:

  A character vector indicating the name of the resource.

- description:

  A character vector with a basic description

- licence:

  A character vector indicating the licence of the resource. In case it
  is a custom licence, put a link to the licence text.

- source:

  Optional, preferably a URL where the data is found.

- type:

  A character vector indicating the type of the resource. Either
  'vector' or 'raster'.

- resources:

  If `NULL` returns a list of all resources (default). Otherwise only
  the ones specified.

## Value

`register_resource()` is called for the side-effect of registering a
resource.

`available_resources()` returns a tibble listing available resources.

## Examples

``` r
# \dontrun{
register_resource(
  name = "gfw_treecover",
  description = "Global Forest Watch - Percentage of canopy closure in 2000",
  licence = "CC-BY 4.0",
  source = "https://data.globalforestwatch.org/documents/tree-cover-2000/explore",
  type = "raster"
)
# }
available_resources()
#> # A tibble: 36 × 5
#>    name                          description                licence source type 
#>    <chr>                         <chr>                      <chr>   <chr>  <chr>
#>  1 accessibility_2000            Accessibility data for th… See JR… https… rast…
#>  2 acled                         Armed Conflict Location &… Visit … Visit… vect…
#>  3 biodiversity_intactness_index Biodiversity Intactness I… CC-BY-… https… rast…
#>  4 chelsa                        Climatologies at High res… Unknow… https… rast…
#>  5 chirps                        Climate Hazards Group Inf… CC - u… https… rast…
#>  6 esalandcover                  Copernicus Land Monitorin… CC-BY … https… rast…
#>  7 fritz_et_al                   Drivers of deforestation … CC-BY … https… rast…
#>  8 gfw_emissions                 Global Forest Watch - CO2… CC-BY … https… rast…
#>  9 gfw_lossyear                  Global Forest Watch - Yea… CC-BY … https… rast…
#> 10 gfw_treecover                 Global Forest Watch - Per… CC-BY … https… rast…
#> # ℹ 26 more rows
```
