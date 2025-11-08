# Armed Conflict Location & Event Data (ACLED)

From ACLED's homepage: *The Armed Conflict Location & Event Data Project
(ACLED) is a disaggregated data collection, analysis, and crisis mapping
project. ACLED collects information on the dates, actors, locations,
fatalities, and types of all reported political violence and protest
events around the world. The ACLED team conducts analysis to describe,
explore, and test conflict scenarios, and makes both data and analysis
open for free use by the public.*

## Usage

``` r
get_acled(
  years = 1997,
  email = Sys.getenv("ACLED_EMAIL"),
  password = Sys.getenv("ACLED_PASSWORD"),
  accept_terms = FALSE
)
```

## Source

Armed Conflict Location & Event Data Project (ACLED).

## Arguments

- years:

  A numeric vector specifying the years for which to make ACLED data
  available (between 1997 and today).

- email:

  Email addressed used to register with ACLED (see Details).

- password:

  Password used to register with ACLED (see Details).

- accept_terms:

  A logical indicating if you agree to abide by ACLED's terms of use.
  Defaults to FALSE, thus must be manually set to TRUE.

## Value

A function that returns an `sf` footprint object.

## Details

In order to access data from the ACLED API, you first must register an
account. Make sure you register with your institutional domain (e.g.,
organization, university, or company) email address rather than your
personal email address, in order to be able to use the API, as explained
[here](https://acleddata.com/myacled-faqs). Note that the ACLED API
provides a *living database* with single events being altered or removed
altogether over time.

## References

Raleigh, C., Kishi, R. & Linke, A. Political instability patterns are
obscured by conflict dataset scope conditions, sources, and coding
choices. Humanit Soc Sci Commun 10, 74 (2023).
[doi:10.1057/s41599-023-01559-4](https://doi.org/10.1057/s41599-023-01559-4)
