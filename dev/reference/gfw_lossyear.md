# Year of forest loss occurrence

This resource is part of the publication by Hansen et al. (2013)
"High-Resolution Global Maps of 21st-Century Forest Cover Change". It
represents "Forest loss during the period 2000–2021, defined as a
stand-replacement disturbance, or a change from a forest to non-forest
state. Encoded as either 0 (no loss) or else a value in the range 1–20,
representing loss detected primarily in the year 2001–2021,
respectively." Due to changes in the satellites products used in the
compilation of the tree loss product, results before the year 2011 and
afterwards are not directly comparable until reprocessing has finished.
Users should be aware of this limitation, especially when the timeframe
of the analysis spans over the two periods delimited by the year 2011.

## Usage

``` r
get_gfw_lossyear(version = "GFC-2024-v1.12")
```

## Source

<https://data.globalforestwatch.org/documents/tree-cover-loss/explore>

## Arguments

- version:

  The version of the dataset to download. Defaults to "GFC-2024-v1.12".
  Check `mapme.biodiversity:::.available_gfw_versions()` to get a list
  of available versions

## Value

A function that returns an `sf` footprint object.

## References

Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A.
Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A.
Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend.
2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.”
Science 342 (15 November): 850–53.
