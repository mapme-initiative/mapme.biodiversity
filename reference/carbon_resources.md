# Carbon Layers

These resources are from the publication by Noon et al. (2022) "Mapping
the irrecoverable carbon in Earth’s ecosystems". This publication
differentiates between 3 different kinds of carbon with varying degrees
of manageability by humans. All three layers are available for above and
below ground carbon, as well as a layer combining the two.

## Usage

``` r
get_irr_carbon()

get_vul_carbon()

get_man_carbon()
```

## Source

<https://zenodo.org/records/4091029>

## Value

A function that returns an `sf` footprint object.

## Details

It may be required to increase the timeout option to successfully
download theses layers from their source location via e.g.
`options(timeout = 600)`.

Irrecoverable carbon is defined as the amount of carbon, that, if lost
today, cannot be recovered until mid 21st century (so within 30 years,
considering the publication date).

Vulnerable carbon is defined as the amount of carbon that would be lost
in a hypothetical but typical conversion event (without including
information of the probability of such an event to be actually
occurring).

Manageable carbon is defined as all land areas, expect cyrosols, because
carbon loss is driven by direct land-use conversion which could be
halted or because climate change impacts affecting the area can
potentially be directly mitigated through adaptive management.

## References

Noon, M.L., Goldstein, A., Ledezma, J.C. et al. Mapping the
irrecoverable carbon in Earth’s ecosystems. Nat Sustain 5, 37–46 (2022).
[doi:10.1038/s41893-021-00803-6](https://doi.org/10.1038/s41893-021-00803-6)
