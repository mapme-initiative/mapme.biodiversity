# Drivers of deforestation for tropical forests

This resource is produced by a nearest-neighbour matching of a
crowd-sourced campaign to map dominant driver of forest loss based on
visual interpretation of VHR images matched with Global Forest Loss data
by Hansen (2013) version 1.7 The forest loss layer was re sampled to a
resolution of 100 and 1.000 meters. Dominant drivers were determined for
the period 2008 to 2009.

## Usage

``` r
get_fritz_et_al(resolution = 100)
```

## Source

<https://zenodo.org/record/7997885>

## Arguments

- resolution:

  An integer indicating the resolution to download. Defaults to 100.

## Value

A function that returns an `sf` footprint object.

## Details

It indicates 9 different classes:

- commercial agriculture

- commercial oil palm plantations

- managed forests

- mining

- natural disturbances

- pasture

- roads

- wildfire

- other subsistence agriculture

- shifting cultivation

## References

Steffen, F., Carlos, J.C.L., See. L., Schepaschenko D., Hofhansl F.,
Jung M., Dürauer M., Georgieva I., Danylo O., Lesiv M., McCallum I.
(2022) A Continental Assessment of the Drivers of Tropical Deforestation
With a Focus on Protected Areas. F.Cos.Sc.(3)
doi:10.3389/fcosc.2022.830248
