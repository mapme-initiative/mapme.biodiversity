# active fire count works

    Code
      .calc_active_fire_counts(shp, list(nasa_firms))
    Output
      # A tibble: 1 x 3
        instrument year  active_fire_counts
        <chr>      <chr>              <int>
      1 VIIRS      2021                  21

---

    Code
      .calc_active_fire_counts(shp, nasa_firms2)
    Output
      # A tibble: 2 x 3
        instrument year  active_fire_counts
        <chr>      <chr>              <int>
      1 MODIS      2021                  21
      2 VIIRS      2021                  21

