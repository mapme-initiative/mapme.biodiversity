# srtm dem works

    Code
      .calc_dem(shp, srtmdem)
    Output
      # A tibble: 1 x 1
        elevation_mean
                 <dbl>
      1          1707.

---

    Code
      .calc_dem(shp, srtmdem, stats = c("mean", "median", "sd"))
    Output
      # A tibble: 1 x 3
        elevation_mean elevation_median elevation_sd
                 <dbl>            <dbl>        <dbl>
      1          1707.             1705         222.

---

    Code
      .calc_dem(shp, srtmdem, engine = "extract")
    Output
      # A tibble: 1 x 1
        elevation_mean
                 <dbl>
      1          1707.

---

    Code
      .calc_dem(shp, srtmdem, engine = "exactextract")
    Output
      # A tibble: 1 x 1
        elevation_mean
                 <dbl>
      1          1707.

