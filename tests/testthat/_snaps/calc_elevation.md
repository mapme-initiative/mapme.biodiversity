# srtm elevation works

    Code
      .calc_elevation(shp, nasa_srtm)
    Output
      # A tibble: 1 x 1
        elevation_mean
                 <dbl>
      1          1707.

---

    Code
      .calc_elevation(shp, nasa_srtm, stats = c("mean", "median", "sd"))
    Output
      # A tibble: 1 x 3
        elevation_mean elevation_median elevation_sd
                 <dbl>            <dbl>        <dbl>
      1          1707.             1705         222.

---

    Code
      .calc_elevation(shp, nasa_srtm, engine = "extract")
    Output
      # A tibble: 1 x 1
        elevation_mean
                 <dbl>
      1          1707.

---

    Code
      .calc_elevation(shp, nasa_srtm, engine = "exactextract")
    Output
      # A tibble: 1 x 1
        elevation_mean
                 <dbl>
      1          1707.

