# terrain ruggedness index works

    Code
      .calc_tri(shp, srtmdem)
    Output
      # A tibble: 1 x 1
        tri_mean
           <dbl>
      1     25.7

---

    Code
      .calc_tri(shp, srtmdem, stats = c("mean", "median", "sd"))
    Output
      # A tibble: 1 x 3
        tri_mean tri_median tri_sd
           <dbl>      <dbl>  <dbl>
      1     25.7       23.4   14.7

---

    Code
      .calc_tri(shp, srtmdem, engine = "extract")
    Output
      # A tibble: 1 x 1
        tri_mean
           <dbl>
      1     25.9

---

    Code
      .calc_tri(shp, srtmdem, engine = "exactextract")
    Output
      # A tibble: 1 x 1
        tri_mean
           <dbl>
      1     25.9

