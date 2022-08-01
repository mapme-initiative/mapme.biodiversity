# terrain ruggedness index works

    Code
      .calc_tri(shp, nasa_srtm)
    Output
      # A tibble: 1 x 1
        tri_mean
           <dbl>
      1     25.7

---

    Code
      .calc_tri(shp, nasa_srtm, stats = c("mean", "median", "sd"))
    Output
      # A tibble: 1 x 3
        tri_mean tri_median tri_sd
           <dbl>      <dbl>  <dbl>
      1     25.7       23.4   14.7

---

    Code
      .calc_tri(shp, nasa_srtm, engine = "extract")
    Output
      # A tibble: 1 x 1
        tri_mean
           <dbl>
      1     25.9

---

    Code
      .calc_tri(shp, nasa_srtm, engine = "exactextract")
    Output
      # A tibble: 1 x 1
        tri_mean
           <dbl>
      1     25.9

