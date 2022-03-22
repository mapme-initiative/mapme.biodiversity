# precipitation indicator works

    Code
      .calc_avgperc(shp, NULL)
    Output
      # A tibble: 1 x 12
        Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec  
        <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl>
      1 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   

---

    Code
      .calc_avgperc(shp, chirps, engine = "zonal")
    Output
      # A tibble: 1 x 12
          Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  12.2  21.5  42.8  101.  192.  112.  73.2  102.  146.  141.  85.6  15.0

---

    Code
      .calc_avgperc(shp, chirps, engine = "extract")
    Output
      # A tibble: 1 x 12
          Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  10.5  20.2  41.6  102.  196.  109.  76.5  107.  151.  143.  83.8  12.2

---

    Code
      .calc_avgperc(shp, chirps, engine = "exactextract")
    Output
      # A tibble: 1 x 12
          Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  10.9  20.5  41.8  102.  195.  112.  75.7  106.  150.  144.  86.4  13.3

---

    Code
      .calc_avgperc(shp, chirps, engine = "zonal")
    Output
      # A tibble: 1 x 12
        Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec  
        <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl>
      1 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   

