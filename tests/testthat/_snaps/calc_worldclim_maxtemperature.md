# worldclim maximum temperature works

    Code
      .calc_worldclim_maxtemperature(shp, maxtemperature)
    Output
      # A tibble: 12 x 2
         tmax_mean date   
             <dbl> <chr>  
       1      20.8 2018-01
       2      20.5 2018-02
       3      22.1 2018-03
       4      22.5 2018-04
       5      21.4 2018-05
       6      22.7 2018-06
       7      23.1 2018-07
       8      23.8 2018-08
       9      23.1 2018-09
      10      22.4 2018-10
      11      22.1 2018-11
      12      21.3 2018-12

---

    Code
      .calc_worldclim_maxtemperature(shp, maxtemperature, stats_maxtemperature = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 12 x 4
         tmax_mean tmax_median tmax_sd date   
             <dbl>       <dbl>   <dbl> <chr>  
       1      20.8        20.6    1.33 2018-01
       2      20.5        20.3    1.30 2018-02
       3      22.1        22.0    1.25 2018-03
       4      22.5        22.4    1.28 2018-04
       5      21.4        21.3    1.29 2018-05
       6      22.7        22.7    1.29 2018-06
       7      23.1        23.1    1.29 2018-07
       8      23.8        23.7    1.28 2018-08
       9      23.1        23.0    1.27 2018-09
      10      22.4        22.3    1.27 2018-10
      11      22.1        21.9    1.31 2018-11
      12      21.3        21.2    1.28 2018-12

---

    Code
      .calc_worldclim_maxtemperature(shp, maxtemperature, engine = "extract")
    Output
      # A tibble: 12 x 2
         tmax_mean date   
             <dbl> <chr>  
       1      20.8 2018-01
       2      20.5 2018-02
       3      22.1 2018-03
       4      22.5 2018-04
       5      21.4 2018-05
       6      22.7 2018-06
       7      23.1 2018-07
       8      23.8 2018-08
       9      23.1 2018-09
      10      22.4 2018-10
      11      22.1 2018-11
      12      21.3 2018-12

---

    Code
      .calc_worldclim_maxtemperature(shp, maxtemperature, engine = "exactextract")
    Output
      # A tibble: 12 x 2
         tmax_mean date   
             <dbl> <chr>  
       1      21.1 2018-01
       2      20.9 2018-02
       3      22.5 2018-03
       4      22.9 2018-04
       5      21.8 2018-05
       6      23.2 2018-06
       7      23.6 2018-07
       8      24.2 2018-08
       9      23.5 2018-09
      10      22.8 2018-10
      11      22.5 2018-11
      12      21.7 2018-12

---

    Code
      .calc_worldclim_maxtemperature(shp, maxtemperature, engine = "zonal")
    Output
      # A tibble: 12 x 2
         tmax_mean date   
             <dbl> <chr>  
       1      21.9 2018-01
       2      21.7 2018-02
       3      23.4 2018-03
       4      23.8 2018-04
       5      22.7 2018-05
       6      24.1 2018-06
       7      24.5 2018-07
       8      25.1 2018-08
       9      24.3 2018-09
      10      23.6 2018-10
      11      23.3 2018-11
      12      22.5 2018-12

