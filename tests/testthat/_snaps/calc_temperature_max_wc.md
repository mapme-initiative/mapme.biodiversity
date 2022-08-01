# worldclim maximum temperature works

    Code
      .calc_temperature_max_wc(shp, worldclim_max_temperature)
    Output
      # A tibble: 12 x 2
         tmax_mean date      
             <dbl> <date>    
       1      20.8 2018-01-01
       2      20.5 2018-02-01
       3      22.1 2018-03-01
       4      22.5 2018-04-01
       5      21.4 2018-05-01
       6      22.7 2018-06-01
       7      23.1 2018-07-01
       8      23.8 2018-08-01
       9      23.1 2018-09-01
      10      22.4 2018-10-01
      11      22.1 2018-11-01
      12      21.3 2018-12-01

---

    Code
      .calc_temperature_max_wc(shp, worldclim_max_temperature, stats_worldclim = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 12 x 4
         tmax_mean tmax_median tmax_sd date      
             <dbl>       <dbl>   <dbl> <date>    
       1      20.8        20.6    1.33 2018-01-01
       2      20.5        20.3    1.30 2018-02-01
       3      22.1        22.0    1.25 2018-03-01
       4      22.5        22.4    1.28 2018-04-01
       5      21.4        21.3    1.29 2018-05-01
       6      22.7        22.7    1.29 2018-06-01
       7      23.1        23.1    1.29 2018-07-01
       8      23.8        23.7    1.28 2018-08-01
       9      23.1        23.0    1.27 2018-09-01
      10      22.4        22.3    1.27 2018-10-01
      11      22.1        21.9    1.31 2018-11-01
      12      21.3        21.2    1.28 2018-12-01

---

    Code
      .calc_temperature_max_wc(shp, worldclim_max_temperature, engine = "extract")
    Output
      # A tibble: 12 x 2
         tmax_mean date      
             <dbl> <date>    
       1      20.8 2018-01-01
       2      20.5 2018-02-01
       3      22.1 2018-03-01
       4      22.5 2018-04-01
       5      21.4 2018-05-01
       6      22.7 2018-06-01
       7      23.1 2018-07-01
       8      23.8 2018-08-01
       9      23.1 2018-09-01
      10      22.4 2018-10-01
      11      22.1 2018-11-01
      12      21.3 2018-12-01

---

    Code
      .calc_temperature_max_wc(shp, worldclim_max_temperature, engine = "exactextract")
    Output
      # A tibble: 12 x 2
         tmax_mean date      
             <dbl> <date>    
       1      21.1 2018-01-01
       2      20.9 2018-02-01
       3      22.5 2018-03-01
       4      22.9 2018-04-01
       5      21.8 2018-05-01
       6      23.2 2018-06-01
       7      23.6 2018-07-01
       8      24.2 2018-08-01
       9      23.5 2018-09-01
      10      22.8 2018-10-01
      11      22.5 2018-11-01
      12      21.7 2018-12-01

---

    Code
      .calc_temperature_max_wc(shp, worldclim_max_temperature, engine = "zonal")
    Output
      # A tibble: 12 x 2
         tmax_mean date      
             <dbl> <date>    
       1      21.9 2018-01-01
       2      21.7 2018-02-01
       3      23.4 2018-03-01
       4      23.8 2018-04-01
       5      22.7 2018-05-01
       6      24.1 2018-06-01
       7      24.5 2018-07-01
       8      25.1 2018-08-01
       9      24.3 2018-09-01
      10      23.6 2018-10-01
      11      23.3 2018-11-01
      12      22.5 2018-12-01

