# worldclim precipitation works

    Code
      .calc_precipitation_wc(shp, worldclim_precipitation)
    Output
      # A tibble: 12 x 2
         prec_mean date      
             <dbl> <date>    
       1      26.9 2018-01-01
       2      26.3 2018-02-01
       3      66.8 2018-03-01
       4      81.8 2018-04-01
       5     327.  2018-05-01
       6      76.0 2018-06-01
       7      98.9 2018-07-01
       8     106.  2018-08-01
       9     214.  2018-09-01
      10     165.  2018-10-01
      11      60.7 2018-11-01
      12      24.9 2018-12-01

---

    Code
      .calc_precipitation_wc(shp, worldclim_precipitation, stats_worldclim = c("mean",
        "median", "sd"))
    Output
      # A tibble: 12 x 4
         prec_mean prec_median prec_sd date      
             <dbl>       <dbl>   <dbl> <date>    
       1      26.9        26.8    4.36 2018-01-01
       2      26.3        26.6    2.91 2018-02-01
       3      66.8        68.8    6.83 2018-03-01
       4      81.8        81.9    2.94 2018-04-01
       5     327.        336.    32.7  2018-05-01
       6      76.0        77.5    5.19 2018-06-01
       7      98.9        98.5    4.57 2018-07-01
       8     106.        108.     5.86 2018-08-01
       9     214.        217.    16.1  2018-09-01
      10     165.        174.    22.7  2018-10-01
      11      60.7        61.9    3.24 2018-11-01
      12      24.9        26.5    2.90 2018-12-01

---

    Code
      .calc_precipitation_wc(shp, worldclim_precipitation, engine = "extract")
    Output
      # A tibble: 12 x 2
         prec_mean date      
             <dbl> <date>    
       1      26.9 2018-01-01
       2      26.3 2018-02-01
       3      66.8 2018-03-01
       4      81.8 2018-04-01
       5     327.  2018-05-01
       6      76.0 2018-06-01
       7      98.9 2018-07-01
       8     106.  2018-08-01
       9     214.  2018-09-01
      10     165.  2018-10-01
      11      60.7 2018-11-01
      12      24.9 2018-12-01

---

    Code
      .calc_precipitation_wc(shp, worldclim_precipitation, engine = "exactextract")
    Output
      # A tibble: 12 x 2
         prec_mean date      
             <dbl> <date>    
       1      26.6 2018-01-01
       2      26.1 2018-02-01
       3      66.8 2018-03-01
       4      82.4 2018-04-01
       5     327.  2018-05-01
       6      75.0 2018-06-01
       7      97.8 2018-07-01
       8     106.  2018-08-01
       9     215.  2018-09-01
      10     162.  2018-10-01
      11      60.6 2018-11-01
      12      24.9 2018-12-01

---

    Code
      .calc_precipitation_wc(shp, worldclim_precipitation, engine = "zonal")
    Output
      # A tibble: 12 x 2
         prec_mean date      
             <dbl> <date>    
       1      24.8 2018-01-01
       2      24.7 2018-02-01
       3      64.7 2018-03-01
       4      83.5 2018-04-01
       5     321.  2018-05-01
       6      72.4 2018-06-01
       7      95.3 2018-07-01
       8     104.  2018-08-01
       9     214.  2018-09-01
      10     152.  2018-10-01
      11      59.2 2018-11-01
      12      24.3 2018-12-01

