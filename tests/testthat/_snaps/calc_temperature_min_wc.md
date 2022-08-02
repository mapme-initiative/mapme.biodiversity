# worldclim minimum temperature works

    Code
      .calc_temperature_min_wc(shp, worldclim_min_temperature)
    Output
      # A tibble: 12 x 2
         tmin_mean date      
             <dbl> <date>    
       1      9.96 2018-01-01
       2     10.2  2018-02-01
       3     10.1  2018-03-01
       4     11.0  2018-04-01
       5     12.6  2018-05-01
       6     14.0  2018-06-01
       7     13.0  2018-07-01
       8     14.0  2018-08-01
       9     13.6  2018-09-01
      10     13.2  2018-10-01
      11     12.5  2018-11-01
      12     10.3  2018-12-01

---

    Code
      .calc_temperature_min_wc(shp, worldclim_min_temperature, stats_worldclim = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 12 x 4
         tmin_mean tmin_median tmin_sd date      
             <dbl>       <dbl>   <dbl> <date>    
       1      9.96        9.85    1.14 2018-01-01
       2     10.2        10.1     1.14 2018-02-01
       3     10.1        10.0     1.18 2018-03-01
       4     11.0        11.0     1.15 2018-04-01
       5     12.6        12.5     1.09 2018-05-01
       6     14.0        13.9     1.01 2018-06-01
       7     13.0        13.0     1.08 2018-07-01
       8     14.0        14.0     1.06 2018-08-01
       9     13.6        13.6     1.04 2018-09-01
      10     13.2        13.1     1.05 2018-10-01
      11     12.5        12.4     1.07 2018-11-01
      12     10.3        10.2     1.09 2018-12-01

---

    Code
      .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "extract")
    Output
      # A tibble: 12 x 2
         tmin_mean date      
             <dbl> <date>    
       1      9.96 2018-01-01
       2     10.2  2018-02-01
       3     10.1  2018-03-01
       4     11.0  2018-04-01
       5     12.6  2018-05-01
       6     14.0  2018-06-01
       7     13.0  2018-07-01
       8     14.0  2018-08-01
       9     13.6  2018-09-01
      10     13.2  2018-10-01
      11     12.5  2018-11-01
      12     10.3  2018-12-01

---

    Code
      .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "exactextract")
    Output
      # A tibble: 12 x 2
         tmin_mean date      
             <dbl> <date>    
       1      10.3 2018-01-01
       2      10.6 2018-02-01
       3      10.4 2018-03-01
       4      11.4 2018-04-01
       5      12.9 2018-05-01
       6      14.3 2018-06-01
       7      13.4 2018-07-01
       8      14.4 2018-08-01
       9      14.0 2018-09-01
      10      13.5 2018-10-01
      11      12.8 2018-11-01
      12      10.7 2018-12-01

---

    Code
      .calc_temperature_min_wc(shp, worldclim_min_temperature, engine = "zonal")
    Output
      # A tibble: 12 x 2
         tmin_mean date      
             <dbl> <date>    
       1      11.0 2018-01-01
       2      11.3 2018-02-01
       3      11.2 2018-03-01
       4      12.2 2018-04-01
       5      13.6 2018-05-01
       6      15.0 2018-06-01
       7      14.1 2018-07-01
       8      15.1 2018-08-01
       9      14.6 2018-09-01
      10      14.2 2018-10-01
      11      13.5 2018-11-01
      12      11.3 2018-12-01

