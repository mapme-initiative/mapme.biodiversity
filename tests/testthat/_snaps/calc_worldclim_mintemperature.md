# worldclim minimum temperature works

    Code
      .calc_worldclim_mintemperature(shp, mintemperature)
    Output
      # A tibble: 12 x 2
         tmin_mean date   
             <dbl> <chr>  
       1      9.96 2018-01
       2     10.2  2018-02
       3     10.1  2018-03
       4     11.0  2018-04
       5     12.6  2018-05
       6     14.0  2018-06
       7     13.0  2018-07
       8     14.0  2018-08
       9     13.6  2018-09
      10     13.2  2018-10
      11     12.5  2018-11
      12     10.3  2018-12

---

    Code
      .calc_worldclim_mintemperature(shp, mintemperature, stats_mintemperature = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 12 x 4
         tmin_mean tmin_median tmin_sd date   
             <dbl>       <dbl>   <dbl> <chr>  
       1      9.96        9.85    1.14 2018-01
       2     10.2        10.1     1.14 2018-02
       3     10.1        10.0     1.18 2018-03
       4     11.0        11.0     1.15 2018-04
       5     12.6        12.5     1.09 2018-05
       6     14.0        13.9     1.01 2018-06
       7     13.0        13.0     1.08 2018-07
       8     14.0        14.0     1.06 2018-08
       9     13.6        13.6     1.04 2018-09
      10     13.2        13.1     1.05 2018-10
      11     12.5        12.4     1.07 2018-11
      12     10.3        10.2     1.09 2018-12

---

    Code
      .calc_worldclim_mintemperature(shp, mintemperature, engine = "extract")
    Output
      # A tibble: 12 x 2
         tmin_mean date   
             <dbl> <chr>  
       1      9.96 2018-01
       2     10.2  2018-02
       3     10.1  2018-03
       4     11.0  2018-04
       5     12.6  2018-05
       6     14.0  2018-06
       7     13.0  2018-07
       8     14.0  2018-08
       9     13.6  2018-09
      10     13.2  2018-10
      11     12.5  2018-11
      12     10.3  2018-12

---

    Code
      .calc_worldclim_mintemperature(shp, mintemperature, engine = "exactextract")
    Output
      # A tibble: 12 x 2
         tmin_mean date   
             <dbl> <chr>  
       1      10.3 2018-01
       2      10.6 2018-02
       3      10.4 2018-03
       4      11.4 2018-04
       5      12.9 2018-05
       6      14.3 2018-06
       7      13.4 2018-07
       8      14.4 2018-08
       9      14.0 2018-09
      10      13.5 2018-10
      11      12.8 2018-11
      12      10.7 2018-12

---

    Code
      .calc_worldclim_mintemperature(shp, mintemperature, engine = "zonal")
    Output
      # A tibble: 12 x 2
         tmin_mean date   
             <dbl> <chr>  
       1      11.0 2018-01
       2      11.3 2018-02
       3      11.2 2018-03
       4      12.2 2018-04
       5      13.6 2018-05
       6      15.0 2018-06
       7      14.1 2018-07
       8      15.1 2018-08
       9      14.6 2018-09
      10      14.2 2018-10
      11      13.5 2018-11
      12      11.3 2018-12

