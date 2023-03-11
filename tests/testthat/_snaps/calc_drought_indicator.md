# drought indicator works

    Code
      .calc_drought_indicator(shp, nasa_grace)
    Output
      [[1]]
      # A tibble: 9 x 2
        wetness_mean date      
               <dbl> <date>    
      1         58.1 2022-01-03
      2         56.0 2022-01-10
      3         54.3 2022-01-17
      4         53.7 2022-01-24
      5         44.2 2022-01-31
      6         54.8 2022-02-07
      7         77.5 2022-02-14
      8         77.7 2022-02-21
      9         77.2 2022-02-28
      

---

    Code
      .calc_drought_indicator(shp, nasa_grace, engine = "zonal")
    Output
      [[1]]
      # A tibble: 9 x 2
        wetness_mean date      
               <dbl> <date>    
      1         58.1 2022-01-03
      2         56.0 2022-01-10
      3         54.3 2022-01-17
      4         53.7 2022-01-24
      5         44.2 2022-01-31
      6         54.8 2022-02-07
      7         77.5 2022-02-14
      8         77.7 2022-02-21
      9         77.2 2022-02-28
      

---

    Code
      .calc_drought_indicator(shp, nasa_grace, stats = c("mean", "median", "sd"))
    Output
      [[1]]
      # A tibble: 9 x 4
        wetness_mean wetness_median wetness_sd date      
               <dbl>          <dbl>      <dbl> <date>    
      1         58.1           58.1       2.46 2022-01-03
      2         56.0           56.0       3.01 2022-01-10
      3         54.3           54.3       4.32 2022-01-17
      4         53.7           53.7       3.99 2022-01-24
      5         44.2           44.2       4.78 2022-01-31
      6         54.8           54.8       1.32 2022-02-07
      7         77.5           77.5       1.29 2022-02-14
      8         77.7           77.7       1.06 2022-02-21
      9         77.2           77.2       1.78 2022-02-28
      

---

    Code
      .calc_drought_indicator(shp, nasa_grace, engine = "extract")
    Output
      [[1]]
      # A tibble: 9 x 2
        wetness_mean date      
               <dbl> <date>    
      1         58.1 2022-01-03
      2         56.0 2022-01-10
      3         54.3 2022-01-17
      4         53.7 2022-01-24
      5         44.2 2022-01-31
      6         54.8 2022-02-07
      7         77.5 2022-02-14
      8         77.7 2022-02-21
      9         77.2 2022-02-28
      

---

    Code
      .calc_drought_indicator(shp, nasa_grace, engine = "exactextract")
    Output
      [[1]]
      # A tibble: 9 x 2
        wetness_mean date      
               <dbl> <date>    
      1         57.3 2022-01-03
      2         55.1 2022-01-10
      3         52.9 2022-01-17
      4         52.5 2022-01-24
      5         42.7 2022-01-31
      6         54.4 2022-02-07
      7         77.1 2022-02-14
      8         77.4 2022-02-21
      9         76.6 2022-02-28
      

