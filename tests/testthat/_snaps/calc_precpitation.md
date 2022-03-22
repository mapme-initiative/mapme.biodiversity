# precipitation indicator works

    Code
      .calc_precipitation(shp, chirps, engine = "zonal", stats_precipitation = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 108 x 4
         year  month stat  precipitation
         <chr> <chr> <chr>         <dbl>
       1 1981  01    mean           21.3
       2 1981  02    mean           30.0
       3 1981  03    mean           37.0
       4 1981  04    mean           40.2
       5 1981  05    mean          301. 
       6 1981  06    mean          151. 
       7 1981  07    mean          119. 
       8 1981  08    mean          121. 
       9 1981  09    mean          144. 
      10 1981  10    mean          229. 
      # ... with 98 more rows

---

    Code
      .calc_precipitation(shp, chirps, engine = "extract", stats_precipitation = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 108 x 4
         year  month stat  precipitation
         <chr> <chr> <chr>         <dbl>
       1 1981  01    mean           20.4
       2 1981  02    mean           27.6
       3 1981  03    mean           35.5
       4 1981  04    mean           41.6
       5 1981  05    mean          298. 
       6 1981  06    mean          146. 
       7 1981  07    mean          122. 
       8 1981  08    mean          124. 
       9 1981  09    mean          146. 
      10 1981  10    mean          224. 
      # ... with 98 more rows

---

    Code
      .calc_precipitation(shp, chirps, engine = "exactextract", stats_precipitation = c(
        "mean", "median", "sd"))
    Output
      # A tibble: 108 x 4
         year  month stat  precipitation
         <chr> <chr> <chr>         <dbl>
       1 1981  01    mean           20.5
       2 1981  02    mean           28.4
       3 1981  03    mean           35.8
       4 1981  04    mean           41.6
       5 1981  05    mean          301. 
       6 1981  06    mean          150. 
       7 1981  07    mean          122. 
       8 1981  08    mean          123. 
       9 1981  09    mean          145. 
      10 1981  10    mean          229. 
      # ... with 98 more rows

