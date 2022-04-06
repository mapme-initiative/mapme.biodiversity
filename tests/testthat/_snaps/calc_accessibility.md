# accessibility works

    Code
      .calc_accessibility(shp, traveltime)
    Output
      # A tibble: 11 x 2
         minutes_mean distance  
                <dbl> <chr>     
       1         317. 100k_200k 
       2         145. 10k_20k   
       3         254. 1mio_5mio 
       4         318. 200k_500k 
       5         155. 20k_110mio
       6         156. 20k_50k   
       7         348. 500k_1mio 
       8         183. 50k_100k  
       9         183. 50k_50mio 
      10         137. 5k_10k    
      11         131. 5k_110mio 

---

    Code
      .calc_accessibility(shp, traveltime, engine = "zonal")
    Output
      # A tibble: 11 x 2
         minutes_mean distance  
                <dbl> <chr>     
       1         305. 100k_200k 
       2         130. 10k_20k   
       3         241. 1mio_5mio 
       4         306. 200k_500k 
       5         142. 20k_110mio
       6         144. 20k_50k   
       7         336. 500k_1mio 
       8         170. 50k_100k  
       9         170. 50k_50mio 
      10         125. 5k_10k    
      11         118. 5k_110mio 

---

    Code
      .calc_accessibility(shp, traveltime, stats = c("mean", "median", "sd", "min",
        "max", "sum", "var"))
    Output
      # A tibble: 11 x 8
         minutes_mean minutes_median minutes_sd minutes_min minutes_max minutes_sum
                <dbl>          <dbl>      <dbl>       <dbl>       <dbl>       <dbl>
       1         317.            310       71.6         162         528       71422
       2         145.            139       60.3          35         350       32643
       3         254.            246       69.9         106         464       57198
       4         318.            308       66.9         210         523       71657
       5         155.            151       64.5          47         360       34830
       6         156.            151       65.1          47         360       35190
       7         348.            338       66.9         239         553       78383
       8         183.            176       63.4          82         395       41228
       9         183.            176       63.5          82         395       41210
      10         137.            129       67.0          35         348       30872
      11         131.            125       62.4          35         348       29544
      # ... with 2 more variables: minutes_var <dbl>, distance <chr>

---

    Code
      .calc_accessibility(shp, traveltime, engine = "extract")
    Output
      # A tibble: 11 x 2
         minutes_mean distance  
                <dbl> <chr>     
       1         317. 100k_200k 
       2         145. 10k_20k   
       3         254. 1mio_5mio 
       4         318. 200k_500k 
       5         155. 20k_110mio
       6         156. 20k_50k   
       7         348. 500k_1mio 
       8         183. 50k_100k  
       9         183. 50k_50mio 
      10         137. 5k_10k    
      11         131. 5k_110mio 

---

    Code
      .calc_accessibility(shp, traveltime, engine = "exactextract")
    Output
      # A tibble: 11 x 2
         minutes_mean distance  
                <dbl> <chr>     
       1         315. 100k_200k 
       2         142. 10k_20k   
       3         252. 1mio_5mio 
       4         316. 200k_500k 
       5         152. 20k_110mio
       6         154. 20k_50k   
       7         346. 500k_1mio 
       8         181. 50k_100k  
       9         181. 50k_50mio 
      10         135. 5k_10k    
      11         129. 5k_110mio 

