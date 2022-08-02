# traveltime works

    Code
      .calc_traveltime(shp, nelson_et_al)
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
      .calc_traveltime(shp, nelson_et_al, engine = "zonal")
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
      .calc_traveltime(shp, nelson_et_al, stats = c("mean", "median", "sd", "min",
        "max", "sum", "var"))
    Output
      # A tibble: 11 x 8
         minutes_mean minutes_median minutes~1 minut~2 minut~3 minut~4 minut~5 dista~6
                <dbl>          <dbl>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>  
       1         317.            310      71.6     162     528   71422   5129. 100k_2~
       2         145.            139      60.3      35     350   32643   3640. 10k_20k
       3         254.            246      69.9     106     464   57198   4891. 1mio_5~
       4         318.            308      66.9     210     523   71657   4477. 200k_5~
       5         155.            151      64.5      47     360   34830   4158. 20k_11~
       6         156.            151      65.1      47     360   35190   4238. 20k_50k
       7         348.            338      66.9     239     553   78383   4474. 500k_1~
       8         183.            176      63.4      82     395   41228   4024. 50k_10~
       9         183.            176      63.5      82     395   41210   4035. 50k_50~
      10         137.            129      67.0      35     348   30872   4486. 5k_10k 
      11         131.            125      62.4      35     348   29544   3891. 5k_110~
      # ... with abbreviated variable names 1: minutes_sd, 2: minutes_min,
      #   3: minutes_max, 4: minutes_sum, 5: minutes_var, 6: distance

---

    Code
      .calc_traveltime(shp, nelson_et_al, engine = "extract")
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
      .calc_traveltime(shp, nelson_et_al, engine = "exactextract")
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

