# .calc_population_count works

    Code
      .calc_population_count(shp, worldpop)
    Output
      # A tibble: 21 x 2
         popcount_sum year 
                <dbl> <chr>
       1        3941. 2000 
       2        3786. 2001 
       3        4012. 2002 
       4        4083. 2003 
       5        4336. 2004 
       6        4323. 2005 
       7        4569. 2006 
       8        4339. 2007 
       9        4784. 2008 
      10        4923. 2009 
      # ... with 11 more rows

---

    Code
      .calc_population_count(shp, worldpop, stats_popcount = c("mean", "median", "sd"))
    Output
      # A tibble: 21 x 4
         popcount_mean popcount_median popcount_sd year 
                 <dbl>           <dbl>       <dbl> <chr>
       1          17.2            13.5        11.3 2000 
       2          16.5            13.1        11.2 2001 
       3          17.5            14.4        11.3 2002 
       4          17.8            14.8        11.9 2003 
       5          18.9            15.3        11.7 2004 
       6          18.9            15.3        12.5 2005 
       7          19.9            16.5        11.8 2006 
       8          18.9            15.2        11.8 2007 
       9          20.9            17.7        12.3 2008 
      10          21.5            17.7        12.7 2009 
      # ... with 11 more rows

---

    Code
      .calc_population_count(shp, worldpop, engine = "extract")
    Output
      # A tibble: 21 x 2
         popcount_sum year 
                <dbl> <chr>
       1        3941. 2000 
       2        3786. 2001 
       3        4012. 2002 
       4        4083. 2003 
       5        4336. 2004 
       6        4323. 2005 
       7        4569. 2006 
       8        4339. 2007 
       9        4784. 2008 
      10        4923. 2009 
      # ... with 11 more rows

---

    Code
      .calc_population_count(shp, worldpop, engine = "exactextract")
    Output
      # A tibble: 21 x 2
         popcount_sum year 
                <dbl> <chr>
       1        3970. 2000 
       2        3819. 2001 
       3        4038. 2002 
       4        4112. 2003 
       5        4358. 2004 
       6        4352. 2005 
       7        4591. 2006 
       8        4361. 2007 
       9        4804. 2008 
      10        4946. 2009 
      # ... with 11 more rows

---

    Code
      .calc_population_count(shp, worldpop, engine = "zonal")
    Output
      # A tibble: 21 x 2
         popcount_sum year 
                <dbl> <chr>
       1        6445. 2000 
       2        6196. 2001 
       3        6555. 2002 
       4        6562. 2003 
       5        6937. 2004 
       6        6967. 2005 
       7        7271. 2006 
       8        6956. 2007 
       9        7518. 2008 
      10        7718. 2009 
      # ... with 11 more rows

