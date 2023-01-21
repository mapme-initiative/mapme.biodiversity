# active fire properties works

    Code
      .calc_active_fire_properties(shp, list(nasa_firms))
    Output
      # A tibble: 21 x 15
         bright_~1  scan track acq_d~2 acq_t~3 satel~4 instr~5 confi~6 version brigh~7
             <dbl> <dbl> <dbl> <chr>     <int> <chr>   <chr>   <chr>     <int>   <dbl>
       1      338.  0.49  0.41 2021-0~    1812 N       VIIRS   n             1    295.
       2      341.  0.39  0.36 2021-0~    1754 N       VIIRS   n             1    297.
       3      298.  0.67  0.73 2021-0~     524 N       VIIRS   n             1    283.
       4      299.  0.67  0.73 2021-0~     524 N       VIIRS   n             1    283.
       5      333.  0.33  0.55 2021-0~    1718 N       VIIRS   n             1    299.
       6      345.  0.68  0.74 2021-0~    1700 N       VIIRS   n             1    294.
       7      346.  0.42  0.45 2021-0~    1736 N       VIIRS   n             1    302.
       8      346.  0.41  0.61 2021-0~    1712 N       VIIRS   n             1    296.
       9      336.  0.39  0.36 2021-0~    1754 N       VIIRS   n             1    307.
      10      331.  0.39  0.36 2021-0~    1754 N       VIIRS   l             1    302.
      # ... with 11 more rows, 5 more variables: frp <dbl>, daynight <chr>,
      #   type <int>, longitude <dbl>, latitude <dbl>, and abbreviated variable names
      #   1: bright_ti4, 2: acq_date, 3: acq_time, 4: satellite, 5: instrument,
      #   6: confidence, 7: bright_ti5

---

    Code
      .calc_active_fire_properties(shp, nasa_firms2)
    Output
      # A tibble: 42 x 15
         bright_~1  scan track acq_d~2 acq_t~3 satel~4 instr~5 confi~6 version brigh~7
             <dbl> <dbl> <dbl> <chr>     <int> <chr>   <chr>   <chr>     <int>   <dbl>
       1      338.  0.49  0.41 2021-0~    1812 N       VIIRS   n             1    295.
       2      341.  0.39  0.36 2021-0~    1754 N       VIIRS   n             1    297.
       3      298.  0.67  0.73 2021-0~     524 N       VIIRS   n             1    283.
       4      299.  0.67  0.73 2021-0~     524 N       VIIRS   n             1    283.
       5      333.  0.33  0.55 2021-0~    1718 N       VIIRS   n             1    299.
       6      345.  0.68  0.74 2021-0~    1700 N       VIIRS   n             1    294.
       7      346.  0.42  0.45 2021-0~    1736 N       VIIRS   n             1    302.
       8      346.  0.41  0.61 2021-0~    1712 N       VIIRS   n             1    296.
       9      336.  0.39  0.36 2021-0~    1754 N       VIIRS   n             1    307.
      10      331.  0.39  0.36 2021-0~    1754 N       VIIRS   l             1    302.
      # ... with 32 more rows, 5 more variables: frp <dbl>, daynight <chr>,
      #   type <int>, longitude <dbl>, latitude <dbl>, and abbreviated variable names
      #   1: bright_ti4, 2: acq_date, 3: acq_time, 4: satellite, 5: instrument,
      #   6: confidence, 7: bright_ti5

