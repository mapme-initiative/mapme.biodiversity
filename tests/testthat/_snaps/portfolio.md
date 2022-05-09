# init_portfolio works

    Code
<<<<<<< HEAD
=======
      (portfolio %>% calc_indicators("treecover", min_size = 1, min_cover = 30) %>%
        write_portfolio(tmpfile, quiet = TRUE))
    Output
      Simple feature collection with 1 feature and 6 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
      Geodetic CRS:  WGS 84
      # A tibble: 1 x 7
        WDPAID NAME        DESIG_ENG ISO3  assetid treecover                      geom
         <dbl> <chr>       <chr>     <chr>   <int> <list>                <POLYGON [°]>
      1 478140 Sierra de ~ National~ DOM         1 <tibble>  ((-71.76134 18.66333, -7~

---

    Code
>>>>>>> main
      read_portfolio(tmpfile)
    Output
      Simple feature collection with 1 feature and 6 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
      Geodetic CRS:  WGS 84
      # A tibble: 1 x 7
        WDPAID NAME        DESIG_ENG ISO3  assetid treecover                      geom
         <dbl> <chr>       <chr>     <chr>   <int> <list>                <POLYGON [°]>
      1 478140 Sierra de ~ National~ DOM         1 <tibble>  ((-71.76134 18.66333, -7~

