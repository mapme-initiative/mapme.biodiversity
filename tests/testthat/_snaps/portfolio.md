# init_portfolio works

    Code
      portfolio %>% calc_indicators("chirpsprec", scales_spi = 3, spi_prev_years = 8,
        engine = "extract") %>% write_portfolio(tmpfile)
    Output
      Writing layer `metadata' to data source `portfolio_out.gpkg' using driver `GPKG'
      Writing 1 features with 5 fields and geometry type Polygon.
      Updating layer `chirpsprec' to data source `portfolio_out.gpkg' using driver `GPKG'
      Writing 252 features with 5 fields without geometries.

---

    Code
      read_portfolio(tmpfile)
    Output
      Simple feature collection with 1 feature and 6 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -71.80933 ymin: 18.57668 xmax: -71.33201 ymax: 18.69931
      Geodetic CRS:  WGS 84
      # A tibble: 1 x 7
        WDPAID NAME       DESIG_ENG ISO3  assetid chirpsprec                      geom
         <dbl> <chr>      <chr>     <chr>   <int> <list>                 <POLYGON [°]>
      1 478140 Sierra de~ National~ DOM         1 <tibble>   ((-71.76134 18.66333, -7~

