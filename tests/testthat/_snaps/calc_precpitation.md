# precipitation indicator works

    Code
      .calc_precipitation(shp, chirps)
    Output
      [[1]]
      # A tibble: 132 x 6
         years months absolute anomaly spi_12   .id
         <dbl>  <dbl>    <dbl>   <dbl>  <dbl> <int>
       1  2000      1     9.47  -2.75   1.79      1
       2  2000      2    22.1    0.577  1.78      1
       3  2000      3    25.6  -17.2    1.59      1
       4  2000      4    66.4  -34.2    1.44      1
       5  2000      5   141.   -50.6    1.52      1
       6  2000      6    61.1  -51.0    1.12      1
       7  2000      7    61.5  -11.8    1.03      1
       8  2000      8   146.    44.5    1.37      1
       9  2000      9   135.   -11.7    1.16      1
      10  2000     10   116.   -25.6    0.490     1
      # ... with 122 more rows
      

---

    Code
      .calc_precipitation(shp, chirps, scales_spi = c(12, 24))
    Output
      [[1]]
      # A tibble: 132 x 7
         years months absolute anomaly spi_12 spi_24   .id
         <dbl>  <dbl>    <dbl>   <dbl>  <dbl>  <dbl> <int>
       1  2000      1     9.47  -2.75   1.79   1.62      1
       2  2000      2    22.1    0.577  1.78   1.61      1
       3  2000      3    25.6  -17.2    1.59   1.54      1
       4  2000      4    66.4  -34.2    1.44   1.43      1
       5  2000      5   141.   -50.6    1.52   1.11      1
       6  2000      6    61.1  -51.0    1.12   0.742     1
       7  2000      7    61.5  -11.8    1.03   0.684     1
       8  2000      8   146.    44.5    1.37   1.01      1
       9  2000      9   135.   -11.7    1.16   0.437     1
      10  2000     10   116.   -25.6    0.490  0.637     1
      # ... with 122 more rows
      

---

    Code
      .calc_precipitation(shp, chirps, engine = "extract")
    Output
      [[1]]
      # A tibble: 132 x 6
         years months absolute anomaly spi_12   .id
         <dbl>  <dbl>    <dbl>   <dbl>  <dbl> <int>
       1  2000      1     7.97  -2.49   1.76      1
       2  2000      2    20.6    0.406  1.74      1
       3  2000      3    25.0  -16.6    1.57      1
       4  2000      4    67.5  -34.5    1.39      1
       5  2000      5   147.   -48.5    1.48      1
       6  2000      6    61.0  -48.3    1.09      1
       7  2000      7    63.7  -12.9    1.01      1
       8  2000      8   156.    48.7    1.37      1
       9  2000      9   139.   -11.3    1.13      1
      10  2000     10   120.   -23.2    0.449     1
      # ... with 122 more rows
      

---

    Code
      .calc_precipitation(shp, chirps, engine = "exactextract")
    Output
      [[1]]
      # A tibble: 132 x 6
         years months absolute anomaly spi_12   .id
         <dbl>  <dbl>    <dbl>   <dbl>  <dbl> <int>
       1  2000      1     8.35  -2.56   1.78      1
       2  2000      2    21.0    0.491  1.77      1
       3  2000      3    25.0  -16.8    1.59      1
       4  2000      4    67.4  -34.7    1.42      1
       5  2000      5   145.   -49.8    1.52      1
       6  2000      6    61.6  -50.5    1.12      1
       7  2000      7    63.3  -12.4    1.04      1
       8  2000      8   152.    46.5    1.39      1
       9  2000      9   138.   -11.7    1.15      1
      10  2000     10   119.   -24.9    0.489     1
      # ... with 122 more rows
      

