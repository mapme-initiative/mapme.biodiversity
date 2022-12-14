# soilpoperties works

    Code
      .calc_soilproperties(shp, soilgrids, engine = "extract", stats_soil = c("mean",
        "median", "sd", "min", "max", "sum", "var"))
    Output
      # A tibble: 4 x 10
        layer depth  stat   mean median    sd   min   max     sum   var
        <chr> <chr>  <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
      1 clay  0-5cm  mean   35.0   34.8  2.61  28.5  41.9 105970.  67.9
      2 clay  5-15cm mean   35.4   35.3  2.49  28.5  42.7 107200.  61.9
      3 silt  0-5cm  mean   33.8   33.7  2.13  28.2  43.1 102528.  45.5
      4 silt  5-15cm mean   33.9   33.8  2.05  27.9  43.7 102636.  41.9

---

    Code
      .calc_soilproperties(shp, soilgrids, engine = "zonal", stats_soil = c("mean",
        "median", "sd", "min", "max", "sum", "var"))
    Output
      # A tibble: 4 x 10
        layer depth  stat   mean median    sd   min   max     sum   var
        <chr> <chr>  <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
      1 clay  0-5cm  mean   35.0   34.8  2.61  28.5  41.9 105970.  67.9
      2 clay  5-15cm mean   35.4   35.3  2.49  28.5  42.7 107200.  61.9
      3 silt  0-5cm  mean   33.8   33.7  2.13  28.2  43.1 102528.  45.5
      4 silt  5-15cm mean   33.9   33.8  2.05  27.9  43.7 102636.  41.9

---

    Code
      .calc_soilproperties(shp, soilgrids, engine = "exactextract", stats_soil = c(
        "mean", "median", "sd", "min", "max", "sum", "var"))
    Output
      # A tibble: 4 x 10
        layer depth  stat   mean median    sd   min   max     sum   var
        <chr> <chr>  <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
      1 clay  0-5cm  mean   35.0   34.8  2.60  28.5  42.6 106026.  67.7
      2 clay  5-15cm mean   35.4   35.3  2.48  28.5  43.8 107262.  61.7
      3 silt  0-5cm  mean   33.8   33.8  2.13  28.2  43.1 102568.  45.5
      4 silt  5-15cm mean   33.8   33.8  2.05  27.9  43.7 102672.  41.9

