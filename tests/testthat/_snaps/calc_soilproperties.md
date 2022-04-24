# soilpoperties works

    Code
      .calc_soilproperties(shp, soilgrids, engine = "extract", stats_soil = c("mean",
        "median", "sd", "min", "max", "sum", "var"))
    Output
        layer  depth stat     mean median       sd  min  max      sum      var
      1  clay  0-5cm mean 34.95063   34.8 2.605542 28.5 41.9 105970.3 67.88848
      2  clay 5-15cm mean 35.35604   35.3 2.487186 28.5 42.7 107199.5 61.86094
      3  silt  0-5cm mean 33.81537   33.7 2.133325 28.2 43.1 102528.2 45.51074
      4  silt 5-15cm mean 33.85079   33.8 2.046956 27.9 43.7 102635.6 41.90029

---

    Code
      .calc_soilproperties(shp, soilgrids, engine = "zonal", stats_soil = c("mean",
        "median", "sd", "min", "max", "sum", "var"))
    Output
        layer  depth stat     mean median       sd  min  max      sum      var
      1  clay  0-5cm mean 34.95063   34.8 2.605542 28.5 41.9 105970.3 67.88848
      2  clay 5-15cm mean 35.35604   35.3 2.487186 28.5 42.7 107199.5 61.86094
      3  silt  0-5cm mean 33.81537   33.7 2.133325 28.2 43.1 102528.2 45.51074
      4  silt 5-15cm mean 33.85079   33.8 2.046956 27.9 43.7 102635.6 41.90029

---

    Code
      .calc_soilproperties(shp, soilgrids, engine = "exactextract", stats_soil = c(
        "mean", "median", "sd", "min", "max", "sum", "var"))
    Output
        layer  depth stat     mean   median       sd  min  max      sum      var
      1  clay  0-5cm mean 34.95086 34.82681 2.602639 28.5 42.6 106026.0 67.73729
      2  clay 5-15cm mean 35.35814 35.27358 2.484514 28.5 43.8 107261.6 61.72811
      3  silt  0-5cm mean 33.81105 33.75185 2.133546 28.2 43.1 102568.3 45.52019
      4  silt 5-15cm mean 33.84528 33.76605 2.046142 27.9 43.7 102672.2 41.86698

