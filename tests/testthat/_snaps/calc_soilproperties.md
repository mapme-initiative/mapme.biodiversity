# soilpoperties works

    Code
      .calc_soilproperties(shp, soilgrids, engine = "extract", stats_soil = c("mean",
        "median", "sd", "min", "max", "sum", "var"))
    Output
        layer  depth stat     mean   median       sd      min      max      sum
      1  clay  0-5cm mean 3500.115 3487.681 260.5677 2852.245 4196.945 10612349
      2  clay 5-15cm mean 3540.568 3532.394 248.6777 2859.288 4271.764 10735003
      3  silt  0-5cm mean 3386.514 3378.540 213.2921 2823.269 4311.799 10267910
      4  silt 5-15cm mean 3390.079 3380.508 204.6968 2799.435 4370.416 10278719
             var
      1 67895.50
      2 61840.57
      3 45493.54
      4 41900.80

---

    Code
      .calc_soilproperties(shp, soilgrids, engine = "zonal", stats_soil = c("mean",
        "median", "sd", "min", "max", "sum", "var"))
    Output
        layer  depth stat     mean   median       sd      min      max      sum
      1  clay  0-5cm mean 3500.115 3487.681 260.5677 2852.245 4196.945 10612349
      2  clay 5-15cm mean 3540.568 3532.394 248.6777 2859.288 4271.764 10735003
      3  silt  0-5cm mean 3386.514 3378.540 213.2921 2823.269 4311.799 10267910
      4  silt 5-15cm mean 3390.079 3380.508 204.6968 2799.435 4370.416 10278719
             var
      1 67895.50
      2 61840.57
      3 45493.54
      4 41900.80

---

    Code
      .calc_soilproperties(shp, soilgrids, engine = "exactextract", stats_soil = c(
        "mean", "median", "sd", "min", "max", "sum", "var"))
    Output
        layer  depth stat     mean   median       sd      min      max      sum
      1  clay  0-5cm mean 3500.144 3487.406 260.2851 2852.245 4264.347 10617947
      2  clay 5-15cm mean 3540.777 3532.233 248.4122 2859.288 4386.652 10741211
      3  silt  0-5cm mean 3386.075 3378.671 213.3053 2823.269 4311.799 10271911
      4  silt 5-15cm mean 3389.527 3380.447 204.6117 2799.435 4370.416 10282383
             var
      1 67748.34
      2 61708.63
      3 45499.13
      4 41865.95

