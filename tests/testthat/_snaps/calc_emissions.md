# emissions works

    Code
      .calc_emissions(shp, treecover2000, lossyear, greenhouse, min_size = 1,
        min_cover = 10)
    Output
      # A tibble: 6 x 2
        years emissions
        <int>     <dbl>
      1  2000        0 
      2  2001     6676.
      3  2002     9772.
      4  2003    29346.
      5  2004    21754.
      6  2005    19859.

---

    Code
      stat
    Output
      # A tibble: 6 x 2
        years emissions
        <int>     <dbl>
      1  2000        0 
      2  2001     6676.
      3  2002     9772.
      4  2003    29346.
      5  2004    21754.
      6  2005    19859.

