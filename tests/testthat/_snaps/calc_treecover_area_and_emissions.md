# treecover area and emissions works

    Code
      .calc_treecover_area_and_emissions(shp, gfw_treecover, gfw_lossyear,
        gfw_emissions, min_size = 1, min_cover = 10)
    Output
      # A tibble: 6 x 3
        years emissions treecover
        <int>     <dbl>     <dbl>
      1  2000         0     2066.
      2  2001         7     2066.
      3  2002        46     2064.
      4  2003      1419     2029.
      5  2004       528     2020.
      6  2005       300     2015.

---

    Code
      stat
    Output
      # A tibble: 6 x 3
        years emissions treecover
        <int>     <dbl>     <dbl>
      1  2000         0     2066.
      2  2001         7     2066.
      3  2002        46     2064.
      4  2003      1419     2029.
      5  2004       528     2020.
      6  2005       300     2015.

