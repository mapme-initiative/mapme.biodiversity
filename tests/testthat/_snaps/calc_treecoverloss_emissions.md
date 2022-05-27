# emissions works

    Code
      .calc_treecoverloss_emissions(shp, gfw_treecover, gfw_lossyear, gfw_emissions,
        min_size = 1, min_cover = 10)
    Output
      # A tibble: 6 x 2
        years emissions
        <int>     <dbl>
      1  2000         0
      2  2001         7
      3  2002        46
      4  2003      1419
      5  2004       528
      6  2005       300

---

    Code
      stat
    Output
      # A tibble: 6 x 2
        years emissions
        <int>     <dbl>
      1  2000         0
      2  2001         7
      3  2002        46
      4  2003      1419
      5  2004       528
      6  2005       300

