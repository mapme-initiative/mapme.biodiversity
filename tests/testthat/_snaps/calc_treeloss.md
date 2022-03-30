# treeloss works

    Code
      .calc_treeloss(shp, treecover2000, lossyear, greenhouse, min_size = 1,
        min_cover = 10)
    Output
      # A tibble: 6 x 3
        years emissions treecover
        <int>     <dbl>     <dbl>
      1  2000        0     13238.
      2  2001     6676.    13221.
      3  2002     9772.    13197.
      4  2003    29346.    13120.
      5  2004    21754.    13066.
      6  2005    19859.    13015.

---

    Code
      stat
    Output
      # A tibble: 6 x 3
        years emissions treecover
        <int>     <dbl>     <dbl>
      1  2000        0     13238.
      2  2001     6676.    13221.
      3  2002     9772.    13197.
      4  2003    29346.    13120.
      5  2004    21754.    13066.
      6  2005    19859.    13015.

