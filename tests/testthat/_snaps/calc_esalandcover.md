# esa global landcover works

    Code
      .calc_esalandcover(shp, esalandcover)
    Output
      # A tibble: 5 x 12
        year  shrubs herbaceous_vegetation cropland closed_forest_ev~ closed_forest_d~
        <chr>  <dbl>                 <dbl>    <dbl>             <dbl>            <dbl>
      1 2015    506.                 1836.     1.15             4654.             10.3
      2 2016    506.                 1836.     1.15             4654.             10.3
      3 2017    506.                 1836.     1.15             4654.             10.3
      4 2018    506.                 1836.     1.15             4654.             10.3
      5 2019    506.                 1836.     1.15             4654.             10.3
      # ... with 6 more variables: closed_forest_mixed <dbl>,
      #   closed_forest_unknown <dbl>, open_forest_evergreen_broad_leaf <dbl>,
      #   open_forest_deciduous_broad_leaf <dbl>, open_forest_mixed <dbl>,
      #   open_forest_unknown <dbl>

