# treeloss works

    Code
      stat
    Output
      # A tibble: 6 x 3
        years emissions treecover
        <int>     <dbl>     <dbl>
      1  2000        0     12308.
      2  2001     6676.    12293.
      3  2002     9772.    12270.
      4  2003    29346.    12195.
      5  2004    21754.    12142.
      6  2005    19859.    12093.

---

    Code
      calc_indicators(portfolio, "treeloss", min_cover = 50.2)$treeloss[[1]]
    Message <simpleMessage>
      Argument 'min_size' for resource 'treeloss' was not specified. Setting to default value of '10'.
    Output
      # A tibble: 6 x 3
        years emissions treecover
        <int>     <dbl>     <dbl>
      1  2000        0     11130.
      2  2001     6676.    11115.
      3  2002     9772.    11095.
      4  2003    29346.    11028.
      5  2004    21754.    10979.
      6  2005    19859.    10932.

---

    Code
      calc_indicators(portfolio, "treeloss", min_size = 1000, min_cover = 100)$
        treeloss[[1]]
    Output
      # A tibble: 6 x 3
        years treecover emissions
        <int>     <dbl>     <dbl>
      1  2000         0         0
      2  2001         0         0
      3  2002         0         0
      4  2003         0         0
      5  2004         0         0
      6  2005         0         0

