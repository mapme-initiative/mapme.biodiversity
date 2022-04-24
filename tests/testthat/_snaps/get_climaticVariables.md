# .get_worldpop works

    Code
      basename(get_resources(portfolio, "mintemperature"))
    Message <simpleMessage>
      Starting process to download resource 'mintemperature'........
      Some target years are not available for tmin.
    Output
      [1] "wc2.1_2.5m_tmin_2000-2009.zip" "wc2.1_2.5m_tmin_2010-2018.zip"

---

    Code
      basename(get_resources(portfolio, "maxtemperature"))
    Message <simpleMessage>
      Starting process to download resource 'maxtemperature'........
      Some target years are not available for tmax.
    Output
      [1] "wc2.1_2.5m_tmax_2000-2009.zip" "wc2.1_2.5m_tmax_2010-2018.zip"

---

    Code
      basename(get_resources(portfolio, "precipitation"))
    Message <simpleMessage>
      Starting process to download resource 'precipitation'........
      Some target years are not available for prec.
    Output
      [1] "wc2.1_2.5m_prec_2000-2009.zip" "wc2.1_2.5m_prec_2010-2018.zip"

