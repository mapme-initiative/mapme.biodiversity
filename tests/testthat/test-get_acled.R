test_that("get_acled works", {
  expect_error(get_acled(accept_terms = FALSE), "Terms of Use")
  expect_error(expect_message(get_acled(accept_terms = TRUE, email = "")), "email")
  expect_error(expect_message(get_acled(accept_terms = TRUE, email = "mymail")), "API key")

  Sys.setenv(ACLED_ACCESS_EMAIL = "my-mail")
  Sys.setenv(ACLED_ACCESS_KEY = "my-key")
  expect_message(get_acled(accept_terms = TRUE), "Terms of Use")
})
