context("ml_connect")

test_that("ml_connect returns the correct credentials", {

  email <- "gonzalo.garciadecastro@upf.edu"
  ml_connect(
    google_email = email,
    formr_email = email,
    formr_password = keyring::key_get("formr", email))

  expect_true(googlesheets4::gs4_has_token(), label = "Google credentials")
})

