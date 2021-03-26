context("ml_connect")

test_that("ml_connect returns the correct credentials", {

  credentials <- jsonlite::read_json(file.path(paste0(.libPaths()[1], "/multilex/secrets/secrets.json")))
  ml_connect(
    google_email = credentials$google_email,
    formr_email = credentials$formr_email,
    formr_password = credentials$formr_password
  )

  expect_true(googlesheets4::gs4_has_token())
})

