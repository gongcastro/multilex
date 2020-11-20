responses <- ml_responses(formr_email = "gonzalo.garciadecastro@upf.edu",
                          google_email = "gonzalo.garciadecastro@upf.edu")

test_that("DoEs sum approximately 100%", {
  expect_true(
    all(dplyr::between(rowSums(responses[,grepl("doe_", colnames(responses))], na.rm = TRUE), 95, 100))
  )
})
