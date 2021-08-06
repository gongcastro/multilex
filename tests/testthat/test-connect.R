context("ml_connect")

test_that("ml_connect returns the correct credentials", {

  expect_true(
    ml_connect(formr_password = key_get("formr", "gonzalo.garciadecastro@upf.edu")),
    "ml_connect returns success code after loggin in"
  )
  expect_error(
    ml_connect(formr_password = key_get("formr", "wrong@email")),
    info = "ml_connect returns failure code if email is wrong"
  )

})



