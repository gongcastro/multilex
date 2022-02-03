context("ml_connect")

test_that("ml_connect returns the correct credentials", {

  expect_true(
    ml_connect(),
    "ml_connect returns success code after loggin in"
  )

})



