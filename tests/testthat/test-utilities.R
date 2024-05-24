test_that("calibration_coefs linear works", {
  expect_equal(calibration_coefs(c(1, 2), c(3, 5)), c(1, 2))
})

test_that("tolp_scale linear works", {
  expect_equal(tolp_scale(c(1, 2), c(1, 2)), c(3, 5))
})

test_that("tolp_scale linear reverse works", {
  expect_equal(tolp_scale(c(3, 5), c(1, 2), reverse = TRUE), c(1, 2))
})

test_that("calibration_coefs log works", {
  expect_equal(calibration_coefs(c(1, 2), c(10, 100), TRUE), c(1, 1))
})

test_that("tolp_scale log works", {
  expect_equal(tolp_scale(c(1, 2), c(1, 1), TRUE), c(10, 100))
})

test_that("tolp_scale log reverse works", {
  expect_equal(tolp_scale(c(10, 100), c(1, 1), TRUE, TRUE), c(1, 2))
})

test_that("tolp_resource works", {
  expect_gt(nchar(tolp_resource("resources", "css", "stylesheet.css")), 0)
})
