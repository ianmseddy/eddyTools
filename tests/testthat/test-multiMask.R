test_that("mask values are not numeric", {
  library(raster)
  testRas <- raster(xmn = -5, xmx = 5, ymn = -5, ymx = 5, res = c(1,1))
  testMask <- raster(xmn = -5, xmx = 5, ymn = -5, ymx = 5, res = c(1,1))
  testRas <- setValues(testRas, round(rnorm(100), digits = 0))
  testMask <- setValues(testMask, round(rnorm(100), digits = 0))

  expect_error(multiMask(maskValues = c("meaningless whisper"), x = testRas, mask = testMask), "mask values must be numeric")
  expect_error(multiMask(maskValues = c(1,2, NA), x = testRas, mask = testMask), "mask values must be numeric")
})

test_that("output is a list", {
  library(raster)
  testRas <- raster(xmn = -5, xmx = 5, ymn = -5, ymx = 5, res = c(1,1))
  testMask <- raster(xmn = -5, xmx = 5, ymn = -5, ymx = 5, res = c(1,1))
  testRas <- setValues(testRas, round(rnorm(100), digits = 0))
  testMask <- setValues(testMask, round(rnorm(100), digits = 0))

  expects_equal(class(multiMask(maskValues = 0, x = testRas, mask = testMask)), class(list()))
})
