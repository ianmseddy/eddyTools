test_that("mask values are not numeric", {
  library(raster)
  testRas <- raster(xmn = -5, xmx = 5, ymn = -5, ymx = 5, res = c(1,1))
  testMask <- testRas
  testRas <- setValues(testRas, round(rnorm(100), digits = 0))
  testMask <- setValues(testMask, round(rnorm(100), digits = 0))

  expect_error(multiMask(maskValues = c("meaningless whisper"), x = testRas, mask = testMask), "maskValues must be numeric")
  expect_error(multiMask(maskValues = c(NA), x = testRas, mask = testMask), "maskValues must be numeric")
})

