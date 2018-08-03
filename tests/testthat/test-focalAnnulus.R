test_that("distance vectors are formatted correctly", {

  testRas <- raster::raster(xmn = -10, xmx = 10, ymn = -10, ymx = 10, res = c(1,1))
  testRas[] <- runif(raster::ncell(testMask),min=1, max = 3)
  distances <- c(0,4)
  expect_warning(focalAnnulus(ras = testRas, distance = distances))
  expect_warning(focalAnnulus(ras = testRas, distance = c(2,3,5)))
  expect_error(focalAnnulus(ras = testRas, distance = 4))
})

