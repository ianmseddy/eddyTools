test_that("slivers are not detected and merged", {
 library(LandR)
 library(rgeos)
 library(eddyTools)
  inner <- LandR::randomStudyArea(size = 10000*100*50, seed = 70)
 outer <- LandR::randomStudyArea(size = 10005*100*50, seed = 70)
 diff <- rgeos::gSymdifference(spgeom1 = inner,spgeom2 =  outer)
 inputArea <- bind(inner, diff)
 inputArea$test <- c("valid", "invalid")
 out <- unSliver(inputArea, threshold = 5*100*51)

})
