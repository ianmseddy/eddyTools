#'A function for creating a matrix with an annulus for use in focal statistics
#'This function is intended to be used with raster::focal
#'@keywords focal raster
#'@param distance a vector of 2 distances specifying inner and outer focal legnth
#'@param ras the raster whose properties are used to generate focal annulus
#'
#'@importFrom raster focalWeight
#'@export
#'@examples
#'focalAnnulus(aRaster, distances = c(5,10))


focalAnnulus <- function(distance, ras){
  if(length(distance)> 2){
    warning("You have supplied more than two distances. The min/max will be used for annulus")
  }
  if(length(distance) < 2){
    stop("You must supply a vector of length 2 to create an annulus")
  }
  inMat <- focalWeight(x = ras, d = min(distance))
  outMat <- focalWeight(x = ras, d = max(distance))
  if(length(inMat) < 5){
    warning("Your distances will not produce an annulus shape. Verify the raster spatial unit")
  }
  #inverse the inner matrix
  inMat[inMat == 0] <- 1
  inMat[inMat < 1] <- 0
  #Get dimensions for matrix
  innerDim <- floor(dim(inMat)[1]/2)
  outerDim <- ceiling(dim(outMat)[1]/2)
  #Merge the two matrices
  outMat[(outerDim-innerDim):(outerDim+innerDim),(outerDim-innerDim):(outerDim+innerDim)] <- inMat
  #Recalculate the matrix value as 1/sum of non-zero values
  outMat[outMat>0] <- 1/length(outMat[outMat>0])
  return(outMat)
}
