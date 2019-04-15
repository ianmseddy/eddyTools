#' A function for masking multiple values at once
#' This function is simply lapply with mask
#'@keywords mask raster
#'@importFrom raster mask
#'@export
#'@examples
#'multiMask(aRaster, maskValues = c(5,10))
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

multiMask <- function(maskValues, ...){
  if(is.numeric(maskValues) == FALSE){
    stop("maskValues must be numeric")
  }
  output <- lapply(maskValues, FUN = function(l) {
    outRas <- mask(maskvalue = l, ...)
    return(outRas)
  })
  names(output) <- paste("masked", maskValues, sep = "")
  return(output)
}
