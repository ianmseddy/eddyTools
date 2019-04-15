#'DRAW CONVEX HILL AROUND POLYGONS -----------------------
#' draws a convex hull aroung vertice points of a polygon shape file.
#' x must be a SpatialPolygons, or SpatialPolygonsDF
#'Courtesy of Ceres Barros
#'@param x A SpatialPolygons or SpatialPolygonsDataFrame object
#'@keywords buffer merge polygons
#'@export
#'@examples
#'outerBuffer()

outerBuffer <- function(x) {
  require(sp); require(dismo)
  if(class(x) == "SpatialPolygons" | class(x) == "SpatialPolygonsDataFrame") {
    ## Get polygon vertices
    pts <- SpatialPoints(do.call(rbind, lapply(x@polygons, FUN = function(x) {
      return(x@Polygons[[1]]@coords)
    })))

    ## Draw convex hull around points and extract polygons slot
    hull <- polygons(convHull(pts))

    return(hull)
  } else(stop("x must be a SpatialPolygons, or SpatialPolygonsDF"))
}
