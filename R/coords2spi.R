#' Convert coordinates to spatial information
#'
#' Wrapper function for `sp::over` in overlaying points over polygons to retrieve relevant intersection information.
#'
#' @name coords2spi
#' @concept demystas
#' @param points a data frame with two columns of coordinates, or a Spatial object listed here under "x" in showMethods(sp::over).
#' These represent points to be mapped onto polygon(s). Coordinates in `points` should be in the same column order as those in `global`
#' @param global an object onto which `points` is mapped, possible classes corresponding to `points` listed under "y" in
#' showMethods(sp::over). If `points` is a data frame, it will be converted into a SpatialPoints object.
#' @return a data frame corresponding to the coordinates in `points` mapped onto `global` with the attributes of `global`
#' @author Atreya Shankar
#' @importFrom sp spTransform
#' @importFrom sp CRS
#' @importFrom sp proj4string
#' @importFrom sp over
#' @importFrom sp SpatialPoints
#' @export
#' @seealso \code{\link[sp]{over}}
#' @examples
#' \dontrun{
#'
#' require(rworldmap)
#' test <- demystas::coords2spi(as.data.frame(cbind(60,50)), getMap(resolution="low"))
#' }

coords2spi <- function(points, global){

  if(is.matrix(points)){
    points <- as.data.frame(points)
  }

  if(is.data.frame(points)){
    points <- SpatialPoints(points, proj4string = CRS(proj4string(global)))
  } else if(proj4string(points) != proj4string(global)){
    points <- spTransform(points, CRS(proj4string(global)))
  }

  spi <- over(points, global)
  return(spi)
}
