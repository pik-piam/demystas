#' Overlaying points onto polygons
#'
#' Overlays coordinates of class data frame, SpatialPoints or SpatialPointsDataFrame onto polygon(s) of class SpatialPolygonsDataFrame
#'
#' @name coords2spi
#' @param points a data frame with two columns of coordinates, or an object of class SpatialPoints or SpatialPointsDataFrame.
#' These represent points to be mapped onto polygon(s).
#' @param global an object of class SpatialPolygonsDataFrame onto which `points` is mapped.
#' @return a data frame corresponding to the coordinates in `points` mapped onto `global` with the attributes of `global`
#' @author Atreya Shankar
#' @export
#' @examples
#' \dontrun{
#'
#' require(rworldmap)
#' test <- coords2spi(as.data.frame(cbind(60,50)), getMap(resolution="low"))
#' }

coords2spi <- function(points, global){

  if(!is.data.frame(points) & class(points) != "SpatialPoints" & class(points) != "SpatialPointsDataFrame"){
    stop("points must be either of class data frame, SpatialPoints or SpatialPointsDataFrame")
  }

  if(class(global) != "SpatialPolygonsDataFrame"){
    stop("global must have the class of SpatialPolygonsDataFrame")
  }

  if(is.data.frame(points)){
    points <- SpatialPoints(points, proj4string = CRS(proj4string(global)))
  } else if(proj4string(points) != proj4string(global)){
    points <- spTransform(points, CRS(proj4string(global)))
  }

  spi <- over(points, global)
  return(spi)
}