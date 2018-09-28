#' Detect nearest polygon (country) to point(s)
#'
#' Finds nearest polygon (country) to point(s), useful for cases where the `sp::over` function yields NA results
#'
#' @name spNearest
#' @aliases demystas
#' @param points a data frame with two columns of coordinates, or an object of class SpatialPoints or SpatialPointsDataFrame.
#' Coordinates in `points` should be in the same column order as those in `global`
#' @param global an object of class SpatialPolygonsDataFrame onto which `points` is mapped.
#' @param inc a numerical value which indicates how much the entire bounding box of `global` shoud be segmented to find nearest countries. Defaults to 100.
#' @return a data frame with attributes from `global` about nearest polygon (country) to `points`
#' @author Atreya Shankar
#' @importFrom methods as
#' @importFrom sp spTransform
#' @importFrom sp CRS
#' @importFrom sp proj4string
#' @importFrom sp over
#' @importFrom raster intersect
#' @importFrom raster extent
#' @importFrom raster crs<-
#' @importFrom geosphere dist2Line
#' @importFrom sp SpatialPoints
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#' @examples
#' \dontrun{
#'
#' require(rworldmap)
#' require(rworldxtra)
#' points <- rbind(c(-81.779,52.234), c(-80.873, 51.126))
#' test <- spNearest(points, getMap(resolution="high"))
#' }

spNearest <- function(points, global, inc = 100){

  if(is.matrix(points)){
    points <- as.data.frame(points)
  } else if(!is.data.frame(points) & class(points) != "SpatialPoints" & class(points) != "SpatialPointsDataFrame"){
    stop("points must be either of class data frame, SpatialPoints or SpatialPointsDataFrame")
  }

  if(class(global) != "SpatialPolygonsDataFrame"){
    stop("global must have the class of SpatialPolygonsDataFrame")
  }

  if(!is.data.frame(points)){
    if(proj4string(points) != proj4string(global)){
      points <- spTransform(points, CRS(proj4string(global)))
    }
    points <- as.data.frame(points@coords)
  }

  result <- list()
  global <- spTransform(global, CRS(proj4string(global)))
  bufferp <- apply(global@bbox, 1, diff)/inc
  bbp <- global@bbox
  bb <- bbp
  buffer <- bufferp

  start <- proc.time()

  pb.overall <- txtProgressBar(min = 0, max = nrow(points), initial = 0, char = "=",
                               width = options()$width, style = 3, file = "")

  i = 1
  while(i <= nrow(points)){

    bb[1,1] <- points[i,1] - buffer[1]
    bb[1,2] <- points[i,1] + buffer[1]
    bb[2,1] <- points[i,2] - buffer[2]
    bb[2,2] <- points[i,2] + buffer[2]

    bpoly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    crs(bpoly) <- CRS(proj4string(global))

    var1 <- tryCatch(intersect(bpoly, global), error=function(e) e, warning=function(w) w)

    if(all(class(var1) %in% c("simpleWarning", "warning", "condition"))){
      if(var1$message == "polygons do not intersect"){
        buffer <- buffer*2
      } else buffer <- buffer/2
    } else{
      nice <- intersect(bpoly, global)
      result[[i]] <- nice@data[dist2Line(points[i,], nice)[,"ID"],]
      buffer <- bufferp
      i = i + 1

      Sys.sleep(1/1000)
      setTxtProgressBar(pb.overall, i, title = NULL, label = NULL)
    }
  }
  close(pb.overall)

  end <- proc.time()
  print(end-start)
  return(do.call("rbind", lapply(result, function(x) return(x))))
}
