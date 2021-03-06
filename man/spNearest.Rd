% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spNearest.R
\name{spNearest}
\alias{spNearest}
\title{Detect nearest polygon (country) to point(s)}
\usage{
spNearest(points, global, inc = 100)
}
\arguments{
\item{points}{a data frame with two columns of coordinates (first longitude, then latitude), or a SpatialPoints* object.}

\item{global}{a data frame with two columns of coordinates (first longitude, then latitude), or a SpatialLines* object
or a SpatialPolygons* object. This represents the object onto which `points` is mapped.}

\item{inc}{a numerical value which indicates how much the entire bounding box of `global` shoud be segmented to find nearest countries. Defaults to 100.}
}
\value{
a data frame with attributes from `global` about nearest polygon (country) to `points`
}
\description{
Finds nearest polygon (country) to point(s), useful for cases where the `sp::over` function yields NA results.
Should be used with unprojected coordinates íe. lon/lat, ideally on the WGS84 ellipsoid
}
\examples{
\dontrun{

require(rworldmap)
require(rworldxtra)
points <- rbind(c(-81.779,52.234), c(-80.873, 51.126))
test <- demystas::spNearest(points, getMap(resolution="high"))
}
}
\seealso{
\code{\link[geosphere]{dist2Line}}
}
\author{
Atreya Shankar
}
\concept{demystas}
