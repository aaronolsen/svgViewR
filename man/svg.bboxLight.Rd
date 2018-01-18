\name{svg.bboxLight}
\alias{svg.bboxLight}
\title{Adds light(s) to Viewer}
\description{Adds point light(s) in Viewer at corners of bounding box.}
\usage{
svg.bboxLight(x=c(1,1,1), col='#FFFFDD', intensity=1, distance=3, hidden = TRUE)
}
\arguments{
  \item{x}{The corner(s) where a light source (vector) or multiple light sources (matrix) should be added.}
  \item{col}{The color of the light.}
  \item{intensity}{The intensity of the light.}
  \item{distance}{The distance from the light where the intensity is 0.}
  \item{hidden}{A logical indicating whether the light source location should be indicated by a yellow sphere. By default the sphere is hidden. Setting this to \code{FALSE} may be useful for troubleshooting light positions (so that they can be viewed directly in the scene).}
}
\details{
This function only works in the webgl plotting mode (i.e. when the \code{mode} parameter of \code{\link{svg.new}} is 'webgl'). This function creates a point light (if \code{x} is a vector) or lights (if \code{x} is a matrix) in the Viewer scene at the specified coordinates (\code{x}). The point light is the PointLight object in the three.js library.
}
\value{NULL}
\author{Aaron Olsen}
\seealso{
\code{\link{svg.new}}
}
\examples{
\dontrun{
# Create new Viewer
svg.new(mode='webgl')

# Add object
svg.cylinder(ends=rbind(c(0,0,0), c(0,10,0)), radius=1, col='green')

# Add lights in four corners of the bounding box
svg.bboxLight(x=rbind(c(1,1,1), c(-1,1,1), c(-1,-1,-1), c(1,-1,-1)))

# Close connection
svg.close()
}
}