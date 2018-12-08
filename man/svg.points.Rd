\name{svg.points}
\alias{svg.points}
\title{Write points to SVG Viewer}
\description{Draws a sequence of points at specified coordinates in an SVG Viewer.}
\usage{
svg.points(x, y=NULL, type="p", col=NULL, col.fill="black", 
               col.stroke="black", z.index=0, layer="", label="", 
               cex=2, lwd=2, opacity.stroke=1, opacity.fill=1, 
               file=NULL)
}
\arguments{
  \item{x}{A vector, matrix or array of 2D or 3D coordinates. Coordinates input as an array will be animated.}
  \item{y}{If \code{x} is a vector, the coordinates on the y-axis.}
  \item{type}{character indicating the type of plotting. Currently, only \code{"p"} is supported.}
  \item{col}{The fill and stroke color of the points(s). If non-\code{NULL}, \code{col} overrides \code{col.fill} and \code{col.stroke}.}
  \item{col.fill}{The fill color of the points(s).}
  \item{col.stroke}{The stroke (border) color of the points(s).}
  \item{z.index}{A number indicating the relative order in which the SVG object will be drawn in the viewer. Higher numbers correspond to closer to the front or top.}
  \item{layer}{A text string indicating the layer in which the SVG object belongs (not yet fully implemented).}
  \item{label}{A label to be added to the SVG object in SVG tag.}
  \item{cex}{The size (radius) of the point(s).}
  \item{lwd}{The thickness of the border of the point(s).}
  \item{opacity.stroke}{A number between \code{0} and \code{1} indicating the opacity of the border of the point(s).}
  \item{opacity.fill}{A number between \code{0} and \code{1} indicating the opacity of the fill of the point(s).}
  \item{file}{File path (having the extenstion ".html") to add lines to a current SVG Viewer file. By default (i.e. \code{NULL}) lines are added to the SVG Viewer opened by \code{\link{svg.new}}.}
}
\details{
This function accepts input similar to the native plot function \code{points()}. If \code{x} and \code{y} are vectors, they are combined into a matrix using \code{cbind()}. If \code{x} is a matrix, this matrix is used directly. Each row of the matrix is drawn as a point. If \code{x} is an array, the array is interpreted as a series of matrices, each representing a state of the point or point(s) in an animation of length \code{dim(x)[3]}. Each of the \code{dim(x)[3]} matrices is used to draw each state in a manner identical to when \code{x} is a matrix. If \code{x} is an array, each state of points will be drawn as an animation.

The graphical parameters \code{col}, \code{col.fill}, \code{col.stroke}, \code{z.index}, \code{layer}, \code{label}, \code{lwd}, \code{opacity.stroke} and \code{opacity.fill} can all be vectors of length one or of the same length as the number of points to be drawn. This allows different parameters to be specified for each point.
}
\value{NULL}
\author{Aaron Olsen}
\seealso{
\code{\link{svg.new}}, 
\code{\link{svg.lines}}, 
\code{\link{svg.pathsC}}
}
\examples{
\dontrun{
## Create animated sinusoid
# Create new viewer
svg.new(file='svgviewr.html', animate.duration=1)

# Create points with varying sin phase
n <- 100
x <- array(NA, dim=c(40, 2, n))
x_seq <- seq(-pi, pi, length=dim(x)[1])
n_seq <- seq(0, 2*pi, length=n)
for(i in 1:dim(x)[3]) x[, , i] <- cbind(x_seq, sin(x_seq + n_seq[i]))

# Draw points
svg.points(x, cex=2, lwd=1, col="blue")

# Close viewer connection
svg.close()

# Open svgviewr.html to visualize
}
}