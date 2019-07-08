\name{svg.lines}
\alias{svg.lines}
\title{Add Connected Line Segments to SVG Viewer}
\description{A function taking coordinates given in various ways and joining the corresponding points with line segments in an SVG Viewer.}
\usage{
svg.lines(x, y=NULL, col="black", z.index=0, layer="", name="line", label="", 
          lwd=1, opacity=1, seg=1, ontop=FALSE, file=NULL)
}
\arguments{
  \item{x}{A vector, matrix or array of 2D or 3D coordinates to be joined by a line or lines. Coordinates input as an array will be animated.}
  \item{y}{If \code{x} is a vector, the coordinates on the y-axis to be joined by a line or lines.}
  \item{col}{The color of the line(s).}
  \item{z.index}{A number indicating the relative order in which the SVG object will be drawn in the viewer. Higher numbers correspond to closer to the front or top.}
  \item{layer}{A text string indicating the layer in which the SVG object belongs (not yet fully implemented).}
  \item{name}{The name of the drawn object.}
  \item{label}{A label to be added to the SVG object in SVG tag.}
  \item{lwd}{The thickness of the line(s).}
  \item{opacity}{A number between \code{0} and \code{1} indicating the opacity of the line(s).}
  \item{seg}{The number of segments to use in drawing the line.}
  \item{ontop}{Whether the line should appear on top of all other objects in the viewer (to manually set order in cases of incorrect ordering due to partial opacities).}
  \item{file}{File path (having the extenstion ".html") to add lines to a current SVG Viewer file. By default (i.e. \code{NULL}) lines are added to the SVG Viewer opened by \code{\link{svg.new}}.}
}
\details{
This function accepts input similar to the native plot function \code{lines()}. If \code{x} and \code{y} are vectors, they are combined into a matrix using \code{cbind()}. If \code{x} is a matrix, this matrix is used directly. Lines are then drawn between points indicated by consecutive rows. So for a 2-row matrix one line would be drawn, for a 3-row matrix two lines would be drawn, etc.

If \code{x} is an array, the array is interpreted as a series of matrices, each representing a state of the line or line(s) in an animation of length \code{dim(x)[3]}. Each of the \code{dim(x)[3]} matrices is used to draw each state in a manner identical to when \code{x} is a matrix.

The graphical parameters \code{col}, \code{z.index}, \code{layer}, \code{label}, \code{lwd}, and \code{opacity} can all be vectors of length one or of the same length as the number of lines to be drawn (see Examples). This allows different parameters to be specified for each line or for different animation states, depending on the number of graphical parameters specified.
}
\value{NULL}
\author{Aaron Olsen}
\seealso{
\code{\link{svg.new}}, 
\code{\link{svg.pathsC}}, 
\code{\link{svg.points}}
}
\examples{
\dontrun{
## Create static and animated lines
# Create new viewer
svg.new(file='svgviewr.html', animate.duration=1)

# Plot 3 connected lines with 3 different colors
svg.lines(x=rbind(c(30,-20,0), c(30,-30,0), c(40,-30,0), c(40,-35,0)), 
	col=c("red", "green", "blue"), lwd=5, opacity=0.7)

# Plot single line that switches among 3 colors
svg.lines(x=rbind(c(15,0,0), c(15,-20,0)), col=c("red", "green", "blue"), lwd=3, opacity=0.7)

# Create a line in two animation states
arr <- array(c(rbind(c(15,-30,0), c(15,-50,0)), rbind(c(10,-30,0), c(10,-50,0))), dim=c(2,3,2))

# Plot
svg.lines(x=arr, col=c("red", "green"), lwd=3, opacity=0.7)

# Create two connected lines in 3 animation states
arr <- array(c(30,30,40, -40,-50,-50, 0,0,0, 40,40,50, -40,-50,-50, 
	0,0,0, 50,50,60, -40,-50,-50, 0,0,0), dim=c(3,3,3))

# Plot
svg.lines(x=arr, col=c("red", "green"), lwd=5, opacity=0.7)

# Close connection
svg.close()

# Open svgviewr.html to visualize
}
}