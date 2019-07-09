\name{svg.pathsC}
\alias{svg.pathsC}
\title{Connect points with path lines in SVG Viewer}
\description{Creates paths by drawing lines between points in the SVG Viewer.}
\usage{
svg.pathsC(path, col = NULL, col.fill = "none", col.stroke = "black", 
               z.index = 0, layer = "", label = "", lwd = 1, 
               opacity.stroke = 1, opacity.fill = 1, index.add = 0, 
               file=NULL)
}
\arguments{
  \item{path}{A vector, matrix or list of integers specifying the points to be connected by lines. See Details.}
  \item{col}{The fill and stroke color of the points(s). If non-\code{NULL}, \code{col} overrides \code{col.fill} and \code{col.stroke}.}
  \item{col.fill}{The fill color of the path(s).}
  \item{col.stroke}{The stroke (border) color of the path line(s).}
  \item{z.index}{A number indicating the relative order in which the SVG object will be drawn in the viewer. Higher numbers correspond to closer to the front or top.}
  \item{layer}{A text string indicating the layer in which the SVG object belongs (not yet fully implemented).}
  \item{label}{A label to be added to the SVG object in SVG tag.}
  \item{lwd}{The thickness of the path line(s).}
  \item{opacity.stroke}{A number between \code{0} and \code{1} indicating the opacity of the border of the path line(s).}
  \item{opacity.fill}{A number between \code{0} and \code{1} indicating the opacity of the fill of the path line(s).}
  \item{index.add}{An integer to add to all indices in \code{path}.}
  \item{file}{File path (having the extenstion ".html") to add lines to a current SVG Viewer file. By default (i.e. \code{NULL}) lines are added to the SVG Viewer opened by \code{\link{svg.new}}.}
}
\details{
This function creates SVG paths by drawing lines between points that have been written to a SVG Viewer through a separate function call. This is particularly useful when animated points have been written to the SVG Viewer and the user simply wants to create paths defined by the animated points. Since paths are drawn, not simply lines, the fill color (\code{col.fill}) and fill opacity (\code{opacity.fill}) of the path can also be specified. Whether the paths written by this function are animated depends on the points that make up the path. If the points making up the path are animated, the path will follow the motion of its constitutive points.

The input \code{path} can be a vector, matrix or list of integers. The integers indicate which points should be joined by lines and in what order; these integers correspond to the points in the same order in which they were written to the SVG Viewer, starting with \code{1}. Thus, \code{1} corresponds to the first point written to the SVG Viewer, \code{2} corresponds to the second point written to the SVG Viewer, etc. If \code{path} is a vector, a single path is drawn connecting the points corresponding to the indices in \code{path}. If \code{path} is a matrix, a separate path is drawn for each matrix row, connecting the points corresponding to the indices in each row. Similarly, if \code{path} is a list, a separate path is drawn for each list element, connecting the points corresponding to the indices in each list element.

The graphical parameters \code{col}, \code{z.index}, \code{layer}, \code{label}, \code{lwd}, and \code{opacity} can all be vectors of length one or of the same length as the number of paths to be drawn. This allows different parameters to be specified for each path.
}
\value{NULL}
\author{Aaron Olsen}
\seealso{
\code{\link{svg.new}}, 
\code{\link{svg.lines}}, 
\code{\link{svg.points}}
}
\examples{
\dontrun{
## Transform a circle into an ellipse
# Create new viewer
svg.new(file='svgviewr.html', animate.reverse=TRUE, animate.duration=1)

# Create points
n <- 100
x <- array(NA, dim=c(100, 2, n))
x_seq <- seq(-1, 1, length=dim(x)[1]/2)
x_seq <- sin(seq(-pi/2, pi/2, length=dim(x)[1]/2))
n_seq <- seq(1, 3, length=n)
for(i in 1:dim(x)[3])
	x[, , i] <- rbind(cbind(x_seq, n_seq[i]*sqrt(1 - x_seq^2)),
		cbind(x_seq[(dim(x)[1]/2):1], -n_seq[i]*sqrt(1 - x_seq[(dim(x)[1]/2):1]^2)))

# Draw points
svg.points(x, cex=1, lwd=1, col="blue")

# Draw paths among points
svg.pathsC(1:dim(x)[1], col.fill="blue", opacity.fill=0.1, 
	col.stroke="green", lwd=2, z.index=-1)

# Close viewer connection
svg.close()

# Open svgviewr.html to visualize
}
}