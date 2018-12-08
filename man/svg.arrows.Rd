\name{svg.arrows}
\alias{svg.arrows}
\title{Write arrows to Viewer}
\description{Draws static and animated arrow(s) in Viewer.}
\usage{
svg.arrows(x, y=NULL, name="arrow", col="black", z.index=0, layer="", 
           label="", lwd=1, len='auto', angle=0.4, opacity=1, file=NULL)
}
\arguments{
  \item{x}{For a single static arrow, a 2x3 matrix in which the rows are the start and end point in 3D. For a single animated arrow, a 2x3xi array in which the first two dimensions specify the start and end point of the arrow and i is the number of animation iterations. For multiple static arrows, a list of length n, each element being a 2x3 matrix, in which n is the number of arrows. For multiple animated arrows, a list of length n, each element being a 2x3xi array indicating the start and end points of each arrow over i iterations.}
  \item{y}{If \code{x} is a vector, the coordinates on the y-axis.}
  \item{name}{The name of the drawn object.}
  \item{col}{The stroke color of the arrow.}
  \item{z.index}{A number indicating the relative order in which the object will be drawn in the viewer. Higher numbers correspond to closer to the front or top.}
  \item{layer}{A text string indicating the layer in which the object belongs (not yet fully implemented).}
  \item{label}{A label to be added to the object in SVG tag.}
  \item{lwd}{The thickness of the arrow.}
  \item{len}{The length of the arrowhead.}
  \item{angle}{The angle (in radians) between the arrowhead lines and the main shaft of the arrow.}
  \item{opacity}{The opacity of the arrow.}
  \item{file}{File path (having the extenstion ".html") to add lines to a current Viewer file. By default (i.e. \code{NULL}) lines are added to the Viewer opened by \code{\link{svg.new}}.}
}
\details{
This function accepts many different input types for \code{x} to plot four different arrow types: a single static arrow, multiple static arrows, a single animated arrow, and multiple animated arrows. For worked examples, please see \href{https://aaronolsen.github.io/tutorials/3d_visualization/plot_arrows.html}{Plotting arrows with svgViewR}. The first of these worked examples is included in the examples below.
}
\value{NULL}
\author{Aaron Olsen}
\seealso{
\code{\link{svg.new}}, 
\code{\link{svg.lines}}
}
\examples{
\dontrun{
## Create arrow
arrow <- rbind(rep(0,3), rep(1,3))

# Open a connection to .html file
svg.new(file='plot_static_arrow.html')

# Add arrow
svg.arrows(arrow)

# Add a coordinate axis planes around the arrow
svg_frame <- svg.frame(arrow)

# Close the file connection
svg.close()
}
}