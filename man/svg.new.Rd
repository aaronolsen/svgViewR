\name{svg.new}
\alias{svg.new}
\title{Create new Viewer file}
\description{Creates a new Viewer file as an HTML document to which objects can be added, with optional specification of various animation parameters.}
\usage{
svg.new(file = NULL, window.title="svgViewR", animate.duration = 1, 
        animate.speed = 1, interpolate = TRUE, timeline = TRUE, mode = c('svg', 'webgl'), 
        animate.reverse = FALSE, animate.repeat = -1, margin = 20, col = "white", 
        times = NULL, clock = FALSE, stats = FALSE, panel = FALSE, show.control = TRUE, 
        start.rotate = TRUE, rotate.speed = 1.2, camera.near = 0.01, fov = 45,
        zoom.speed = 1, pan.speed = 0.2, layers = NULL, connection = TRUE, 
        close.on.done = TRUE, file.type = NULL, app.dir.src = NULL, debug = FALSE, 
        src.link = NULL)
}
\arguments{
  \item{file}{File path and name (having the extenstion ".html") where Viewer will be created. If file is \code{NULL} then the viewer will open over a local server in the default web browser and mode 'webgl' will be automatically selected.}
  \item{window.title}{The Viewer title, visible at the top of the web browser window.}
  \item{animate.duration}{Only used in 'svg' mode. The approximate duration in seconds of the animation. When the number of objects to be displayed is large, the actual duration might exceed this number.}
  \item{animate.speed}{Only used in 'webgl' mode. The relative speed at which the animation will playback. For example, to play the animation at half the real speed, \code{animate.speed} would be 0.5.}
  \item{interpolate}{Whether transformations should be interpolated between input transformations (i.e. keyframes).}
  \item{timeline}{If the visualization includes an animation, whether the timeline is viewable. The timeline includes controls for navigating through the animation (play, pause, change animation speed, etc.).}
  \item{mode}{Whether to draw using the old mode ('svg') or the new mode ('webgl'). See details.}
  \item{animate.reverse}{A logical indicating whether the animation is to be played in reverse after each iteration. Only used in 'svg' mode.}
  \item{animate.repeat}{An integer specifying the number of times the animation will repeat. A value of \code{-1} will cause the animation to repeat indefinitely. Only used in 'svg' mode.}
  \item{margin}{Margin when resizing visualization to fit the browser window.}
  \item{col}{Viewer background color.}
  \item{times}{If including animated shapes without svg.transform, this parameter can be used to input time points corresponding to each frame of the animation.}
  \item{clock}{Whether a clock should be visible or not.}
  \item{stats}{Whether processing stats should be visible or not.}
  \item{panel}{Whether a panel is visible showing all objects by name with a toggle option to show or hide.}
  \item{show.control}{Whether control panel should be visible or not. Only used in 'svg' mode.}
  \item{start.rotate}{Whether visualization should start with 'rotate' enabled or 'translate' enabled. Only used in 'svg' mode.}
  \item{rotate.speed}{How much the camera rotates in response to mouse click and drag. Only used in 'webgl' mode.}
  \item{camera.near}{Distance of the camera to the plotted objects.}
  \item{fov}{Field of view of the camera. This will determine the strength of perspective effects.}
  \item{zoom.speed}{How much the camera zooms in response to mouse click and drag. Only used in 'webgl' mode.}
  \item{pan.speed}{How much the camera pans in response to mouse click and drag. Only used in 'webgl' mode.}
  \item{layers}{Not yet fully enabled.}
  \item{connection}{Whether to open a file connection or create a closed file.}
  \item{close.on.done}{If screenshots are being saved as images, whether browser should close after saving out screenshots as images.}
  \item{file.type}{If screenshots are being saved as images, the type of image (e.g. jpeg, jpg, tiff, png).}
  \item{app.dir.src}{A filepath to the code source files for use when running package from source (used for debugging only).}
  \item{debug}{Whether to run viewer in debug mode (used for debugging only).}
  \item{src.link}{Whether to include javascript code as a source links rather than directly embedded in html file (used for debugging only).}
}
\details{
This function is used to initialize a new Viewer. Before adding shapes to a Viewer, this function is called to create the HTML file to which the objects can be added. svgViewR is currently undergoing a significant overhaul. The previous plotting using SVG (scalable vector graphics) is being replaced with visualizations created using WebGL (the Web Graphics Library) and the javascript library three.js. All backward compatability with the svg format should be maintained. To use the new plotting mode, set the \code{mode} parameter to 'webgl'. Plotting in the 'webgl' mode is limited as I have only begun implementing it.

The 'svg' (old) mode has a single visualization type: an .html file. The 'webgl' mode (new) has two visualization types: a local server based visualization (using the R package Rook) and an .html file. The .html file output is ideal if you want to create portable files that can be easily visualized anytime and shared without the need for hosting a server. The server output is ideal if you want to visualize many different tranformations of large mesh files; creating files for each visualization would take up a large amount of space because all of the mesh specifications (vertices, normals, faces) would have to be contained within each html file.

For worked examples, please see \href{https://aaronolsen.github.io/tutorials/visualization3d.html}{3D visualization in R}. Here are common interactive commands between the two modes:

	\itemize{
		\item \bold{spacebar} : Pauses and plays the animation
		\item \bold{browser refresh} : Returns shapes to state when browser was originally opened
		\item \bold{scroll up/down} : Zoom in/out by moving the shapes into and out of the screen
	}

To rotate the camera in 'webgl' mode, left-click and drag the mouse. To pan the camera in 'webgl' mode, right-click and drag the mouse. For a key to the interactive commands in the 'svg' (old) mode, see \href{https://aaronolsen.github.io/software/svgviewr/interactive.html}{svgViewR Interactive Commands}.
}
\value{NULL}
\author{Aaron Olsen}
\seealso{
\code{\link{svg.close}}
}
\examples{
\dontrun{
# Set number of points to draw
n <- 300

# Create a cloud of normally distributed 3D points
points3d <- cbind(rnorm(n, sd=3), rnorm(n, sd=2), rnorm(n, sd=1))

# Open a connection to .html file
svg.new(file='plot_static_points.html')

# Get distance of points from the center of point cloud
pdist <- sqrt(rowSums((points3d - matrix(colMeans(points3d), n, 3, byrow=TRUE))^2))

# Set color gradient between red and blue
colfunc <- colorRampPalette(c('red', 'blue'))

# Set desired number of colors along gradient
col_grad <- colfunc(50)

# Scale distances to indices and find corresponding color along gradient
col <- col_grad[(length(col_grad)-1)*(pdist - min(pdist)) / diff(range(pdist))+1]

# Add points to file
svg.points(points3d, col=col)

# Add a coordinate axis planes around the points
svg_frame <- svg.frame(points3d)

# Close the file connection
svg.close()
}
}