\name{svg.close}
\alias{svg.close}
\title{Closes Viewer connection}
\description{Closes the connection to the Viewer file.}
\usage{
svg.close(wait = FALSE, quiet = TRUE)
}
\arguments{
  \item{wait}{Whether connection should stay open (wait) until the user closes the browser window. This parameter only applies if the \code{file} input to \code{\link{svg.new}} is \code{NULL}.}
  \item{quiet}{Whether message should be printed to console.}
}
\details{
This function closes the connection to the Viewer file opened by \code{\link{svg.new}}. If no file is input to \code{\link{svg.new}} and the mode is 'webgl' then calling \code{svg.close} will signal that all objects have been plotted and the Viewer will open in the default web browser.
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

# Close connection
svg.close()
}
}