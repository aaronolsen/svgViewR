\name{svg.mesh}
\alias{svg.mesh}
\title{Write mesh to Viewer}
\description{Draws mesh in Viewer.}
\usage{
svg.mesh(file=NULL, 
         name=gsub('[.][A-Za-z]+$', '', tail(strsplit(file, '/')[[1]], 1)), 
         col='#F5F5F5', emissive='black', opacity=1, get.lim=TRUE)
}
\arguments{
  \item{file}{Preferably a mesh file in the .json format (see \code{\link{objToJSON}}). An .obj file can be input but this will take longer to run since the function will first convert the OBJ to JSON. If \code{\link{svg.new}} is called without a filename (loading the Viewer as a local server), you will be prompted to create a .json file since the server must read a .json file.}
  \item{name}{The name of the mesh. By default, the filename is used (without the file extension). This is used when applying transformations to drawn objects.}
  \item{col}{The mesh color.}
  \item{emissive}{The mesh emissive color.}
  \item{opacity}{The mesh opacity. A value of 1 (default) is fully opaque.}
  \item{get.lim}{A logical indicating whether the limits of the mesh should be found and returned. This can be used to position other objects relative to the mesh bounds. This will be done at a later point (if not done when \code{svg.mesh} is called) so it does not save processing time to set this to \code{FALSE}.}
}
\details{
This function only works in the webgl plotting mode (i.e. when the \code{mode} parameter of \code{\link{svg.new}} is 'webgl'). This function adds a mesh to the Viewer. See \code{\link{readOBJ}} for more details and limitations on what meshes can be plotted.
}
\value{
If \code{get.lim} is \code{TRUE}, a list with the following elements:
	\item{lim}{The minimum and maximum values of each dimension of the mesh vertices.}
	\item{corners}{The eight corners of the bounding box surrounding the mesh.}
}
\author{Aaron Olsen}
\seealso{
\code{\link{readOBJ}}, 
\code{\link{objToJSON}}
}
\examples{
\dontrun{
# Create new viewer
svg.new(mode='webgl')

# Add mesh
svg.mesh(file='mesh.json')

# Close connection
svg.close()
}
}