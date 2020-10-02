\name{svg.mesh}
\alias{svg.mesh}
\title{Write mesh to Viewer}
\description{Draws mesh in Viewer.}
\usage{
svg.mesh(file=NULL, name=NULL, col='#F5F5F5', emissive='black',
         material = c('auto', 'lambert', 'phong')[1],
         opacity=1, ontop=FALSE, get.lim=TRUE,
         scaling = 1, debug = FALSE, vertex.normals = NULL,
         face.normals = NULL, vertex.labels = NULL,
         vertex.spheres = NULL, dbl.side = c('auto', TRUE, FALSE)[1])
}
\arguments{
  \item{file}{A mesh file in the .obj or .json.}
  \item{name}{The name of the mesh. By default, name is \code{NULL}. This is used when applying transformations to drawn objects.}
  \item{col}{The mesh color.}
  \item{emissive}{The mesh emissive color.}
  \item{material}{The mesh material. This affects how light reflects off of the mesh surface.}
  \item{opacity}{The mesh opacity. A value of 1 (default) is fully opaque.}
  \item{ontop}{Whether the mesh should appear on top of all other objects in the viewer (to manually set order in cases of incorrect ordering due to partial opacities).}
  \item{get.lim}{A logical indicating whether the limits of the mesh should be found and returned. This can be used to position other objects relative to the mesh bounds. This will be done at a later point (if not done when \code{svg.mesh} is called) so it does not save processing time to set this to \code{FALSE}.}
  \item{scaling}{A scaling factor for the mesh, default is 1.}
  \item{debug}{A logical indicating whether to draw the mesh in debug mode. Debug mode will label the vertices. This is useful if you create your own mesh.}
  \item{vertex.normals}{A logical indicating whether to draw vertex normals. If \code{debug} is \code{TRUE} this will default to \code{TRUE}. Otherwise default is \code{FALSE}.}
  \item{face.normals}{A logical indicating whether to draw face normals. If \code{debug} is \code{TRUE} this will default to \code{TRUE}. Otherwise default is \code{FALSE}.}
  \item{vertex.labels}{A logical indicating whether to draw vertex labels. If \code{debug} is \code{TRUE} this will default to \code{TRUE}. Otherwise default is \code{FALSE}.}
  \item{vertex.spheres}{Whether to draw the vertices as spheres.  If \code{debug} is \code{TRUE} this will default to \code{TRUE}. Otherwise default is \code{FALSE}.}
  \item{dbl.side}{Whether mesh faces are double sided.}
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
svg.mesh(file='mesh.obj')

# Close connection
svg.close()
}
}