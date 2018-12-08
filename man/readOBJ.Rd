\name{readOBJ}
\alias{readOBJ}
\alias{print.obj}
\title{Reads an OBJ file}
\description{Reads a mesh file in the OBJ format and returns an object of class 'obj'}
\usage{
readOBJ(file, scaling = 1)

\method{print}{obj}(x, ...)
}
\arguments{
  \item{file}{A filepath to a file of type '.obj'.}
  \item{scaling}{A scaling factor to be applied to the vertices.}
  \item{x}{an object used to select a method.}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
This function convert a Wavefront .obj file or OBJ object into a JSON (JavaScript Object Notation) object and optionally saves this JSON object as a .json file. This function only provides basic functionality, reading only vertices, normals, and faces. It has only been tested on the .obj files returned from the DICOM Medical Image Viewer Horos (formerly Osirix). This function was written in order directly convert CT scan meshes exported from Horos into the JSON format for web visualization, eliminating intermediate conversions in programs such as meshlab and Blender.
}
\value{
a list of class \code{"obj"} with the following elements:
	\item{vertices}{Mesh vertices.}
	\item{normals}{Normal vectors, at each vertex.}
	\item{faces}{Mesh faces (indices to vertices).}
	\item{metadata}{Mesh metadata.}
}
\author{Aaron Olsen}
\seealso{
\code{\link{objToJSON}}
}
\examples{
\dontrun{
# Read an .obj file
obj <- readOBJ('obj_file.obj')

# Print obj object
print(obj)
}
}