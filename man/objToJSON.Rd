\name{objToJSON}
\alias{objToJSON}
\title{Converts OBJ to JSON}
\description{Converts a mesh file in the OBJ format into the JSON format}
\usage{
objToJSON(obj, file = NULL)
}
\arguments{
  \item{obj}{A filepath to a file of type '.obj' or an 'obj' object, the value returned by the function \code{\link{readOBJ}}.}
  \item{file}{A filepath to a file of type '.json' where the converted obj object will be saved. If is \code{NULL} (default) then no file is written and the JSON object is returned.}
}
\details{
This function convert a Wavefront .obj file or OBJ object into a JSON (JavaScript Object Notation) object and optionally saves this JSON object as a .json file. This function was written in order directly convert CT scan meshes exported from Horos into the JSON format for web visualization, eliminating intermediate conversions in programs such as meshlab and Blender. See \code{\link{readOBJ}} for more details and limitations in .obj file reading.
}
\value{A JSON object, if file is \code{NULL}}
\author{Aaron Olsen}
\seealso{
\code{\link{readOBJ}}
}
\examples{
\dontrun{
# Convert OBJ to JSON file
objToJSON(obj='obj_file.obj', file='json_file.json')

# Convert OBJ to JSON list object
json_list <- objToJSON(obj='obj_file.obj')
}
}