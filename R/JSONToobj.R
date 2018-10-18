JSONToobj <- function(json, file = NULL){

	if(!is.list(json)) json <- fromJSON(file=json)

	obj <- list()
	class(obj) <- 'obj'

	# Convert vertices from vector to matrix
	obj$vertices <- matrix(json$vertices, ncol=3, byrow=TRUE)
	obj$normals <- matrix(json$normals, ncol=3, byrow=TRUE)
	obj$faces <- matrix(json$faces, ncol=6, byrow=TRUE)

	if(!is.null(file)) writeOBJ(obj, file=file)

	return(obj)
}