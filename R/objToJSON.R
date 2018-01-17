objToJSON <- function(obj){

	obj$vertices <- t(obj$vertices)
	obj$normals <- t(obj$normals)
	faces <- matrix(NA, nrow=nrow(obj$faces), ncol=7)
	faces[, 1] <- 32
	faces[, 2:4] <- obj$faces - 1
	faces[, 5:7] <- obj$faces - 1
	obj$faces <- t(faces)

	to_json <- toJSON(obj)

	to_json <- gsub('"uvs":null', '"uvs":[]', to_json)
	to_json <- gsub('"metadata":[{]', '"metadata":{\n\t\t', to_json)
	to_json <- gsub('([0-9]),"', '\\1,\n\t\t"', to_json)
	to_json <- gsub('[{]"', '{\n\t"', to_json)
	to_json <- gsub('],', '],\n\t', to_json)
	to_json <- gsub(']}', ']\n}', to_json)
	to_json <- gsub('[}]{2}', '\n\t}\n}', to_json)

	
	to_json
}