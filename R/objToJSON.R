objToJSON <- function(obj, file = NULL){

	if(is.vector(obj)) obj <- readOBJ(file=obj)

	if(class(obj) == 'obj'){

		obj$vertices <- t(obj$vertices)
		obj$normals <- t(obj$normals)
		faces <- matrix(NA, nrow=nrow(obj$faces), ncol=7)
		faces[, 1] <- 32
		faces[, 2:4] <- obj$faces[, c(1,3,5)] - 1
		faces[, 5:7] <- obj$faces[, c(2,4,6)] - 1
		obj$faces <- t(faces)

		to_json <- toJSON(obj)
	}

	if(!is.null(file)){

		to_json_str <- gsub('"uvs":null', '"uvs":[]', to_json)
		to_json_str <- gsub('"metadata":[{]', '"metadata":{\n\t\t', to_json_str)
		to_json_str <- gsub('([0-9]),"', '\\1,\n\t\t"', to_json_str)
		to_json_str <- gsub('[{]"', '{\n\t"', to_json_str)
		to_json_str <- gsub('],', '],\n\t', to_json_str)
		to_json_str <- gsub(']}', ']\n}', to_json_str)
		to_json_str <- gsub('[}]{2}', '\n\t}\n}', to_json_str)

		write(x=to_json_str, file=file)

	}else{
		
		# Convert to list
		to_json_list <- list()
		
		for(name in names(obj)){
			if(!is.list(obj[[name]])){
				to_json_list[[name]] <- c(obj[[name]])
			}else{
				to_json_list[[name]] <- list()
				for(sub_name in names(obj[[name]])){
					to_json_list[[name]][[sub_name]] <- c(obj[[name]][[sub_name]])
				}
			}
		}
		
		return(to_json_list)
	}
}