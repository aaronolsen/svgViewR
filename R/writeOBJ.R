writeOBJ <- function(obj, file){

	# Check if obj is a list of objs
	if(is.list(obj[[1]])) obj <- addMeshes(obj)

	# Get numbers of each
	n_vertices <- nrow(obj$vertices)
	n_normals <- nrow(obj$normals)
	n_faces <- nrow(obj$faces)

	# Write header
	header <- paste0('####\n#\n# OBJ File written by svgViewR\n#\n####\n# Object ', 
		basename(file), '\n#\n# Vertices: ', n_vertices, '\n# Normals: ', n_normals, '\n# Faces: ', n_faces, '\n#\n####\n')

	# Create vector of vertices and normals
	vn <- paste0('vn ', apply(obj$normals, 1, 'paste0', collapse=' '))
	v <- paste0('v ', apply(obj$vertices, 1, 'paste0', collapse=' '))
	#vertices_normals <- rep(NA, length=n_vertices+n_normals)
	#vertices_normals[seq(1,length(vertices_normals),by=2)] <- vn
	#vertices_normals[seq(2,length(vertices_normals),by=2)] <- v
	
	# Make faces matrix consistent
	if(ncol(obj$faces) == 3){
		new_faces <- matrix(NA, nrow(obj$faces), 6)
		new_faces[, 1:2] <- obj$faces[, 1]
		new_faces[, 3:4] <- obj$faces[, 2]
		new_faces[, 5:6] <- obj$faces[, 3]
		obj$faces <- new_faces
	}
	
	if(any(obj$faces == 0)) obj$faces <- obj$faces + 1
	
	# Create list of faces
	face_mat <- matrix(' ', nrow=n_faces, ncol=11)
	face_mat[,c(1,3)] <- obj$faces[,c(1,2)]
	face_mat[,2] <- '//'
	face_mat[,c(5,7)] <- obj$faces[,c(3,4)]
	face_mat[,6] <- '//'
	face_mat[,c(9,11)] <- obj$faces[,c(5,6)]
	face_mat[,10] <- '//'
	faces <- paste0('f ', apply(face_mat, 1, 'paste0', collapse=''))

	# Combine to get lines to write to file
	write_lines <- paste0(header, paste0(v, collapse='\n'), '\n', paste0(vn, collapse='\n'), '\n\n', paste0(faces, collapse='\n'), '\n# End of File')
	#write_lines <- paste0(header, paste0(vertices_normals, collapse='\n'), '\n\n', paste0(faces, collapse='\n'), '\n# End of File')

	# Write to file
	write(x=write_lines, file=file)
}