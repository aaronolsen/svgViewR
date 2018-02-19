writeOBJ <- function(obj, file){

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
	vertices_normals <- rep(NA, length=n_vertices+n_normals)
	vertices_normals[seq(1,length(vertices_normals),by=2)] <- vn
	vertices_normals[seq(2,length(vertices_normals),by=2)] <- v
	
	# Create list of faces
	face_mat <- matrix(' ', nrow=n_faces, ncol=11)
	face_mat[,c(1,3)] <- obj$faces[,1]
	face_mat[,2] <- '//'
	face_mat[,c(5,7)] <- obj$faces[,2]
	face_mat[,6] <- '//'
	face_mat[,c(9,11)] <- obj$faces[,3]
	face_mat[,10] <- '//'
	faces <- paste0('f ', apply(face_mat, 1, 'paste0', collapse=''))

	# Combine to get lines to write to file
	write_lines <- paste0(header, paste0(vertices_normals, collapse='\n'), '\n\n', 
		paste0(faces, collapse='\n'), '\n# End of File')

	# Write to file
	write(x=write_lines, file=file)
}