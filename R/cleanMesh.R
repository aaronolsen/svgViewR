cleanMesh <- function(mesh){

	# Copy element to return objects
	vertices <- mesh$vertices
	faces <- mesh$faces
	normals <- mesh$normals
	
	## Remove vertices not included in faces
	# Find vertices not included in faces
	if(!is.null(faces)){
		
		# Get unique face indices
		unique_faces <- unique(c(faces))

		# Get vertex numbers
		vertices_num <- 0:(nrow(vertices)-1)
		
		# Get vertices not indexed in faces
		not_in_faces <- vertices_num[!vertices_num %in% unique_faces]
		
		# Remove each vertex
		if(length(not_in_faces) > 0){

			for(i in 1:length(not_in_faces)){
				
				# Find all values greater than value to remove in faces
				faces <- faces - (faces > not_in_faces[i])*1
				
				# Remove row from vertices
				vertices <- vertices[-(not_in_faces[i]+1),]
				if(!is.null(normals)) normals <- normals[-(not_in_faces[i]+1),]
				
				# Shift remaining not in faces since a row was removed from vertices
				not_in_faces[(i+1):length(not_in_faces)] <- not_in_faces[(i+1):length(not_in_faces)] - 1
			}
		}
	}

	## Remove faces if they reference non-existant vertices
	
	list('vertices'=vertices, 'faces'=faces, 'normals'=normals)
}