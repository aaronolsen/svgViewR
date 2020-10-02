addMeshes <- function(mesh1, mesh2 = NULL, mesh3 = NULL, mesh4 = NULL, mesh5 = NULL, mesh6 = NULL){

	# Check if first mesh is list of OBJs
	if(is.list(mesh1[[1]])){
		
		# If single mesh return mesh
		if(length(mesh1) == 1) return(mesh1[[1]])

		# Add subsequent meshes
		for(i in 2:length(mesh1)) mesh1[[1]] <- addMeshes(mesh1[[1]], mesh1[[i]])
		
		return(mesh1[[1]])
	}

	# If only one mesh return mesh
	if(is.null(mesh2)) return(mesh1)

	if(!is.null(mesh6)) return(addMeshes(addMeshes(addMeshes(addMeshes(addMeshes(mesh1, mesh2), mesh3), mesh4), mesh5), mesh6))
	if(!is.null(mesh5)) return(addMeshes(addMeshes(addMeshes(addMeshes(mesh1, mesh2), mesh3), mesh4), mesh5))
	if(!is.null(mesh4)) return(addMeshes(addMeshes(addMeshes(mesh1, mesh2), mesh3), mesh4))
	if(!is.null(mesh3)) return(addMeshes(addMeshes(mesh1, mesh2), mesh3))

	# Combine vertices
	vertices <- rbind(mesh1$vertices, mesh2$vertices)

	# Advance indices for faces in mesh2 by number of vertices in mesh1
	mesh2$faces <- mesh2$faces + nrow(mesh1$vertices)

	# Combine faces
	faces <- rbind(mesh1$faces, mesh2$faces)

	# Combine normals
	normals <- rbind(mesh1$normals, mesh2$normals)
	
	# Create new mesh
	new_mesh <- list('vertices'=vertices, 'faces'=faces, 'normals'=normals)
}