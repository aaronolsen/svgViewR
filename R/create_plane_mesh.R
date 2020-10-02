create_plane_mesh <- function(corners, seg=c(1,1), create.uvs = FALSE, vertices = NULL, faces = NULL, rev.normals = FALSE){

	## On March 7, 2020 I flipped the direction of the vertex normals so they match the direction of the face normals
	# From 	normals <- matrix((norm1+norm2)/2, nrow=n_vertices, ncol=3, byrow=TRUE)
	# To 	normals <- -matrix((norm1+norm2)/2, nrow=n_vertices, ncol=3, byrow=TRUE)
	# Not sure if this will cause issues downstream

	# Create empty faces matrix if NULL
	if(is.null(vertices)) vertices <- matrix(NA, 0, 3)
	if(is.null(faces)) faces <- matrix(NA, 0, 3)
	if(length(seg) == 1) seg <- rep(seg, 2)

	# Set number of vertices
	vertex_ct <- seg+1
	n_vertices <- prod(vertex_ct)

	# Find edge vertices
	edge1_vert <- edge3_vert <- matrix(NA, nrow=vertex_ct[1], ncol=3)
	for(i in 1:3) edge1_vert[,i] <- seq(corners[1,i], corners[2,i], length=vertex_ct[1])
	for(i in 1:3) edge3_vert[,i] <- seq(corners[4,i], corners[3,i], length=vertex_ct[1])

	# Create new_vertices
	new_vertices <- matrix(NA, nrow=n_vertices, ncol=3)
	for(i in 1:vertex_ct[1]){
		start_rows <- ((i-1)*(vertex_ct[2])+1)
		add_rows <- start_rows:(start_rows+vertex_ct[2]-1)
		for(j in 1:3) new_vertices[add_rows,j] <- seq(edge1_vert[i,j], edge3_vert[i,j], length=vertex_ct[2])
	}
	
	# Get number of faces
	n_faces <- 2*prod(seg)
	
	# Create faces matrix
	new_faces <- matrix(NA, n_faces, 3)
	
	# Get normal vectors
	norm1 <- cprod_svg(corners[2,]-corners[1,], corners[3,]-corners[1,])
	norm2 <- cprod_svg(corners[2,]-corners[1,], corners[4,]-corners[1,])
	if(abs(avec_svg(norm1, norm2)) > pi/2) norm1 <- -norm1
	normals <- -matrix((norm1+norm2)/2, nrow=n_vertices, ncol=3, byrow=TRUE)
	
	# Fill new_faces matrix
	n <- 1
	for(i in 0:(vertex_ct[2]-2)){
		
		cross_idx <- seq(i, n_vertices-vertex_ct[2]+i, by=vertex_ct[2])
		for(j in 1:(length(cross_idx)-1)){
			
			new_faces[n, ] <- c(cross_idx[j], cross_idx[j]+1, cross_idx[j+1])
			n <- n + 1

			new_faces[n, ] <- c(cross_idx[j]+1, cross_idx[j+1]+1, cross_idx[j+1])
			n <- n + 1
		}
	}
	
	# Add new vertices and faces to existing
	faces <- rbind(faces, new_faces+nrow(vertices))
	vertices <- rbind(vertices, new_vertices)

	if(rev.normals){
		normals <- -normals
		faces <- faces[, 3:1]
	}

	# Output vertices and faces
	if(!create.uvs) return(list('vertices'=vertices, 'faces'=faces, 'normals'=normals))

	if(create.uvs){
		vertices_v <- rep(seq(0, 1, length=vertex_ct[2]), vertex_ct[1])
		vertices_u <- c(matrix(rep(seq(0, 1, length=vertex_ct[1]), vertex_ct[2]), nrow=vertex_ct[2], ncol=vertex_ct[1], byrow=TRUE))
	}

	uvs <- matrix(NA, nrow=n_faces, ncol=6)

	for(i in 1:nrow(faces)){
		uvs[i, ] <- c(vertices_u[faces[i,1]+1], vertices_v[faces[i,1]+1], 
			vertices_u[faces[i,2]+1], vertices_v[faces[i,2]+1], 
			vertices_u[faces[i,3]+1], vertices_v[faces[i,3]+1])
	}

	# Output vertices and faces
	list('vertices'=vertices, 'faces'=faces, 'normals'=normals, 'uvs'=uvs, 'vertices_norm'=cbind(vertices_u, vertices_v))
}
