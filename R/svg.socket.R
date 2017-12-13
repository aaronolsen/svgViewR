svg.socket <- function(center = NULL, axes = NULL, outer.radius = 1, inner.radius = 0.8*outer.radius, 
	outer.col='blue', inner.col=outer.col, ring.col=outer.col, emissive=rgb(0.03, 0.15, 0.21), 
	name = 'socket', seg = 40, portion=0.5){

	# Make sure that type is webgl
	#if('webgl' != getOption("svgviewr_glo_type")) stop("Socket drawing is currently only available with webgl svgViewR output.")

	# Duplicate single segment value
	if(length(seg) == 1) seg <- rep(seg, 2)

	# Create mesh list
	mesh_list <- list()

	# Create sphere mesh
	mesh_list[['outer_sphere']] <- create_sphere_mesh(center=center, radius=outer.radius, axes=axes, 
		seg=seg, portion=portion)
	mesh_list[['outer_sphere']]$col <- outer.col
	mesh_list[['inner_sphere']] <- create_sphere_mesh(center=center, radius=inner.radius, axes=axes, 
		seg=seg, portion=portion)
	mesh_list[['inner_sphere']]$col <- inner.col

	# Get vertices of ring between inner and outer spheres
	out_vert <- mesh_list[['outer_sphere']]$vertices
	out_ring <- out_vert[(nrow(out_vert)-seg[2]+1):nrow(out_vert), ]

	in_vert <- mesh_list[['inner_sphere']]$vertices
	in_ring <- in_vert[(nrow(in_vert)-seg[2]+1):nrow(in_vert), ]

	# Create ring faces matrix
	faces <- matrix(NA, 2*nrow(out_ring), 3)
	
	# Fill faces matrix
	faces[1:nrow(out_ring), 1] <- 0:(nrow(out_ring)-1)
	faces[c(nrow(out_ring), 1:(nrow(out_ring)-1)), 2] <- 0:(nrow(out_ring)-1)
	faces[1:nrow(out_ring), 3] <- 0:(nrow(out_ring)-1) + nrow(out_ring)

	add <- nrow(out_ring)
	faces[c(nrow(out_ring), 1:(nrow(out_ring)-1))+add, 1] <- 0:(nrow(out_ring)-1)
	faces[1:nrow(out_ring)+add, 2] <- faces[1:nrow(out_ring)+add, 1] + nrow(out_ring)
	faces[1:nrow(out_ring)+add, 3] <- 0:(nrow(out_ring)-1) + nrow(out_ring)

	# Create ring vertices
	mesh_list[['ring']] <- list()
	mesh_list[['ring']]$vertices <- rbind(out_ring, in_ring)
	mesh_list[['ring']]$faces <- faces
	mesh_list[['ring']]$col <- ring.col

	if('webgl' == getOption("svgviewr_glo_type")){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))
		
		for(mesh_num in 1:length(mesh_list)){
			
			vertices <- mesh_list[[mesh_num]]$vertices
			faces <- mesh_list[[mesh_num]]$faces
			color <- mesh_list[[mesh_num]]$col

			# Add to meshes
			add_at <- length(svgviewr_env$mesh)+1

			# Add vertices
			env$svgviewr_env$mesh[[add_at]] <- list()
			env$svgviewr_env$mesh[[add_at]]$vertices <- t(vertices)
			env$svgviewr_env$mesh[[add_at]]$faces <- t(faces)
			env$svgviewr_env$mesh[[add_at]]$col <- webColor(color)
			env$svgviewr_env$mesh[[add_at]]$emissive <- webColor(emissive)
			env$svgviewr_env$mesh[[add_at]]$computeVN <- TRUE

			# Add object reference data
			env$svgviewr_env$ref$names <- c(env$svgviewr_env$ref$names, name)
			env$svgviewr_env$ref$num <- c(env$svgviewr_env$ref$num, add_at)
			env$svgviewr_env$ref$type <- c(env$svgviewr_env$ref$type, 'mesh')

			# Add limits
			obj_ranges <- apply(vertices, 2, 'range', na.rm=TRUE)
		
			# Set corners
			corners <- lim2corners(obj_ranges)
		
			# Add limits to object
			env$svgviewr_env$mesh[[add_at]][['lim']] <- obj_ranges
			env$svgviewr_env$mesh[[add_at]][['corners']] <- corners
		}

	}else{

		add <- 0
		for(mesh_num in 1:length(mesh_list)){

			vertices <- mesh_list[[mesh_num]]$vertices
			faces <- mesh_list[[mesh_num]]$faces

			svg.points(vertices[1,], col='red')
			svg.points(vertices[2:(nrow(vertices)-1), ])
			svg.points(vertices[nrow(vertices),], col='blue')

			svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=0.5)
			
			# Shift faces
			faces <- faces + add

			# Draw faces
			faces <- cbind(faces, faces[,1])
			svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)
			
			add <- add + nrow(vertices)
		}
	}

	# Suppress return of value in console
	ret = NULL
}