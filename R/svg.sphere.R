svg.sphere <- function(center = NULL, ends = NULL, radius = NULL, width = NULL, axes = NULL, 
	col='blue', emissive=rgb(0.03, 0.15, 0.21), name = 'sphere', seg = 40){

	# Make sure that type is webgl
	#if('webgl' != getOption("svgviewr_glo_type")) stop("Sphere drawing is currently only available with webgl svgViewR output.")

	# Types of input
	#	sphere: center, radius
	#	ellipsoid: center, width
	#	sphere: ends
	#	ellipsoid: end, axes (3), width (3)
	#	ellipsoid: ends, axes (2), width (2)

	# Duplicate single segment value
	if(length(seg) == 1) seg <- rep(seg, 2)
	
	# Create sphere mesh
	sphere_mesh <- create_sphere_mesh(center=center, ends=ends, radius=radius, width=width, axes=axes, seg=seg)
	
	# Get vertices and faces
	vertices <- sphere_mesh$vertices
	faces <- sphere_mesh$faces

	if('webgl' == getOption("svgviewr_glo_type")){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add to meshes
		add_at <- length(svgviewr_env$mesh)+1

		# Add vertices
		env$svgviewr_env$mesh[[add_at]] <- list()
		env$svgviewr_env$mesh[[add_at]]$vertices <- t(vertices)
		env$svgviewr_env$mesh[[add_at]]$faces <- t(faces)
		env$svgviewr_env$mesh[[add_at]]$col <- webColor(col)
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

	}else{

		svg.points(vertices[1,], col='red')
		svg.points(vertices[2:(nrow(vertices)-1), ])
		svg.points(vertices[nrow(vertices),], col='blue')
		
		#svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=0.8)

		# Draw faces
		faces <- faces[!is.na(faces[,1]), ]
		#print(faces)
		faces <- cbind(faces, faces[,1])
		svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)
	}

	# Suppress return of value in console
	ret = NULL
}
