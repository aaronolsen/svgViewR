svg.sphere <- function(center = NULL, ends = NULL, radius = NULL, width = NULL, axes = NULL, 
	col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, name = 'sphere', seg = 40, ontop = FALSE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")

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

	if('svg' == getOption("svgviewr_glo_type")){

		svg.points(vertices[1,], col='red')
		svg.points(vertices[2:(nrow(vertices)-1), ])
		svg.points(vertices[nrow(vertices),], col='blue')
		
		#svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=0.8)

		# Draw faces
		faces <- faces[!is.na(faces[,1]), ]
		#print(faces)
		faces <- cbind(faces, faces[,1])
		svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)

	}else{

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add to meshes
		add_at <- length(svgviewr_env$svg$mesh)+1

		# Add vertices
		svgviewr_env$svg$mesh[[add_at]] <- list()
		svgviewr_env$svg$mesh[[add_at]]$name <- name
		svgviewr_env$svg$mesh[[add_at]]$vertices <- t(vertices)
		svgviewr_env$svg$mesh[[add_at]]$faces <- t(faces)
		svgviewr_env$svg$mesh[[add_at]]$col <- setNames(webColor(col), NULL)
		svgviewr_env$svg$mesh[[add_at]]$emissive <- setNames(webColor(emissive), NULL)
		svgviewr_env$svg$mesh[[add_at]]$opacity <- setNames(opacity, NULL)
		svgviewr_env$svg$mesh[[add_at]]$computeVN <- TRUE
		svgviewr_env$svg$mesh[[add_at]]$parseModel <- FALSE
		svgviewr_env$svg$mesh[[add_at]]$depthTest <- !ontop

		# Add object reference data
		svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
		svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
		svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'mesh')

		# Add limits
		obj_ranges <- apply(vertices, 2, 'range', na.rm=TRUE)
		
		# Set corners
		corners <- lim2corners(obj_ranges)
		
		# Add limits to object
		svgviewr_env$svg$mesh[[add_at]][['lim']] <- obj_ranges
		svgviewr_env$svg$mesh[[add_at]][['corners']] <- corners
	}

	# Suppress return of value in console
	ret = NULL
}
