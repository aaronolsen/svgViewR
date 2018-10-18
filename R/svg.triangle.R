svg.triangle <- function(corners, col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, name = 'triangle', 
	ontop = FALSE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Sphere drawing is currently only available with webgl svgViewR output.")
	
	# Create mesh
	triangle_mesh <- list('vertices'=corners, 'faces'=c(0,1,2))

	# Get vertices and faces
	vertices <- triangle_mesh$vertices
	faces <- triangle_mesh$faces

	if('svg' == getOption("svgviewr_glo_type")){

	}else{

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add to meshes
		add_at <- length(svgviewr_env$svg$mesh)+1

		# Add vertices
		svgviewr_env$svg$mesh[[add_at]] <- list()
		svgviewr_env$svg$mesh[[add_at]]$vertices <- t(vertices)
		svgviewr_env$svg$mesh[[add_at]]$faces <- t(faces)
		svgviewr_env$svg$mesh[[add_at]]$col <- setNames(webColor(col), NULL)
		svgviewr_env$svg$mesh[[add_at]]$opacity <- setNames(opacity, NULL)
		svgviewr_env$svg$mesh[[add_at]]$emissive <- setNames(webColor(emissive), NULL)
		svgviewr_env$svg$mesh[[add_at]]$computeVN <- TRUE
		svgviewr_env$svg$mesh[[add_at]]$parseModel <- FALSE
		svgviewr_env$svg$mesh[[add_at]]$depthTest <- !ontop

		# Add object reference data
		svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
		svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
		svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'mesh')

		# Add limits
		if(!is.null(corners)){
			obj_ranges <- apply(corners, 2, 'range', na.rm=TRUE)
		}else{
			obj_ranges <- apply(vertices, 2, 'range', na.rm=TRUE)
		}
		
		# Set corners
		corners <- lim2corners(obj_ranges)
		
		# Add limits to object
		svgviewr_env$svg$mesh[[add_at]][['lim']] <- obj_ranges
		svgviewr_env$svg$mesh[[add_at]][['corners']] <- corners
	}

	# Suppress return of value in console
	ret = NULL
}