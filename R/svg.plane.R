svg.plane <- function(corners, col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, name = 'sphere', 
	seg = 30, ontop = FALSE, create.uvs = FALSE, return.shape = FALSE, plot = TRUE){

	# Make sure that type is webgl
	#if('live' != getOption("svgviewr_glo_type")) stop("Plane drawing is currently only available with webgl svgViewR output.")

	# Duplicate single segment value
	if(length(seg) == 1) seg <- rep(seg, 2)
	
	# Create plane mesh
	plane_mesh <- create_plane_mesh(corners=corners, seg=seg, create.uvs=create.uvs)
	
	# Get vertices and faces
	vertices <- plane_mesh$vertices
	faces <- plane_mesh$faces
	
	# If not plotting, return vertices and faces
	if(!plot) return(list('vertices'=vertices, 'faces'=faces))

	if('svg' == getOption("svgviewr_glo_type")){

		svg.points(vertices[1,], col='red')
		svg.points(vertices[2:(nrow(vertices)-1), ])
		svg.points(vertices[nrow(vertices),], col='blue')
		
		svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=0.8)
		svg.text(vertices, labels=paste0(signif(plane_mesh$vertices_norm[,1],2), ',', signif(plane_mesh$vertices_norm[,2],2)), font.size=0.3, col='red')

		# Draw faces
		faces <- faces[!is.na(faces[,1]), ]
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

	if(return.shape){
		return(list('vertices'=vertices, 'faces'=faces))
	}else{
		# Suppress return of value in console
		return(NULL)
	}
}
