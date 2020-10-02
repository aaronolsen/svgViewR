svg.cylinder <- function(ends=NULL, center=NULL, radius=1, axis=NULL, 
	length=NULL, rseg=20, hseg=2, open.ended=FALSE, theta.start=0, theta.length=2*pi, col='blue', 
	emissive=rgb(0.03, 0.15, 0.21), opacity = 1, ontop = FALSE, name='cylinder'){

	# Check that only single name given
	if(length(name) > 1) stop("Input parameter 'name' is a vector of length greater than one. 'name' must be of length 1.")

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")

	## Create mesh
	if(!is.null(axis)){

		# Convert from array
		if(length(dim(axis)) == 3) axis <- axis[,,1]

		# Make sure vector is unit length
		axis <- uvector_svg(axis)
		
		# Make sure axis is matrix
		if(is.vector(axis)) axis <- matrix(axis, 1, 3)
	}

	# Center input
	if(!is.null(center)){

		#
		if(!is.null(ends)) stop("'ends' and 'center' cannot both be non-NULL")
		
		# Check that vector and length are specified
		if(is.null(axis) || is.null(length)) stop("If 'center' is non-NULL then 'axis' and 'length' must both be non-NULL.")

		# Set ends from center
		ends <- rbind(
			center + (length/2)*axis[1,],
			center - (length/2)*axis[1,]
		)
	}

	# If ends is single point, use axis to find other end point
	if(is.vector(ends) || nrow(ends) == 1){
	
		# Check that vector and length are specified
		if(is.null(axis) || is.null(length)) stop("If 'ends' is a single point then 'axis' and 'length' must both be non-NULL.")

		# Find ends
		ends <- rbind(ends, ends + length*axis)

	}else{
		
		# Find axis, make unit
		axis <- ends[2,]-ends[1,]
		
		# Get length
		length <- sqrt(sum((axis)^2))

		# Make unit
		axis <- uvector_svg(axis)
	}
	
	# Add one to segments for easier vertex creation
	rseg <- rseg

	# Get vector orthogonal to axis
	o_axis <- vorthogonal_svg(axis)

	# Set thetas for end circle
	thetas <- seq(from=theta.start, to=theta.start+theta.length, length=rseg+1)[1:rseg]

	# Create matrix for end points
	ends_circle <- matrix(NA, nrow=rseg, ncol=3)

	# Get points on circumference of cylinder end circle
	for(i in 1:rseg) ends_circle[i, ] <- uvector_svg(o_axis %*% tMatrixEP_svg(axis, thetas[i]))

	# Draw vertices at lengths
	at_lengths <- seq(from=0, to=length, length=2+hseg-1)

	# Create vertices matrix
	n_vertices <- rseg*(hseg+1) + 2
	vertices <- matrix(NA, n_vertices, ncol=3)

	# Add vertices
	vertices[1, ] <- ends[1,]
	vertices[nrow(vertices), ] <- ends[2,]
	for(i in 1:length(at_lengths)){
		at_rows <- 1 + (i-1)*rseg + (1:rseg)
		vertices[at_rows, ] <- radius*ends_circle + matrix(ends[1,]+at_lengths[i]*axis, nrow=rseg, ncol=3, byrow=TRUE)
	}
	
	# Get ends vertices (split up so that vertex normals don't smooth over cylinder edge)
	ends_vertices <- vertices[c(1:(rseg+1), (n_vertices-rseg):n_vertices), ]
	side_vertices <- vertices[2:(n_vertices-1), ]
		
	# Create side faces matrix
	side_faces <- matrix(NA, nrow=2*rseg*hseg, 3)

	# Fill side faces matrix
	for(i in 0:(hseg-1)){

		rows1 <- i*rseg*2 + (1:rseg)
		side_faces[rows1, 1] <- i*rseg + (1:rseg)
		side_faces[rows1, 2] <- side_faces[rows1, 1] + rseg
		side_faces[rows1, 3] <- side_faces[rows1, 2] + 1
		side_faces[tail(rows1,1), 3] <- side_faces[rows1[1], 1] + rseg

		rows2 <- rows1 + rseg
		side_faces[rows2, 1] <- i*rseg + (1:rseg)
		side_faces[rows2, 3] <- side_faces[rows2, 1] + 1
		side_faces[tail(rows2,1), 3] <- side_faces[rows2[1], 1]
		side_faces[rows2, 2] <- side_faces[rows1, 2] + 1
		side_faces[tail(rows2,1), 2] <- side_faces[rows1[1], 1] + rseg
	}
	
	# Shift down since end center is removed
	side_faces <- side_faces - 1

	# Create ends of cylinder - triangles radiating around cylinder
	if(!open.ended){

		# Create faces matrix
		ends_faces <- matrix(NA, 2*rseg, ncol=3)
	
		# Add faces
		ends_faces[1:rseg, 1] <- 0
		ends_faces[1:rseg, 2] <- 1:rseg
		ends_faces[c(rseg,1:(rseg-1)), 3] <- 1:rseg
		ends_faces[rseg, 3] <- 1
		
		add <- rseg
		shift <- -rseg*(hseg-1)
		ends_faces[add + (1:rseg), 1] <- n_vertices-1+shift
		ends_faces[add + (1:rseg), 2] <- (n_vertices-rseg-1):(n_vertices-2)+shift
		ends_faces[add + c(rseg,1:(rseg-1)), 3] <- (n_vertices-rseg-1):(n_vertices-2)+shift
		ends_faces[add + rseg, 3] <- n_vertices-rseg-1+shift

		# Combine faces into one matrix
		faces <- matrix(NA, nrow=2*rseg + 2*rseg*hseg, 3)
		faces[1:(2*rseg), ] <- ends_faces
		faces[(2*rseg+1):nrow(faces), ] <- side_faces

	}else{
		
		# Save sides as face matrix
		ends_faces <- NULL
	}
	
	if('svg' == getOption("svgviewr_glo_type")){

		#svg.points(ends_vertices)
		#svg.text(ends_vertices, labels=0:(nrow(ends_vertices)-1), font.size=0.8)
		#ends_faces <- cbind(ends_faces, ends_faces[,1])
		#svg.pathsC(lapply(seq_len(nrow(ends_faces)), function(i) ends_faces[i,]+1), col='black', opacity.fill=0.2)

		svg.points(side_vertices)
		svg.text(side_vertices, labels=0:(nrow(side_vertices)-1), font.size=0.8)
		side_faces <- cbind(side_faces, side_faces[,1])
		svg.pathsC(lapply(seq_len(nrow(side_faces)), function(i) side_faces[i,]+1), col='black', opacity.fill=0.2)

	}else{

		# Get viewer environment
		#env <- as.environment(getOption("svgviewr_glo_env"))
		
		mesh_list <- list(list('vertices'=side_vertices, 'faces'=side_faces))

		if(!is.null(ends_faces)) mesh_list[[2]] <- list('vertices'=ends_vertices, 'faces'=ends_faces)
		
		for(list_num in 1:length(mesh_list)){
			
			faces <- mesh_list[[list_num]]$faces
			vertices <- mesh_list[[list_num]]$vertices

			# Add to meshes
			add_at <- length(svgviewr_env$svg$mesh)+1

			# Add vertices
			svgviewr_env$svg$mesh[[add_at]] <- list()
			svgviewr_env$svg$mesh[[add_at]]$vertices <- t(vertices)
			svgviewr_env$svg$mesh[[add_at]]$faces <- t(faces)
			svgviewr_env$svg$mesh[[add_at]]$itmat <- diag(4)
			svgviewr_env$svg$mesh[[add_at]]$name <- name
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
			obj_ranges <- apply(vertices, 2, 'range', na.rm=TRUE)
		
			# Set corners
			corners <- lim2corners(obj_ranges)
		
			# Add limits to object
			svgviewr_env$svg$mesh[[add_at]][['lim']] <- obj_ranges
			svgviewr_env$svg$mesh[[add_at]][['corners']] <- corners
		}
	}

	ret = NULL
}