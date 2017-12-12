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

	# Set width
	if(is.null(width) && !is.null(radius)) width <- rep(radius, 3)

	# Set default axes
	if(is.null(ends) && is.null(axes)) axes <- diag(3)

	if(!is.null(ends) && is.null(axes)){
		axes <- matrix(NA, 2, 3)
		axes[1,] <- ends[2,]-ends[1,]
		axes[2,] <- vorthogonal_svg(axes[1,])
	}
	
	# Make sure axes are unit vectors
	axes <- uvector_svg(axes)
	
	# Make sure axes is matrix
	if(is.vector(axes)) axes <- matrix(axes, 1, 3)

	# Set other variables in first input case
	if(!is.null(center) && !is.null(width)){

		# Set ends
		ends <- matrix(NA, 2, 3)
		ends[1,] <- center - (width[1]/2)*axes[1,]
		ends[2,] <- center + (width[1]/2)*axes[1,]
	}

	# Make sure ends is matrix
	if(is.vector(ends)) ends <- matrix(ends, 1, 3)

	# Add second row to ends if not given
	if(nrow(ends) == 1) ends <- rbind(ends, ends+width[1]*uvector_svg(axes[1,]))

	# Set first axis to ends if only one given
	if(nrow(axes) == 1) axes <- rbind(uvector_svg(ends[2,]-ends[1,]), axes)

	# Set third axis if only two given
	if(nrow(axes) == 2) axes <- rbind(axes, uvector_svg(cprod_svg(axes[1,], axes[2,])))

	# Add first width from ends if width length is 2
	if(is.null(width)) width <- rep(sqrt(sum((ends[2,]-ends[1,])^2)), 3)
	if(length(width) == 1) width <- rep(width, 3)
	if(length(width) == 2) width <- c(sqrt(sum((ends[2,]-ends[1,])^2)), width)

	# Duplicate single segment value
	if(length(seg) == 1) seg <- rep(seg, 2)

	# Create vertices matrix
	n_vertices <- 2+(seg[1]-1)*seg[2]
	vertices <- matrix(NA, n_vertices, 3)
	
	# Set points along main axis at which to draw circles/ellipses
	a_ends <- seq(0, pi, length=seg[1]+1)[2:seg[1]]
	
	# Set angle
	T <- seq(0, 2*pi, length=seg[2]+1)[1:seg[2]]

	# Add ends
	vertices[c(1, n_vertices), ] <- ends

	# Fill vertex matrix
	ri <- 2
	for(a_end in a_ends){
	
		# Set center of shape
		t_center <- ends[2, ] - width[1]*((cos(a_end)+1)/2)*axes[1,]

		# Set rows to fill
		fill_rows <- ri:(ri+seg[2]-1)
		
		# Set radius
		width_seg <- width[2:3]
		width_seg[1] <- (width[2]/2)*sin(a_end)
		width_seg[2] <- (width[3]/2)*sin(a_end)

		# Add points
		for(i in 1:length(fill_rows)) vertices[fill_rows[i], ] <- t_center + width_seg[1]*cos(T[i])*axes[2,] + width_seg[2]*sin(T[i])*axes[3,]
		
		ri <- tail(fill_rows, 1)+1
	}

	# Create faces matrix
	n_faces <- 2*seg[2]*((seg[1]-2) + 1)
	faces <- matrix(NA, n_faces, 3)

	# Add end faces
	end_faces <- 1:seg[2]
	faces[end_faces, 1] <- 0
	faces[end_faces, 2] <- end_faces
	faces[c(tail(end_faces,1), end_faces[1]:(tail(end_faces,1)-1)), 3] <- end_faces

	end_faces <- (n_faces-seg[2]+1):n_faces
	end_row <- (n_vertices-seg[2]+1):n_vertices
	faces[end_faces, 1] <- n_vertices-1
	faces[end_faces, 3] <- end_row-2
	faces[c(tail(end_faces,1), end_faces[1]:(tail(end_faces,1)-1)), 2] <- end_row-2

	# Fill faces matrix
	for(i in 1:(seg[1]-2)){

		# Set rows to fill
		fill_start <- ((i-1)*2*seg[2]) + seg[2] + 1
		fill_end <- fill_start + 2*seg[2] - 1
		fill_rows <- fill_start:fill_end
		fill_rows1 <- fill_rows[1:(length(fill_rows)/2)]
		fill_rows2 <- fill_rows[((length(fill_rows)/2)+1):length(fill_rows)]

		# Set indices of first row and second row
		first_row <- (1+(i-1)*seg[2]):(1+(i-1)*seg[2] + seg[2] - 1)
		second_row <- (1+i*seg[2]):((i+1)*seg[2])
		#print(first_row)
		#print(second_row)
		
		# 
		faces[fill_rows1, 1] <- first_row
		faces[fill_rows1, 2] <- second_row
		faces[c(tail(fill_rows1,1), fill_rows1[1]:(tail(fill_rows1,1)-1)), 3] <- second_row

		faces[fill_rows2, 1] <- first_row
		faces[c(tail(fill_rows2,1), fill_rows2[1]:(tail(fill_rows2,1)-1)), 3] <- first_row
		faces[c(tail(fill_rows2,1), fill_rows2[1]:(tail(fill_rows2,1)-1)), 2] <- second_row
	}

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
