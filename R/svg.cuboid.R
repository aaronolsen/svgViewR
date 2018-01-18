svg.cuboid <- function(ends=NULL, center=NULL, axes=NULL, length=NULL, width=1,
	col='blue', emissive=rgb(0.03, 0.15, 0.21), name='cuboid'){

	# Make sure that type is webgl
	#if('live' != getOption("svgviewr_glo_type")) stop("Cuboid drawing is currently only available with webgl svgViewR output.")

	# If ends is single point, use axis to find other end point
	if(is.null(center)){

		if(is.vector(ends) || nrow(ends) == 1){

			if(is.null(axes) || is.null(length) && !is.null(width)){
				if(length(width) == 3){
					length <- width[1]
					width <- width[2:3]
				}
			}
	
			# Check that vector and length are specified
			if(is.null(axes) || is.null(length)) stop("If 'ends' is a single point then 'axes' and 'length' must both be non-NULL.")

			# Convert to matrix
			if(length(dim(axes)) == 3) axes <- axes[,,1]
			if(is.vector(axes)) axes <- matrix(axes, 1, 3)

			if(nrow(axes) == 1) stop("If 'ends' is a single point then 'axes' must be a 2-row matrix.")

			# Make sure vector is unit length
			axes <- uvector_svg(axes)

			# Find ends
			ends <- rbind(ends, ends + length*axes[1,])

		}else{
		
			# Find axis, make unit
			axes <- rbind(ends[2,]-ends[1,], axes)
		
			# Get length
			length <- sqrt(sum((axes[1, ])^2))

			# Make unit
			axes <- uvector_svg(axes)
		}

		# Set widths if only one given
		if(length(width) == 1) width <- rep(width, 2)
	
		# Get vector orthogonal to axes
		axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))

	}else{
		
		# Set widths if only one given
		if(length(width) == 1) width <- rep(width, 3)
	
		# Make sure unit
		axes <- uvector_svg(axes)
		
		# Set ends
		ends <- matrix(NA, 2, 3)
		ends[1,] <- center + (width[1]/2)*axes[1,]
		ends[2,] <- center - (width[1]/2)*axes[1,]
		
		# Make sure there are 3 axes
		if(nrow(axes) == 2) axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))
		
		# 
		length <- width[1]
		width <- width[2:3]
	}
	
	# Create vertices matrix
	vertices <- matrix(NA, 8, 3)

	# Add vertices
	vertices[1,] <- ends[1,] + (width[1]/2)*axes[2,] + (width[2]/2)*axes[3,]
	vertices[2,] <- ends[1,] + (width[1]/2)*axes[2,] - (width[2]/2)*axes[3,]
	vertices[3,] <- ends[1,] - (width[1]/2)*axes[2,] + (width[2]/2)*axes[3,]
	vertices[4,] <- ends[1,] - (width[1]/2)*axes[2,] - (width[2]/2)*axes[3,]
	vertices[5,] <- ends[2,] + (width[1]/2)*axes[2,] + (width[2]/2)*axes[3,]
	vertices[6,] <- ends[2,] + (width[1]/2)*axes[2,] - (width[2]/2)*axes[3,]
	vertices[7,] <- ends[2,] - (width[1]/2)*axes[2,] + (width[2]/2)*axes[3,]
	vertices[8,] <- ends[2,] - (width[1]/2)*axes[2,] - (width[2]/2)*axes[3,]

	# Create faces matrix
	faces <- matrix(NA, 12, 3)
	
	# Add faces
	faces[1,] <- c(0,1,2)
	faces[2,] <- c(1,2,3)
	faces[3,] <- c(4,5,6)
	faces[4,] <- c(5,6,7)
	faces[5,] <- c(2,3,6)
	faces[6,] <- c(3,6,7)
	faces[7,] <- c(1,3,7)
	faces[8,] <- c(1,5,7)
	faces[9,] <- c(4,6,2)
	faces[10,] <- c(4,0,2)
	faces[11,] <- c(1,5,4)
	faces[12,] <- c(1,0,4)
	
	faces <- faces[!is.na(faces[,1]), ]

	if('svg' == getOption("svgviewr_glo_type")){

		svg.points(vertices)

		svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=1)
		
		# Draw faces
		faces <- cbind(faces, faces[,1])
		svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)

	}else{

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add to meshes
		add_at <- length(svgviewr_env$svg$mesh)+1

		# Add vertices
		svgviewr_env$svg$mesh[[add_at]] <- list()
		svgviewr_env$svg$mesh[[add_at]]$vertices <- t(vertices)
		svgviewr_env$svg$mesh[[add_at]]$faces <- t(faces)
		svgviewr_env$svg$mesh[[add_at]]$col <- webColor(col)
		svgviewr_env$svg$mesh[[add_at]]$emissive <- webColor(emissive)
		svgviewr_env$svg$mesh[[add_at]]$computeVN <- FALSE
		svgviewr_env$svg$mesh[[add_at]]$parseModel <- FALSE

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

	ret = NULL
}