svg.ccuboid <- function(ends=NULL, center=NULL, axes=NULL, length=NULL, width=1, seg=c(5,5,5), 
	ccenter=center, cvector=NULL, cradius.scale=1, col='blue', 
	emissive=rgb(0.03, 0.15, 0.21), opacity = 1, ontop = FALSE, name='cuboid'){

	## Creates curved cuboid

	## I've written this function to generate separate vertex sets for each face of the 
	##	cuboid. This makes the face indexing much simpler but uses more vertices than are 
	## 	necessary. This function could be re-written to use fewer vertices (re-using the 
	##	vertices along the edges and corners of the cuboid) if maximum rendering 
	##	efficiency is needed.

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

			if(is.vector(axes)) axes <- matrix(axes, 1, 3)

			if(nrow(axes) == 3) stop("If 'ends' is a 2-row matrix then 'axes' must be a 1 or 2-row matrix.")

			# Find axis, make unit
			axes <- rbind(ends[2,]-ends[1,], axes)
			
			# Issue warning if length is not used
			if(!is.null(length)) warning('Input parameter "length" is ignored with input of two end points ("ends").')
		
			# Get length
			length <- sqrt(sum((axes[1, ])^2))

			# Make unit
			axes <- uvector_svg(axes)
		}
		
		# Set center
		center <- colMeans(ends)

		# Set widths if only one given
		if(length(width) == 1) width <- rep(width, 2)
	
		# Get vector orthogonal to axes
		if(nrow(axes) == 2) axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))

		# Set external dims
		edims <- c(length, width)

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
		
		# Set external dims
		edims <- width
	}
	
	# Set half dimensions
	edims_h <- edims / 2

	# Flip axis to match ends order
	if(abs(avec_svg(axes[1,], ends[1,]-ends[2,])) > 1) axes[1,] <- -axes[1,]
	
	# Set axis combos
	axis_combos <- list(c(1,1), c(1,-1), c(-1,1), c(-1,-1))

	# Create outer cuboid corners matrix
	vertices <- matrix(NA, 8, 3)
	for(side in 1:2){
		v_i <- (side-1)*4 + 1
		for(i in 1:4) vertices[v_i+i-1,] <- ends[side,] + colSums(axis_combos[[i]]*edims_h[2:3]*axes[2:3,])
	}
	
	# Create list of vertices for each side
	side_idx_list <- list(
		c(1,2,4,3),
		c(6,5,7,8),
		c(5,6,2,1),
		c(3,4,8,7),
		c(5,1,3,7),
		c(2,6,8,4)
	)
	
	# Make sure seg is 3-length
	if(length(seg) == 1) seg <- rep(seg, 3)

	# Set index of segments to use with each face
	seg_idx_list <- list(c(3,2), c(3,1), c(1,2))
	
	# Set segment number for each plane
	
	# Start vertices and faces for function
	plane <- list('vertices'=NULL, 'faces'=NULL)

	# Add vertices and faces for each side of cuboid
	for(i in 1:length(side_idx_list)){
		plane <- create_plane_mesh(corners=vertices[side_idx_list[[i]],], vertices=plane$vertices, 
			seg=seg[seg_idx_list[[ceiling(i/2)]]], faces=plane$faces)
	}

	# Set vertices and faces from plane function calls
	vertices <- plane$vertices
	faces <- plane$faces
	
	# Curve vertices
	if(!is.null(cvector)){
	
		# Make sure unit
		cvector <- uvector_svg(cvector)
		
		if(nrow(cvector) == 2){
			cvector <- rbind(cvector, cprod_svg(cvector[1,], cvector[2,]))
		}

		# For each vertex
		plps <- matrix(NA, nrow=nrow(vertices), 3)
		plp_dist <- rep(NA, nrow(vertices))
		for(i in 1:nrow(vertices)){

			# Project vertex onto circle "x-axis"
			plps[i,] <- pointLineProj_svg(vertices[i,], ccenter, ccenter+cvector[2,])
			
			# Get distance from point to ccenter
			plp_dist[i] <- dppt_svg(plps[i,], ccenter)
		}
		
		# Set maximum as radius
		cradius <- max(plp_dist)
		
		# Scale radius and create second radius
		cradius <- rep(cradius, 2)*cradius.scale

		# For each vertex
		for(i in 1:nrow(vertices)){

			# Project vertex into circle plane
			ppp <- pointPlaneProj_svg(vertices[i,], ccenter, n=cvector[1,])

			# Set x-value sign
			x_sign <- 1
			if(dppt_svg(plps[i,], ccenter-cvector[2,]) < dppt_svg(plps[i,], ccenter+cvector[2,])){ x_sign <- -1 }else { x_sign <- 1 }

			# Set y-value sign
			y_sign <- 1
			if(dppt_svg(ppp, ccenter-cvector[3,]) < dppt_svg(ppp, ccenter+cvector[3,])){ y_sign <- -1 }else { y_sign <- 1 }

			# Set radii based on depth of cuboid
			add_to_rad <- dppt_svg(ppp, plps[i,])
			new_rad <- cradius + add_to_rad
			
			# Calculate ratio to make sure points are evenly distributed around circumference of circle
			if(add_to_rad == 0){
				dist_ratio <- plp_dist[i] / cradius[1]
			}else{
				dist_ratio <- ((plp_dist[i] / cradius[1])*new_rad[1]) / new_rad[1]
			}

			# Set x-value
			x_val <- x_sign*new_rad[1]*sin(dist_ratio*(pi/2))
			
			# Check that x-value is less than radius
			if(abs(x_val) > new_rad[1]) stop("Point on cuboid is further than circle axis than radius. Increase the circle radius.")
			
			# Get "y-value"
			y_val <- y_sign*new_rad[2] * sin(acos(x_val / new_rad[1]))
			
			# Project vertex back
			vertices[i, ] <- ccenter + x_val*cvector[2,] + y_val*cvector[3,] + (vertices[i,] - ppp)
 		}
	}

	if('svg' == getOption("svgviewr_glo_type")){

		svg.points(vertices)

		svg.text(vertices, labels=(0:(nrow(vertices)-1)+1), font.size=1)
		
		# Draw faces
		faces <- cbind(faces, faces[,1])
		svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)
		
		svg.points(ccenter)
		svg.arrows(rbind(ccenter, ccenter+cvector[1,]))
		svg.arrows(rbind(ccenter, ccenter+cvector[2,]))

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
		svgviewr_env$svg$mesh[[add_at]]$opacity <- setNames(webColor(opacity), NULL)
		svgviewr_env$svg$mesh[[add_at]]$computeVN <- FALSE
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

	ret = NULL
}