svg.triangle <- function(corners, col='blue', seg=1, emissive=rgb(0.03, 0.15, 0.21), opacity = 1, name = 'triangle', 
	ontop = FALSE, return.shape = FALSE, plot = TRUE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")
	
	# Create points along edges of triangle
	if(seg > 1){
		
		# Get parameters for each edge
		edge1_len <- dppt_svg(corners[1,], corners[2,])
		edge1_vec <- uvector_svg(corners[1,]-corners[2,])
		edge1_spa <- edge1_len / seg
		edge2_len <- dppt_svg(corners[2,], corners[3,])
		edge2_vec <- uvector_svg(corners[3,]-corners[2,])
		edge2_spa <- edge2_len / seg
		edge3_len <- dppt_svg(corners[1,], corners[3,])
		edge3_vec <- uvector_svg(corners[3,]-corners[1,])
		edge3_spa <- edge3_len / seg

		# Create matrix for edge points
		edge1_pts <- edge2_pts <- edge3_pts <- matrix(NA, seg-1, 3)

		for(i in 1:(seg-1)){
			edge1_pts[i, ] <- corners[2,] + i*edge1_spa*edge1_vec
			edge2_pts[i, ] <- corners[2,] + i*edge2_spa*edge2_vec
			edge3_pts[i, ] <- corners[1,] + i*edge3_spa*edge3_vec
		}
		
		#vertices <- rbind(corners, edge1_pts, edge2_pts, edge3_pts)
		vertices <- rbind(corners[2,], edge1_pts[1,], edge2_pts[1,])

		# Set internal vertices
		if(seg > 2){

			for(i in 2:(seg-1)){

				int_len <- dppt_svg(edge1_pts[i, ], edge2_pts[i, ])
				int_vec <- uvector_svg(edge2_pts[i, ]-edge1_pts[i, ])
				int_spa <- int_len / i

				vertices <- rbind(vertices, edge1_pts[i, ])

				for(j in 1:(i-1)){
					vertices <- rbind(vertices, edge1_pts[i, ] + j*int_spa*int_vec)
				}

				vertices <- rbind(vertices, edge2_pts[i, ])
			}
			
		}else{

		}

		# Set remaining vertices without internal vertices
		vertices <- rbind(vertices, corners[1,], edge3_pts, corners[3,])

		# Set faces
		faces <- matrix(NA, 0, 3)
		start_face <- 0
		for(i in 1:max(2, (seg))){
		
			#cat(paste0(i, '\n'))
			
			#
			for(j in 1:((i-1)*2+1)){

				#cat(paste0('\t', j, ':', start_face, '\n'))

				if(j == 1 || j %% 2 == 1){

					# Set faces
					faces <- rbind(faces, c(start_face, start_face + i, start_face + i + 1))

					# Set starting face for next round
					start_face <- start_face + 1

				}else{

					# Set faces
					faces <- rbind(faces, c(start_face, start_face - 1, start_face + i))
				}
			}
		}

	}else{

		# Create single triangle
		vertices <- corners
		faces <- c(0,1,2)
	}

	# If not plotting, return vertices and faces
	if(!plot) return(list('vertices'=vertices, 'faces'=faces))

	if('svg' == getOption("svgviewr_glo_type")){
		
		if(is.vector(faces)) faces <- matrix(faces, 1, 3)

		svg.points(vertices[1,], col='red')
		svg.points(vertices[2:(nrow(vertices)-1), ])
		svg.points(vertices[nrow(vertices),], col='blue')
		
		svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=0.1)

		# Draw faces
		if(!is.null(faces)){
			faces <- cbind(faces, faces[,1])
			svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)
		}

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