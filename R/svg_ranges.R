svg_ranges <- function(x = NULL){

	# If x is null
	if(is.null(x)){

		if(getOption("svgviewr_glo_type") %in% c('live', 'html')){

			## Find range of all current svg objects
			# Get viewer environment
			env <- as.environment(getOption("svgviewr_glo_env"))

			# Get static ranges
			ranges <- matrix(NA, nrow=0, 3)
   			
			for(oi in 1:length(svgviewr_env$ref$type)){
				
				# Get object
				obj <- svgviewr_env[['svg']][[svgviewr_env$ref$type[oi]]][[svgviewr_env$ref$num[oi]]]
				
				corners <- NULL
				
				if(svgviewr_env$ref$type[oi] %in% c('mesh', 'image')){

					if(is.null(obj[['lim']])){

						# Read source file
						obj_json <- fromJSON(paste(suppressWarnings(readLines(obj$file)), collapse=""))
				
						# Get number of vertices
						num_vertices <- length(obj_json$vertices)

						# Get xyz limits
						obj_ranges <- cbind(range(obj_json$vertices[seq(1, num_vertices-2, by=3)], na.rm=TRUE),
							range(obj_json$vertices[seq(2, num_vertices-1, by=3)], na.rm=TRUE),
							range(obj_json$vertices[seq(3, num_vertices, by=3)], na.rm=TRUE))
						
						# Get corners
						corners <- lim2corners(obj_ranges)

						# Add to object
						svgviewr_env[['svg']][[svgviewr_env$ref$type[oi]]][[svgviewr_env$ref$num[oi]]][['lim']] <- obj_ranges
						svgviewr_env[['svg']][[svgviewr_env$ref$type[oi]]][[svgviewr_env$ref$num[oi]]][['corners']] <- corners

					}else{

						# Limits determined when object was loaded
						corners <- obj[['corners']]
					}

					# Apply initial transformation to corners
					if(!is.null(obj[['itmat']])){

						# Apply transformations to each and get ranges
						corners <- apply_transform_svg(corners, obj[['itmat']])

						# Add range
						#corners <- apply(obj_tm, 2, 'range', na.rm=TRUE)
					}

					# Apply transformation to corners
					if(!is.null(obj[['tmat']])){

						# Apply transformations to each and get ranges
						obj_tm <- apply_transform_svg(corners, obj[['tmat']])

						# Add range
						corners <- apply(obj_tm, 2, 'range', na.rm=TRUE)
					}

				}else if(svgviewr_env$ref$type[oi] == 'sphere'){

					# Get limits
					if(is.null(obj$x_animated)){
						lim <- rbind(obj$x + obj$radius*c(1,1,1), obj$x - obj$radius*c(1,1,1))
					}else{

						# All NA values
						if(sum(!is.na(unlist(obj$x_animated))) == 0){
							corners <- NULL
							next
						}

						mat_lim <- apply(matrix(unlist(obj$x_animated), ncol = 3, byrow = TRUE), 2, 'range', na.rm=TRUE)
						lim <- rbind(mat_lim[1,] + obj$radius*c(1,1,1), mat_lim[1,] - obj$radius*c(1,1,1), 
							mat_lim[2,] + obj$radius*c(1,1,1), mat_lim[2,] - obj$radius*c(1,1,1))
						lim <- apply(lim, 2, 'range', na.rm=TRUE)
					}
					
					# Get corners
					corners <- lim2corners(lim)

					# Apply initial transformation to corners
					if(!is.null(obj[['itmat']])){

						# Apply transformations to each and get ranges
						obj_tm <- apply_transform_svg(corners, obj[['itmat']])

						# Add range
						corners <- apply(obj_tm, 2, 'range', na.rm=TRUE)
					}

				}else if(svgviewr_env$ref$type[oi] == 'line'){

					# Get limits
					if(is.null(obj$x_tm)){
						lim <- apply(t(obj$x), 2, 'range', na.rm=TRUE)
					}else{

						# All NA values
						if(sum(!is.na(unlist(obj$x_tm))) == 0){
							corners <- NULL
							next
						}

						lim <- apply(t(matrix(unlist(obj$x_tm), nrow = 3, byrow = FALSE)), 2, 'range', na.rm=TRUE)
					}

					# Get corners
					corners <- lim2corners(lim)
				}
				
				if(is.null(corners)) next

				# Add ranges
				ranges <- rbind(ranges, corners)
			}
			
		}else{

			## Find range of drawn objects
			#stop("NULL 'x' input is not yet supported for non-WebGL output.")
		}

	}else{
		
		ranges <- x
	}

	# Find ranges (should work for matrices and arrays)
	ranges <- apply(ranges, 2, 'range', na.rm=TRUE)

	ranges
}