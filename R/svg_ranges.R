svg_ranges <- function(x = NULL){

	# If x is null
	if(is.null(x)){

		if('webgl' == getOption("svgviewr_glo_type")){

			## Find range of all current svg objects
			# Get viewer environment
			env <- as.environment(getOption("svgviewr_glo_env"))

			# Get all svg objects with xyz limits
			svg_obj_names <- names(env$svgviewr_env)[!names(env$svgviewr_env) %in% c('names', 'js_var', 'tm')]

			# Get static ranges
			ranges <- matrix(NA, nrow=0, 3)
   			
			for(oi in 1:length(env$svgviewr_env$ref$type)){
				
				# Get object
				obj <- env$svgviewr_env[[env$svgviewr_env$ref$type[oi]]][[env$svgviewr_env$ref$num[oi]]]
				
				# If transformed via transformation, skip static ranges
				if(!is.null(obj$tm)) next

				if(env$svgviewr_env$ref$type[oi] == 'mesh'){

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
						corners <- limits2corners(obj_ranges)

						# Add to object
						env$svgviewr_env[[env$svgviewr_env$ref$type[oi]]][[env$svgviewr_env$ref$num[oi]]][['lim']] <- obj_ranges
						env$svgviewr_env[[env$svgviewr_env$ref$type[oi]]][[env$svgviewr_env$ref$num[oi]]][['corners']] <- corners

					}else{

						# Limits determined when object was loaded
						corners <- obj[['corners']]
					}
				}else if(env$svgviewr_env$ref$type[oi] == 'sphere'){
				
					# Get limits
					if(is.null(obj$x_tm)){
						lim <- rbind(obj$x + obj$radius*c(1,1,1), obj$x - obj$radius*c(1,1,1))
					}else{
						mat_lim <- apply(matrix(unlist(obj$x_tm), ncol = 3, byrow = TRUE), 2, 'range', na.rm=TRUE)
						lim <- rbind(mat_lim[1,] + obj$radius*c(1,1,1), mat_lim[1,] - obj$radius*c(1,1,1), 
							mat_lim[2,] + obj$radius*c(1,1,1), mat_lim[2,] - obj$radius*c(1,1,1))
						lim <- apply(lim, 2, 'range', na.rm=TRUE)
					}
					
					# Get corners
					corners <- lim2corners(lim)

				}else if(env$svgviewr_env$ref$type[oi] == 'line'){

					# Get limits
					if(is.null(obj$x_tm)){
						lim <- apply(t(obj$x), 2, 'range', na.rm=TRUE)
					}else{
						lim <- apply(t(matrix(unlist(obj$x_tm), nrow = 3, byrow = FALSE)), 2, 'range', na.rm=TRUE)
					}

					# Get corners
					corners <- lim2corners(lim)
				}

				# Add ranges
				ranges <- rbind(ranges, corners)
			}

			# Apply each animation
			if(!is.null(env$svgviewr_env$tm)){

				# For each animation
				for(animation in names(env$svgviewr_env$tm)){
			
					# For each named set
					for(applyto in names(env$svgviewr_env$tm[[animation]])){
				
						# Get transformations
						tmat <- env$svgviewr_env$tm[[animation]][[applyto]][['tmat']]
					
						# Find matching names in ref
						names_matches <- which(applyto == env$svgviewr_env$ref$names)
						obj_type_matches <- env$svgviewr_env$ref$type[names_matches]
						obj_num_matches <- env$svgviewr_env$ref$num[names_matches]

						# Get range for all matching objects
						for(mn in 1:length(obj_type_matches)){
					
							if(!obj_type_matches[mn] %in% c('mesh')) next

							# Get corners
							obj_range <- env$svgviewr_env[[obj_type_matches[mn]]][[obj_num_matches[mn]]][['corners']]
						
							# Apply transformations to each and get ranges
							obj_tm <- apply_transform_svg(obj_range, tmat)

							# Add range
							ranges <- rbind(ranges, apply(obj_tm, 2, 'range', na.rm=TRUE))
						}
					}
				}
			}

		}else{

			## Find range of drawn objects
			stop("NULL 'x' input is not yet supported for non-WebGL output.")
		}
	}

	# Find ranges (should work for matrices and arrays)
	ranges <- apply(ranges, 2, 'range', na.rm=TRUE)

	ranges
}