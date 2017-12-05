svg_ranges <- function(x = NULL){

	# If x is null
	if(is.null(x)){

		if('webgl' == getOption("svgviewr_glo_type")){

			## Find range of all current svg objects
			# Get viewer environment
			env <- as.environment(getOption("svgviewr_glo_env"))

			# Get all svg objects with xyz limits
			svg_obj_names <- names(env$svgviewr_env)[!names(env$svgviewr_env) %in% c('names', 'js_var', 'tm')]
			
			# Start svg_range matrix
			x <- matrix(NA, nrow=0, 3)

			# Get ranges for each object
			for(svg_obj_name in svg_obj_names){

				if(svg_obj_name == 'mesh'){
				
					for(mesh_num in 1:length(env$svgviewr_env[[svg_obj_name]])){

						# Get mesh
						mesh <- env$svgviewr_env[[svg_obj_name]][[mesh_num]]

						# Read source file
						mesh_json <- fromJSON(paste(suppressWarnings(readLines(mesh$file)), collapse=""))
						
						# Get number of vertices
						num_vertices <- length(mesh_json$vertices)

						# Get xyz limits
						vertex_ranges <- cbind(range(mesh_json$vertices[seq(1, num_vertices-2, by=3)], na.rm=TRUE),
							range(mesh_json$vertices[seq(2, num_vertices-1, by=3)], na.rm=TRUE),
							range(mesh_json$vertices[seq(3, num_vertices, by=3)], na.rm=TRUE))
						
						# Add ranges to environment object
						env$svgviewr_env[[svg_obj_name]][[mesh_num]][['range']] <- vertex_ranges

						x <- rbind(x, vertex_ranges)
					}
				}
			}

			# Apply any transformations to ranges for each element
			# Replace any previous

		}else{

			## Find range of drawn objects
			stop("NULL 'x' input is not yet supported for non-WebGL output.")
		}
	}

	# Find ranges (should work for matrices and arrays)
	ranges <- apply(x, 2, 'range', na.rm=TRUE)

	ranges
}