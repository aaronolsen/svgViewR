svg.mesh <- function(file = NULL, name = gsub('[.][A-Za-z]+$', '', tail(strsplit(file, '/')[[1]], 1)), 
	col = '#F5F5F5', emissive = 'black', opacity = 1, get.lim = TRUE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Mesh drawing is currently only available with webgl svgViewR output.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all input parameters as list
	input_params <- mget(names(formals()),sys.frame(sys.nframe()))

	# Set type
	input_params$type <- gsub('svg[.]', '', input_params$fcn)

	if(!is.null(file)){

		# Set where to add object
		add_at <- length(svgviewr_env$svg$mesh)+1

		# Check that file exists
		if(!file.exists(file)) stop(paste0('Input file "', file, '" not found.'))
		
		# Check that file is json format
		if(!grepl('[.](json|obj)$', file)) stop(paste0('Input file "', file, '" is of unrecognized file type. Allowed file types are .obj and .json.'))

		#
		if('html' == getOption("svgviewr_glo_type")){

			# Read mesh file
			if(grepl('[.]json$', file)){
				obj_json <- fromJSON(paste(suppressWarnings(readLines(file)), collapse=""))
			}else{
				obj_json <- objToJSON(obj=file)
			}

			# Add mesh properties to input parameters
			for(obj_name in names(obj_json)) input_params[[obj_name]] <- obj_json[[obj_name]]

			input_params[['scale']] <- 1

		}else{

			if(grepl('[.](obj)$', file)){
				
				# For server webgl visualization
				json_file <- gsub('[.]obj$', '.json', file)
				cat(paste0('For server ("live") visualization mesh file should be .json format. To convert an .obj file to .json, you can use the svgViewR function objToJSON(). For example:\n'))
				cat(paste0('\tobjToJSON(obj=\'', file, '\', file=\'', gsub('[.]obj$', '.json', file), '\')\n'))
				cat(paste0('\nThen replace the file input to this function with the \'.json\' file.\n\n'))
				
				response <- readline(prompt="Would you like to do this now? (y/n) : ");
				if(tolower(response) %in% c('yes', 'y')){

					response <- readline(prompt=paste0("Enter the file path of the converted .json file (to use '", json_file, "' simply press return): "))

				}else{
					stop('Please input a mesh in the .json format')
				}
				
				if(response != '') json_file <- response

				objToJSON(obj=file, file=json_file)
				
				file <- json_file
			}

			# Get absolute file path (rook app doesn't work with relative paths)
			file <- normalizePath(path=file)

			# Separate directory and filename
			file_strsplit <- strsplit(file, '/')[[1]]

			# Set filename
			input_params$fname <- tail(file_strsplit, 1)

			# Read source file
			if(get.lim) obj_json <- fromJSON(paste(suppressWarnings(readLines(file)), collapse=""))

			# Set directory path
			input_params$src <- ''
			if(length(file_strsplit) > 1) input_params$src <- paste0(paste0(file_strsplit[1:(length(file_strsplit)-1)], collapse='/'), '/')
		}

		# Set opacity
		input_params[['opacity']] <- opacity
		input_params[['col']] <- webColor(col)
		input_params[['emissive']] <- webColor(emissive)
		input_params[['parseModel']] <- TRUE

		# Add to meshes
		svgviewr_env$svg$mesh[[add_at]] <- input_params

		# Add object reference data
		svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
		svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
		svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'mesh')

	}else{
		
		# 
		stop("Mesh import without source file input is not yet supported.")
	}

	# Get xyz limits of mesh		
	if(get.lim){

		# Get number of vertices
		num_vertices <- length(obj_json$vertices)

		# Get xyz limits
		obj_ranges <- cbind(range(obj_json$vertices[seq(1, num_vertices-2, by=3)], na.rm=TRUE),
			range(obj_json$vertices[seq(2, num_vertices-1, by=3)], na.rm=TRUE),
			range(obj_json$vertices[seq(3, num_vertices, by=3)], na.rm=TRUE))
		colnames(obj_ranges) <- c('x', 'y', 'z')
		
		# Set corners
		corners <- lim2corners(obj_ranges)
		
		# Add limits to object
		svgviewr_env$svg$mesh[[add_at]][['lim']] <- obj_ranges
		svgviewr_env$svg$mesh[[add_at]][['corners']] <- corners

		return(list('lim'=obj_ranges, 'corners'=corners))
	}
	
	ret = NULL
}