svg.mesh <- function(file = NULL, name = gsub('[.][A-Za-z]+$', '', tail(strsplit(file, '/')[[1]], 1)), 
	opacity = 1, get.lim = TRUE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Mesh drawing is currently only available with webgl svgViewR output.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all input parameters as list
	input_params <- mget(names(formals()),sys.frame(sys.nframe()))

	# Set type
	input_params$type <- gsub('svg[.]', '', input_params$fcn)

	if(!is.null(file)){
	
		# Set opacity
		input_params[['opacity']] <- opacity

		# Set where to add object
		add_at <- length(svgviewr_env$mesh)+1

		#
		if('html' == getOption("svgviewr_glo_type")){
			
			# Read mesh file
			obj_json <- fromJSON(paste(suppressWarnings(readLines(file)), collapse=""))
			
			# Add mesh properties to input parameters
			input_params[['vertices']] <- obj_json$vertices
			input_params[['faces']] <- obj_json$faces
			input_params[['normals']] <- obj_json$normals

		}else{

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

		# Add to meshes
		env$svgviewr_env$mesh[[add_at]] <- input_params

		# Add object reference data
		env$svgviewr_env$ref$names <- c(env$svgviewr_env$ref$names, name)
		env$svgviewr_env$ref$num <- c(env$svgviewr_env$ref$num, add_at)
		env$svgviewr_env$ref$type <- c(env$svgviewr_env$ref$type, 'mesh')

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
		env$svgviewr_env$mesh[[add_at]][['lim']] <- obj_ranges
		env$svgviewr_env$mesh[[add_at]][['corners']] <- corners

		return(list('lim'=obj_ranges, 'corners'=corners))
	}
	
	NULL
}