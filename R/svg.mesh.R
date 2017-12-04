svg.mesh <- function(file = NULL, name = gsub('[.][A-Za-z]+$', '', tail(strsplit(file, '/')[[1]], 1))){

	# Make sure that type is webgl
	if('webgl' != getOption("svgviewr_glo_type")) stop("Mesh drawing is currently only available with webgl svgViewR output.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all input parameters as list
	input_params <- mget(names(formals()),sys.frame(sys.nframe()))

	# Set named element with function name as string
	input_params$fcn <- as.character(match.call()[[1]])

	# Set type
	input_params$type <- gsub('svg[.]', '', input_params$fcn)

	if(!is.null(file)){
	
		# Get absolute file path (rook app doesn't work with relative paths)
		file <- normalizePath(path=file)

		# Separate directory and filename
		file_strsplit <- strsplit(file, '/')[[1]]

		# Set filename
		input_params$fname <- tail(file_strsplit, 1)
	
		# Set directory path
		input_params$src <- ''
		if(length(file_strsplit) > 1) input_params$src <- paste0(paste0(file_strsplit[1:(length(file_strsplit)-1)], collapse='/'), '/')

		# Add file
		if(is.null(env$svgviewr_env$mesh)){
			env$svgviewr_env$mesh <- list(input_params)
		}else{
			env$svgviewr_env$mesh[[length(svgviewr_env$mesh)+1]] <- input_params
		}
		
		# Add name of object
		env$svgviewr_env$names <- c(env$svgviewr_env$names, name)

	}else{
		
		# 
		stop("Mesh import without source file input is not yet supported.")
	}
}