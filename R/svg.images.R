svg.images <- function(file = NULL, name = gsub('[.][A-Za-z]+$', '', tail(strsplit(file, '/')[[1]], 1)), 
	opacity = 1){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Image plotting is currently only available with webgl svgViewR output.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all input parameters as list
	input_params <- mget(names(formals()),sys.frame(sys.nframe()))

	# Set type
	input_params$type <- gsub('svg[.]', '', input_params$fcn)

	# Set where to add object
	add_at <- length(svgviewr_env$svg$image)+1

	# Check that file exists
	if(!file.exists(file)) stop(paste0('Input file "', file, '" not found.'))

	# Check that file is json format
	if(!grepl('[.](jpeg|jpg)$', file)) stop(paste0('Input file "', file, '" is of unrecognized file type. Currently only jpeg files are allowed.'))

	#
	if('html' == getOption("svgviewr_glo_type")) stop('Image plotting is currently only available with server-based visualization.')

	# Get absolute file path (rook app doesn't work with relative paths)
	file <- normalizePath(path=file)

	# Separate directory and filename
	file_strsplit <- strsplit(file, '/')[[1]]

	# Set filename
	input_params$fname <- tail(file_strsplit, 1)

	# Set directory path
	input_params$src <- ''
	if(length(file_strsplit) > 1) input_params$src <- paste0(paste0(file_strsplit[1:(length(file_strsplit)-1)], collapse='/'), '/')

	# Set opacity
	input_params[['opacity']] <- opacity

	# Add to images
	svgviewr_env$svg$image[[add_at]] <- input_params

	# Add object reference data
	svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
	svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
	svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'image')
	
	ret = NULL
}