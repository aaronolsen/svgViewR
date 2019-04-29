svg.images <- function(file, corners, name = gsub('[.][A-Za-z]+$', '', tail(strsplit(file[1], '/')[[1]], 1)), 
	seg = 2, opacity = 1, ontop = FALSE, times = NULL){

	# Make sure that type is webgl and html
	if('svg' == getOption("svgviewr_glo_type")) stop("Image plotting is currently only available with webgl svgViewR output.")
	if('html' == getOption("svgviewr_glo_type")) stop('Image plotting is currently only available with server-based visualization.')

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all input parameters as list
	input_params <- mget(names(formals()),sys.frame(sys.nframe()))

	# Set type
	input_params$type <- gsub('svg[.]', '', input_params$fcn)

	# Set where to add object
	add_at <- length(svgviewr_env$svg$image)+1

	# Check that file exists
	if(!file.exists(file[1])) stop(paste0("File '", file[1], "' not found."))

	# If directory, read all files in directory
	if(file.info(file[1])$isdir) file <- paste0(file[1], '/', list.files(file[1]))

	# Create vector for filenames
	input_params$fname <- rep(NA, length(file))

	# For each file
	for(i in 1:length(file)){

		# Check that file exists
		if(!file.exists(file[i])) stop(paste0('Input file "', file[i], '" not found.'))

		# Check that file is json format
		if(!grepl('[.](jpeg|jpg)$', file[i])) stop(paste0('Input file "', file[i], '" is of unrecognized file type. Currently only jpeg files are allowed.'))

		# Set filename
		input_params$fname[i] <- tail(strsplit(file[i], '/')[[1]], 1)
	}
	
	## Get source directory
	# Get absolute file path (rook app doesn't work with relative paths)
	file_norm <- normalizePath(path=file[1])

	# Separate directory and filename
	file_norm_split <- strsplit(file_norm, '/')[[1]]

	# Set directory path
	input_params$src <- ''
	if(length(file_norm_split) > 1) input_params$src <- paste0(paste0(file_norm_split[1:(length(file_norm_split)-1)], collapse='/'), '/')

	# Duplicate single segment value
	if(length(seg) == 1) seg <- rep(seg, 2)

	# If multiple images, define number of animation times if not already defined
	if(!is.null(times) && length(file) > 1){
		if(is.null(svgviewr_env$svg$animate$times)) svgviewr_env$svg$animate$times <- times
	}

	# Create plane mesh
	plane_mesh <- create_plane_mesh(corners, seg, create.uvs=TRUE)

	# Get vertices and faces
	vertices <- plane_mesh$vertices
	faces <- plane_mesh$faces
	uvs <- plane_mesh$uvs

	# Set opacity
	input_params[['opacity']] <- setNames(opacity, NULL)
	input_params[['depthTest']] <- !ontop

	# Add to images
	svgviewr_env$svg$image[[add_at]] <- input_params
	svgviewr_env$svg$image[[add_at]]$vertices <- t(vertices)
	svgviewr_env$svg$image[[add_at]]$faces <- t(faces)
	svgviewr_env$svg$image[[add_at]]$uvs <- t(uvs)
	#svgviewr_env$svg$image[[add_at]]$computeVN <- TRUE

	# Add object reference data
	svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
	svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
	svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'image')

	# Add limits
	obj_ranges <- apply(corners, 2, 'range', na.rm=TRUE)
	
	# Set corners
	corners <- lim2corners(obj_ranges)
	
	# Add limits to object
	svgviewr_env$svg$image[[add_at]][['lim']] <- obj_ranges
	svgviewr_env$svg$image[[add_at]][['corners']] <- corners

	ret = NULL
}