svg.spheres <- function(x, radius = 1, col = 'black', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, 
	name = 'sphere', ontop = FALSE, wseg = 16, hseg = 16, as.array = FALSE){

	# Check that only single name given
	if(length(name) > 1) stop("Input parameter 'name' is a vector of length greater than one. 'name' must be of length 1.")

	# Return NULL if zero length
	if(length(x) == 0) return(NULL)

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# If vector, make into matrix
	if(is.vector(x)) x <- matrix(x, nrow=1)

	# If data.frame, convert to matrix
	if(is.data.frame(x)) x <- as.matrix(x)
	
	# If as.array is TRUE convert matrix to array
	if(as.array){
		if(dim(x)[1] == 3){
			x <- array(x, c(1,3,ncol(x)))
		}else{
			x <- array(t(x), c(1,3,nrow(x)))
		}	
	}

	# Repeat properties to match dimensions
	if(length(col) < dim(x)[1]) col <- rep(col, dim(x)[1])
	if(length(emissive) < dim(x)[1]) emissive <- rep(emissive, dim(x)[1])
	if(length(name) < dim(x)[1]) name <- rep(name, dim(x)[1])

	if(length(dim(x)) == 2){

		# Add object
		for(i in 1:nrow(x)){

			add_at <- length(svgviewr_env$svg$sphere)+1
			svgviewr_env$svg$sphere[[add_at]] <- list('type'='sphere', 'name'=name[i], 'x'=setNames(x[i,], NULL), 
				'radius'=radius, 'col'=setNames(webColor(col[i]), NULL), 'emissive'=setNames(webColor(emissive[i]), NULL), 'wseg'=wseg, 'hseg'=hseg, 
				'opacity'=setNames(opacity, NULL), 'itmat'=diag(4), 'depthTest'=!ontop)

			# Add object reference data
			svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name[i])
			svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
			svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'sphere')
		}

	}else{

		for(i in 1:dim(x)[1]){
		
			# Add object
			add_at <- length(svgviewr_env$svg$sphere)+1
			svgviewr_env$svg$sphere[[add_at]] <- list('type'='sphere', 'name'=name[i], 'x'=setNames(x[i,,1], NULL), 
				'radius'=radius, 'col'=setNames(webColor(col[i]), NULL), 'emissive'=setNames(webColor(emissive[i]), NULL), 'wseg'=wseg, 'hseg'=hseg, 
				'opacity'=setNames(opacity, NULL), 'itmat'=diag(4), 'depthTest'=!ontop)

			# Add object reference data
			svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name[i])
			svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
			svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'sphere')

			# Add animated x
			svgviewr_env[['svg']][['sphere']][[add_at]][['x_animated']] <- lapply(seq_len(dim(x)[3]), function(iter) as.list(setNames(signif(x[i,,iter], digits=env[['svgviewr_env']][['js_var']][['signif_digits']]), c('x', 'y', 'z'))))
			
			# Set number of timelines
			svgviewr_env$js_var[['n_timelines']] <- 1
		}
	}

	# Suppress return of value in console
	ret = NULL
}
