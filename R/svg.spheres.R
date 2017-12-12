svg.spheres <- function(x, radius = 1, col = 'black', name = 'sphere', wseg = 16, hseg = 16){

	# Make sure that type is webgl
	if('webgl' != getOption("svgviewr_glo_type")) stop("Sphere drawing is currently only available with webgl svgViewR output.")

	if('webgl' == getOption("svgviewr_glo_type")){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# If vector, make into matrix
		if(is.vector(x)) x <- matrix(x, nrow=1)

		# Add object
		for(i in 1:nrow(x)){

			add_at <- length(svgviewr_env$sphere)+1
			env$svgviewr_env$sphere[[add_at]] <- list('type'='sphere', 'name'=name, 'x'=setNames(x[i,], NULL), 
				'radius'=radius, 'col'=webColor(col),  
				'wseg'=16, 'hseg'=16)

			# Add object reference data
			env$svgviewr_env$ref$names <- c(env$svgviewr_env$ref$names, name)
			env$svgviewr_env$ref$num <- c(env$svgviewr_env$ref$num, add_at)
			env$svgviewr_env$ref$type <- c(env$svgviewr_env$ref$type, 'sphere')
		}

	}else{
		
	}

	# Suppress return of value in console
	ret = NULL
}
