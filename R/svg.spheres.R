svg.spheres <- function(x, radius = 1, col = 'black', emissive=rgb(.27,.27,.27), name = 'sphere', wseg = 16, hseg = 16){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Sphere drawing is currently only available with webgl svgViewR output.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# If vector, make into matrix
	if(is.vector(x)) x <- matrix(x, nrow=1)

	if(length(dim(x)) == 2){

		# Add object
		for(i in 1:nrow(x)){

			add_at <- length(svgviewr_env$svg$sphere)+1
			svgviewr_env$svg$sphere[[add_at]] <- list('type'='sphere', 'name'=name, 'x'=setNames(x[i,], NULL), 
				'radius'=radius, 'col'=webColor(col), 'emissive'=webColor(emissive), 'wseg'=16, 'hseg'=16)

			# Add object reference data
			svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
			svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
			svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'sphere')
		}

	}else{

		for(i in 1:dim(x)[1]){
		
			# Add object
			add_at <- length(svgviewr_env$svg$sphere)+1
			svgviewr_env$svg$sphere[[add_at]] <- list('type'='sphere', 'name'=name, 'x'=setNames(x[i,,1], NULL), 
				'radius'=radius, 'col'=webColor(col), 'emissive'=webColor(emissive), 'wseg'=16, 'hseg'=16)

			# Add object reference data
			svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
			svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
			svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'sphere')

			# Add animation
			svgviewr_env[['svg']][['sphere']][[add_at]][['x_tm']] <- lapply(seq_len(dim(x)[3]), function(iter) x[i,,iter])
		}
	}

	# Suppress return of value in console
	ret = NULL
}
