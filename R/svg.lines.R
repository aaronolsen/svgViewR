svg.lines <- function(x, y=NULL, col="black", z.index=0, layer="", name="line", 
	label="", lwd=1, opacity=1, file=NULL){

	if('webgl' != getOption("svgviewr_glo_type")){

		# If file is null, set current connection
		if(is.null(file)){

			# Give error if svg.new has not been called
			if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

			# Get current connection
			file <- getOption("svg_glo_con")
		}
	}

	if('webgl' == getOption("svgviewr_glo_type")){

		## Add objects to svgViewR environment
		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))
		
		# Add arrow
		add_at <- length(svgviewr_env$line)+1
		env$svgviewr_env$line[[add_at]] <- list('type'='line', 
			'name'=name, x=t(x), 'col'=webColor(col), 'lwd'=lwd)

		# Add object reference data
		env$svgviewr_env$ref$names <- c(env$svgviewr_env$ref$names, name)
		env$svgviewr_env$ref$num <- c(env$svgviewr_env$ref$num, add_at)
		env$svgviewr_env$ref$type <- c(env$svgviewr_env$ref$type, 'line')

	}else{

		svgviewr.lines(x, file=file, y=y, col=col, z.index=z.index, layer=layer, 
			label=label, lwd=lwd, opacity=opacity)
	}
	
	# Suppress return of value in console
	ret = NULL
}