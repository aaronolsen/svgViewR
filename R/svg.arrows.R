svg.arrows <- function(x, y=NULL, name="arrow", col="black", z.index=0, layer="", 
	label="", lwd=1, len='auto', angle=0.4, opacity=1, file=NULL){

	#print(names(file))
	#print(names(svg_glo$con))
	#cat('--------\n')
	
	if('live' != getOption("svgviewr_glo_type")){

		# If file is null, set current connection
		if(is.null(file)){

			# Give error if svg.new has not been called
			if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

			# Get current connection
			file <- getOption("svg_glo_con")
		}
	}

	# Determine arrow length from line length
	if(len == 'auto'){
		if(is.matrix(x)) len <- sqrt(sum((x[1, ]-x[nrow(x), ])^2))*0.05
		if(is.array(x) && length(dim(x)) > 2) len <- max(sqrt(colSums((x[1, , ]-x[dim(x)[1], , ])^2)))*0.05
	}

	if('live' == getOption("svgviewr_glo_type")){

		## Add objects to svgViewR environment
		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))
		
		# Get arrow length
		arrow_len = sqrt(sum((x[2,]-x[1,])^2))

		# Add arrow
		add_at <- length(svgviewr_env$svg$arrow)+1
		svgviewr_env$svg$arrow[[add_at]] <- list('type'='arrow', 
			'name'=name, 'origin'=x[1,], 'dir'=uvector_svg(x[2,]-x[1,]), 'length'=arrow_len, 
			'len'=len*2.5, 'col'=setNames(webColor(col), NULL), 'lwd'=lwd, 'opacity'=setNames(opacity, NULL))

		# Add object reference data
		svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
		svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
		svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'arrow')

	}else{

		# Plot line
		svgviewr.lines(x, file=file, y=y, col=col, z.index=z.index, layer=layer, 
			label=label, lwd=lwd, opacity=opacity, arrow.len=len, arrow.angle=angle, 
			tag.name='arrow')
	}

	# Suppress return of value in console
	ret = NULL
}