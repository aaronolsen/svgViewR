svg.arrows <- function(x, y=NULL, col="black", z.index=0, layer="", 
	label="", lwd=1, len='auto', angle=0.4, opacity=1, file=NULL){

	#print(names(file))
	#print(names(svg_glo$con))
	#cat('--------\n')
	
	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	# Determine arrow length from line length
	if(len == 'auto'){
		if(is.matrix(x)) len <- sqrt(sum((x[1, ]-x[nrow(x), ])^2))*0.05
		if(is.array(x) && length(dim(x)) > 2) len <- max(sqrt(colSums((x[1, , ]-x[dim(x)[1], , ])^2)))*0.05
	}

	# Plot line
	svgviewr.lines(x, file=file, y=y, col=col, z.index=z.index, layer=layer, 
		label=label, lwd=lwd, opacity=opacity, arrow.len=len, arrow.angle=angle, 
		tag.name='arrow')

	# Suppress return of value in console
	ret = NULL
}