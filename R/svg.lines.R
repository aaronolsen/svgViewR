svg.lines <- function(x, y=NULL, col="black", z.index=0, layer="", 
	label="", lwd=1, opacity=1, file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	svgviewr.lines(x, file=file, y=y, col=col, z.index=z.index, layer=layer, 
		label=label, lwd=lwd, opacity=opacity)
	
	# Suppress return of value in console
	ret = NULL
}