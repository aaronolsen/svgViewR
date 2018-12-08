svg.paths <- function(d, col=NULL, col.fill=NULL, col.stroke="black", 
	lwd=2, opacity.stroke=1, opacity.fill=1, z.index=0, layer="", label="", file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	svgviewr.paths(d=d, file=file, col=col, col.fill=col.fill, col.stroke=col.stroke, 
		lwd=lwd, opacity.stroke=opacity.stroke, opacity.fill=opacity.fill, z.index=z.index,
		layer=layer, label=label)

	# Suppress return of value in console
	ret = NULL
}