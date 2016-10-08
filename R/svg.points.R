svg.points <- function(x, y=NULL, type="p", col=NULL, col.fill="black", 
	col.stroke="black", z.index=0, layer="", label="", cex=2, lwd=2, opacity.stroke=1, 
	opacity.fill=1, file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	svgviewr.points(x=x, file=file, y=y, type=type, col=col, col.fill=col.fill, 
		col.stroke=col.stroke, z.index=z.index, layer=layer, label=label, cex=cex, lwd=lwd, 
		opacity.stroke=opacity.stroke, opacity.fill=opacity.fill)
	
	# Suppress return of value in console
	ret = NULL
}