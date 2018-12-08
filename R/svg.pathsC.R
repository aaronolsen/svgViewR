svg.pathsC <- function(path, col = NULL, col.fill = "none", col.stroke = "black", 
	z.index = 0, layer = "", label = "", lwd = 1, opacity.stroke = 1, opacity.fill = 1, 
	index.add = 0, file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	svgviewr.pathsC(path=path, file=file, col=col, col.fill=col.fill, col.stroke=col.stroke, 
		z.index=z.index, layer=layer, label=label, lwd=lwd, opacity.stroke=opacity.stroke, 
		opacity.fill=opacity.fill, index.add=index.add)

	# Suppress return of value in console
	ret = NULL
}