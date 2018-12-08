svg.pointsC <- function(x, y=NULL, type="p", close=FALSE, 
	col=NULL, col.fill="black", col.stroke="black", z.index=0, layer="", label="", cex=2, 
	lwd=2, opacity.stroke=1, opacity.fill=1, col.fill.C="none", col.stroke.C="black", 
	z.index.C=0, lwd.C=1, opacity.stroke.C=1, opacity.fill.C=1, layer.C=NULL, file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	svgviewr.pointsC(x=x, file=file, y=y, type=type, close=close, 
		col=col, col.fill=col.fill, col.stroke=col.stroke, z.index=z.index, layer=layer, 
		label=label, cex=cex, lwd=lwd, opacity.stroke=opacity.stroke, opacity.fill=opacity.fill,
		col.fill.C=col.fill.C, col.stroke.C=col.stroke.C, z.index.C=z.index.C, 
		lwd.C=lwd.C, opacity.stroke.C=opacity.stroke.C, opacity.fill.C=opacity.fill.C, 
		layer.C=layer.C)

	# Suppress return of value in console
	ret = NULL
}