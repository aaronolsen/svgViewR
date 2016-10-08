svg.text <- function(x, y = NULL, labels = NULL, layer="", 
	font.size = 12, col = "black", text.anchor = "middle", font.family = "Arial", 
	opacity = 1, font.style = "", font.weight = "", letter.spacing = 0, writing.mode = "", 
	glyph.orientation.vertical = "", z.index=0, file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	svgviewr.text(x=x, file=file, y=y, labels=labels, layer=layer, 
		font.size=font.size, col=col, text.anchor=text.anchor, font.family=font.family, 
		opacity=opacity, font.style=font.style, font.weight=font.weight, 
		letter.spacing=letter.spacing, writing.mode=writing.mode, 
		glyph.orientation.vertical=glyph.orientation.vertical, z.index=z.index)

	# Suppress return of value in console
	ret = NULL
}