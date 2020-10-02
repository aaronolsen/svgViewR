svg.text <- function(x, y = NULL, labels = NULL, layer="", name="text", 
	font.size = 12, size = 1, col = "black", text.anchor = "middle", font.family = "Arial", 
	opacity = 1, font.style = "", font.weight = "", letter.spacing = 0, writing.mode = "", 
	glyph.orientation.vertical = "", z.index=0, file=NULL){

	if('svg' == getOption("svgviewr_glo_type")){

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

	}else{

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Convert to matrix if vector
		if(is.vector(x)) x <- matrix(x, nrow=1, 3)
		
		colnames(x) <- NULL

		if(length(dim(x)) == 2){

			for(i in 1:nrow(x)){

				# Add text to environment
				add_at <- length(svgviewr_env$svg$text)+1
				svgviewr_env$svg$text[[add_at]] <- list('type'='text', 'labels'=as.character(labels[i]), 
					'name'=name, 'x'=x[i,], 'col'=setNames(webColor('black'), NULL), 'size'=size)

				# Add object reference data
				svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
				svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
				svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'text')
			}
		}
	}

	# Suppress return of value in console
	ret = NULL
}