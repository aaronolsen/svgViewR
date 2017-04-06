svgviewr.paths <- function(d, file=NULL, col=NULL, col.fill=NULL, col.stroke="black", 
	lwd=2, opacity.stroke=1, opacity.fill=1, z.index=0, layer="", label="", append=TRUE){

	# IF D IS MATRIX, CONVERT TO PATH
	if(is.matrix(d)){

		# SUPRESS EXPONENTIAL FORMAT FOR NEARLY ZERO VALUES (CANNOT BE READ BY SVG READER)
		options(scipen=10)
		d <- round(d, 8)

		row_collapse <- apply(d, 1, 'paste', collapse=',')
		d <- list(paste0('M', row_collapse[1], ' ', paste('L', row_collapse[2:length(row_collapse)], collapse=' ')))
		if(is.null(col.fill)) col.fill <- 'none'
	}
	
	if(is.null(col.fill)) col.fill <- 'black'

	# IF D IS VECTOR, CONVERT TO LIST
	if(!is.list(d)) d <- list(d)

	# IF COL IS SPECIFIED, OVERWRITE FILL AND STROKE
	if(!is.null(col)){col.fill <- col;col.stroke <- col}

	# SET GRAPHICAL PARAMETERS
	svg_gp <- c("col", "col.fill", "col.stroke", "label", "layer", "opacity.fill", 
		"opacity.stroke", "lwd", "z.index")

	# CONVERT GRAPHICAL PARAMETERS TO VECTORS WITH SAME NUMBER OF ELEMENTS OF FIRST X DIMENSION
	for(gpar in svg_gp) if(length(get(gpar)) != length(d)) assign(gpar, rep(get(gpar), length(d)))

	# EMPTY NEW LINES
	new_lines <- rep(NA, 0)

	# WRITE LINES TO SVG STRUCTURE
	for(i in 1:length(d)){
		new_lines <- c(new_lines, paste("\t<path z-index=\"", z.index[i], "\" layer=\"", layer[i], 
			"\" d=\"", d[[i]], "\" label=\"", label[i], "\" stroke=\"", webColor(col.stroke[i]), 
			"\" stroke-width=\"", lwd[i], "\" fill=\"", webColor(col.fill[i]), 
			"\" stroke-opacity=\"", opacity.stroke[i], "\" fill-opacity=\"", opacity.fill[i], 
			"\" ></path>", sep=""))
	}

	# REMOVE NA LINES
	new_lines <- new_lines[!is.na(new_lines)]

	# IF FILE IS NULL, RETURN LINES OF SVG OBJECTS
	if(is.null(file)) return(new_lines)

	# SAVE NEW LINES TO FILE
	svgviewr.write(new_lines, file, append=append)
}