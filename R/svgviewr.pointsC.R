svgviewr.pointsC <- function(x, file=NULL, y=NULL, type="p", close=FALSE, 
	col=NULL, col.fill="black", col.stroke="black", z.index=0, layer="", label="", cex=2, 
	lwd=2, opacity.stroke=1, opacity.fill=1, col.fill.C="none", col.stroke.C="black", 
	z.index.C=0, lwd.C=1, opacity.stroke.C=1, opacity.fill.C=1, layer.C=NULL, append=TRUE, 
	tag.name="point"){

	# IF Y IS NON-NULL, ADD AS SECOND COLUMN TO X
	if(!is.null(y)) x <- cbind(x, y)

	# IF POINT COORDINATES ARE VECTOR SET AS MATRIX
	if(is.vector(x)) x <- matrix(x, nrow=1, ncol=length(x))

	# SUPRESS EXPONENTIAL FORMAT FOR NEARLY ZERO VALUES (CANNOT BE READ BY SVG READER)
	options(scipen=10)
	x <- round(x, 8)

	# EMPTY NEW LINES
	new_lines <- rep(NA, dim(x)[1])

	# IF POINTS ARE MATRIX, MAKE INTO AN ARRAY
	if(length(dim(x)) == 2) x <- array(x, dim=c(dim(x), 1))

	# IF SECOND DIMENSION IS OF LENGTH TWO, ADD THIRD DIMENSION OF ZEROS
	if(dim(x)[2] == 2){
		xn <- array(NA, dim=c(dim(x)[1], 3, dim(x)[3]))
		for(i in 1:dim(x)[3]){
			if(is.matrix(x[, , i])){
				xn[, , i] <- cbind(x[, , i], rep(0, nrow(x[, , i])))
			}else{
				xn[, , i] <- c(x[, , i], 0)
			}
		}
		x <- xn
	}

	# IF COL IS SPECIFIED, OVERWRITE FILL AND STROKE
	if(!is.null(col)){col.fill <- col;col.stroke <- col;col.fill.C <- col;col.stroke.C <- col}

	# SET GRAPHICAL PARAMETERS
	if(is.null(layer.C)) layer.C <- layer
	svg_gp <- c("col", "col.fill", "col.stroke", "label", "layer", "opacity.fill", 
		"opacity.stroke", "cex", "lwd", "z.index")
	#"col.fill.C", "col.stroke.C", 
	#	"z.index.C", "lwd.C", "opacity.stroke.C", "opacity.fill.C"

	# CONVERT GRAPHICAL PARAMETERS TO VECTORS WITH SAME NUMBER OF ELEMENTS OF FIRST X DIMENSION
	for(gpar in svg_gp){
		if(length(get(gpar)) < dim(x)[1]){
			assign(gpar, rep(get(gpar), dim(x)[1]))
		}else if(length(get(gpar)) > dim(x)[1]){
			assign(gpar, paste(get(gpar), collapse=","))
		}
	}

	cp <- ""
	if(tag.name == "circle") cp <- "c"

	# WRITE POINTS TO SVG STRUCTURE
	for(i in 1:dim(x)[1]){

		# COLLAPSE VALUES INTO COMMA-SEPARATED STRING
		xc <- paste(x[i, 1, ], collapse=",")
		yc <- paste(x[i, 2, ], collapse=",")
		zc <- paste(x[i, 3, ], collapse=",")

		# CHECK THAT POINTS CHANGE POSITION BEFORE PRINTING ANIMATION STRING
		sum_sd <- sum(abs(diff(x[i, 1, ]))) + sum(abs(diff(x[i, 2, ]))) + sum(abs(diff(x[i, 3, ])))
		#sum_sd <- 1
		if(!is.na(sum_sd) && sum_sd == 0){
			xc <- x[i, 1, 1]
			yc <- x[i, 2, 1]
			zc <- x[i, 3, 1]
		}

		new_lines[i] <- paste("\t<", tag.name, " z-index=\"", z.index[i], "\" layer=\"", layer[i], 
			"\" ", cp, "x=\"", xc, "\" ", cp, 
			"y=\"", yc, "\" ", cp, 
			"z=\"", zc, 
			"\" label=\"", label[i], "\" r=\"", cex[i], "\" stroke=\"", col.stroke[i], 
			"\" stroke-width=\"", lwd[i], "\" fill=\"", col.fill[i], "\" fill-opacity=\"", opacity.fill[i], 
			"\" stroke-opacity=\"", opacity.stroke[i], "\" />", sep="")
	}
	
	# WRITE LINES TO SVG
	num_points <- 1
	if(!is.null(file)){

		# READ FILE
		if(class(file)[1] == 'character'){
			file_read <- readChar(file, file.info(file)$size)
		}else{

			# Read lines - readChar requires number of characters to read and I could not figure out how to do that with a connection
			file_read <- paste(readLines(file$con, n=-1), collapse='')
			
			# readLines leaves internal current position of connection at the end - reset to beginning
			seek(file$con, 0, rw = "r")
		}
		
		# COUNT THE NUMBER OF POINT TAGS ALREADY IN THE SVG FILE
		str_split <- strsplit(file_read, "<point ")[[1]]
		num_points <- length(str_split) - 1
	}
	
	# CREATE VECTOR OF INDICES CONNECTING POINTS
	connect_indices <- 1:dim(x)[1] + num_points
	
	# IF CLOSING CONNECTING PATHS
	if(close) connect_indices <- c(connect_indices, connect_indices[1])

	# STRING OF POINTS TO CONNECT
	new_lines[length(new_lines)+1] <- paste("\t<pathC z-index=\"", z.index.C[1], "\" layer=\"", layer.C[1], 
		"\" d=\"", paste(connect_indices, collapse=","), "\" stroke=\"", webColor(col.stroke.C[1]), 
		"\" stroke-width=\"", lwd.C[1], "\" fill=\"", webColor(col.fill.C[1]), "\" fill-opacity=\"", opacity.fill.C[1], 
		"\" stroke-opacity=\"", opacity.stroke.C[1], "\" ></pathC>", sep="")

	# REMOVE SCIENTIFIC NOTATION
	options(scipen=0)

	# REMOVE NA LINES
	new_lines <- new_lines[!is.na(new_lines)]

	# IF FILE IS NULL, RETURN LINES OF SVG OBJECTS
	if(is.null(file)) return(new_lines)

	# SAVE NEW LINES TO FILE
	svgviewr.write(new_lines, file, append=append)
}