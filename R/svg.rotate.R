svg.rotate <- function(vec, mag, file = NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	# Convert vector to unit vector
	vec <- uvector_svg(vec)

	if(is.matrix(vec) && nrow(vec) > 1){

		if(length(mag) != nrow(vec)) mag <- rep(mag[1], nrow(vec))
		rmat <- array(NA, dim=c(3,3,nrow(vec)))
		for(i in 1:nrow(vec)) rmat[, , i] <- tMatrixEP_svg(vec[i, ], mag[i])

	}else{

		rmat <- array(NA, dim=c(3,3,length(mag)))
		for(i in 1:length(mag)) rmat[, , i] <- tMatrixEP_svg(vec, mag[i])
	}
	
	#print(rmat)

	# SUPRESS EXPONENTIAL FORMAT FOR NEARLY ZERO VALUES (CANNOT BE READ BY SVG READER)
	options(scipen=10)
	rmat <- round(rmat, 8)

	# WRITE LINE
	new_line <- paste0('\t<rotate r="', paste(c(rmat), collapse=","), '" ></rotate>')
	
	# IF FILE IS NULL, RETURN LINES OF SVG OBJECTS
	if(is.null(file)) return(new_line)
	
	# SAVE NEW LINES TO FILE
	svgviewr.write(new_line, save_as_fpath=file, append=TRUE)
}