svg.translate <- function(vec, mag, file = NULL){

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
		tmat <- matrix(NA, nrow=nrow(vec), ncol=3)
		for(i in 1:nrow(vec)) tmat[i, ] <- vec[i, ]*mag[i]

	}else{

		tmat <- matrix(NA, nrow=length(mag), ncol=3)
		for(i in 1:length(mag)) tmat[i, ] <- vec*mag[i]
	}
	
	# SUPRESS EXPONENTIAL FORMAT FOR NEARLY ZERO VALUES (CANNOT BE READ BY SVG READER)
	options(scipen=10)
	tmat <- round(tmat, 8)

	# WRITE LINE
	new_line <- paste0('\t<translate x="', paste(tmat[, 1], collapse=","), '" y="', paste(tmat[, 2], collapse=","), '" z="', paste(tmat[, 3], collapse=","), '" ></translate>')

	# IF FILE IS NULL, RETURN LINES OF SVG OBJECTS
	if(is.null(file)) return(new_line)
	
	# SAVE NEW LINES TO FILE
	svgviewr.write(new_line, save_as_fpath=file, append=TRUE)
}