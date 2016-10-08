svg.close <- function(){

	# Give error if svg.new has not been called
	if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

	# Get current connection
	file <- getOption("svg_glo_con")
	
	# Close
	svgviewr.new(file=file, conn.type='close', layers=file$layers, fdir=file$fdir, debug=file$debug)

	# Suppress return of value in console
	ret = NULL
}