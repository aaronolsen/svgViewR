svg.new <- function(file, window.title="SVG Viewer", animate.duration = 1, 
	animate.reverse = FALSE, animate.repeat = -1, margin = 20, col = "white", 
	show.control = TRUE, start.rotate = TRUE, layers = NULL, connection = TRUE, 
	fdir = NULL, debug = FALSE){

	# Set connection type
	conn.type <- 'open'
	if(!connection) conn.type <- 'new'

	# Get basic viewer document lines
	con <- svgviewr.new(file=file, window.title=window.title, 
		animate.duration=animate.duration, animate.reverse=animate.reverse, 
		animate.repeat=animate.repeat, margin=margin, col=col, show.control=show.control, 
		start.rotate=start.rotate, layers=layers, fdir=fdir, debug=debug, conn.type=conn.type)

	if(connection){
		if(!is.null(layers)) con$layers <- layers
		if(!is.null(fdir)) con$fdir <- fdir
		if(!is.null(debug)) con$debug <- debug
	}

	# Save connection through options to enable global access
	options("svg_glo_con"=con)

	# Suppress return of value in console
	ret = NULL
}