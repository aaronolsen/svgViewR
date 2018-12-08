svg.bboxLight <- function(x = c(1,1,1), col = '#FFFFDD', intensity = 1, distance = 3, 
	hidden = TRUE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Lighting is currently only available with webgl svgViewR output.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# If is vector, convert to matrix
	if(is.vector(x)) x <- matrix(x, nrow=1, ncol=3)

	#
	for(i in 1:nrow(x)){

		# Add to environment
		add_at <- length(svgviewr_env$svg$bboxLight)+1

		# Add vertices
		svgviewr_env$svg$bboxLight[[add_at]] <- list()
		svgviewr_env$svg$bboxLight[[add_at]]$x <- x[i,]
		svgviewr_env$svg$bboxLight[[add_at]]$col <- setNames(webColor(col), NULL)
		svgviewr_env$svg$bboxLight[[add_at]]$intensity <- intensity
		svgviewr_env$svg$bboxLight[[add_at]]$distance <- distance
		svgviewr_env$svg$bboxLight[[add_at]]$hidden <- hidden
	}

	ret = NULL
}