svgviewr_ranges <- function(x, ranges, tick.num){
	
	if(!is.null(x) && is.array(x)) ranges <- apply(x, 2, 'range', na.rm=TRUE)
	if(!is.null(x) && is.matrix(x)) ranges <- apply(x, 2, 'range', na.rm=TRUE)
	
	# Round to 1-2 significant digits
	rangesr <- round(ranges, -log(abs(ranges), base=10)+1)

	# Make variables for limits
	xlim <- rangesr[, 1]
	ylim <- rangesr[, 2]
	zlim <- rangesr[, 3]
	
	ranges_diff <- rangesr[2, ] - rangesr[1, ]

	# Set increment for grid squares
	max_range <- max(ranges_diff)
	grid_by <- max_range / tick.num
	
	xlim <- c(rangesr[1,1]-grid_by, rangesr[1,1]+grid_by*ceiling(ranges_diff[1] / grid_by) + grid_by)
	ylim <- c(rangesr[1,2]-grid_by, rangesr[1,2]+grid_by*ceiling(ranges_diff[2] / grid_by) + grid_by)
	zlim <- c(rangesr[1,3]-grid_by, rangesr[1,3]+grid_by*ceiling(ranges_diff[3] / grid_by) + grid_by)
	
	return(list(
		'xlim' = xlim,
		'ylim' = ylim,
		'zlim' = zlim,
		'lim' = cbind(xlim, ylim, zlim),
		'max_range' = max_range,
		'grid_by' = grid_by
		)
	)
}