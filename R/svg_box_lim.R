svg_box_lim <- function(ranges, tick.num, lim.exact=FALSE){
	
	if(lim.exact){
		# Trying exact - tick labels may not be correct yet
		rangesr <- ranges
	}else{
		# Round to 1-2 significant digits
		rangesr <- round(ranges, -log(abs(ranges), base=10)+1)
	}
	
	# Make variables for limits
	xlim <- rangesr[, 1]
	ylim <- rangesr[, 2]
	zlim <- rangesr[, 3]
	
	ranges_diff <- rangesr[2, ] - rangesr[1, ]

	# Set increment for grid squares
	max_diff <- max(ranges_diff)
	grid_by <- max_diff / tick.num
	
	if(lim.exact){
		xlim <- range(rangesr[,1])
		ylim <- range(rangesr[,2])
		zlim <- range(rangesr[,3])
	}else{
		xlim <- c(rangesr[1,1]-grid_by, rangesr[1,1]+grid_by*ceiling(ranges_diff[1] / grid_by) + grid_by)
		ylim <- c(rangesr[1,2]-grid_by, rangesr[1,2]+grid_by*ceiling(ranges_diff[2] / grid_by) + grid_by)
		zlim <- c(rangesr[1,3]-grid_by, rangesr[1,3]+grid_by*ceiling(ranges_diff[3] / grid_by) + grid_by)
	}

	return(list(
		'xlim' = xlim,
		'ylim' = ylim,
		'zlim' = zlim,
		'lim' = cbind(xlim, ylim, zlim),
		'max_diff' = max_diff,
		'med_diff' = median(ranges_diff),
		'grid_by' = grid_by
		)
	)
}