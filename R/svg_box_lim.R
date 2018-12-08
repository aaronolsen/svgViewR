svg_box_lim <- function(ranges, tick.num, lim.exact=FALSE){
	
	if(lim.exact){
		# Trying exact - tick labels may not be correct yet
		rangesr <- ranges
	}else{
		# Round to 3 significant digits
		rangesr <- signif(ranges, digits=3)
	}
	
	# Make variables for limits
	xlim <- rangesr[, 1]
	ylim <- rangesr[, 2]
	zlim <- rangesr[, 3]
	
	ranges_diff <- rangesr[2, ] - rangesr[1, ]

	# Set increment for grid squares
	max_diff <- max(ranges_diff, na.rm=TRUE)
	grid_by <- max_diff / tick.num
	
	if(lim.exact){
		xlim <- range(rangesr[,1], na.rm=TRUE)
		ylim <- range(rangesr[,2], na.rm=TRUE)
		zlim <- range(rangesr[,3], na.rm=TRUE)
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
		'med_diff' = median(ranges_diff, na.rm=TRUE),
		'grid_by' = grid_by
		)
	)
}