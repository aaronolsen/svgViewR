findPlotDims <- function(read_html, width, height, margin = NULL){
	
	params <- read_html$params
	shapes <- read_html$shapes
	#params$margin <- 0
	
	if(!is.null(margin)) params$margin <- margin
	
	if(length(params$margin) == 1) params$margin <- rep(params$margin, 2)
	
	# Matrix for all coordinates
	xyz_all <- matrix(NA, nrow=0, ncol=3)

	# Fill matrix
	for(lnum in 1:length(shapes)){

		if(is.null(shapes[[lnum]][['xyz']])) next

		xyz <- shapes[[lnum]][['xyz']]
		if(length(dim(xyz)) == 2){
			if(ncol(xyz) == 3) xyz_all <- rbind(xyz_all, xyz)
			if(ncol(xyz) == 6) xyz_all <- rbind(xyz_all, xyz[, 1:3], xyz[, 4:6])
		}else if(length(dim(xyz)) == 3){
			for(i in 1:dim(xyz)[3]) xyz_all <- rbind(xyz_all, xyz[, , i])
		}
	}
	
	# Get ranges
	ranges <- apply(xyz_all, 2, 'range')
	dimnames(ranges) <- list(c('min', 'max'), c('x', 'y', 'z'))
	diff_range <- ranges[2, ] - ranges[1, ]
	
	# If all ranges are zero, make scaling 1
	if(sum(abs(diff_range)) == 0){
		scaling <- 1
	}else{

		# Solve for scaling from each dimension
		scaling <- c(NA, NA)
		if(width != 'default') scaling[1] <- 1 / ((((diff_range[1] / 2) * (params$depth - params$eyez)) / (params$eyez * ((width - params$margin[1]) - width/2))) + ((diff_range[3] / 2) / params$eyez))
		if(height != 'default') scaling[2] <- 1 / ((((diff_range[2] / 2) * (params$depth - params$eyez)) / (params$eyez * ((height - params$margin[2]) - height/2))) + ((diff_range[3] / 2) / params$eyez))

		# Take minimum scaling
		scaling <- min(scaling, na.rm=TRUE)
	}
	
	# Find center of the range of each dimension
	center <- colMeans(ranges, na.rm=TRUE)

	# Center and scale coordinates
	xyz_all <- xyz_all - matrix(center, nrow=nrow(xyz_all), ncol=3, byrow=TRUE)
	xyz_all <- xyz_all*scaling

	# Project all coordinates
	u1 <- -(params$depth - params$eyez) / (xyz_all[,3] - params$eyez)
	xy_all <- cbind(u1 * xyz_all[,1], -u1 * xyz_all[,2])

	# Find coordinate range
	ranges_xy <- apply(xy_all, 2, 'range')
	
	# Find height to width ratio
	ranges_xy_diff <- abs(ranges_xy[2, ] - ranges_xy[1, ])
	hw <- ranges_xy_diff[2] / ranges_xy_diff[1]

	# If only one of dimensions is default, set based on h:w ratio
	if(height != 'default' && width == 'default') width <- height*(1/hw)
	if(height == 'default' && width != 'default') height <- width*hw

	# Find additional scaling to fit dimensions
	scaling_add <- 1 + ((width - params$margin[1]*2) - ranges_xy_diff[1]) / ranges_xy_diff[1]

	# Test on range
	#params[['x.shift']] <- width/2
	#params[['y.shift']] <- height/2
	#shift <- c(width/2, height/2)
	ranges_xy <- ranges_xy*scaling_add
	shift_add <- params$margin - ranges_xy[1, ]
	#ranges_xy <- (ranges_xy + rbind(shift.add, shift.add))

	list(
		'ranges'=ranges,
		'scaling'=scaling,
		'center'=center,
		'width'=width,
		'height'=height,
		'shift.add'=shift_add,
		'scaling.add'=scaling_add,
		'hw'=hw
	)
}