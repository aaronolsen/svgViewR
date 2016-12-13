fitShapes <- function(read_html, width, height){
	## Fits shapes to plot
	
	params <- read_html$params
	shapes <- read_html$shapes
	
	# Matrix for all coordinates
	xyz_all <- matrix(NA, nrow=0, ncol=3)

	# Fill matrix
	for(lnum in 1:length(shapes)){
		if(is.null(shapes[[lnum]][['xyz']])) print(shapes[[lnum]])
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
		scaling <- c(
			1 / ((((diff_range[1] / 2) * (params$depth - params$eyez)) / (params$eyez * ((width - params$margin) - width/2))) + ((diff_range[3] / 2) / params$eyez)),
			1 / ((((diff_range[2] / 2) * (params$depth - params$eyez)) / (params$eyez * ((height - params$margin) - height/2))) + ((diff_range[3] / 2) / params$eyez))
		)

		# Take minimum scaling
		scaling <- min(scaling)
	}

	# Find center
	center <- colMeans(ranges)

	# Center all shapes
	for(lnum in 1:length(shapes)){
		xyz <- shapes[[lnum]][['xyz']]
		if(length(dim(xyz)) == 2){
			if(ncol(xyz) == 3) shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']] - matrix(center, nrow=nrow(xyz), ncol=3, byrow=TRUE)
			if(ncol(xyz) == 6) shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']] - cbind(matrix(center, nrow=nrow(xyz), ncol=3, byrow=TRUE), matrix(center, nrow=nrow(xyz), ncol=3, byrow=TRUE))
		}else if(length(dim(xyz)) == 3){
			for(i in 1:dim(xyz)[3]) shapes[[lnum]][['xyz']][, , i] <- shapes[[lnum]][['xyz']][, , i] - matrix(center, nrow=dim(xyz)[1], ncol=dim(xyz)[2], byrow=TRUE)
		}
	}

	# Scale all shapes
	for(lnum in 1:length(shapes)) shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']]*scaling

	params[['x.shift']] <- width/2
	params[['y.shift']] <- height/2

	list(
		'params'=params,
		'shapes'=shapes,
		'z.index'=read_html$z.index
	)
}