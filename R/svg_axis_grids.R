svg_axis_grids <- function(polygons, grid_by){

	grid <- list()
	
	#
	type <- c()
	
	## Create grids
	for(i in 1:length(polygons)){

		# Create grid in one direction
		x1 <- cbind(seq(polygons[[i]][1, 1], polygons[[i]][2, 1], by=grid_by),
			seq(polygons[[i]][1, 2], polygons[[i]][2, 2], by=grid_by),
			seq(polygons[[i]][1, 3], polygons[[i]][2, 3], by=grid_by))

		x2 <- cbind(seq(polygons[[i]][4, 1], polygons[[i]][3, 1], by=grid_by),
			seq(polygons[[i]][4, 2], polygons[[i]][3, 2], by=grid_by),
			seq(polygons[[i]][4, 3], polygons[[i]][3, 3], by=grid_by))
		
		type <- c(type, c('out', rep('in', nrow(x1)-2), 'out'))
		for(j in 1:nrow(x1)) grid[[length(grid)+1]] <- rbind(c(x1[j, ]), c(x2[j, ]))

		# Create grid in other direction
		x1 <- cbind(seq(polygons[[i]][2, 1], polygons[[i]][3, 1], by=grid_by),
			seq(polygons[[i]][2, 2], polygons[[i]][3, 2], by=grid_by),
			seq(polygons[[i]][2, 3], polygons[[i]][3, 3], by=grid_by))

		x2 <- cbind(seq(polygons[[i]][5, 1], polygons[[i]][4, 1], by=grid_by),
			seq(polygons[[i]][5, 2], polygons[[i]][4, 2], by=grid_by),
			seq(polygons[[i]][5, 3], polygons[[i]][4, 3], by=grid_by))
	
		type <- c(type, c('out', rep('in', nrow(x1)-2), 'out'))
		for(j in 1:nrow(x1)) grid[[length(grid)+1]] <- rbind(c(x1[j, ]), c(x2[j, ]))
	}
	
	list('grid'=grid, 'type'=type)
}