svg_axis_polygons <- function(xlim, ylim, zlim, sides){

	polygons <- list()
	
	# XY plane at origin
	polygons[[1]] <- rbind(c(xlim[1], ylim[1], zlim[1]), c(xlim[2], ylim[1], zlim[1]), 
		c(xlim[2], ylim[2], zlim[1]), c(xlim[1], ylim[2], zlim[1]), c(xlim[1], ylim[1], zlim[1]))

	# YZ plane at origin
	polygons[[2]] <- rbind(c(xlim[1], ylim[1], zlim[1]), c(xlim[1], ylim[2], zlim[1]),
		c(xlim[1], ylim[2], zlim[2]), c(xlim[1], ylim[1], zlim[2]), c(xlim[1], ylim[1], zlim[1]))

	# XZ plane at origin
	polygons[[3]] <- rbind(c(xlim[1], ylim[1], zlim[1]), c(xlim[2], ylim[1], zlim[1]), 
		c(xlim[2], ylim[1], zlim[2]), c(xlim[1], ylim[1], zlim[2]), c(xlim[1], ylim[1], zlim[1]))

	# XY plane not at origin
	polygons[[4]] <- rbind(c(xlim[1], ylim[1], zlim[2]), c(xlim[2], ylim[1], zlim[2]), 
		c(xlim[2], ylim[2], zlim[2]), c(xlim[1], ylim[2], zlim[2]), c(xlim[1], ylim[1], zlim[2]))
	
	# YZ plane not at origin
	polygons[[5]] <- rbind(c(xlim[2], ylim[1], zlim[1]), c(xlim[2], ylim[2], zlim[1]), 
		c(xlim[2], ylim[2], zlim[2]), c(xlim[2], ylim[1], zlim[2]), c(xlim[2], ylim[1], zlim[1]))

	# XZ plane not at origin
	polygons[[6]] <- rbind(c(xlim[1], ylim[2], zlim[1]), c(xlim[2], ylim[2], zlim[1]), 
		c(xlim[2], ylim[2], zlim[2]), c(xlim[1], ylim[2], zlim[2]), c(xlim[1], ylim[2], zlim[1]))

	# Restrict to input sides
	polygons <- polygons[sides]
	
	polygons
}