svg.box <- function(x = NULL, ranges = NULL, sides = 1:6, grid.lwd = 1, 
	tick.axes = c(2,3,2), tick.labels = c(2,3,2), tick.lwd = 1, tick.num = 10, 
	tick.label.size = 0.13, tick.label.opacity = 1, axis.label.opacity = 1, 
	axis.label.size = 0.14, grid.opacity = 0.1, file=NULL){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	# Get ranges
	ranges_list <- svgviewr_ranges(x, ranges, tick.num)
	xlim <- ranges_list$xlim
	ylim <- ranges_list$ylim
	zlim <- ranges_list$zlim
	lim <- ranges_list$lim
	max_range <- ranges_list$max_range
	grid_by <- ranges_list$grid_by

	# Create polygons
	polygons <- svg_axis_polygons(xlim, ylim, zlim, sides)
	
	# Create grids
	if(grid.lwd > 0) grids <- svg_axis_grids(polygons, grid_by)

	# Create ticks
	tick_len <- max_range*0.03
	ticks <- svg_axis_ticks(xlim, ylim, zlim, grid_by, tick.label.size, tick_len, tick.axes, 
		tick.labels)

	# Draw polygons
	for(polygon in polygons){
		path <- paste0('M', paste(polygon[1, ], collapse=' '), ' L ', paste(polygon[2, ], collapse=' '), 
			' L ', paste(polygon[3, ], collapse=' '), ' L ', paste(polygon[4, ], collapse=' '), 
			' Z')
		svg.paths(file=file, d=path, col.stroke='none', lwd=0, opacity.fill=0, layer='Bounding panel fill')
		svg.paths(file=file, d=path, opacity.stroke=0.4, lwd=1, col.fill='none', layer='Bounding panel outline')
		#svg.pointsC(file=file, x=polygon, cex=0, opacity.stroke.C=0.4, 
		#	col.fill.C='black', opacity.fill.C=0)
	}

	# Draw grid
	if(grid.lwd > 0) for(grid in grids) svg.lines(file=file, x=grid, col='black', 
		lwd=grid.lwd, opacity=grid.opacity, layer='Grid')

	# Draw ticks
	if(tick.lwd > 0){
		for(i in 1:length(ticks$ticks)){
			svg.lines(file=file, x=ticks$ticks[[i]], col='black', opacity=0.5, layer='Ticks')
		}
	}

	# Draw tick labels
	if(tick.label.size > 0){
		i <- 1
		for(i in 1:length(ticks$ticklabels)){
			svg.text(file=file, x=ticks$ticklabelspos[[i]], labels=ticks$ticklabels[[i]], col='black', 
				opacity=tick.label.opacity, font.size=max_range*tick.label.size, layer='Tick labels')
			i <- i + 1
		}
	}

	# Draw axis labels
	if(tick.label.size > 0){
		i <- 1
		for(i in 1:length(ticks$axislabels)){
			svg.text(file=file, x=ticks$axislabelspos[[i]], labels=ticks$axislabels[[i]], col='black', 
				opacity=axis.label.opacity, font.size=max_range*axis.label.size, layer='Axis labels')
			i <- i + 1
		}
	}
	
	list('lim'=lim)
}