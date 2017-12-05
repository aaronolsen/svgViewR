svg.box <- function(x = NULL, ranges = NULL, sides = 1:6, grid.lwd = 1, tick.axes = c(2,3,2), 
	tick.labels = c(2,3,2), tick.lwd = 1, tick.num = 10, tick.label.size = 0.13, 
	tick.label.opacity = 1, axis.label.opacity = 1, axis.label.size = 0.14, grid.opacity = 0.1, 
	col = 'black', z.index=0, lim.exact=FALSE, file=NULL){

	if('webgl' != getOption("svgviewr_glo_type")){

		# If file is null, set current connection
		if(is.null(file)){

			# Give error if svg.new has not been called
			if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

			# Get current connection
			file <- getOption("svg_glo_con")
		}
	}

	# If x is not NULL, find ranges
	if(is.null(ranges)) ranges <- svg_ranges(x)

	# Get ranges
	ranges_list <- svg_box_lim(ranges, tick.num, lim.exact=lim.exact)

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

	if('webgl' == getOption("svgviewr_glo_type")){

		## Add objects to svgViewR environment
		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add polygons
		for(i in 1:length(polygons)){
			env$svgviewr_env$lines[[length(svgviewr_env$lines)+1]] <- list('type'='line', 
				'names'='frame.panel', 'xyz'=t(polygons[[i]]), col=webColor(col), lwd=grid.lwd)
		}

		# Add grid
		if(grid.lwd > 0){
			for(grid in grids) env$svgviewr_env$lines[[length(svgviewr_env$lines)+1]] <- 
				list('type'='line', 'names'='frame.grid', 'xyz'=t(grid), 'col'=webColor(col), 
				'lwd'=grid.lwd)
		}

		# Add ticks
		if(tick.lwd > 0){
			for(i in 1:length(ticks$ticks)){
				env$svgviewr_env$lines[[length(svgviewr_env$lines)+1]] <- list('type'='line', 
					'names'='frame.tick', 'xyz'=t(ticks$ticks[[i]]), 'col'=webColor(col), 
					'lwd'=grid.lwd)
			}
		}
		
		# Add tick labels
		if(tick.label.size > 0){
			for(i in 1:length(ticks$ticklabels)){
				env$svgviewr_env$text[[length(svgviewr_env$text)+1]] <- list('type'='text', 
					'labels'=ticks$ticklabels[[i]], 'names'='frame.ticklabel', 
					'x'=ticks$ticklabelspos[[i]], 'col'=webColor(col), 
					'size'=max_range*tick.label.size)
			}
		}

		# Add axis labels
		if(axis.label.size > 0){
			for(i in 1:length(ticks$axislabels)){
				env$svgviewr_env$text[[length(svgviewr_env$text)+1]] <- list('type'='text', 
					'labels'=ticks$axislabels[[i]], 'names'='frame.axislabel', 
					'x'=ticks$axislabelspos[[i]], 'col'=webColor(col), 
					'size'=max_range*axis.label.size)
			}
		}

	}else{

		# Draw polygons
		for(i in 1:length(polygons)){
			svg.pointsC(file=file, x=polygons[[i]], cex=0, opacity.stroke.C=0.4, 
				col.fill.C=col, opacity.fill.C=0, lwd=1, z.index=z.index, z.index.C=z.index)
		}

		# Draw grid
		if(grid.lwd > 0) for(grid in grids) svg.lines(file=file, x=grid, col=col, 
			lwd=grid.lwd, opacity=grid.opacity, layer='Grid', z.index=z.index)

		# Draw ticks
		if(tick.lwd > 0){
			for(i in 1:length(ticks$ticks)){
				svg.lines(file=file, x=ticks$ticks[[i]], col=col, opacity=0.5, layer='Ticks', z.index=z.index)
			}
		}

		# Draw tick labels
		if(tick.label.size > 0){
			for(i in 1:length(ticks$ticklabels)){
				svg.text(file=file, x=ticks$ticklabelspos[[i]], labels=ticks$ticklabels[[i]], col=col, 
					opacity=tick.label.opacity, font.size=max_range*tick.label.size, layer='Tick labels', z.index=z.index)
			}
		}

		# Draw axis labels
		if(axis.label.size > 0){
			for(i in 1:length(ticks$axislabels)){
				svg.text(file=file, x=ticks$axislabelspos[[i]], labels=ticks$axislabels[[i]], col=col, 
					opacity=axis.label.opacity, font.size=max_range*axis.label.size, layer='Axis labels', z.index=z.index)
			}
		}

	}
	
	list('lim'=lim)
}