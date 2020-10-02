svg.box <- function(x = NULL, ranges = NULL, sides = 1:6, grid.lwd = 1, tick.axes = c(2,3,2), 
	tick.labels = c(2,3,2), tick.lwd = 1, tick.num = 10, tick.label.size = 'auto', 
	tick.label.opacity = 1, axis.label = c('x', 'y', 'z'), axis.label.opacity = 1, 
	axis.label.size = 'auto', grid.opacity = 0.1, 
	axis.col = rgb(0.5,0.5,0.5), grid.col = rgb(0.8,0.8,0.8), text.col = 'black', z.index=0, 
	lim.exact=FALSE, name = NULL, file=NULL){
	
	if('svg' == getOption("svgviewr_glo_type")){

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
	max_diff <- ranges_list$max_diff
	med_diff <- ranges_list$med_diff
	grid_by <- ranges_list$grid_by

	# Create polygons
	polygons <- svg_axis_polygons(xlim, ylim, zlim, sides)
	
	# Create grids
	if(grid.lwd > 0){
		svg_grids <- svg_axis_grids(polygons, grid_by)
		grids <- svg_grids$grid
		grids_type <- svg_grids$type
	}

	# Set label sizes	
	if('svg' == getOption("svgviewr_glo_type")){

		if(tick.label.size == 'auto') tick.label.size <- 0.13
		if(axis.label.size == 'auto') axis.label.size <- 0.14

	}else{

		if(tick.label.size == 'auto') tick.label.size <- 0.02
		if(axis.label.size == 'auto') axis.label.size <- 0.02

	}

	# Create ticks
	tick_len <- max_diff*0.03
	ticks <- svg_axis_ticks(xlim, ylim, zlim, grid_by, tick.label.size, tick_len, tick.axes, 
		tick.labels)
	
	# Set axis labels
	ticks$axislabels <- as.list(axis.label)

	if(getOption("svgviewr_glo_type") == 'svg'){

		# Draw polygons (using pointsC allows for filled polygons but messes up linkR v1.1.1 
		#	drawlinkage because it adds points which then disrupts pathC indices)
		for(i in 1:length(polygons)){
			svg.lines(file=file, x=polygons[[i]], col='black', opacity=0.4, layer='Grid border', z.index=z.index)
			#svg.pointsC(file=file, x=polygons[[i]], cex=0, opacity.stroke.C=0.4, 
			#	col.fill.C='black', opacity.fill.C=0, lwd=1, z.index=z.index, z.index.C=z.index)
		}

		# Draw grid
		if(grid.lwd > 0) for(grid in grids) svg.lines(file=file, x=grid, col='black', 
			lwd=grid.lwd, opacity=grid.opacity, layer='Grid', z.index=z.index)

		# Draw ticks
		if(tick.lwd > 0){
			for(i in 1:length(ticks$ticks)){
				svg.lines(file=file, x=ticks$ticks[[i]], col='black', opacity=0.5, layer='Ticks', z.index=z.index)
			}
		}

		# Draw tick labels
		if(tick.label.size > 0){
			for(i in 1:length(ticks$ticklabels)){
				svg.text(file=file, x=ticks$ticklabelspos[[i]], labels=ticks$ticklabels[[i]], col=text.col, 
					opacity=tick.label.opacity, font.size=max_diff*tick.label.size, layer='Tick labels', z.index=z.index)
			}
		}

		# Draw axis labels
		if(axis.label.size > 0){
			for(i in 1:length(ticks$axislabels)){
				svg.text(file=file, x=ticks$axislabelspos[[i]], labels=ticks$axislabels[[i]], col=text.col, 
					opacity=axis.label.opacity, font.size=max_diff*axis.label.size, layer='Axis labels', z.index=z.index)
			}
		}

	}else{

		## Add objects to svgViewR environment
		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add polygons
		if(is.null(name)){ shape_name <- 'frame.panel' }else{ shape_name <- name }
		for(i in 1:length(polygons)){
			svgviewr_env$svg$line[[length(svgviewr_env$svg$line)+1]] <- list('type'='line', 
				'name'=shape_name, 'x'=t(polygons[[i]]), col=setNames(webColor(axis.col), NULL), lwd=grid.lwd, 
				'depthTest'=TRUE)
		}

		# Add grid
		if(is.null(name)){ shape_name <- 'frame.grid' }else{ shape_name <- name }
		if(grid.lwd > 0){
			grids_in <- grids[grids_type == 'in']
			for(grid in grids_in) svgviewr_env$svg$line[[length(svgviewr_env$svg$line)+1]] <- 
				list('type'='line', 'name'=shape_name, 'x'=t(grid), 'col'=setNames(webColor(grid.col), NULL), 
				'lwd'=grid.lwd, 'depthTest'=TRUE)
		}

		# Add ticks
		if(is.null(name)){ shape_name <- 'frame.tick' }else{ shape_name <- name }
		if(tick.lwd > 0){
			for(i in 1:length(ticks$ticks)){
				svgviewr_env$svg$line[[length(svgviewr_env$svg$line)+1]] <- list('type'='line', 
					'name'=shape_name, 'x'=t(ticks$ticks[[i]]), 'col'=setNames(webColor(axis.col), NULL), 
					'lwd'=grid.lwd, 'depthTest'=TRUE)
			}
		}
		
		# Add tick labels
		if(is.null(name)){ shape_name <- 'frame.ticklabel' }else{ shape_name <- name }
		if(tick.label.size > 0){
			for(i in 1:length(ticks$ticklabels)){
				svg.text(labels=ticks$ticklabels[[i]], name=shape_name, x=ticks$ticklabelspos[[i]], 
					col=setNames(webColor(text.col), NULL), size=max_diff*tick.label.size)
			}
		}

		# Add axis labels
		if(is.null(name)){ shape_name <- 'frame.axislabel' }else{ shape_name <- name }
		if(axis.label.size > 0){
			for(i in 1:length(ticks$axislabels)){
				svg.text(labels=ticks$axislabels[[i]], name=shape_name, x=ticks$axislabelspos[[i]], 
					col=setNames(webColor(text.col), NULL), size=max_diff*axis.label.size)
			}
		}
	}
	
	list('lim'=lim)
}