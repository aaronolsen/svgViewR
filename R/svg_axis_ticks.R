svg_axis_ticks <- function(xlim, ylim, zlim, grid_by, label.size, tick_len, tick.axes, tick.labels){

	#options(scipen=999)

	ranges <- cbind(xlim, ylim, zlim)
	max_range <- max(ranges[2, ] - ranges[1, ])

	ticks <- list()
	ticklabels <- list()
	ticklabelspos <- list()
	axislabels <- list()
	axislabelspos <- list()
	
	# Set indices for vectors to define ticks
	tick_idxs <- list(
		c(1,2,1,1,1,1, 1,2,1,1,2,2),
		c(1,2,1,1,2,2, 1,2,1,1,1,1),
		c(1,2,2,2,1,1, 1,2,2,2,2,2),
		c(1,2,2,2,2,2, 1,2,2,2,1,1),

		c(1,1,1,2,1,1, 2,2,1,2,1,1),
		c(1,1,1,2,2,2, 1,1,1,2,1,1),
		c(2,2,1,2,1,1, 1,1,1,2,1,1),
		c(2,2,1,2,2,2, 1,1,1,2,2,2),

		c(1,1,1,1,1,2, 2,2,1,1,1,2),
		c(2,2,1,1,1,2, 1,1,1,1,1,2),
		c(1,1,2,2,1,2, 2,2,2,2,1,2),
		c(2,2,2,2,1,2, 1,1,2,2,1,2)
	)
	
	# Select tick axes if 3 input
	if(length(tick.axes) == 3) tick.axes <- tick.axes + c(0,4,8)
	if(length(tick.axes) == 3) tick.labels <- tick.labels + c(0,4,8)
	
	tick_axis_ends <- c()
	tick_labels_ends <- c()

	for(i in 1:length(tick_idxs)){
	
		t <- tick_idxs[[i]]

		# Set tick mark points
		x1 <- cbind(seq(xlim[t[1]], xlim[t[2]], by=grid_by), seq(ylim[t[3]], ylim[t[4]], by=grid_by), seq(zlim[t[5]], zlim[t[6]], by=grid_by))
		uv <- -uvector_svg(cbind(seq(xlim[t[7]], xlim[t[8]], by=grid_by), seq(ylim[t[9]], ylim[t[10]], by=grid_by), seq(zlim[t[11]], zlim[t[12]], by=grid_by)) - x1)
		x2 <- x1 + tick_len*uv

		# Add to list
		if(i %in% tick.axes){
			for(j in 1:nrow(x1)){
				
				# Check at ends if tick has already been created
				if(paste(c(x1[j, ], x2[j, ]), collapse='_') %in% tick_axis_ends) next

				ticks[[length(ticks)+1]] <- rbind(x1[j, ], x2[j, ])
				if(j == 1 || j == nrow(x1)) tick_axis_ends <- c(tick_axis_ends, paste(c(x1[j, ], x2[j, ]), collapse='_'))
			}
		}

		# Get label dimension
		if(i %in% tick.labels){

			if(t[1] == 1 && t[2] == 2){
				labels <- seq(xlim[1], xlim[2], by=grid_by)
				axis <- 'x'
			}
			if(t[3] == 1 && t[4] == 2){
				labels <- seq(ylim[1], ylim[2], by=grid_by)
				axis <- 'y'
			}
			if(t[5] == 1 && t[6] == 2){
				labels <- seq(zlim[1], zlim[2], by=grid_by)
				axis <- 'z'
			}
			
			labels <- round(labels, grid_by*10)
			labels <- format(labels, digits=1)

			# Set tick labels
			for(j in 1:nrow(x1)){

				# Set position
				pos <- x1[j, ] + 2*tick_len*uv[j, ]
				
				# Adjust vertical alignment to mid point of tick
				pos[2] <- pos[2] - max_range*label.size*0.07
				
				# Check at ends if tick has already been created
				if(paste(pos, collapse='_') %in% tick_labels_ends) next

				ticklabels[[length(ticklabels)+1]] <- labels[j]
				ticklabelspos[[length(ticklabelspos)+1]] <- pos

				if(j == 1 || j == nrow(x1)) tick_labels_ends <- c(tick_labels_ends, paste(pos, collapse='_'))
			}

			# Set axis label position			
			pos <- colMeans(x1) + 5*tick_len*colMeans(uv)

			# Adjust vertical alignment to mid point of axis
			pos[2] <- pos[2] - max_range*label.size*0.07
			
			axislabels[[length(axislabels)+1]] <- axis
			axislabelspos[[length(axislabelspos)+1]] <- pos
		}
	}
	
	return(list(
		'ticks'=ticks,
		'ticklabels'=ticklabels,
		'ticklabelspos'=ticklabelspos,
		'axislabels'=axislabels,
		'axislabelspos'=axislabelspos
		)
	)	
}