fitShapes <- function(read_html, plot.dims, margin = NULL){
	## Fits shapes to plot
	
	params <- read_html$params
	shapes <- read_html$shapes
	width <- plot.dims$width
	height <- plot.dims$height

	center <- plot.dims$center
	scaling <- plot.dims$scaling

	# Center and scale
	for(lnum in 1:length(shapes)){

		if(is.null(shapes[[lnum]][['xyz']])) next
	
		# Center xyz coordinates
		xyz <- shapes[[lnum]][['xyz']]
		if(length(dim(xyz)) == 2){
			if(ncol(xyz) == 3) shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']] - matrix(center, nrow=nrow(xyz), ncol=3, byrow=TRUE)
			if(ncol(xyz) == 6) shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']] - cbind(matrix(center, nrow=nrow(xyz), ncol=3, byrow=TRUE), matrix(center, nrow=nrow(xyz), ncol=3, byrow=TRUE))
		}else if(length(dim(xyz)) == 3){
			for(i in 1:dim(xyz)[3]) shapes[[lnum]][['xyz']][, , i] <- shapes[[lnum]][['xyz']][, , i] - matrix(center, nrow=dim(xyz)[1], ncol=dim(xyz)[2], byrow=TRUE)
		}

		# Scale shapes
		shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']]*scaling

		# Scale arrowhead length
		if(shapes[[lnum]][['type']] == 'arrow') shapes[[lnum]][['l']] <- shapes[[lnum]][['l']]*scaling
		if(shapes[[lnum]][['type']] == 'text') shapes[[lnum]][['font-size']] <- shapes[[lnum]][['font-size']]*scaling
	}

	# Add additional shift and scaling parameters
	params[['shift.add']] <- plot.dims$shift.add
	params[['scaling.add']] <- plot.dims$scaling.add

	list(
		'params'=params,
		'shapes'=shapes,
		'z.index'=read_html$z.index
	)
}