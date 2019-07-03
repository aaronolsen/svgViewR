svg.farrow <- function(ends=NULL, center=NULL, length=NULL, width.prop=0.05, width=NULL, 
	head.width.prop=1.5*width.prop, head.width=NULL, head.length.prop=0.2, head.length=NULL, 
	depth.prop=0.05, depth=NULL, axes=c(1,0,0), type=c('single', 'double')[1], seg=c(1,1), col='blue', 
	emissive=rgb(0.03, 0.15, 0.21), opacity = 1, ontop = FALSE, name='farrow', return.shape=FALSE, 
	plot=TRUE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")
	
	# Can input separate segments for shaft and head, if not repeat
	if(length(seg) == 1) seg <- rep(seg, 2)

	if(is.null(ends)){

		# Check that required parameters are not NULL		
		if(is.null(center)) stop("'center' must be a non-NULL if ends is NULL.")
		if(is.null(length)) stop("'length' must be a non-NULL if ends is NULL.")

	}else{

		if(!is.null(length)) warning("'length' will be ignored because length is already set by 'ends'.")

		# Set length from ends
		length <- dppt_svg(ends)
	}

	# Set lengths from relative if absolutes are null
	if(is.null(width)) width <- width.prop*length
	if(is.null(head.width)) head.width <- head.width.prop*length
	if(is.null(head.length)) head.length <- head.length.prop*length
	if(is.null(depth)) depth <- depth.prop*length

	if(is.null(ends)){
		
		# Check that head length is less than length
		if(head.length >= length) stop("Head length must be less than length.")

		# Make axes 3 rows if only 2
		if(!is.matrix(axes) || nrow(axes) == 1) axes <- rbind(axes, vorthogonal_svg(axes))
		if(nrow(axes) == 2) axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))

		# Make unit
		axes <- uvector_svg(axes)

		# Set ends based on center, length, and axes
		ends <- rbind(center - (length/2)*axes[1,], center + (length/2)*axes[1,])

	}else{

		# Make sure ends is matrix
		if(class(ends) == 'data.frame') stop("'ends' must be a vector or matrix, not a data.frame.")

		# Make sure axes is matrix
		if(is.vector(axes)) axes <- matrix(axes, 1, 3)

		#
		if(nrow(axes) == 3) stop("If 'ends' is a 2-row matrix then 'axes' must be a 1 or 2-row matrix.")

		# Set axes
		if(nrow(axes) == 1){
			axes <- rbind(ends[2,]-ends[1,], axes, cprod_svg(ends[2,]-ends[1,], axes))
		}else if(nrow(axes) == 2){
			axes <- rbind(cprod_svg(axes[1,], axes[2,]))
		}

		# Make unit
		axes <- uvector_svg(axes)
	}

	## Create arrow
	# Set 7 vertices of arrow
	if(type == 'single'){
		arr_vert <- rbind(
			'headtip'=ends[2,],
			'headbase1'=ends[2,] - head.length*axes[1,] + head.width*axes[2,],
			'headbase2'=ends[2,] - head.length*axes[1,] - head.width*axes[2,],
			'shaftneck1'=ends[2,] - head.length*axes[1,] + width*axes[2,],
			'shaftneck2'=ends[2,] - head.length*axes[1,] - width*axes[2,],
			'shaftfoot1'=ends[1,] + width*axes[2,],
			'shaftfoot2'=ends[1,] - width*axes[2,]
		)
	}else{
		arr_vert <- rbind(
			'headtip'=ends[2,],
			'headbase1'=ends[2,] - head.length*axes[1,] + head.width*axes[2,],
			'headbase2'=ends[2,] - head.length*axes[1,] - head.width*axes[2,],
			'shaftneck1'=ends[2,] - head.length*axes[1,] + width*axes[2,],
			'shaftneck2'=ends[2,] - head.length*axes[1,] - width*axes[2,],
			'shaftfoot1'=ends[1,] + head.length*axes[1,] + width*axes[2,],
			'shaftfoot2'=ends[1,] + head.length*axes[1,] - width*axes[2,],
			'foottip'=ends[1,],
			'footbase1'=ends[1,] + head.length*axes[1,] + head.width*axes[2,],
			'footbase2'=ends[1,] + head.length*axes[1,] - head.width*axes[2,]
		)
	}
	
	# Duplicate vertices to create each main face of the arrow
	arr_vert_a <- arr_vert + matrix((depth/2)*axes[3,], nrow(arr_vert), ncol(arr_vert), byrow=TRUE)
	arr_vert_b <- arr_vert - matrix((depth/2)*axes[3,], nrow(arr_vert), ncol(arr_vert), byrow=TRUE)
	
	# Create each face of the arrow
	svg_shape <- list()

	## Shaft planes
	# End cap
	if(type == 'single'){
		svg_shape[[1]] <- svg.plane(corners=rbind(arr_vert_b['shaftfoot1', ], arr_vert_b['shaftfoot2', ], arr_vert_a['shaftfoot2', ], arr_vert_a['shaftfoot1', ]), 
			seg=seg[1], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	}

	svg_shape[[2]] <- svg.plane(corners=rbind(arr_vert_a['shaftfoot1', ], arr_vert_a['shaftneck1', ], arr_vert_a['shaftneck2', ], arr_vert_a['shaftfoot2', ]), 
		seg=seg[1], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[3]] <- svg.plane(corners=rbind(arr_vert_b['shaftfoot2', ], arr_vert_b['shaftneck2', ], arr_vert_b['shaftneck1', ], arr_vert_b['shaftfoot1', ]),
		seg=seg[1], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[4]] <- svg.plane(corners=rbind(arr_vert_b['shaftfoot1', ], arr_vert_b['shaftneck1', ], arr_vert_a['shaftneck1', ], arr_vert_a['shaftfoot1', ]), 
		seg=seg[1], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[5]] <- svg.plane(corners=rbind(arr_vert_b['shaftfoot2', ], arr_vert_b['shaftneck2', ], arr_vert_a['shaftneck2', ], arr_vert_a['shaftfoot2', ]), 
		seg=seg[1], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)

	## Head planes
	svg_shape[[6]] <- svg.plane(corners=rbind(arr_vert_a['shaftneck1', ], arr_vert_a['headbase1', ], arr_vert_b['headbase1', ], arr_vert_b['shaftneck1', ]), 
		seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[7]] <- svg.plane(corners=rbind(arr_vert_a['shaftneck2', ], arr_vert_a['headbase2', ], arr_vert_b['headbase2', ], arr_vert_b['shaftneck2', ]), 
		seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[8]] <- svg.plane(corners=rbind(arr_vert_a['headbase1', ], arr_vert_a['headtip', ], arr_vert_b['headtip', ], arr_vert_b['headbase1', ]), 
		seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[9]] <- svg.plane(corners=rbind(arr_vert_a['headbase2', ], arr_vert_a['headtip', ], arr_vert_b['headtip', ], arr_vert_b['headbase2', ]), 
		seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)

    # Create triangle head planes
	svg_shape[[10]] <- svg.triangle(corners=rbind(arr_vert_a['headbase1', ], arr_vert_a['headtip', ], arr_vert_a['headbase2', ]), 
		seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	svg_shape[[11]] <- svg.triangle(corners=rbind(arr_vert_b['headbase1', ], arr_vert_b['headtip', ], arr_vert_b['headbase2', ]), 
		seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)


	## Foot planes
	if(type == 'double'){

		svg_shape[[12]] <- svg.plane(corners=rbind(arr_vert_a['shaftfoot1', ], arr_vert_a['footbase1', ], arr_vert_b['footbase1', ], arr_vert_b['shaftfoot1', ]), 
			seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
		svg_shape[[13]] <- svg.plane(corners=rbind(arr_vert_a['shaftfoot2', ], arr_vert_a['footbase2', ], arr_vert_b['footbase2', ], arr_vert_b['shaftfoot2', ]), 
			seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
		svg_shape[[14]] <- svg.plane(corners=rbind(arr_vert_a['footbase1', ], arr_vert_a['foottip', ], arr_vert_b['foottip', ], arr_vert_b['footbase1', ]), 
			seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
		svg_shape[[15]] <- svg.plane(corners=rbind(arr_vert_a['footbase2', ], arr_vert_a['foottip', ], arr_vert_b['foottip', ], arr_vert_b['footbase2', ]), 
			seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)

		# Create triangle foot planes
		svg_shape[[16]] <- svg.triangle(corners=rbind(arr_vert_a['footbase1', ], arr_vert_a['foottip', ], arr_vert_a['footbase2', ]), 
			seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
		svg_shape[[17]] <- svg.triangle(corners=rbind(arr_vert_b['footbase1', ], arr_vert_b['foottip', ], arr_vert_b['footbase2', ]), 
			seg=seg[2], col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name, plot=plot)
	}

	if(return.shape || !plot){
	
		# Matrix for all vertices and faces
		vertices <- matrix(NA, 0, 3)
		faces <- matrix(NA, 0, 3)
		
		# 
		for(i in 1:length(svg_shape)){
			faces <- rbind(faces, svg_shape[[i]]$faces + nrow(vertices))
			vertices <- rbind(vertices, svg_shape[[i]]$vertices)
		}

		return(list('vertices'=vertices, 'faces'=faces))

	}else{
		# Suppress return of value in console
		return(NULL)
	}
}