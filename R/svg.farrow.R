svg.farrow <- function(ends=rbind(c(0,0,0), c(1,0,0)), head.width=0.25, head.length=0.25, 
	depth=0.1, width=0.2, axes=c(1,0,0), seg=1, col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, 
	ontop = FALSE, name='farrow'){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Flat arrow drawing is currently only available with webgl svgViewR output.")
	
	# Repeat some params if length 1
	if(length(seg) == 1) seg <- rep(seg, 2)

	# Make sure ends is matrix
	if(class(ends) == 'data.frame') stop("'ends' must be a vector or matrix, not a data.frame.")

	# Make sure axes is matrix
	if(is.vector(axes)) axes <- matrix(axes, 1, 3)

	if(nrow(axes) == 3) stop("If 'ends' is a 2-row matrix then 'axes' must be a 1 or 2-row matrix.")

	# Set axes
	if(nrow(axes) == 1){
		axes <- rbind(ends[2,]-ends[1,], axes, cprod_svg(ends[2,]-ends[1,], axes))
	}else if(nrow(axes) == 2){
		axes <- rbind(cprod_svg(axes[1,], axes[2,]))
	}

	# Make unit
	axes <- uvector_svg(axes)

	
	## Create arrow
	# Set 7 vertices of arrow
	arr_vert <- rbind(
		'headtip'=ends[2,],
		'headbase1'=ends[2,] - head.length*axes[1,] + head.width*axes[2,],
		'headbase2'=ends[2,] - head.length*axes[1,] - head.width*axes[2,],
		'shaftneck1'=ends[2,] - head.length*axes[1,] + width*axes[2,],
		'shaftneck2'=ends[2,] - head.length*axes[1,] - width*axes[2,],
		'shaftfoot1'=ends[1,] + width*axes[2,],
		'shaftfoot2'=ends[1,] - width*axes[2,]
	)
	
	# Duplicate vertices to create each main face of the arrow
	arr_vert_a <- arr_vert + matrix(depth*axes[3,], 7, 3, byrow=TRUE)
	arr_vert_b <- arr_vert - matrix(depth*axes[3,], 7, 3, byrow=TRUE)
	
	# Create each face of the arrow
	# Shaft planes
	svg.plane(corners=rbind(arr_vert_b['shaftfoot1', ], arr_vert_b['shaftfoot2', ], arr_vert_a['shaftfoot2', ], arr_vert_a['shaftfoot1', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_a['shaftfoot1', ], arr_vert_a['shaftneck1', ], arr_vert_a['shaftneck2', ], arr_vert_a['shaftfoot2', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_b['shaftfoot2', ], arr_vert_b['shaftneck2', ], arr_vert_b['shaftneck1', ], arr_vert_b['shaftfoot1', ]),
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_b['shaftfoot1', ], arr_vert_b['shaftneck1', ], arr_vert_a['shaftneck1', ], arr_vert_a['shaftfoot1', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_b['shaftfoot2', ], arr_vert_b['shaftneck2', ], arr_vert_a['shaftneck2', ], arr_vert_a['shaftfoot2', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)

	# Head planes
	svg.plane(corners=rbind(arr_vert_a['shaftneck1', ], arr_vert_a['headbase1', ], arr_vert_b['headbase1', ], arr_vert_b['shaftneck1', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_a['shaftneck2', ], arr_vert_a['headbase2', ], arr_vert_b['headbase2', ], arr_vert_b['shaftneck2', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_a['headbase1', ], arr_vert_a['headtip', ], arr_vert_b['headtip', ], arr_vert_b['headbase1', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	svg.plane(corners=rbind(arr_vert_a['headbase2', ], arr_vert_a['headtip', ], arr_vert_b['headtip', ], arr_vert_b['headbase2', ]), 
		seg=seg, col=col, emissive=emissive, opacity=opacity, ontop=ontop, name=name)

    # Create triangle head planes
	svg.triangle(corners=rbind(arr_vert_a['headbase1', ], arr_vert_a['headtip', ], arr_vert_a['headbase2', ]), seg=1)
	svg.triangle(corners=rbind(arr_vert_b['headbase1', ], arr_vert_b['headtip', ], arr_vert_b['headbase2', ]), seg=1)
}