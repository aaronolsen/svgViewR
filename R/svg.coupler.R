svg.coupler <- function(end, axes, base.width, side.sep, side.length, name = 'coupler',
	col = 'blue', emissive=rgb(0.03, 0.15, 0.21), opacity=1){

	# Make sure there are 3 axes
	if(nrow(axes) == 2) axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))
	if(nrow(axes) < 3) stop("'axes' must be a 3-row matrix.")
	
	# Check base.width
	if(length(base.width) == 2) stop("'base.width' must be a 3-length vector.")

	# Make sure axes are unit vectors
	axes <- uvector_svg(axes)

	# Set side width
	side_width <- (base.width[3] - side.sep)/2

	# Add base of coupler
	svg.cuboid(ends=end, axes=axes, width=base.width, name=name, col=col, emissive=emissive, opacity=opacity)

	# Add side
	svg.cuboid(ends=end+base.width[1]*axes[1,]+((side.sep/2)+(side_width/2))*axes[3,], 
		axes=axes, width=c(side.length,base.width[2],side_width), name=name, col=col, 
		emissive=emissive, opacity=opacity)

	svg.cuboid(ends=end+base.width[1]*axes[1,]-((side.sep/2)+(side_width/2))*axes[3,], 
		axes=axes, width=c(side.length,base.width[2],side_width), name=name, col=col, 
		emissive=emissive, opacity=opacity)

	ret = NULL
}