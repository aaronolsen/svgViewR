svg.coupler <- function(end, axes, base.width, side.sep, side.length, name = 'coupler',
	col = 'blue', emissive=rgb(0.03, 0.15, 0.21)){

	# Make sure axes are unit vectors
	axes <- uvector_svg(axes)

	# Set side width
	side_width <- (base.width[3] - side.sep)/2

	# Add base of coupler
	svg.cuboid(ends=end, axes=axes, width=base.width, name=name, col=col, emissive=emissive)

	# Add side
	svg.cuboid(ends=end+base.width[1]*axes[1,]+((side.sep/2)+(side_width/2))*axes[3,], 
		axes=axes, width=c(side.length,base.width[2],side_width), name=name, col=col, 
		emissive=emissive)
	svg.cuboid(ends=end+base.width[1]*axes[1,]-((side.sep/2)+(side_width/2))*axes[3,], 
		axes=axes, width=c(side.length,base.width[2],side_width), name=name, col=col, 
		emissive=emissive)

	ret = NULL
}