svg.arrow <- function(ends = NULL, center = NULL, length = NULL, radius=1, head.radius=radius*1.5, 
	head.prop=0.25, axis=NULL, head.length = NULL, rseg=c(20,30), hseg=c(2,10), 
	open.ended=FALSE, theta.start=0, theta.length=2*pi, col='blue', emissive=rgb(0.03, 0.15, 0.21), 
	opacity = 1, ontop = FALSE, name='arrow'){

	# Check that only single name given
	if(length(name) > 1) stop("Input parameter 'name' is a vector of length greater than one. 'name' must be of length 1.")

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Arrow drawing is currently only available with webgl svgViewR output.")
	
	# Check inputs
	if(head.prop < 0 || head.prop > 1) stop(paste0("head.prop (", head.prop, ") must be a value between 0 and 1."))
	
	# Repeat some params if length 1
	if(length(rseg) == 1) rseg <- rep(rseg, 2)
	if(length(hseg) == 1) hseg <- rep(hseg, 2)

	# If center and length are provided, find ends
	if(is.null(ends) && !is.null(center) && !is.null(length) && is.null(axis)) stop("If center and length are provided axis must also be provided")
	if(is.null(ends) && !is.null(center) && !is.null(length) && !is.null(axis)){

		# Make sure vector is unit length
		axis <- uvector_svg(axis)

		ends <- rbind(center - 0.5*length*axis, center + 0.5*length*axis)
	}

	if(class(ends) == 'data.frame') stop("'ends' must be a vector or matrix, not a data.frame.")

	if(any(is.na(ends))) stop("NA values in 'ends'")

	## Get ends of arrow base and head portions
	# If ends is single point, use axis to find other end point
	if(is.vector(ends) || nrow(ends) == 1){
	
		# Check that vector and length are specified
		if(is.null(axis) || is.null(length)) stop("If 'ends' is a single point then 'axis' and 'length' must both be non-NULL.")
		
		# Convert from array
		if(length(dim(axis)) == 3) axis <- axis[,,1]

		# Make sure vector is unit length
		axis <- uvector_svg(axis)

		# Find ends
		ends <- rbind(ends, ends + length*axis)

	}else{

		# Find axis, make unit
		axis <- ends[2,]-ends[1,]
		
		# Get length
		length <- sqrt(sum((axis)^2))

		# Make unit
		axis <- uvector_svg(axis)
	}
	
	# Get ends of each piece of arrow
	if(is.null(head.length)){
		cylinder_ends <- rbind(ends[1,], ends[1,] + (1-head.prop)*length*axis)
		cone_ends  <- rbind(cylinder_ends[2,], ends[2,])
	}else{
		cylinder_ends <- ends
		cone_ends  <- rbind(ends[2,], ends[2,] + head.length*axis)
	}
	
	# Create cylinder portion of arrow
	svg.cylinder(ends=cylinder_ends, radius=radius, rseg=rseg[1], hseg=hseg[1], 
		theta.start=theta.start, theta.length=theta.length, col=col, emissive=emissive, 
		opacity=opacity, ontop=ontop, name=name)
	
	# Create arrow head
	svg.cone(ends=cone_ends, radius=head.radius, rseg=rseg[2], hseg=hseg[2], 
		theta.start=theta.start, theta.length=theta.length, col=col, emissive=emissive, 
		opacity=opacity, ontop=ontop, name=name)
}