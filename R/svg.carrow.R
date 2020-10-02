svg.carrow <- function(center, axis, point.radius=NULL, radius=NULL, point.radius.shift=0, 
	length.prop=0.75, width.prop=0.05, width=NULL, head.width.prop=1.5*width.prop, head.width=NULL, 
	head.length.prop=0.2, head.length=NULL, depth.prop=0.05, depth=NULL, type=c('single', 'double')[1], 
	seg=c(25,10), col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, ontop = FALSE, 
	name='carrow'){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")
	
	if(is.null(point.radius) && is.null(radius)) stop("Both 'point.radius' and 'radius' are NULL. One must be non-NULL.")
	
	# Check that point.radius is point
	if(!is.null(point.radius) && length(point.radius) != 3) stop("'point.radius' must be vector of length 3.")

	# Define circle
	circle <- defineCircle_svg(center=center, nvector=axis, point_on_radius=point.radius, radius=radius)
	
	# Set center from defined circle, in case it had to be redefined
	center <- circle$C
	
	# This parameter allows the user to rotate the arrow about the input axis (in radians) to achieve the position the user wants
	if(point.radius.shift != 0){
		
		# Shift point radius
		point.radius <- circlePoint_svg(circle, T=point.radius.shift)
		
		# Redefine circle
		circle <- defineCircle_svg(center=center, nvector=axis, point_on_radius=point.radius, radius=radius)
	}
	
	# Set axes based on 
	arrow_axes <- rbind(cprod_svg(axis, circle$U), axis, circle$U)

	# Set point on circle (arrow center)
	if(is.null(point.radius)) point.radius <- center + radius*circle$U

	# Set radius
	if(is.null(radius)) radius <- circle$R

	# Set circle circumference
	circle_circum_half <- pi*circle$R

	# Set arrow length
	arrow_length <- length.prop*2*circle_circum_half

	# Set lengths from relative if absolutes are null
	if(is.null(width)) width <- width.prop*arrow_length
	if(is.null(head.width)) head.width <- head.width.prop*arrow_length
	if(is.null(head.length)) head.length <- head.length.prop*arrow_length
	if(is.null(depth)) depth <- depth.prop*arrow_length
	
	# Set arrow center
	arrow_center <- point.radius + (depth/2)*circle$U
	arrow_ends <- rbind(arrow_center - arrow_length*arrow_axes[1,], arrow_center + arrow_length*arrow_axes[1,])

	# Get arrow vertices and faces
	arrow_mesh <- svg.farrow(center=arrow_center, length=arrow_length, head.width=head.width, 
		head.length=head.length, width=width, depth=depth, axes=arrow_axes, 
		type=type, seg=seg, plot=FALSE)

	# Get vertices and faces
	vertices <- arrow_mesh$vertices
	faces <- arrow_mesh$faces
	
	#svg.spheres(circle$C, radius=0.1)
	#svg.arrow(rbind(circle$C, circle$C+3*circle$N), radius=0.1)

	# Transform vertices into circle
	for(i in 1:nrow(vertices)){
		
		# Find relative position of vertex along length of arrow
		dist_from_center <- distancePointToLine_svg(vertices[i,], arrow_center, arrow_center+arrow_axes[2,])
		
		# Set angle on circle based on distance from center
		angle_on_circle <- (dist_from_center / circle_circum_half) * pi

		# Find whether vertex is toward tip or base of arrow and set sign accordingly
		if(dppt_svg(vertices[i,], arrow_ends[1,]) < dppt_svg(vertices[i,], arrow_ends[2,])){
			angle_on_circle <- -angle_on_circle
		}
		
		# Find point on circle corresponding to angle
		circle_point <- circlePoint_svg(circle, angle_on_circle)

		# Find distance of vertex (which side of the arrow the point is on)
		dpp <- abs(distPointToPlane_svg(p=vertices[i,], n=arrow_axes[3,], point.radius))
		
		# Adjust circle point based on where vertex is in terms of arrow depth
		circle_point <- circle_point + dpp*uvector_svg(circle_point-circle$C)
		
		# Find projection of vertex onto circle point line
		vertices[i,] <- pointLineProj_svg(vertices[i,], circle_point, circle_point+circle$N)
	}

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Add to meshes
	add_at <- length(svgviewr_env$svg$mesh)+1

	# Add vertices
	svgviewr_env$svg$mesh[[add_at]] <- list()
	svgviewr_env$svg$mesh[[add_at]]$vertices <- t(vertices)
	svgviewr_env$svg$mesh[[add_at]]$faces <- t(faces)
	svgviewr_env$svg$mesh[[add_at]]$col <- setNames(webColor(col), NULL)
	svgviewr_env$svg$mesh[[add_at]]$name <- name
	svgviewr_env$svg$mesh[[add_at]]$opacity <- setNames(opacity, NULL)
	svgviewr_env$svg$mesh[[add_at]]$emissive <- setNames(webColor(emissive), NULL)
	svgviewr_env$svg$mesh[[add_at]]$computeVN <- TRUE
	svgviewr_env$svg$mesh[[add_at]]$parseModel <- FALSE
	svgviewr_env$svg$mesh[[add_at]]$depthTest <- !ontop

	# Add object reference data
	svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
	svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
	svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'mesh')

	# Add limits
	obj_ranges <- apply(vertices, 2, 'range', na.rm=TRUE)
	
	# Set corners
	corners <- lim2corners(obj_ranges)
	
	# Add limits to object
	svgviewr_env$svg$mesh[[add_at]][['lim']] <- obj_ranges
	svgviewr_env$svg$mesh[[add_at]][['corners']] <- corners
}