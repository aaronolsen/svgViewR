svg.cylinder <- function(ends=rbind(c(0,0,0), c(1,0,0)), radius=1, axis=NULL, length=NULL, 
	rseg=20, hseg=2, open.ended=FALSE, theta.start=0, theta.length=2*pi, col='blue', 
	emissive=rgb(0.03, 0.15, 0.21), name='cylinder'){

	# Make sure that type is webgl
	if('webgl' != getOption("svgviewr_glo_type")) stop("Cylinder drawing is currently only available with webgl svgViewR output.")

	## Create mesh
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
	
	# Add one to segments for easier vertex creation
	rseg <- rseg + 1

	# Get vector orthogonal to axis
	o_axis <- vorthogonal_svg(axis)

	# Set thetas for end circle
	thetas <- seq(from=theta.start, to=theta.start+theta.length, length=rseg)

	# Create matrix for end points
	ends_circle <- matrix(NA, nrow=rseg, ncol=3)

	# Get points on circumference of cylinder end circle
	for(i in 1:rseg) ends_circle[i, ] <- uvector_svg(o_axis %*% tMatrixEP_svg(axis, thetas[i]))

	# Draw vertices at lengths
	at_lengths <- seq(from=0, to=length, length=2+hseg-1)

	# Create vertices matrix
	vertices <- matrix(NA, rseg*(hseg+1) + 2, ncol=3)

	# Add vertices
	vertices[1, ] <- ends[1,]
	vertices[nrow(vertices), ] <- ends[2,]
	for(i in 1:length(at_lengths)){
		at_rows <- (i-1)*rseg + (2:(rseg+1))
		vertices[at_rows, ] <- radius*ends_circle + matrix(ends[1,]+at_lengths[i]*axis, nrow=rseg, ncol=3, byrow=TRUE)
	}
		
	# Create side faces matrix
	side_faces <- matrix(NA, nrow=2*(rseg-1)*hseg, 3)
	
	# Fill side faces matrix
	for(i in 0:(hseg-1)){

		rows1 <- i*(rseg-1)*2 + (1:(rseg-1))
		side_faces[rows1, 1] <- i*rseg + (1:(rseg-1))
		side_faces[rows1, 2] <- side_faces[rows1, 1] + 1
		side_faces[rows1, 3] <- i*rseg + (rseg+1):(2*rseg-1)

		rows2 <- rows1 + rseg - 1
		side_faces[rows2, 1] <- i*rseg + (1:(rseg-1)) + 1
		side_faces[rows2, 2] <- i*rseg + (rseg+1):(2*rseg-1)
		side_faces[rows2, 3] <- side_faces[rows2, 2] + 1
	}

	# Create ends of cylinder - triangles radiating around cylinder
	if(!open.ended){

		# Create faces matrix
		ends_faces <- matrix(NA, 2*rseg, ncol=3)
	
		# Add faces
		ends_faces[1:rseg, 1] <- 0
		ends_faces[1:rseg, 2] <- (1:rseg)
		ends_faces[1:(rseg-1), 3] <- ((1:(rseg-1))+1)
		ends_faces[rseg, 3] <- 1

		ends_faces[rseg + (1:rseg), 1] <- rseg*2 + rseg*(hseg-1) + 1
		ends_faces[rseg + (1:rseg), 2] <- rseg*hseg + (1:rseg)
		ends_faces[rseg + (1:(rseg-1)), 3] <- rseg*hseg + (1:(rseg-1)) + 1
		ends_faces[rseg + rseg, 3] <- rseg*hseg + 1

		# Combine faces into one matrix
		faces <- matrix(NA, nrow=2*rseg+2*(rseg-1)*hseg, 3)
		faces[1:(2*rseg), ] <- ends_faces
		faces[(2*rseg+1):nrow(faces), ] <- side_faces

	}else{
		
		# Save sides as face matrix
		faces <- side_faces
	}
	
	if('webgl' == getOption("svgviewr_glo_type")){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add to meshes
		add_at <- length(svgviewr_env$mesh)+1

		# Add vertices
		env$svgviewr_env$mesh[[add_at]] <- list()
		env$svgviewr_env$mesh[[add_at]]$vertices <- t(vertices)
		env$svgviewr_env$mesh[[add_at]]$faces <- t(faces)
		env$svgviewr_env$mesh[[add_at]]$col <- webColor(col)
		env$svgviewr_env$mesh[[add_at]]$emissive <- webColor(emissive)

		# Add object reference data
		env$svgviewr_env$ref$names <- c(env$svgviewr_env$ref$names, name)
		env$svgviewr_env$ref$num <- c(env$svgviewr_env$ref$num, add_at)
		env$svgviewr_env$ref$type <- c(env$svgviewr_env$ref$type, 'mesh')

		# Add limits
		obj_ranges <- apply(vertices, 2, 'range', na.rm=TRUE)
		
		# Set corners
		corners <- lim2corners(obj_ranges)
		
		# Add limits to object
		env$svgviewr_env$mesh[[add_at]][['lim']] <- obj_ranges
		env$svgviewr_env$mesh[[add_at]][['corners']] <- corners

	}else{

		svg.points(vertices)
		
		# Draw faces
		faces <- cbind(faces, faces[,1])
		svg.pathsC(lapply(seq_len(nrow(faces)), function(i) faces[i,]+1), col='black', opacity.fill=0.2)
	}

	NULL
}

#var geometry = new THREE.CylinderGeometry( 5, 5, 20, 32 );
#var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
#var cylinder = new THREE.Mesh( geometry, material );
#scene.add( cylinder );

#CylinderGeometry(radiusTop, radiusBottom, height, radiusSegments, heightSegments, openEnded, thetaStart, thetaLength)