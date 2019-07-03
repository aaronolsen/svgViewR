svg.hcuboid <- function(ends=NULL, center=NULL, axes=NULL, length=NULL, width=1,
	hcenters=matrix(center,3,3,byrow=TRUE), hdims=NULL,
	col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, ontop = FALSE, name='hcuboid'){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")

	# If ends is single point, use axis to find other end point
	if(is.null(center)){

		if(is.vector(ends) || nrow(ends) == 1){

			if(is.null(axes) || is.null(length) && !is.null(width)){
				if(length(width) == 3){
					length <- width[1]
					width <- width[2:3]
				}
			}
	
			# Check that vector and length are specified
			if(is.null(axes) || is.null(length)) stop("If 'ends' is a single point then 'axes' and 'length' must both be non-NULL.")

			# Convert to matrix
			if(length(dim(axes)) == 3) axes <- axes[,,1]
			if(is.vector(axes)) axes <- matrix(axes, 1, 3)

			if(nrow(axes) == 1) stop("If 'ends' is a single point then 'axes' must be a 2-row matrix.")

			# Make sure vector is unit length
			axes <- uvector_svg(axes)

			# Find ends
			ends <- rbind(ends, ends + length*axes[1,])

		}else{

			if(is.vector(axes)) axes <- matrix(axes, 1, 3)

			if(nrow(axes) == 3) stop("If 'ends' is a 2-row matrix then 'axes' must be a 1 or 2-row matrix.")

			# Find axis, make unit
			axes <- rbind(ends[2,]-ends[1,], axes)
			
			# Issue warning if length is not used
			if(!is.null(length)) warning('Input parameter "length" is ignored with input of two end points ("ends").')
		
			# Get length
			length <- sqrt(sum((axes[1, ])^2))

			# Make unit
			axes <- uvector_svg(axes)
		}
		
		# Set center
		center <- colMeans(ends)

		# Set widths if only one given
		if(length(width) == 1) width <- rep(width, 2)
	
		# Get vector orthogonal to axes
		if(nrow(axes) == 2) axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))

		# Set external dims
		edims <- c(length, width)

	}else{
		
		# Set widths if only one given
		if(length(width) == 1) width <- rep(width, 3)
	
		# Make sure unit
		axes <- uvector_svg(axes)
		
		# Set ends
		ends <- matrix(NA, 2, 3)
		ends[1,] <- center + (width[1]/2)*axes[1,]
		ends[2,] <- center - (width[1]/2)*axes[1,]
		
		# Make sure there are 3 axes
		if(nrow(axes) == 2) axes <- rbind(axes, cprod_svg(axes[1,], axes[2,]))
		
		# Set external dims
		edims <- width
	}
	
	# Flip axis to match ends order
	if(abs(avec_svg(axes[1,], ends[1,]-ends[2,])) > 1) axes[1,] <- -axes[1,]
	
	# Make sure hcenters is matrix
	if(!is.matrix(hcenters)) hcenters <- matrix(hcenters,1,3)
	
	if(!is.null(hdims)){
		# Make sure hdims is matrix
		if(!is.matrix(hdims)) hdims <- matrix(hdims,1,3)

		# Make sure hdims and hcenters have the same number of dimensions
		if(nrow(hcenters) != nrow(hdims)) stop("hcenters and hdims must have the same number of rows.")
	}
	
	# Set default hdims if NULL (half of faces orthogonal to hollow axis)
	if(is.null(hdims)){
		hdims <- matrix(diag(3)[1:nrow(hcenters),]*edims, nrow(hcenters), 3)
		for(i in 1:nrow(hcenters)) hdims[i,hdims[i,] == 0] <- edims[hdims[i,] == 0] / 2
	}
	
	# Standardize hcenters and hdims to have 3 rows (3 hollow cuboids)
	if(nrow(hcenters) == 1){
		hcenters <- rbind(hcenters, center, center)
		hdims <- rbind(hdims, c(0,edims[2],0), c(0,0,edims[3]))
	}
	if(nrow(hcenters) == 2){
		hcenters <- rbind(hcenters, center)
		hdims <- rbind(hdims, c(0,0,edims[3]))
	}

	# Set half dimensions
	edims_h <- edims / 2
	hdims_h <- hdims / 2
	
	# Set axis combos
	axis_combos <- list(c(1,1), c(1,-1), c(-1,1), c(-1,-1))

	# Create outer cuboid corners matrix
	vertices <- matrix(NA, 8, 3)
	for(side in 1:2){
		v_i <- (side-1)*4 + 1
		for(i in 1:4) vertices[v_i+i-1,] <- ends[side,] + colSums(axis_combos[[i]]*edims_h[2:3]*axes[2:3,])
	}
	
	# Set scaling from vertices
	centroid_size <- mean(sqrt(rowSums((vertices - matrix(colMeans(vertices), nrow=nrow(vertices), ncol(vertices), byrow=TRUE))^2)))
	zero_val <- centroid_size*0.01

	# Project corners from each hcuboid
	hcuboid_vertices <- list()
	hcuboid_ends <- list()
	for(ic_n in 1:3){

		# Set hcuboid ends
		hcuboid_ends[[ic_n]] <- matrix(NA, 2, 3)
		hcuboid_ends[[ic_n]][1,] <- hcenters[ic_n, ] + hdims_h[ic_n, 1]*axes[1,]
		hcuboid_ends[[ic_n]][2,] <- hcenters[ic_n, ] - hdims_h[ic_n, 1]*axes[1,]

		hcuboid_vertices[[ic_n]] <- matrix(NA, 8, 3)
		for(side in 1:2){
			v_i <- (side-1)*4 + 1
			for(i in 1:4) hcuboid_vertices[[ic_n]][v_i+i-1,] <- hcuboid_ends[[ic_n]][side,] + colSums(axis_combos[[i]]*hdims_h[ic_n, 2:3]*axes[2:3,])
		}
	}
	
	# Set centers of each side
	side_centers <- matrix(NA, 6, 3)
	side_centers[c(1,3,5),] <- matrix(center,3,3,byrow=TRUE) + edims_h*axes
	side_centers[c(2,4,6),] <- matrix(center,3,3,byrow=TRUE) - edims_h*axes

	# Set inner cuboid dimensions
	# For each axis, find the maximum range not at the ends
	icuboid_ppoints <- matrix(NA,6,3)
	icuboid_dims <- rep(NA, 3)
	icuboid_cvec <- matrix(NA,3,3)
	for(i in 1:3){
		
		proj_pts <- matrix(NA,24,3)
		proj_dist <- rep(NA, 24)
		
		n <- 1
		for(ic_n in 1:3){
			for(j in 1:nrow(hcuboid_vertices[[ic_n]])){
				proj_pts[n, ] <- pointLineProj_svg(hcuboid_vertices[[ic_n]][j, ], center, center+axes[i, ])
				proj_dist[n] <- dppt_svg(center, proj_pts[n, ])
				n <- n + 1
			}
		}
		
		# Exclude points at ends
		proj_pts <- proj_pts[proj_dist < edims_h[i]-zero_val, ]
		proj_dist <- proj_dist[proj_dist < edims_h[i]-zero_val]
		
		# Find the point closest to each side along axis
		for(j in (i*2-1):(i*2)){
			icuboid_ppoints[j, ] <- proj_pts[which.min(dppt_svg(side_centers[j, ], proj_pts))[1], ]
		}

		# Set dimension along axis
		icuboid_dims[i] <- sqrt(sum((icuboid_ppoints[i*2-1, ]-icuboid_ppoints[i*2, ])^2))
		
		# Get center of axis
		icuboid_cvec[i, ] <- icuboid_ppoints[i*2-1,] + (icuboid_dims[i]/2)*uvector_svg(icuboid_ppoints[i*2,] - icuboid_ppoints[i*2-1,])
	}
	
	# Project first axis icuboid points into center plane
	axis1_ic_proj <- pointPlaneProj_svg(icuboid_ppoints[1,], center, axes[1,])

	# Create vectors to set icuboid vertices
	icuboid_side_vecs <- icuboid_ppoints[3:6,] - matrix(axis1_ic_proj,4,3,byrow=TRUE)

	# Set icuboid vertices
	icuboid_vertices <- rbind(
		icuboid_ppoints[1,] + colSums(icuboid_side_vecs[c(1,3),]),
		icuboid_ppoints[1,] + colSums(icuboid_side_vecs[c(1,4),]),
		icuboid_ppoints[1,] + colSums(icuboid_side_vecs[c(2,3),]),
		icuboid_ppoints[1,] + colSums(icuboid_side_vecs[c(2,4),]),
		icuboid_ppoints[2,] + colSums(icuboid_side_vecs[c(1,3),]),
		icuboid_ppoints[2,] + colSums(icuboid_side_vecs[c(1,4),]),
		icuboid_ppoints[2,] + colSums(icuboid_side_vecs[c(2,3),]),
		icuboid_ppoints[2,] + colSums(icuboid_side_vecs[c(2,4),])
	)
	
	# Project external hcuboid sides into icuboid
	hcuboid_ivertices <- list()
	hcuboid_evertices <- list()
	for(side in 1:6){
		
		# Set vertex indices
		if(side == 1) v_ind <- 1:4
		if(side == 2) v_ind <- 5:8
		if(side == 3) v_ind <- c(5,6,1,2)
		if(side == 4) v_ind <- c(3,4,7,8)
		if(side == 5) v_ind <- c(5,1,7,3)
		if(side == 6) v_ind <- c(2,6,4,8)
		
		# Save external vertices
		hcuboid_evertices[[side]] <- hcuboid_vertices[[ceiling(side/2)]][v_ind, ]
		
		# Project into icuboid
		hcuboid_ivertices[[side]] <- pointPlaneProj_svg(hcuboid_evertices[[side]], icuboid_ppoints[side,], axes[ceiling(side/2), ])
	}

	# Convert lists to matrices
	hcuboid_evertices_m <- matrix(NA, nrow(hcuboid_evertices[[1]])*length(hcuboid_evertices), 3)
	for(i in 1:length(hcuboid_evertices)){
		idx <- (i-1)*nrow(hcuboid_evertices[[1]])+1
		hcuboid_evertices_m[idx:(idx+3),] <- hcuboid_evertices[[i]]
	}

	hcuboid_ivertices_m <- matrix(NA, nrow(hcuboid_ivertices[[1]])*length(hcuboid_ivertices), 3)
	for(i in 1:length(hcuboid_ivertices)){
		idx <- (i-1)*nrow(hcuboid_ivertices[[1]])+1
		hcuboid_ivertices_m[idx:(idx+3),] <- hcuboid_ivertices[[i]]
	}
	
	# Create matrix of all vertices
	all_vertices <- rbind(vertices, hcuboid_evertices_m, hcuboid_ivertices_m, icuboid_vertices)	
	v_cols <- c(rep('orange', nrow(vertices)), rep('red', 8), rep('green', 8), rep('blue', 8), rep('red', 8), rep('green', 8), rep('blue', 8), rep('black', 8))

#	print(hcuboid_evertices)
#	print(matrix(unlist(hcuboid_evertices), nrow=nrow(hcuboid_evertices[[1]])*length(hcuboid_evertices), 3, byrow=TRUE))
	
	bib_idx_list <- list(
		list(c(9,10,12,11), c(1,2,4,3)),
		list(c(14,13,15,16), c(6,5,7,8)),
		list(c(17,18,20,19), c(5,6,2,1)),
		list(c(21,22,24,23), c(3,4,8,7)),
		list(c(25,26,28,27), c(5,1,3,7)),
		list(c(29,30,32,31), c(2,6,8,4)),

		list(c(33,34,36,35), c(57,58,60,59)),
		list(c(38,37,39,40), c(62,61,63,64)),
		list(c(41,42,44,43), c(61,62,58,57)),
		list(c(45,46,48,47), c(59,60,64,63)),
		list(c(49,50,52,51), c(61,57,59,63)),
		list(c(53,54,56,55), c(58,62,64,60))
	)
	
	# Create vertices and faces
	all_faces <- matrix(NA, 0, 3)
	in_plane <- rep(TRUE, 6)
	for(i in 1:length(bib_idx_list)){
#	for(i in 1){

		ivert <- all_vertices[bib_idx_list[[i]][[1]], ]
		overt <- all_vertices[bib_idx_list[[i]][[2]], ]

		# Get outer corner plane normal vector
		normvec <- cprod_svg(overt[3,]-overt[1,], overt[2,]-overt[1,])
	
		bibf <- box_in_box_faces(all_vertices, all_faces, icorners=bib_idx_list[[i]][[1]], ocorners=bib_idx_list[[i]][[2]])
		v_cols <- c(v_cols, rep('purple', nrow(bibf$vertices)-nrow(all_vertices)))
		all_vertices <- bibf$vertices

		# Not in plane - set internal faces
		if(!in_plane[(i-1) %% 6 + 1] || dppt_svg(ivert[1,], pointPlaneProj_svg(ivert[1,], overt[1,], normvec)) > zero_val){

			# Use only outer corners to make face
			all_faces <- rbind(all_faces, bib_idx_list[[i]][[2]][c(1,2,4)], bib_idx_list[[i]][[2]][c(4,2,3)])

			# Set to not in plane
			in_plane[(i-1) %% 6 + 1] <- FALSE

			next
		}

		# In plane
		all_faces <- bibf$faces
	}
	
	# Add faces connecting hcuboid_evertices and hcuboid_ivertices
	add_faces <- c(9,10,33, 10,33,34, 10,12,34, 12,34,36, 12,11,36, 11,36,35, 9,11,33, 11,33,35)
	for(i in 0:5){
		if(in_plane[i+1]) all_faces <- rbind(all_faces, matrix(add_faces + i*4, nrow=length(add_faces)/3, 3, byrow=TRUE))
	}
	
	# Remove vertices not include in faces - doesn't seem necessary for proper run
	#all_v_idx <- (1:nrow(all_vertices))
	#print(all_v_idx[!all_v_idx %in% unique(c(all_faces))])

	if('svg' == getOption("svgviewr_glo_type")){

		#svg.points(vertices)
		svg.points(all_vertices, col=v_cols)
		#svg.text(all_vertices[v_cols %in% c('red', 'green', 'blue'),], labels=c(1:nrow(all_vertices))[v_cols %in% c('red', 'green', 'blue')], font.size=0.5, col=v_cols[v_cols %in% c('red', 'green', 'blue')])
		#svg.text(all_vertices, labels=c(1:nrow(all_vertices)), font.size=0.5, col=v_cols)

		#svg.points(vertices, col='orange')
		#svg.lines(vertices[c(1,2,4,3,1,5,6,8,7,5,6,2,4,8,7,3),], col='orange')
		#svg.text(vertices, labels=1:nrow(vertices), font.size=1)

		#svg.points(axis1_ic_proj, col='black', cex=5)
		#svg.points(icuboid_vertices, col='black')
		#svg.lines(icuboid_vertices[c(1,2,4,3,1,5,6,8,7,5,6,2,4,8,7,3),], col='black')

		#svg.points(side_centers, col='brown', cex=3)
		#svg.points(icuboid_ppoints, col='black', cex=2)
		#svg.text(icuboid_ppoints, labels=1:nrow(icuboid_ppoints), font.size=1)
		#svg.points(icuboid_cvec, col='black', cex=4)

		cols <- c('red', 'green', 'blue')
		cols2 <- c('red', 'red', 'green', 'green', 'blue', 'blue')
		#svg.points(hcenters, col=cols, cex=4)

		#for(i in 1:length(hcuboid_evertices)) svg.points(hcuboid_evertices[[i]], col=cols2[i])
		#for(i in 1:length(hcuboid_ivertices)) svg.points(hcuboid_ivertices[[i]], col=cols2[i])

		for(i in 1:length(hcuboid_evertices)){
			all_points <- rbind(hcuboid_evertices[[i]], hcuboid_ivertices[[i]])
			svg.lines(all_points[c(1,2,4,3,1,5,6,8,7,5,6,2,4,8,7,3),], col=cols2[i])
		}

		for(i in 1:length(hcuboid_vertices)){
			svg.points(hcuboid_vertices[[i]], col=cols[i])
			#svg.text(hcuboid_vertices[[i]], labels=1:nrow(hcuboid_vertices[[i]]), font.size=1, col=cols[i])
			svg.lines(hcuboid_vertices[[i]][c(1,2,4,3,1,5,6,8,7,5,6,2,4,8,7,3),], col=cols[i])
		}

		#svg.points(proj_pts, col='blue', cex=1)

		#svg.text(vertices, labels=0:(nrow(vertices)-1), font.size=1)
		
		# Draw faces
		all_faces <- cbind(all_faces, all_faces[,1])
		#svg.pathsC(lapply(seq_len(nrow(all_faces)), function(i) all_faces[i,]))
		#print(all_faces)
		svg.pathsC(lapply(seq_len(nrow(all_faces)), function(i) all_faces[i,]), col='black', opacity.fill=0.2)

	}else{

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Add to meshes
		add_at <- length(svgviewr_env$svg$mesh)+1

		# Add vertices
		svgviewr_env$svg$mesh[[add_at]] <- list()
		svgviewr_env$svg$mesh[[add_at]]$vertices <- t(all_vertices)
		svgviewr_env$svg$mesh[[add_at]]$faces <- t(all_faces-1)
		svgviewr_env$svg$mesh[[add_at]]$col <- setNames(webColor(col), NULL)
		svgviewr_env$svg$mesh[[add_at]]$emissive <- setNames(webColor(emissive), NULL)
		svgviewr_env$svg$mesh[[add_at]]$opacity <- setNames(webColor(opacity), NULL)
		svgviewr_env$svg$mesh[[add_at]]$computeVN <- FALSE
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

	ret = NULL
}