apply_transform_svg <- function(to, tmat, assoc = NULL){

	# Default, set 'to' as points - will eventually work with various shapes so that many 
	#	points do not have to be transformed to transform whole shape
	pts <- to

	# If single point convert to matrix
	if(is.vector(pts)) pts <- matrix(pts, nrow=1, ncol=length(pts))

	# If assoc is NULL and tmat is more than a single transformation matrix, set body associations based on point names
	if(is.null(assoc) && length(dim(tmat)) > 2 && sum(grepl('_|-', rownames(pts))) == nrow(pts)){
		
		# Get part of rowname preceding '_' for each point
		assoc <- unlist(lapply(strsplit(dimnames(pts)[[1]], '_|-'), 'head', 1))
	}

	# Transformation matrix
	if(length(dim(tmat)) == 2){
	
		if(length(dim(pts)) == 3){
			
			pcoor <- pts
			for(i in 1:dim(pcoor)[3]) pcoor[,, i] <- mtransform_svg(pcoor[,, i], tmat)
			return(pcoor)
		}
	
		# Get point coordinates as matrix for transformation - coerce to matrix if single point
		pcoor <- matrix(1, nrow(pts), 4, dimnames=list(rownames(pts), NULL))
		pcoor[, 1:3] <- pts

		# Apply transformation
		parr <- pcoor %*% t(tmat)

		# Remove 1s row and transpose
		return(parr[, 1:3])

	# Transformation 3-d array
	}else if(length(dim(tmat)) == 3){
	
		if(length(dim(pts)) == 2){

			if(is.null(dimnames(tmat)[[3]])){

				## Single body
				# Create array for transformed points
				parr <- array(NA, dim=c(nrow(pts), 3, dim(tmat)[3]), dimnames=list(rownames(pts), NULL, NULL))

				# Get point coordinates as matrix for transformation - coerce to matrix if single point
				pcoor <- matrix(1, 4, nrow(pts), dimnames=list(NULL, rownames(pts)))
				pcoor[1:3, ] <- t(pts)

				# Apply transformation
				tcoor <- apply(tmat, 3, '%*%', pcoor)

				# Convert to array
				tcoor_arr <- array(tcoor, dim=c(4, nrow(pts), dim(tmat)[3]))

				# Swap first two dimensions (transpose each "matrix" within array) and remove 1s
				if(dim(tcoor_arr)[2] == 1){
					return(t(tcoor_arr[1:3, 1, ]))
				}else{
					return(aperm(tcoor_arr[1:3, , ], perm=c(2,1,3)))
				}
			}else{

				# Multiple bodies - apply transformations for each body
				for(body in 1:dim(tmat)[3]){

					# Get body name
					body_name <- dimnames(tmat)[[3]][body]

					# Find points associated with body
					body_assoc <- which(assoc == body_name)
	
					# Skip if no points associated with body
					if(length(body_assoc) == 0) next

					pts[body_assoc, ] <- mtransform_svg(pts[body_assoc, ], tmat[, , body])
				}
			
				return(pts)

			}

		}else if(length(dim(pts)) == 3){
		
			# 
			n_iter <- dim(tmat)[3]
			for(iter in 1:n_iter) pts[, , iter] <- applyTransform_svg(pts[, , iter], tmat[, , iter])
		
			return(pts)

			#tmat <- matrix(tmat, nrow=4, ncol=4*dim(tmat)[3])
			#rmat1 <- rbind(matrix(pts, nrow=3, ncol=dim(pts)[1]*dim(pts)[3]), rep(1, dim(pts)[1]*dim(pts)[3]))
			#rmat2 <- matrix(rmat1, nrow=4*dim(pts)[3], ncol=dim(pts)[1])
		}

	# Transformation 4-d array
	}else{

		if(length(dim(pts)) == 2){

			# Create array for transformed points
			parr <- array(NA, dim=c(nrow(pts), 3, dim(tmat)[4]), dimnames=list(rownames(pts), NULL, NULL))

			# Multiple bodies - apply transformations for each body
			for(body in 1:dim(tmat)[3]){

				# Get body name
				body_name <- dimnames(tmat)[[3]][body]

				# Find points associated with body
				body_assoc <- which(assoc == body_name)
	
				# Skip if no points associated with body
				if(length(body_assoc) == 0) next

				# Get point coordinates as matrix for transformation - coerce to matrix if single point
				pcoor <- rbind(matrix(t(pts[body_assoc, ]), ncol=length(body_assoc)), rep(1, length(body_assoc)))
				colnames(pcoor) <- rownames(pts)[body_assoc]
	
				# Apply transformation
				tcoor <- apply(tmat[, , body, ], 3, '%*%', pcoor)

				# Convert to array
				tcoor_arr <- array(tcoor, dim=c(4, length(body_assoc), dim(tmat)[4]))

				# Swap first two dimensions (transpose each "matrix" within array) and remove 1s
				parr[colnames(pcoor), , ] <- aperm(tcoor_arr[1:3, , ], perm=c(2,1,3))
			}

			class(parr) <- 'xyz'
			return(parr)

		}else if(length(dim(pts)) == 3){

			parr <- pts

			for(body in 1:dim(tmat)[3]){

				# Get body name
				body_name <- dimnames(tmat)[[3]][body]

				# Find points associated with body
				body_assoc <- which(assoc == body_name)
	
				# Skip if no points associated with body
				if(length(body_assoc) == 0) next

				# Transform
				parr[body_assoc, , ] <- applyTransform_svg(parr[body_assoc, , ], tmat[, , body, ])
			}
			
			return(parr)
		}

	}

	parr
}