applyTransformations <- function(read_html){

	params <- read_html$params
	shapes <- read_html$shapes
	tform <- read_html$tform

	# Get total number of iterations in shapes
	n_iter <- c()
	for(lnum in 1:length(shapes)){
		if(is.null(shapes[[lnum]][['xyz']])) next
		if(shapes[[lnum]][['type']] == 'path') next
		xyz <- shapes[[lnum]][['xyz']]
		if(length(dim(xyz)) == 2){
			n_iter <- c(n_iter, nrow(xyz))
		}else if(length(dim(xyz)) == 3){
			n_iter <- c(n_iter, dim(xyz)[3])
		}
	}
	shapes_n_iter <- max(n_iter)

	if(is.null(read_html$tform[['rotate']]) && is.null(read_html$tform[['translate']])){
		read_html[['n.iter']] <- shapes_n_iter
		return(read_html)
	}

	# Get total number of iterations in transformations
	if(!is.null(tform[['rotate']])) rotate_n_iter <- dim(tform[['rotate']])[3]
	if(!is.null(tform[['translate']])) translate_n_iter <- dim(tform[['translate']])[1]

	# Set total number of iterations as maximum of both
	n_iter <- max(c(shapes_n_iter, rotate_n_iter, translate_n_iter))
	
	if(length(tform[['translate']]) > 0){translate <- TRUE}else{translate <- FALSE}
	if(length(tform[['rotate']]) > 0){rotate <- TRUE}else{rotate <- FALSE}

	# Add 0 rows to match number of iterations
	if(translate) if(nrow(tform[['translate']]) < n_iter) tform[['translate']] <- rbind(tform[['translate']], matrix(0, nrow=n_iter-nrow(tform[['translate']]), ncol=3))

	#t1 <- proc.time()

	for(lnum in 1:length(shapes)){
		
		if(is.null(shapes[[lnum]][['xyz']])) next
		if(shapes[[lnum]][['type']] == 'path') next

		xyz <- shapes[[lnum]][['xyz']]
		xyz_dim <- dim(shapes[[lnum]][['xyz']])

		# Expand xyz values to new number of iterations
		if(length(xyz_dim) == 2){
			if(xyz_dim[1] == 1){
				shapes[[lnum]][['xyz']] <- matrix(xyz, nrow=n_iter, ncol=xyz_dim[2], byrow=TRUE)
			}else{
				for(i in 1:(round(n_iter/xyz_dim[1])-1)) xyz <- rbind(xyz, xyz)
				shapes[[lnum]][['xyz']] <- xyz
			}
		}else if(length(xyz_dim) == 3){
			if(xyz_dim[3] == 1){
				shapes[[lnum]][['xyz']] <- array(xyz, dim=c(xyz_dim[1:2], n_iter))
			}else{
				shapes[[lnum]][['xyz']] <- array(xyz, dim=c(xyz_dim[1:2], n_iter))
			}
		}

		# Get new dimensions
		xyz_dim <- dim(shapes[[lnum]][['xyz']])

		# Apply rotations -- this part is slow for a lot of iterations, can't figure out how to do it without for loops
		# will probably need to use compiled code
		if(rotate){
			if(length(xyz_dim) == 2){
				for(iter in 1:rotate_n_iter){
					r <- matrix(shapes[[lnum]][['xyz']][iter, ], ncol=3, byrow=TRUE) %*% tform[['rotate']][, , iter]
					shapes[[lnum]][['xyz']][iter, ] <- t(r)
				}
			}else if(length(xyz_dim) == 3){
				for(iter in 1:rotate_n_iter){
					shapes[[lnum]][['xyz']][, , iter] <- shapes[[lnum]][['xyz']][, , iter] %*% tform[['rotate']][, , iter]
				}
			}
		}

		# Apply translations
		if(translate){
			if(length(xyz_dim) == 2){
				shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']] + matrix(tform[['translate']], nrow=xyz_dim[1], ncol=xyz_dim[2], byrow=TRUE)
			}else if(length(xyz_dim) == 3){
				tarr <- array(apply(tform[['translate']], 1, 'matrix', nrow=xyz_dim[1], ncol=xyz_dim[2], byrow=TRUE), dim=xyz_dim)
				shapes[[lnum]][['xyz']] <- shapes[[lnum]][['xyz']] + tarr
			}
		}
	}
	
	#t2 <- proc.time()
	#print(t2-t1)

	rlist <- list(
		'params'=params,
		'shapes'=shapes,
		'tform'=tform,
		'n.iter'=n_iter,
		'z.index'=read_html$z.index
	)
	
	return(rlist)	
}