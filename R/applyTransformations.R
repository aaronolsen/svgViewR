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
	
	# Expand xyz values to new number of iterations
	for(lnum in 1:length(shapes)){

		if(is.null(shapes[[lnum]][['xyz']])) next
		if(shapes[[lnum]][['type']] == 'path') next
		xyz <- shapes[[lnum]][['xyz']]

		if(length(dim(xyz)) == 2){
			if(nrow(xyz) == 1){
				shapes[[lnum]][['xyz']] <- matrix(xyz, nrow=n_iter, ncol=ncol(xyz), byrow=TRUE)
			}else{
				for(i in 1:(round(n_iter/nrow(xyz))-1)) xyz <- rbind(xyz, xyz)
				shapes[[lnum]][['xyz']] <- xyz
			}
		}else if(length(dim(xyz)) == 3){
			if(dim(xyz)[3] == 1){
				shapes[[lnum]][['xyz']] <- array(xyz, dim=c(dim(xyz)[1:2], n_iter))
			}else{
				shapes[[lnum]][['xyz']] <- array(xyz, dim=c(dim(xyz)[1:2], n_iter))
			}
		}
	}

	# For each iteration
	for(iter in 1:n_iter){

		for(lnum in 1:length(shapes)){

			if(is.null(shapes[[lnum]][['xyz']])) next
			if(shapes[[lnum]][['type']] == 'path') next

			xyz <- shapes[[lnum]][['xyz']]

			# For each transformation
			if(length(tform[['rotate']]) > 0 && iter <= rotate_n_iter){

				if(length(dim(xyz)) == 2){
					r <- matrix(shapes[[lnum]][['xyz']][iter, ], ncol=3, byrow=TRUE) %*% tform[['rotate']][, , iter]
					shapes[[lnum]][['xyz']][iter, ] <- c(t(r))
				}else if(length(dim(xyz)) == 3){
					shapes[[lnum]][['xyz']][, , iter] <- shapes[[lnum]][['xyz']][, , iter] %*% tform[['rotate']][, , iter]
				}
			}
			if(length(tform[['translate']]) > 0 && iter <= translate_n_iter){
				
				if(length(dim(xyz)) == 2){
					shapes[[lnum]][['xyz']][iter, ] <- shapes[[lnum]][['xyz']][iter, ] + matrix(tform[['translate']][iter, ], nrow=1, ncol=ncol(xyz))
				}else if(length(dim(xyz)) == 3){
					shapes[[lnum]][['xyz']][, , iter] <- shapes[[lnum]][['xyz']][, , iter] + matrix(tform[['translate']][iter, ], nrow=nrow(xyz), ncol=ncol(xyz), byrow=TRUE)
				}
			}
		}
	}
	
	rlist <- list(
		'params'=params,
		'shapes'=shapes,
		'tform'=tform,
		'n.iter'=n_iter,
		'z.index'=read_html$z.index
	)
	
	return(rlist)	
}