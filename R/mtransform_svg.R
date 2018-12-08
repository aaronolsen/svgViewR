mtransform_svg <- function(mat, tmat){
	
	if(!is.matrix(mat)){
		mat <- matrix(mat, nrow=1, ncol=length(mat))
		dimnames_mat <- NULL
	}else{
		dimnames_mat <- dimnames(mat)
	}

	pmat <- matrix(1, nrow(mat), ncol(mat)+1)
	pmat[, 1:3] <- mat
	mat <- t(tmat %*% t(pmat))[, 1:3]
	dimnames(mat) <- dimnames_mat
	mat
}