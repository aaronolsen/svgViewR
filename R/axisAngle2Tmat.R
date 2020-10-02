axisAngle2Tmat <- function(axis, angle, p=NULL){

	if(length(angle) == 1){

		# Create empty transformation matrix
		tmat <- diag(4)
		
		# Fill matrix
		tmat[1:3, 1:3] <- tMatrixEP_svg(v=axis, a=angle)

		if(!is.null(p)){
			tmat1 <- tmat2 <- diag(4)
			tmat1[1:3, 4] <- p
			tmat2[1:3, 4] <- -p
			tmat <- tmat1 %*% tmat %*% tmat2
		}

	}else{

		# Create empty transformation array
		tmat <- array(diag(4), dim=c(4,4,length(angle)))

		# Fill array
		for(i in 1:length(angle)){
			tmat[,,i] <- axisAngle2Tmat(axis=axis, angle=angle[i], p=p)
		}
	}

	tmat
}