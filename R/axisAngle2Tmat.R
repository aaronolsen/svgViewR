axisAngle2Tmat <- function(axis, angle){

	if(length(angle) == 1){

		# Create empty transformation matrix
		tmat <- diag(4)
		
		# Fill matrix
		tmat[1:3, 1:3] <- tMatrixEP_svg(v=axis, a=angle[i])

	}else{

		# Create empty transformation array
		tmat <- array(diag(4), dim=c(4,4,length(angle)))

		# Fill array
		for(i in 1:length(angle)){
			tmat[1:3, 1:3, i] <- tMatrixEP_svg(v=axis, a=angle[i])
		}
	}

	tmat
}