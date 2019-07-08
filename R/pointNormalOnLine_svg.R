pointNormalOnLine_svg <- function(pt, l1, l2 = NULL, checks = TRUE){

	# FINDS POSITION OF POINT X ON A LINE AT THE MINIMUM DISTANCE FROM POINT X TO THE INPUT POINT
	# Adapted from : http://paulbourke.net/geometry/pointline/

	if(checks){

		# IF L2 ARGUMENT IS NULL THEN TREAT L1 AS VECTOR AND GET SECOND POINT ON LINE THROUGH ORIGIN
		if(is.null(l2)) l2 <- c(0,0,0)
	
		# CHECK THAT POINTS DEFINING LINE ARE NOT COINCIDENT
		if(sum((l2 - l1)^2) == 0) return(NA)
	}

	# Convert to matrices
	if(is.vector(pt)) pt <- matrix(pt, 1, length(pt))

	# Convert to matrix
	nrow_pt <- nrow(pt)
	l1 <- matrix(l1, nrow_pt, 3, byrow=TRUE)
	l2 <- matrix(l2, nrow_pt, 3, byrow=TRUE)

	# Solve for position of point on line
	u <- rowSums((pt - l1)*(l2 - l1)) / rowSums((l2 - l1)^2)

	# Solve for points on line
	r <- l1 + u*(l2 - l1)
	
	r
}