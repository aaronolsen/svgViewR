set_plane_corners <- function(center, dims, vecs){

	corners <- matrix(NA, 4, 3, byrow=TRUE)

	# Add vertices
	corners[1,] <- center + colSums(c(1,1,0)*0.5*dims*vecs)
	corners[2,] <- center + colSums(c(1,-1,0)*0.5*dims*vecs)
	corners[3,] <- center + colSums(c(-1,-1,0)*0.5*dims*vecs)
	corners[4,] <- center + colSums(c(-1,1,0)*0.5*dims*vecs)
	
	corners
}