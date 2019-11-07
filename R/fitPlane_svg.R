fitPlane_svg <- function(mat, scale = c(1,1)){
	
	# Center by centroid
	centroid.align <- mat - matrix(colMeans(mat), nrow(mat), ncol(mat), byrow=TRUE)

	# Check for differences in point position
	if(mean(apply(centroid.align, 2, 'sd')) < 1e-10) return(NULL)

	# SVD of centroid-aligned points
	meanX <- apply(mat, 2, mean) 
	pca <- prcomp(mat)

	v <- matrix(NA, nrow=3, ncol=3)
	endpts <- array(NA, dim=c(2,3,3))
	side_len <- rep(NA, 3)

	for(i in 1:3){

		t <- range(pca$x[, i])

		# Get end points along axis
		endpts[1,,i] <- meanX + t[1]*pca$rotation[, i]
		endpts[2,,i] <- meanX + t[2]*pca$rotation[, i]

		# Get vector corresponding to axis
		v[i, ] <- uvector_svg(endpts[2,,i]-endpts[1,,i])

		# Get half of length
		side_len[i] <- dppt_svg(endpts[1,,i], endpts[2,,i])
	}
	
	# Make scale a vector of length 2
	if(length(scale) == 1) scale <- rep(scale, 2)

	half_side_len <- (side_len[1:2] / 2) * scale
	corners_center <- colMeans(rbind(endpts[,,1], endpts[,,2], endpts[,,3]))
	corners <- matrix(NA, 4, 3, byrow=TRUE)
	corners[1,] <- corners_center + half_side_len[1]*v[1,] + half_side_len[2]*v[2,]
	corners[2,] <- corners_center - half_side_len[1]*v[1,] + half_side_len[2]*v[2,]
	corners[3,] <- corners_center - half_side_len[1]*v[1,] - half_side_len[2]*v[2,]
	corners[4,] <- corners_center + half_side_len[1]*v[1,] - half_side_len[2]*v[2,]

	shape_obj <- list('N'=v[3, ], 'Q'=corners_center, 'center'=corners_center, 
		'corners'=corners, 'vectors'=v, 'len'=side_len[1:2], 'halflen'=side_len[1:2]/2)
	class(shape_obj) <- 'plane'

	shape_obj
}