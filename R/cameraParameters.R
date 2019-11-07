cameraParameters <- function(intrinsic, extrinsic, image.size, plane.dist = NULL){

	# Set plane dist (any value > 0 to find prin vec and axis)
	if(is.null(plane.dist)) plane.dist <- 1

	# Find pinhole in world space
	pinhole_xyz <- t(t(extrinsic[1:3,1:3]) %*% (solve(intrinsic) %*% c(0,0,0) - extrinsic[,4]))

	# Find image plane corners
	if(is.null(image.size)) image.size <- c(1,1)
	
	# Find image plane corners in xy
	iplane_xy <- t(cbind(rbind(c(0,image.size[2]), image.size, c(image.size[1],0), c(0,0)) - matrix(c(0.5,0.5), nrow=4, ncol=2), rep(1,4)))*plane.dist

	# Find image plane corners in xyz
	iplane_xyz <- t(t(extrinsic[1:3,1:3]) %*% (solve(intrinsic) %*% iplane_xy - extrinsic[,4]))

	# Principal unit vector
	prin_uvec <- uvector_svg(cprod_svg(iplane_xyz[1,]-iplane_xyz[2,], iplane_xyz[3,]-iplane_xyz[2,]))

	# Find camera principal vector
	prin_vec <- plane.dist*prin_uvec

	# Find camera principal axis
	prin_axis <- rbind(pinhole_xyz, pinhole_xyz+prin_vec)

	# 

	list(
		'intrinsic'=intrinsic,
		'extrinsic'=extrinsic,
		'image.size'=image.size,
		'pinhole'=pinhole_xyz,
		'plane.corners'=iplane_xyz,
		'plane.dist'=plane.dist,
		'prin.uvec'=prin_uvec,
		'prin.vec'=prin_vec,
		'prin.axis'=prin_axis
	)
}