image2world <- function(xy, camera, d){

	# If camera is not list (ie camera object) assume camera file and read
	if(!is.list(camera)) camera <- readCam(camera)

	# Transform image coordinates into world coordinates
	cam_xy <- rbind(t(xy), rep(1,nrow(xy)))
	cam_xy <- cam_xy*d
	cam_xyn <- solve(camera$intrinsic) %*% cam_xy
	cam_xyz <- t(t(camera$extrinsic[1:3,1:3]) %*% (cam_xyn - camera$extrinsic[,4]))
	
	cam_xyz
}