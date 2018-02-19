svg.camera <- function(intrinsic, extrinsic, focal = 60, set = TRUE, name = NULL, cone = FALSE, cone.dist = NULL, cone.dim = NULL){

	# Find pinhole in world space
	pinhole_xyz <- t(t(extrinsic[1:3,1:3]) %*% (solve(intrinsic) %*% c(0,0,0) - extrinsic[,4]))

	# Find image plane corners
	if(is.null(cone.dim)) cone.dim <- c(1,1)

	# Find image plane corners in xy
	iplane_xy <- t(cbind(rbind(c(0,0), c(cone.dim[1],0), cone.dim, c(0,cone.dim[2])) - matrix(c(0.5,0.5), nrow=4, ncol=2), rep(1,4)))*cone.dist

	# Find image plane corners in xyz
	iplane_xyz <- t(t(extrinsic[1:3,1:3]) %*% (solve(intrinsic) %*% iplane_xy - extrinsic[,4]))

	# Find camera principal vector
	prin_vec <- cone.dist*uvector_svg(cprod_svg(iplane_xyz[2,]-iplane_xyz[1,], iplane_xyz[3,]-iplane_xyz[1,]))

	# Find camera principal axis
	prin_axis <- rbind(pinhole_xyz, pinhole_xyz+prin_vec)

	# Add cone
	if(cone){

		svg.lines(x=rbind(pinhole_xyz,iplane_xyz[1,]))
		svg.lines(x=rbind(pinhole_xyz,iplane_xyz[2,]))
		svg.lines(x=rbind(pinhole_xyz,iplane_xyz[3,]))
		svg.lines(x=rbind(pinhole_xyz,iplane_xyz[4,]))
		svg.lines(x=rbind(iplane_xyz,iplane_xyz[1,]))
		svg.lines(x=prin_axis, col='red')
	}
	
	# Get index where camera will be added
	add_at <- length(svgviewr_env$svg$camera)+1

	# Set camera name if NULL
	if(is.null(name)) name <- paste0('Camera ', add_at)

	# Add camera
	svgviewr_env$svg$camera[[add_at]] <- list('type'='camera', 'name'=name, 'x'=pinhole_xyz, 'target'=pinhole_xyz+prin_vec, 'focal'=focal, 'set'=set)

	# Add object reference data
	svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
	svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
	svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'camera')


	ret=NULL
}