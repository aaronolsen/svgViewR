svg.camera <- function(camera, image = NULL, cone = FALSE, focal = 23.75, set = TRUE, name = NULL, 
	times = NULL, image.opacity = 1, plane.dist = NULL){

	# If camera is not list (ie camera object) assume camera file and read
	if(!is.list(camera)) camera <- readCam(camera)

	# Set intrinsic and extrinsic matrices
	intrinsic <- camera$intrinsic
	extrinsic <- camera$extrinsic
	cone.dim <- camera$image.size

	# Calculate camera parameters if not already in camera object
	if(is.null(camera$pinhole) || (!is.null(camera$pinhole) && camera$plane.dist != plane.dist)){
		cam_params <- cameraParameters(intrinsic, extrinsic, cone.dim, plane.dist=plane.dist)
		camera$pinhole <- cam_params$pinhole
		camera$plane.corners <- cam_params$plane.corners
		camera$prin.uvec <- cam_params$prin.uvec
		camera$prin.vec <- cam_params$prin.vec
		camera$prin.axis <- cam_params$prin.axis
	}

	# Add cone
	if(cone){

		svg.lines(x=rbind(camera$pinhole,camera$plane.corners[1,]))
		svg.lines(x=rbind(camera$pinhole,camera$plane.corners[2,]))
		svg.lines(x=rbind(camera$pinhole,camera$plane.corners[3,]))
		svg.lines(x=rbind(camera$pinhole,camera$plane.corners[4,]))
		svg.lines(x=rbind(camera$plane.corners,camera$plane.corners[1,]))
		svg.lines(x=camera$prin.axis, col='red')
	}

	if(set){

		# Get index where camera will be added
		add_at <- length(svgviewr_env$svg$camera)+1

		# Set camera name if NULL
		if(is.null(name)) name <- paste0('Camera ', add_at)

		# Add camera
		svgviewr_env$svg$camera[[add_at]] <- list('type'='camera', 'name'=name, 'x'=camera$pinhole, 
			'target'=camera$pinhole+camera$prin.vec, 'focal'=focal, 'set'=set, 'far'=plane.dist*20)

		# Add object reference data
		svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
		svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
		svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'camera')
	}	

	# Add camera image if non NULL
	if(!is.null(image)) svg.images(file=image, corners=camera$plane.corners, opacity=image.opacity, times=times)

	ret=NULL
}