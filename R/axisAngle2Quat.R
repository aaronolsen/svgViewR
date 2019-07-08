axisAngle2Quat <- function(axis, angle){

	## From: https://www.euclideanspace.com/maths/geometry/rotations/conversions/angleToQuaternion/index.htm

	# Make sure axis is normalized
	axis <- uvector_svg(axis)

	angle2 <- angle / 2

	sin_angle <- sin(angle2)
	x <- axis[1]*sin_angle
	y <- axis[2]*sin_angle
	z <- axis[3]*sin_angle
	w <- cos(angle2)

	c(x, y, z, w)
}