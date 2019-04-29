axisAngle2Quat2 <- function(axis, angle){

	## Converts rotation of angle about axis to a quaternion
	angle2 <- angle / 2
	
	# Make sure axis is normalized
	axis <- uvector(axis)

	c(cos(angle2), axis*sin(angle2))
}