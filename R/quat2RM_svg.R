quat2RM_svg <- function(q){

	# http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToMatrix/index.htm
	# assumed input order is qx, qy, qz, qw
	
	RM <- cbind(
		c(1 - 2*q[2]^2 - 2*q[3]^2, 2*q[1]*q[2] - 2*q[3]*q[4], 2*q[1]*q[3] + 2*q[2]*q[4]),
		c(2*q[1]*q[2] + 2*q[3]*q[4], 1 - 2*q[1]^2 - 2*q[3]^2, 2*q[2]*q[3] - 2*q[1]*q[4]),
		c(2*q[1]*q[3] - 2*q[2]*q[4], 2*q[2]*q[3] + 2*q[1]*q[4], 1 - 2*q[1]^2 - 2*q[2]^2)
	)
	
	RM
}