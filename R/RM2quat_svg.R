RM2Quat_svg <- function(RM){
	
	# http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
	
	tr <- RM[1,1] + RM[2,2] + RM[3,3]

	if (tr > 0) { 
	  S <- sqrt(tr+1.0) * 2 # S <- 4*qw 
	  qw <- 0.25 * S
	  qx <- (RM[3,2] - RM[2,3]) / S
	  qy <- (RM[1,3] - RM[3,1]) / S 
	  qz <- (RM[2,1] - RM[1,2]) / S 
	} else if ((RM[1,1] > RM[2,2])&(RM[1,1] > RM[3,3])) { 
	  S <- sqrt(1.0 + RM[1,1] - RM[2,2] - RM[3,3]) * 2 # S <- 4*qx 
	  qw <- (RM[3,2] - RM[2,3]) / S
	  qx <- 0.25 * S
	  qy <- (RM[1,2] + RM[2,1]) / S 
	  qz <- (RM[1,3] + RM[3,1]) / S 
	} else if (RM[2,2] > RM[3,3]) { 
	  S <- sqrt(1.0 + RM[2,2] - RM[1,1] - RM[3,3]) * 2 # S <- 4*qy
	  qw <- (RM[1,3] - RM[3,1]) / S
	  qx <- (RM[1,2] + RM[2,1]) / S 
	  qy <- 0.25 * S
	  qz <- (RM[2,3] + RM[3,2]) / S 
	} else { 
	  S <- sqrt(1.0 + RM[3,3] - RM[1,1] - RM[2,2]) * 2 # S <- 4*qz
	  qw <- (RM[2,1] - RM[1,2]) / S
	  qx <- (RM[1,3] + RM[3,1]) / S
	  qy <- (RM[2,3] + RM[3,2]) / S
	  qz <- 0.25 * S
	}

	# Create quaternion vector
	quat <- c(-qx, -qy, -qz, qw)

	# Is always unit?
	#print(sum(quat^2))

	quat
}