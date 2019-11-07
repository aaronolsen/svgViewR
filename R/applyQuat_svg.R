applyQuat_svg <- function(p, q){
	## Copied from https://gamedev.stackexchange.com/questions/28395/rotating-vector3-by-a-quaternion

	if(is.null(dim(p))){
		return(2 * sum(q[1:3] * p) * q[1:3] + (q[4]^2 - sum(q[1:3]*q[1:3])) * p + 2 * q[4] * cprod_svg(q[1:3], p))
	}else if(length(dim(p) == 2)){
		for(i in 1:nrow(p)) p[i,] <- applyQuat_svg(p[i,], q)
		return(p)
	}
}