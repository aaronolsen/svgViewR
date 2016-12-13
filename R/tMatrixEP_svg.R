tMatrixEP_svg <- function(v, a){
	# Top version from https://en.wikipedia.org/wiki/Rotation_matrix
	# Bottom from ?
	# Both give the same result

	# For some reason the resulting rotation matrix does not follow the right-hand rule... Flip angle so that it is right-hand
	a <- -a

	v <- uvector_svg(v)

	r <- matrix(0, 3, 3)
	r[1, ] <- c(cos(a)+v[1]^2*(1-cos(a)), v[1]*v[2]*(1-cos(a))-v[3]*sin(a), v[1]*v[3]*(1-cos(a))+v[2]*sin(a))
	r[2, ] <- c(v[2]*v[1]*(1-cos(a))+v[3]*sin(a), cos(a) + v[2]^2*(1-cos(a)), v[2]*v[3]*(1-cos(a))-v[1]*sin(a))
	r[3, ] <- c(v[3]*v[1]*(1-cos(a))-v[2]*sin(a), v[3]*v[2]*(1-cos(a))+v[1]*sin(a), cos(a)+v[3]^2*(1-cos(a)))
	
	return(r)
}
