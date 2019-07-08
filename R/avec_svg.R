avec_svg <- function(u, v, axis=NULL, about.axis=FALSE, max.pi=FALSE){

	if(anyNA(u) || anyNA(v)) return(NA)

	if(sqrt(sum(u * u)) == 0) stop("Input vector 'u' is zero-length")
	if(sqrt(sum(v * v)) == 0) stop("Input vector 'v' is zero-length")

	uu <- uvector_svg(u)
	vu <- uvector_svg(v)
	
	c <- sum(uu*vu) / sqrt(sum(uu*uu)) * sqrt(sum(vu*vu))

	if(round(abs(c), digits=12) == 1){
		angle <- 0

		# Check for opposite direction
		if(sum(abs(c(uu[1]+vu[1], uu[2]+vu[2], uu[3]+vu[3]))) < 1e-9){
			angle <- pi
		}

	}else{
		if(max.pi){
			angle <- min(acos(c), pi-acos(c))
		}else{
			angle <- acos(c)
		}
	}

	if(!is.null(axis)){
	
		if(sqrt(sum(axis * axis)) == 0){
			if(abs(angle) < 1e-10) return(0)
			stop("Input vector 'axis' is zero-length")
		}

		axis <- uvector_svg(axis)
		
		if(about.axis){

			# DETERMINE ANGLE BETWEEN VECTORS ABOUT AXIS
			up <- pointPlaneProj_svg(uu, c(0,0,0), axis)
			vp <- pointPlaneProj_svg(vu, c(0,0,0), axis)
			
			if(sqrt(sum(up * up)) == 0 || sqrt(sum(vp * vp)) == 0) return(0)

			return(avec_svg(up, vp, axis=axis, about.axis=FALSE))

		}else{

			# DETERMINE DIRECTION USING AXIS
			if(dppt_svg(uvector_svg(cprod_svg(uu, vu)), axis) < dppt_svg(uvector_svg(cprod_svg(vu, uu)), axis)){
				return(-angle)
			}else{
				return(angle)
			}
		}
	}else{

		# DETERMINE DIRECTION USING CROSS-PRODUCT VECTOR AND EULER 
		if(abs(angle) > 0){
		
			cprod_uv <- cprod_svg(uu, vu)
			um <- sqrt(sum(u^2))
			vm <- sqrt(sum(u^2))
			
			if(dppt_svg((uu %*% tMatrixEP_svg(cprod_uv, angle))*vm, v) <= dppt_svg((uu %*% tMatrixEP_svg(cprod_uv, -angle))*vm, v)){
				return(angle)
			}else{
				return(-angle)
			}
		}
	}
	
	return(angle)
}