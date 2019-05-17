defineCircle_svg <- function(center, nvector, point_on_radius=NULL, radius=NULL){

	# 1) Let N be a unit normal vector for the plane.
	# 2) Let C be the circle center, and let R be the radius.
	# 3) Let U be a unit vector from C toward a point on the circle.
	# 4) Let V = N x U
	# 5) Let T be the parameter.
	# T point P is on the circle if...
	# 	P = C + R cos(T) U + R sin(T) V

	if(is.null(point_on_radius) && !is.null(radius)){

		# FIND UNIT VECTOR IN CIRCLE PLANE
		v <- uvector_svg(vorthogonal_svg(nvector))
		
		# SCALE VECTOR TO RADIUS
		point_on_radius <- v*radius + center
	}

	# CHECK THAT POINT ON RADIUS IS WITHIN THE PLANE OF THE CIRCLE
	d <- abs(distPointToPlane_svg(point_on_radius, nvector, center))

	if(d > 10^-10){

		# ORIGINAL CENTER
		center_i <- center

		# IF POINT ON RADIUS IS NOT WITHIN THE PLANE OF THE CIRCLE THEN RE-DEFINE CENTER TO SATISFY THIS
		center <- c(pointNormalOnLine_svg(point_on_radius, center, center + nvector))
	}

	C <- list()
	C$N <- uvector_svg(nvector)
	C$C <- center
	C$U <- uvector_svg(point_on_radius - center)
	
	if(is.null(radius)){
		C$R <- dppt_svg(point_on_radius, center)
	}else{
		C$R <- radius
	}
	C$V <- cprod_svg(C$N, C$U)

	class(C) <- 'circle'

	C
}