svg.dcylinder <- function(ends = NULL, center = NULL, axis = NULL, length = NULL, n=2, gap.prop=0.2, 
	radius=1, rseg=20, hseg=2, open.ended=FALSE, 
	theta.start=0, theta.length=2*pi, col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, 
	ontop = FALSE, name='cylinder'){

	## Dashed cylinder
	if(is.null(ends)) ends <- rbind(center + (length/2)*axis, center - (length/2)*axis)
	
	# Set cylinder length
	cyl_len <- dppt_svg(ends)
	
	# Set cylinder axis
	cyl_axis <- uvector_svg(ends[2,]-ends[1,])
	
	# Set length of each segment/dash
	dash_len <- (cyl_len / n)
	gap_len <- gap.prop*dash_len
	
	for(i in 1:n){
		
		# Set cylinder start
		if(i == 1){
			dash_start <- ends[1,]
		}else{
			dash_start <- ends[1,] + (i-1)*dash_len*cyl_axis + gap_len*cyl_axis
		}

		if(i < n){
			dash_end <- ends[1,] + i*dash_len*cyl_axis - gap_len*cyl_axis
		}else{
			dash_end <- ends[2,]
		}
		
		# Draw cylinder
		svg.cylinder(ends=rbind(dash_start, dash_end), radius=radius, rseg=rseg, hseg=hseg, 
			open.ended=open.ended, theta.start=theta.start, theta.length=theta.length, col=col, 
			emissive=emissive, opacity=opacity, ontop=ontop, name=name)
	}
 
	ret = NULL
}