svg.cplanes <- function(corners1, corners2 = NULL, col='blue', emissive=rgb(0.03, 0.15, 0.21), opacity = 1, name = 'cplanes', 
	seg = 30, ontop = FALSE, create.uvs = FALSE, plot = TRUE){

	## Draws two planes and then draws planes between them on all four sides. Can be used 
	## to create irregularly shaped cuboids

	# Make sure that type is webgl
	#if('live' != getOption("svgviewr_glo_type")) stop("Plane drawing is currently only available with webgl svgViewR output.")

	svg.plane(corners1, col=col, emissive=emissive, opacity = opacity, name = name, 
		seg = seg, ontop = ontop, create.uvs = create.uvs, return.shape = FALSE, plot = plot)

	svg.plane(rbind(corners1[1,], corners2[1,], corners2[2,], corners1[2,]), col=col, emissive=emissive, opacity = opacity, name = name, 
		seg = seg, ontop = ontop, create.uvs = create.uvs, return.shape = FALSE, plot = plot)
	svg.plane(rbind(corners1[2,], corners2[2,], corners2[3,], corners1[3,]), col=col, emissive=emissive, opacity = opacity, name = name, 
		seg = seg, ontop = ontop, create.uvs = create.uvs, return.shape = FALSE, plot = plot)
	svg.plane(rbind(corners1[3,], corners2[3,], corners2[4,], corners1[4,]), col=col, emissive=emissive, opacity = opacity, name = name, 
		seg = seg, ontop = ontop, create.uvs = create.uvs, return.shape = FALSE, plot = plot)
	svg.plane(rbind(corners1[4,], corners2[4,], corners2[1,], corners1[1,]), col=col, emissive=emissive, opacity = opacity, name = name, 
		seg = seg, ontop = ontop, create.uvs = create.uvs, return.shape = FALSE, plot = plot)

	svg.plane(corners2, col=col, emissive=emissive, opacity = opacity, name = name, 
		seg = seg, ontop = ontop, create.uvs = create.uvs, return.shape = FALSE, plot = plot)
}
