plot_svg_shapes <- function(svg_list, dgp, iter = 1){

	params <- svg_list$params
	shapes <- svg_list$shapes
	z.index <- svg_list$z.index

	z_order <- order(z.index)

	for(lnum in z_order){

		shape <- shapes[[lnum]]
		xyz <- shape$xyz
		
		#
		if(length(dim(xyz)) == 2){
			if(nrow(xyz) == 1){
				citer <- 1
			}else if(nrow(xyz) > 1 && nrow(xyz) >= iter){
				citer <- iter
			}

		}else{
		}

		# Set default graphic parameters		
		gparams <- dgp
		
		for(pname in names(gparams)){
			if(!is.null(shape[[pname]])){
				if(length(shape[[pname]]) == 1){
					gparams[[pname]] <- shape[[pname]]
				}else if(length(shape[[pname]]) > 1 && length(shape[[pname]]) >= iter){
					gparams[[pname]] <- shape[[pname]][iter]
				}else{
					gparams[[pname]] <- shape[[pname]][(iter-1) %% length(shape[[pname]]) + 1]
				}
			}
			
			if(pname %in% c('stroke', 'fill')) if(!is.na(gparams[[pname]]) && gparams[[pname]] == 'none') gparams[[pname]] <- 'NA'
		}
		
		if(shape$type == 'arrow'){
			#print(gparams)
			#print(shapes[[lnum]])
		}

		if(shape$type %in% c('point', 'circle')){

			u <- -(params$depth - params$eyez) / (xyz[citer, 3] - params$eyez)
			xy <- c(u * xyz[citer, 1] + params$x.shift, y=-(u * xyz[citer, 2]) + params$y.shift)

			grid.circle(x=xy[1], y=xy[2], r=gparams[['r']], gp=gpar(col='NA', 
				fill=gparams[['fill']], lwd=0, alpha=gparams[['fill-opacity']]), default.units="native")
			grid.circle(x=xy[1], y=xy[2], r=gparams[['r']], gp=gpar(col=gparams[['stroke']], 
				fill='NA', lwd=gparams[['stroke-width']], alpha=gparams[['stroke-opacity']]), default.units="native")
		}
	}
}