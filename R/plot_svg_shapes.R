plot_svg_shapes <- function(svg_list, dgp, as, iter = 1){

	params <- svg_list$params
	shapes <- svg_list$shapes
	z.index <- svg_list$z.index

	z_order <- order(z.index)
	
	# Save xy coordinates that are plotted
	#xy_all <- matrix(NA, nrow=0, ncol=2)

	for(lnum in z_order){

		shape <- shapes[[lnum]]
		xyz <- shape$xyz

		#
		if(length(dim(xyz)) == 2){
			if(nrow(xyz) == 1){
				citer <- 1
			}else if(nrow(xyz) > 1 && nrow(xyz) >= iter){
				citer <- iter
			}else{
				next
			}
			xyz <- xyz[citer, ]
		}else{
			xyz <- xyz[, , citer]
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
		
		if(is.matrix(xyz)){

			u1 <- -(params$depth - params$eyez) / (xyz[,3] - params$eyez)
			xy <- cbind((u1 * xyz[,1])*params$scaling.add + params$shift.add[1], -(u1 * xyz[,2])*params$scaling.add + params$shift.add[2])
			#xy_all <- rbind(xy_all, xy)

		}else{
			u1 <- -(params$depth - params$eyez) / (xyz[3] - params$eyez)
			xy1 <- c(u1 * xyz[1], -(u1 * xyz[2]))*params$scaling.add + params$shift.add
			#xy_all <- rbind(xy_all, xy1)

			u2 <- -(params$depth - params$eyez) / (xyz[6] - params$eyez)
			xy2 <- c(u2 * xyz[4], -(u2 * xyz[5]))*params$scaling.add + params$shift.add
			#xy_all <- rbind(xy_all, xy2)
		}

		if(shape$type %in% c('arrow', 'line')){

			grid.lines(x=c(xy1[1], xy2[1]), y=c(xy1[2], xy2[2]), gp=gpar(col=gparams[['stroke']],  lwd=gparams[['stroke-width']], 
				alpha=gparams[['stroke-opacity']]), default.units="native")

			# Plot arrowhead
			if(shape$type %in% c('arrow')){

				v21 <- c(xyz[4:6]-xyz[1:3])
				v12 <- c(xyz[1:3]-xyz[4:6])
				v12u <- uvector_svg(v12)
				c_prod1 <- uvector_svg(cprod_svg(v21, c(0,0,1)))
				c_prod2 <- uvector_svg(cprod_svg(v21, c_prod1))
				
				if(sum(c_prod2) == 0) c_prod2 <- c(0,1,0)
				if(abs(c_prod2[1]) > 0.98) c_prod2 <- c(0,1,0)

				rm1 <- tMatrixEP_svg(c_prod2, shape$a);
				rm2 <- tMatrixEP_svg(c_prod2, -shape$a);

				# Adjust arrowhead length so that it is longer when z-component is higher (or else front on view hides most of arrowhead)
				ahl <- shape$l+(abs(v12u[3])^10)*0.6*shape$l -(abs(v12u[1]*v12u[2])/0.5)*0.2*shape$l
				ahl <- shape$l+(abs(v12u[3])^10)*0.6*shape$l -(abs(v12u[1]*v12u[2])/0.5)*0.2*shape$l
		
				ha <- c(rm1 %*% v12u*ahl) + xyz[4:6]
				hb <- c(rm2 %*% v12u*ahl) + xyz[4:6]

				hau <- -(params$depth - params$eyez) / (ha[3] - params$eyez);
				hbu <- -(params$depth - params$eyez) / (hb[3] - params$eyez);
		
				ha <- c((hau * ha[1]), -(hau * ha[2]))*params$scaling.add + params$shift.add
				hb <- c((hbu * hb[1]), -(hbu * hb[2]))*params$scaling.add + params$shift.add

				grid.lines(x=c(xy2[1], ha[1]), y=c(xy2[2], ha[2]), gp=gpar(col=gparams[['stroke']],  lwd=gparams[['stroke-width']], 
					alpha=gparams[['stroke-opacity']]), default.units="native")
				grid.lines(x=c(xy2[1], hb[1]), y=c(xy2[2], hb[2]), gp=gpar(col=gparams[['stroke']],  lwd=gparams[['stroke-width']], 
					alpha=gparams[['stroke-opacity']]), default.units="native")
			}

			next
		}

		if(shape$type %in% c('point', 'circle')){

			grid.circle(x=xy1[1], y=xy1[2], r=gparams[['r']], gp=gpar(col='NA', 
				fill=gparams[['fill']], lwd=0, alpha=gparams[['fill-opacity']]), default.units="native")
			grid.circle(x=xy1[1], y=xy1[2], r=gparams[['r']], gp=gpar(col=gparams[['stroke']], 
				fill='NA', lwd=gparams[['stroke-width']], alpha=gparams[['stroke-opacity']]), default.units="native")

			next
		}

		if(shape$type %in% c('text')){
		
			text_hjust <- 0
			if(shape[['text-anchor']] == "start") text_hjust <- 0
			if(shape[['text-anchor']] == "middle") text_hjust <- 0.5
			if(shape[['text-anchor']] == "end") text_hjust <- 1
			
			#if(text_rot == 90) text_just <- "bottom"
			grid.text(label=shape[['value']], x=xy1[1], y=xy1[2], just="bottom", hjust=text_hjust, 
				vjust = NULL, rot = 0, gp=gpar(fontface="plain", fontfamily=gparams[['font-family']], 
				fontsize=max(round(shape[['font-size']]), 1), col=gparams[['fill']], alpha=gparams[['opacity']]), 
				default.units="native")
			
			next
		}

		if(shape$type %in% c('pathC')){

			# Open shape
			if(is.null(gparams[['fill']]) || gparams[['fill']] == 'none'){
				grid.lines(x=xy[,1], y=xy[,2], gp=gpar(col=gparams[['stroke']], fill='NA', 
					lwd=gparams[['stroke-width']], alpha=gparams[['stroke-opacity']]), 
					default.units="native")
			}else{
				# Draw fill
				grid.polygon(x=xy[,1], y=xy[,2], gp=gpar(col='NA', fill=gparams[['fill']], 
					lwd=0, alpha=gparams[['fill-opacity']]), default.units="native")			

				# Draw stroke
				grid.polygon(x=xy[,1], y=xy[,2], gp=gpar(col=gparams[['stroke']], fill='NA', 
					lwd=gparams[['stroke-width']], alpha=gparams[['stroke-opacity']]), 
					default.units="native")
			}
		}

		if(shape$type %in% c('path')){
		}

		#print(shapes[[lnum]][['type']])
		#break
	}
	
	#xy_all
}