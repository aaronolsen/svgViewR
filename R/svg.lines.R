svg.lines <- function(x, y=NULL, col="black", z.index=0, layer="", name="line", 
	label="", lwd=1, opacity=1, seg=1, ontop = FALSE, file=NULL){

	if('svg' == getOption("svgviewr_glo_type")){

		# If file is null, set current connection
		if(is.null(file)){

			# Give error if svg.new has not been called
			if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

			# Get current connection
			file <- getOption("svg_glo_con")
		}
	}

	if('svg' == getOption("svgviewr_glo_type")){

		svgviewr.lines(x, file=file, y=y, col=col, z.index=z.index, layer=layer, 
			label=label, lwd=lwd, opacity=opacity)

	}else{

		## Add objects to svgViewR environment
		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))
		
		if(length(dim(x)) == 2){

			# Get line segments
			if(seg > 1 && nrow(line_pts) == 2){
				line_pts <- x
				line_start <- line_pts[1,]
				line_end <- line_pts[2,]
				line_vec <- uvector_svg(line_end-line_start)
				line_mag <- dppt_svg(line_start, line_end)
				new_line_pts <- matrix(line_start, nrow=seg+1, 3, byrow=TRUE) + seq(0, line_mag, length=seg+1)*matrix(line_vec, nrow=seg+1, 3, byrow=TRUE)
			}else{
				new_line_pts <- x
			}

			# Add line
			add_at <- length(svgviewr_env$svg$line)+1
			svgviewr_env$svg$line[[add_at]] <- list('type'='line', 
				'name'=name, x=t(new_line_pts), 'col'=setNames(webColor(col), NULL), 'lwd'=lwd, 'itmat'=diag(4), 
				'opacity'=setNames(opacity, NULL), 'depthTest'=!ontop)

		}else{

			# Add object
			add_at <- length(svgviewr_env$svg$line)+1
			svgviewr_env$svg$line[[add_at]] <- list('type'='line', 
				'name'=name, x=setNames(t(x[,,1]), NULL), 'col'=setNames(webColor(col), NULL), 'lwd'=lwd, 'itmat'=diag(4), 
				'opacity'=setNames(opacity, NULL), 'depthTest'=!ontop)

			# Add animation
			svgviewr_env[['svg']][['line']][[add_at]][['x_tm']] <- lapply(seq_len(dim(x)[3]), function(iter) t(x[,,iter]))
			
			# Add animated x
			#svgviewr_env[['svg']][['line']][[add_at]][['x_animated']] <- lapply(seq_len(dim(x)[3]), function(iter) as.list(setNames(signif(x[,,iter], digits=env[['svgviewr_env']][['js_var']][['signif_digits']]), c('x', 'y', 'z'))))
			#print(svgviewr_env[['svg']][['line']][[add_at]][['x_animated']])

			# Set number of timelines
			svgviewr_env$js_var[['n_timelines']] <- 1
		}

		# Add object reference data
		svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
		svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
		svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'line')
	}
	
	# Suppress return of value in console
	ret = NULL
}