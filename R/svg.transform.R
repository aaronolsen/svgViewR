svg.transform <- function(tmarr, applyto = '', times = 1:dim(tmarr)[3], add = FALSE, regexp = FALSE){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Transform is currently only available with webgl svgViewR output.")
	
	if(length(dim(tmarr)) == 4){

		# Check that applyto length matches tmarr if applyto names in tmarr are NULL
		if(is.null(dimnames(tmarr)[[3]]) && length(applyto) != dim(tmarr)[3]) 
			stop(paste0("If dimnames(tmarr)[[3]] is NULL then names of what to apply transformation to should be given through applyto. length(applyto) does not equal dim(tmarr)[3]."))

		# Set applyto through tmarr dimnames
		if(!is.null(dimnames(tmarr)[[3]])) applyto <- dimnames(tmarr)[[3]]

		# Apply each body transform in array
		for(i in 1:dim(tmarr)[3]) svg.transform(tmarr=tmarr[, , i, ], 
			applyto=applyto[i], times=times, add=add, regexp=regexp)

		return(NULL)
	}

	# Make sure number of times matches the number of iterations in transformation array
	if(length(times) != dim(tmarr)[3]) stop(paste0("The number of times in 'times' (", length(times), ") does not match the number of iterations in 'tmarr' (", dim(tmarr)[3], ")."))

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get reference objects
	ref_names <- svgviewr_env$ref$names
	ref_types <- svgviewr_env$ref$type
	ref_nums <- svgviewr_env$ref$num

	# Get matching names
	if(!regexp){

		# Use exact matching
		applyto_which <- which(ref_names %in% applyto)

	}else{

		# Use regular expression matching
		applyto_which <- c()
		for(i in 1:length(applyto)) applyto_which <- c(applyto_which, which(grepl(applyto[i], ref_names)))
	}

	# Make sure name was found
	if(length(applyto_which) == 0){
		warning(paste0("No objects matching the name(s) '", paste0(applyto, collapse="','"), "' have been created."))
		return(NULL)
	}

	# Add transformations to animate
	svgviewr_env$svg$animate$names <- c(svgviewr_env$svg$animate$names, ref_names[applyto_which])
	svgviewr_env$svg$animate$type <- c(svgviewr_env$svg$animate$type, ref_types[applyto_which])
	svgviewr_env$svg$animate$num <- c(svgviewr_env$svg$animate$num, ref_nums[applyto_which])

	# Define number of animation iterations and times if not already defined
	if(is.null(svgviewr_env$svg$animate$times)) svgviewr_env$svg$animate$times <- times

	# For each object for which position will become 
	for(idx in applyto_which){

		if(ref_types[idx] == 'mesh'){

			# Set position
			position <- lapply(seq_len(dim(tmarr)[3]), function(i) t(signif(tmarr[1:3, 4, i], digits=env[['svgviewr_env']][['js_var']][['signif_digits']])))
			svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['position']] <- position

			# Set rotation
			rotation <- lapply(seq_len(dim(tmarr)[3]), function(i) -rev(signif(rm2euler(t(tmarr[1:3, 1:3, i]))[[1]], digits=env[['svgviewr_env']][['js_var']][['signif_digits']])))
			svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['rotation']] <- rotation

			# Save transformation
			svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['tmat']] <- tmarr
		}

		if(ref_types[idx] == 'sphere'){
		
			# Apply transformation to coordinate
			x_tm <- apply_transform_svg(to=svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x']], 
				tmat=tmarr)

			# Transform center
			svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x_tm']] <- lapply(seq_len(nrow(x_tm)), function(i) x_tm[i,])
		}

		if(ref_types[idx] == 'line'){

			# Apply transformation to coordinates
			x_tm <- apply_transform_svg(to=t(svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x']]), 
				tmat=tmarr)

			# Transform center
			svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x_tm']] <- lapply(seq_len(dim(x_tm)[3]), function(i) t(x_tm[, , i]))
		}
	}

	NULL
}