svg.transform <- function(tmarr, applyto = '', times = NULL, add = FALSE, regexp = FALSE){

	# If times is null, set based on dimensions of tmarr
	if(is.null(times)){
		if(length(dim(tmarr)) == 4){
			times <- 1:dim(tmarr)[4]
		}else if(length(dim(tmarr)) == 3){
			times <- 1:dim(tmarr)[3]
		}else if(length(dim(tmarr)) == 2){
			times <- 1
		}else{
		}
	}

	# If times do not start at 0, shift to start at 0
	if(times[1] != 0) times <- times - min(times, na.rm=TRUE)

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("Transform is currently only available with webgl svgViewR output.")
	
	# If transformations are all NA, return NULL
	if(sum(is.na(tmarr)) == length(tmarr)) return(NULL)

	if(length(dim(tmarr)) == 4 || (length(dim(tmarr)) == 3 && !is.null(dimnames(tmarr)[[3]]))){

		# Check that applyto length matches tmarr if applyto names in tmarr are NULL
		if(is.null(dimnames(tmarr)[[3]]) && length(applyto) != dim(tmarr)[3]) 
			stop(paste0("If dimnames(tmarr)[[3]] is NULL then names of what to apply transformation to should be given through applyto. length(applyto) does not equal dim(tmarr)[3]."))

		# Set applyto through tmarr dimnames
		if(applyto == '' && !is.null(dimnames(tmarr)[[3]])) applyto <- dimnames(tmarr)[[3]]

		for(i in 1:dim(tmarr)[3]){

			# Apply each body transform in array
			if(length(dim(tmarr)) == 3){
				suppressWarnings(svg.transform(tmarr=tmarr[, , i], applyto=applyto[i], add=add, regexp=regexp))
			}else{
				suppressWarnings(svg.transform(tmarr=tmarr[, , i, ], applyto=applyto[i], times=times, add=add, regexp=regexp))
			}
		}

	}else{
	
		# If 3-D array and 3rd dim has names, use as applyto
		if(length(dim(tmarr)) == 3 && !is.null(dimnames(tmarr)[[3]])) applyto <- dimnames(tmarr)[[3]]

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

		if(length(dim(tmarr)) == 2){

			# Single transformation (rather than animation) -- add to matching objects
			for(idx in applyto_which){
			
				# If NULL, set as identity matrix for default
				if(is.null(svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['itmat']])){
					svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['itmat']] <- diag(4)
				}
				
				# Apply to current itmat
				itmat <- applyTransform(to=svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['itmat']], tmat=tmarr)

				if(ref_types[idx] == 'mesh'){
				
					# Set initial position
					iposition <- signif(itmat[1:3, 4], digits=env[['svgviewr_env']][['js_var']][['signif_digits']])
					svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['iposition']] <- iposition

					#tmarr2 <- array(itmat, dim=c(dim(itmat), 1))
					#irotation <- lapply(seq_len(dim(tmarr2)[3]), function(i) -rev(signif(rm2euler(t(tmarr2[1:3, 1:3, 1]))[[1]], digits=env[['svgviewr_env']][['js_var']][['signif_digits']])))
					#print(irotation)

					# Set initial rotation
					irotation <- -rev(signif(rm2euler(t(itmat[1:3, 1:3]))[[1]], digits=env[['svgviewr_env']][['js_var']][['signif_digits']]))
					svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['irotation']] <- irotation

				}else if(ref_types[idx] == 'sphere'){

					# Apply transformation to coordinate
					x_tm <- apply_transform_svg(to=svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x']], tmat=itmat)

					# Transform center
					svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['iposition']] <- x_tm

				}else if(ref_types[idx] == 'line'){

					# Apply transformation to coordinates
					x_tm <- apply_transform_svg(to=t(svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x']]), 
						tmat=tmarr)

					stop('Initial transformation for lines not yet written')
				}

				# Save transformation for setting frame limits and for subsequent initial transformations
				svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['itmat']] <- itmat
			}

		}else{

			# Make sure number of times matches the number of iterations in transformation array
			if(length(times) != dim(tmarr)[3]) stop(paste0("The number of times in 'times' (", length(times), ") does not match the number of iterations in 'tmarr' (", dim(tmarr)[3], ")."))

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

				}else if(ref_types[idx] == 'sphere'){
		
					# Apply transformation to coordinate
					x_tm <- apply_transform_svg(to=svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x']], 
						tmat=tmarr)

					# Transform center
					svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x_tm']] <- lapply(seq_len(nrow(x_tm)), function(i) x_tm[i,])

				}else if(ref_types[idx] == 'line'){

					# Apply transformation to coordinates
					x_tm <- apply_transform_svg(to=t(svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x']]), 
						tmat=tmarr)

					# Transform center
					svgviewr_env$svg[[ref_types[idx]]][[ref_nums[idx]]][['x_tm']] <- lapply(seq_len(dim(x_tm)[3]), function(i) t(x_tm[, , i]))
				}
			}
		}
	}

	ret = NULL
}