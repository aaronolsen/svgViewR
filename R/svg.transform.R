svg.transform <- function(tmarr, applyto = '', time = 1:dim(tmarr)[3], add = FALSE, regexp = FALSE){

	# For now only allow one animation name - eventually make it so that different animations 
	# can be applied to the same objects (e.g. different trials)
	name <- 'animation'

	# Make sure that type is webgl
	if('webgl' != getOption("svgviewr_glo_type")) stop("Transform is currently only available with webgl svgViewR output.")
	
	# Make sure number of times matches the number of iterations in transformation array
	if(length(time) != dim(tmarr)[3]) stop(paste0("The number of times in 'time' (", length(time), ") does not match the number of iterations in 'tmarr' (", dim(tmarr)[3], ")."))

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all current object names
	obj_names <- env$svgviewr_env$ref$names

	# Get matching names
	if(!regexp){

		# Use exact matching
		applyto_which <- which(obj_names %in% applyto)

	}else{

		# Use regular expression matching
		applyto_which <- c()
		for(i in 1:length(applyto)) applyto_which <- c(applyto_which, which(grepl(applyto[i], obj_names)))
	}

	# Make sure name was found
	if(length(applyto_which) == 0) stop(paste0("No objects matching the name(s) '", paste0(applyto, collapse="','"), "' have been created."))

	# Get reference objects
	ref_type <- env$svgviewr_env$ref$type
	ref_num <- env$svgviewr_env$ref$num

	# For some objects (mesh from file) use transform, for others add transform into object position
	applyto_w_tm <- applyto_which[which(env$svgviewr_env$ref$type[applyto_which] %in% c('mesh'))]
	applyto_w_ob <- applyto_which[!applyto_which %in% applyto_w_tm]

	# Get names for transform
	applyto_tm_names <- env$svgviewr_env$ref$names[unique(applyto_w_tm)]

	####
	# ***** Place times in general animation object so that it can be accessed whether 
	# animation is through tm or x_tm, etc.

	# Add transformation
	if(length(applyto_tm_names) > 0){
	
		for(idx in applyto_w_tm) env$svgviewr_env[[ref_type[idx]]][[ref_num[idx]]][['tm']] <- TRUE

		# Check if transformation array has already been created
		if(is.null(env$svgviewr_env$tm)){

			# If no, create new transformation list
			tm <- list()
			tm[[name]] <- list()

			# Add transformations to each named object
			for(applyto_name in applyto_tm_names){
				tm[[name]][[applyto_name]] <- list(
					'time'=time,
					'tmat'=tmarr
				)
			}

			# Add to environment
			env$svgviewr_env$tm <- tm

		}else{
	
			# Get from environment
			tm <- env$svgviewr_env$tm

			# Add transformations to each named object
			for(applyto_name in applyto_tm_names){

				# Check if apply to already exists
				if(applyto_name %in% names(env$svgviewr_env$tm[['animation']])){
			
					stop("Applying transformation to transformation is not yet supported.")

					# If yes, and not adding to the end, check that dimensions match
					if(!add){

					}else{

					}

				}else{

					tm[[name]][[applyto_name]] <- list(
						'time'=time,
						'tmat'=tmarr
					)
				}
			}

			# Add to environment
			env$svgviewr_env$tm <- tm
		}
	}

	if(length(applyto_w_ob) > 0){

		# For each object for which position will become 
		for(idx in applyto_w_ob){

			if(ref_type[idx] == 'sphere'){
			
				x_tm <- apply_transform_svg(to=env$svgviewr_env[[ref_type[idx]]][[ref_num[idx]]][['x']], 
					tmat=tmarr)

				# Transform center
				env$svgviewr_env[[ref_type[idx]]][[ref_num[idx]]][['x_tm']] <- lapply(seq_len(nrow(x_tm)), function(i) x_tm[i,])
			}

			if(ref_type[idx] == 'line'){

				x_tm <- apply_transform_svg(to=t(env$svgviewr_env[[ref_type[idx]]][[ref_num[idx]]][['x']]), 
					tmat=tmarr)

				# Transform center
				env$svgviewr_env[[ref_type[idx]]][[ref_num[idx]]][['x_tm']] <- lapply(seq_len(dim(x_tm)[3]), function(i) t(x_tm[, , i]))
			}
		}
	}


return(1)

}