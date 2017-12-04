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
	obj_names <- env$svgviewr_env$names

	# Get matching names
	if(!regexp){

		# Use exact matching
		applyto_names <- obj_names[obj_names %in% applyto]

	}else{

		# Use regular expression matching
		applyto_names <- c()
		for(i in 1:length(applyto)) applyto_names <- c(applyto_names, obj_names[grepl(applyto[i], obj_names)])
	}

	# Get unique names
	applyto_names <- unique(applyto_names)
	
	# Make sure name was found
	if(length(applyto_names) == 0) stop(paste0("No objects matching the name(s) '", paste0(applyto, collapse="','"), "' have been created."))

	# Check if transformation array has already been created
	if(is.null(env$svgviewr_env$tm)){

		# If no, create new transformation list
		tm <- list()
		tm[[name]] <- list()

		# Add transformations to each named object
		for(applyto_name in applyto_names){
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
		for(applyto_name in applyto_names){

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