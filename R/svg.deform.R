svg.deform <- function(dmarr, applyto = '', times = NULL, regexp = FALSE){

	# If times do not start at 0, shift to start at 0
	if(times[1] != 0) times <- times - min(times, na.rm=TRUE)

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")
	
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

	# Add deformations to deform
	svgviewr_env$svg$deform$names <- c(svgviewr_env$svg$deform$names, ref_names[applyto_which])
	svgviewr_env$svg$deform$type <- c(svgviewr_env$svg$deform$type, ref_types[applyto_which])
	svgviewr_env$svg$deform$num <- c(svgviewr_env$svg$deform$num, ref_nums[applyto_which])

	# Define number of animation iterations and times if not already defined
	if(is.null(svgviewr_env$svg$animate$times)) svgviewr_env$svg$animate$times <- times

	# For each object for which position will become 
	for(idx in applyto_which){

		# Create list from deformation array (each element is a time point)
		deform <- lapply(seq_len(dim(dmarr)[3]), function(i) t(signif(dmarr[,,i], digits=env[['svgviewr_env']][['js_var']][['signif_digits']])))
		
		# Save deformation
		svgviewr_env[['svg']][[ref_types[idx]]][[ref_nums[idx]]][['deform']] <- deform
	}

	ret = NULL
}