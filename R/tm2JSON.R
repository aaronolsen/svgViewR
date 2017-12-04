tm2JSON <- function(tm, time.units = 'sec', digits = 6){
	
	# Create animation list
	# 	**** Move units into each animation
	anim_list <- list(
		'units'=time.units,
		'animations'=list()
	)	

	# For each animation
	# 	**** Javascript should eventually allow multiple animations to be applied to the 
	# 	same objects (e.g. multiple trials for the same individual)
	for(animation in 1:length(tm)){
		
		# Create tracks list
		tracks <- list()
		
		# Add transformations for each object
		for(object in names(tm[[animation]])){

			position_keys <- list()
			rotation_keys <- list()
			
			# Convert transformation matrix into translation and rotation
			for(iter in 1:dim(tm[[animation]][[object]]$tmat)[3]){
		
				# Get translation
				position_keys[[iter]] <- list()
				position_keys[[iter]][['time']] <- signif(tm[[animation]][[object]]$time[iter], digits=digits)
				position_keys[[iter]][['value']] <- signif(tm[[animation]][[object]]$tmat[1:3, 4, iter], digits=digits)

				# Get rotation
				rotation_keys[[iter]] <- list()
				rotation_keys[[iter]][['time']] <- signif(tm[[animation]][[object]]$time[iter], digits=digits)
				rotation_keys[[iter]][['value']] <- signif(-rev(rm2euler(t(tm[[animation]][[object]]$tmat[1:3, 1:3, iter]))[[1]]), digits=digits)
			}
			
			# Add tracks
			tracks[[length(tracks)+1]] <- list(
				'type'='vector3',
				'keys'=position_keys,
				"apply"=object,
				"set"="position"
			)
			tracks[[length(tracks)+1]] <- list(
				'type'='vector3',
				'keys'=rotation_keys,
				"apply"=object,
				"set"="rotation"
			)
		}
		
		# Create tracks list
		anim_list[['animations']][[length(anim_list[['animations']])+1]] <- list(
			'name'=names(tm)[animation],
			'tracks'=tracks
		)
	}

	# Convert list to json
	anim_json <- rjson::toJSON(x=anim_list)

	# Make json easier to read for manual review/editing (requires jsonlite)
	#anim_json_prettify <- prettify(anim_json)
	#anim_json_prettify <- gsub('[[][ \t\n]+([0-9.]+,)[ \t\n]+', '[\\1', anim_json_prettify)
	#anim_json_prettify <- gsub(',[ \t\n]+([0-9.]+)[ \t\n]+', ',\\1', anim_json_prettify)

	anim_json
}