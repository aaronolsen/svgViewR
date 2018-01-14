svg.close <- function(){

	# Get connection type
	conn_type <- getOption("svgviewr_glo_type")

	if(conn_type %in% c('live', 'html')){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Set svg objects with source directories
		src_objs <- c('mesh')
		
		# Get unique source directories
		srcs <- c()
		for(src_obj in src_objs){
			for(i in 1:length(env$svgviewr_env[[src_obj]])){
				if(!is.null(env$svgviewr_env[[src_obj]][[i]]$src) && env$svgviewr_env[[src_obj]][[i]]$src != ''){
					srcs <- c(srcs, env$svgviewr_env[[src_obj]][[i]]$src)
				}else{
					srcs <- c(srcs, NA)
				}
			}
		}
		
		unique_srcs <- NULL
		if(!is.null(srcs)){

			# Get unique source directories
			unique_srcs <- unique(srcs)
			unique_srcs <- unique_srcs[!is.na(unique_srcs)]

			# Assign matching source directory index (javascript, first index 0)
			for(src_obj in src_objs){
				for(i in 1:length(env$svgviewr_env[[src_obj]])){
					if(is.null(env$svgviewr_env[[src_obj]][[i]]$src) || env$svgviewr_env[[src_obj]][[i]]$src == '') next
					env$svgviewr_env[[src_obj]][[i]]$src_idx <- which(unique_srcs == srcs[i]) - 1
				}
			}

			# Add directories to app
			for(i in 1:length(unique_srcs)) R.server$add(app = File$new(unique_srcs[i]), name = paste0("app_dir", i))
		}

		# If there is an animation, process animation parameters
		if(!is.null(env$svgviewr_env$animate$times)){

			# Get time units
			time_units <- env$svgviewr_env$js_var[['time_units']]
			
			# Standardize time units
			if(grepl('^(seconds|s)$', time_units)) time_units <- 'sec'
			if(grepl('millisecond[s]?', time_units)) time_units <- 'msec'

			# Convert to msec
			if(time_units == 'sec'){
				env$svgviewr_env$js_var[['time_units']] <- 'msec'
				env$svgviewr_env$animate$times <- env$svgviewr_env$animate$times*1000
			}
			
			# Apply play speed factor
			env$svgviewr_env$animate$times <- env$svgviewr_env$animate$times*(1/env$svgviewr_env$js_var[['play_speed']])
			
			# Set js variables
			env$svgviewr_env$js_var[['animation_start']] <- min(env$svgviewr_env$animate$times)
			env$svgviewr_env$js_var[['animation_end']] <- max(env$svgviewr_env$animate$times)
			env$svgviewr_env$js_var[['animation_ntimes']] <- length(env$svgviewr_env$animate$times)
			env$svgviewr_env$js_var[['animation_duration']] <- env$svgviewr_env$js_var[['animation_end']] - 
				env$svgviewr_env$js_var[['animation_start']]
		}

		# Get js variables
		js_var <- env$svgviewr_env$js_var

		# Set elements to NULL that aren't written to json
		env$svgviewr_env$tm <- NULL
		env$svgviewr_env$js_var <- NULL

		# Set tmat element within objects to NULL (no need to write to JSON)
		for(name1 in names(env$svgviewr_env)){
			if(is.null(env$svgviewr_env[[name1]])) next
			if(!is.null(names(env$svgviewr_env[[name1]]))) next
			if(length(env$svgviewr_env[[name1]]) == 0) next
			for(num in 1:length(env$svgviewr_env[[name1]])){
				if('tmat' %in% names(env$svgviewr_env[[name1]][[num]])) env$svgviewr_env[[name1]][[num]][['tmat']] <- NULL
			}
		}

		# Convert svg objects to json
		svg_json <- rjson::toJSON(x = as.list(env$svgviewr_env))

		if(conn_type == 'live'){

			# Start server
			R.server$start(quiet=TRUE)

			# Create app to handle requests and responses
			Rook.app <- function(env) {

				request <- Request$new(env)
				response <- Response$new()

				page_html <- write_HTML(srcs=unique_srcs, json=svg_json, js.var=js_var, server=R.server)
				
				response$write(page_html)

				response$finish()
			}

			# Write html for page

			# Add your Rook app to the Rhttp object
			R.server$add(app = Rook.app, name = "svgViewR")

			# view your web app in a browser
			R.server$browse("svgViewR")

		}else{

			page_html <- write_HTML(srcs=unique_srcs, json=svg_json, js.var=js_var)

			# Create file before opening connection - also clears existing file, if present
			file.create(js_var[['file']])

			# Open file connection (does not create the file)
			con <- file(js_var[['file']], "r+")
	
			# Write lines to connection
			write(page_html, con)

			# Close the connection
			close(con)
		}
		
	}else{

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	
		# Close
		svgviewr.new(file=file, conn.type='close', layers=file$layers, debug=file$debug)
	}

	ret = NULL
}