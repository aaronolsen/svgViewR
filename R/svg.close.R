svg.close <- function(){

	# Get connection type
	conn_type <- getOption("svgviewr_glo_type")

	if(conn_type %in% c('live', 'html')){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))
		
		# If no lights have been added, add lights
		if(is.null(svgviewr_env$svg$bboxLight)){

			# Set light color
			light_col <- rgb(1,1,230/255)
			
			#
			hidden <- TRUE

			# Add high intensity lights
			svg.bboxLight(x=rbind(c(1,1,1), c(-1,1,1), c(-1,-1,-1), c(1,-1,-1)), intensity=1, distance=3.5, col=light_col, hidden=hidden)

			# Add low intensity lights
			svg.bboxLight(x=rbind(c(1,-1,1), c(1,1,-1), c(-1,1,-1), c(-1,-1,1)), intensity=0.4, distance=3.5, col=light_col, hidden=hidden)
		}


		# Set svg objects with source directories
		src_objs <- c('mesh')
		
		# Get unique source directories
		srcs <- c()
		for(src_obj in src_objs){
			for(i in 1:length(svgviewr_env$svg[[src_obj]])){
				if(!is.null(svgviewr_env$svg[[src_obj]][[i]]$src) && svgviewr_env$svg[[src_obj]][[i]]$src != ''){
					srcs <- c(srcs, svgviewr_env$svg[[src_obj]][[i]]$src)
				}else{
					srcs <- c(srcs, NA)
				}
			}
		}
		
		unique_srcs <- NULL
		if(!is.null(srcs) && conn_type == 'live'){

			# Get unique source directories
			unique_srcs <- unique(srcs)
			unique_srcs <- unique_srcs[!is.na(unique_srcs)]

			# Assign matching source directory index (javascript, first index 0)
			for(src_obj in src_objs){
				for(i in 1:length(svgviewr_env$svg[[src_obj]])){
					if(is.null(svgviewr_env$svg[[src_obj]][[i]]$src) || svgviewr_env$svg[[src_obj]][[i]]$src == '') next
					svgviewr_env$svg[[src_obj]][[i]]$src_idx <- which(unique_srcs == srcs[i]) - 1
				}
			}

			# Add directories to app
			for(i in 1:length(unique_srcs)) svgviewr_env$R.server$add(app = File$new(unique_srcs[i]), name = paste0("app_dir", i))
		}

		# If there is an animation, process animation parameters
		if(!is.null(svgviewr_env$svg$animate$times)){

			# Get time units
			time_units <- svgviewr_env$js_var[['time_units']]
			
			# Standardize time units
			if(grepl('^(seconds|s)$', time_units)) time_units <- 'sec'
			if(grepl('millisecond[s]?', time_units)) time_units <- 'msec'

			# Convert to msec
			if(time_units == 'sec'){
				svgviewr_env$js_var[['time_units']] <- 'msec'
				svgviewr_env$svg$animate$times <- svgviewr_env$svg$animate$times*1000
			}
			
			# Apply play speed factor
			svgviewr_env$svg$animate$times <- svgviewr_env$svg$animate$times*(1/svgviewr_env$js_var[['play_speed']])
			
			# Set js variables
			svgviewr_env$js_var[['animation_start']] <- min(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['animation_end']] <- max(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['animation_ntimes']] <- length(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['animation_duration']] <- svgviewr_env$js_var[['animation_end']] - svgviewr_env$js_var[['animation_start']]
		}

		# Get js variables
		js_var <- svgviewr_env$js_var

		# Set tmat element within objects to NULL (no need to write to JSON)
		for(name1 in names(svgviewr_env$svg)){
			if(is.null(svgviewr_env$svg[[name1]])) next
			if(!is.null(names(svgviewr_env$svg[[name1]]))) next
			if(length(svgviewr_env$svg[[name1]]) == 0) next
			for(num in 1:length(svgviewr_env$svg[[name1]])){
				if('tmat' %in% names(svgviewr_env$svg[[name1]][[num]])) svgviewr_env$svg[[name1]][[num]][['tmat']] <- NULL
			}
		}

		# Convert svg objects to json
		svg_json <- rjson::toJSON(x = as.list(svgviewr_env$svg))

		if(conn_type == 'live'){

			# Try stopping server, if running
			tryCatch({ svgviewr_env$R.server$stop() }, error = function(e) {}, warning = function(e) {})

			# Start server
			svgviewr_env$R.server$start(quiet=TRUE)

			# Create app to handle requests and responses
			Rook.app <- function(rook_env) {

				request <- Request$new(rook_env)
				response <- Response$new()

				page_html <- write_HTML(srcs=unique_srcs, json=svg_json, js.var=js_var, server=svgviewr_env$R.server)
				
				response$write(page_html)

				response$finish()
			}

			# Write html for page

			# Add your Rook app to the Rhttp object
			svgviewr_env$R.server$add(app = Rook.app, name = "svgViewR")

			# view your web app in a browser
			svgviewr_env$R.server$browse("svgViewR")

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