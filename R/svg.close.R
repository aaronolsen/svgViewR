svg.close <- function(wait = FALSE, quiet = TRUE){

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
		src_objs <- c('mesh', 'image')
		
		# Get unique source directories
		srcs <- c()
		j <- 1
		for(src_obj in src_objs){
			for(i in 1:length(svgviewr_env$svg[[src_obj]])){
				if(!is.null(svgviewr_env$svg[[src_obj]][[i]]$src) && svgviewr_env$svg[[src_obj]][[i]]$src != ''){
					srcs <- c(srcs, svgviewr_env$svg[[src_obj]][[i]]$src)
				}else{
					srcs <- c(srcs, NA)
				}
				j <- j + 1
			}
		}
		
		unique_srcs <- NULL
		if(!is.null(srcs) && conn_type == 'live'){

			# Get unique source directories
			unique_srcs <- unique(srcs)
			unique_srcs <- unique_srcs[!is.na(unique_srcs)]

			# Assign matching source directory index (javascript, first index 0)
			j <- 1
			for(src_obj in src_objs){
				for(i in 1:length(svgviewr_env$svg[[src_obj]])){
					if(is.null(svgviewr_env$svg[[src_obj]][[i]]$src) || svgviewr_env$svg[[src_obj]][[i]]$src == '') { j <- j + 1; next }
					svgviewr_env$svg[[src_obj]][[i]]$src_idx <- which(unique_srcs == srcs[j]) - 1
					j <- j + 1
				}
			}
			
			# Add directories to app
			for(i in 1:length(unique_srcs)) svgviewr_env$R.server$add(app = File$new(unique_srcs[i]), name = paste0("app_dir", i))
		}
		
		# If number of timelines is null, set to 0
		if(is.null(svgviewr_env$js_var[['n_timelines']])) svgviewr_env$js_var[['n_timelines']] <- 0

		# If there is an animation, process animation parameters
		if(!is.null(svgviewr_env$svg$animate$times)){

			# Turn on animate
			svgviewr_env$js_var[['animate']] <- TRUE

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

			# Set timeline start and end
			svgviewr_env$js_var[['timeline_start']] <- min(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['timeline_end']] <- max(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['timeline_duration']] <- svgviewr_env$js_var[['timeline_end']] - svgviewr_env$js_var[['timeline_start']]

			# Set timeline variables to match display/interface values
			if(time_units == 'sec'){
				svgviewr_env$js_var[['timeline_start_disp']] <- svgviewr_env$js_var[['timeline_start']] / 1000
				svgviewr_env$js_var[['timeline_end_disp']] <- svgviewr_env$js_var[['timeline_end']] / 1000
				svgviewr_env$js_var[['timeline_duration_disp']] <- svgviewr_env$js_var[['timeline_duration']] / 1000
			}

			# Apply play speed factor
			#svgviewr_env$svg$animate$times <- svgviewr_env$svg$animate$times*(1/svgviewr_env$js_var[['play_speed']])
			
			# Set js variables
			svgviewr_env$js_var[['browser_open']] <- TRUE
			svgviewr_env$js_var[['animation_start']] <- min(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['animation_end']] <- max(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['animation_ntimes']] <- length(svgviewr_env$svg$animate$times)
			svgviewr_env$js_var[['animation_duration']] <- svgviewr_env$js_var[['animation_end']] - svgviewr_env$js_var[['animation_start']]

		}else{
			svgviewr_env$js_var[['animate']] <- FALSE
		}
		
		# Check save as image parameters
		if(svgviewr_env$js_var[['save_as_img']]){
			
			if(!is.null(svgviewr_env$svg$animate$times)){
				if(!is.null(svgviewr_env$js_var[['save_as_img_paths']])){
				
					# Check that number of images matches number of frames
					if(length(svgviewr_env$js_var[['save_as_img_paths']]) != svgviewr_env$js_var[['animation_ntimes']]){
						stop(paste0("The number of specified image filenames (", length(svgviewr_env$js_var[['save_as_img_paths']]), 
							") does not match the number of animation time points (", svgviewr_env$js_var[['animation_ntimes']], ")"))
					}
				}else{
					
					# Create image names from frames
					svgviewr_env$js_var[['save_as_img_paths']] <- paste0(svgviewr_env$js_var[['save_as_img_dir']], 
						'/', formatC(1:svgviewr_env$js_var[['animation_ntimes']], width=5, flag='0'), '.', 
						svgviewr_env$js_var[['save_as_img_type']])
				}
			}
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
		
#		print(names(svgviewr_env$svg$mesh[[4]]))
#		print(svgviewr_env$svg$mesh[[4]]$position)

		# Convert svg objects to json
		svg_json <- rjson::toJSON(x = as.list(svgviewr_env$svg))

		if(conn_type == 'live'){

			# Try stopping server, if running
			tryCatch({ svgviewr_env$R.server$stop() }, error = function(e) {}, warning = function(e) {})

			# Start server
			svgviewr_env$R.server$start(quiet=quiet)

			viewer_app <- function(rook_env) {

#cat('start viewer app\n')
				request <- Request$new(rook_env)
				response <- Response$new()

				# If non NULL, process POST request
				if(!is.null(request$POST())){
					
					request_post <- request$POST()
					
					# If JSON string, parse string after decoding
					if(request_post[['type']] == 'jsonstring') request_post <- fromJSON(URLdecode(request_post[['jsonstring']]))
					
					# Save image
					if(request_post[['function']] == 'save_image'){

						viewer_save_image(from=request_post$image$tempfile, to=request_post$save_image_as)

						#response$write('TEST2')
						#print('save_image')

					}else if(request_post[['function']] == 'close'){
						
						# Send response back to browser
						svgviewr_env$js_var[['browser_open']] <- FALSE
						response$write(toJSON(list('call'='window.close();')))

						# Print to console
						#print(request_post)
						#print('close')

					}else{
					}

				}else{

#print(list(srcs=unique_srcs, js.var=js_var, server=svgviewr_env$R.server))
					page_html <- write_HTML(srcs=unique_srcs, json=svg_json, js.var=js_var, server=svgviewr_env$R.server)
					response$write(page_html)
				}

				#cat('end viewer app\n')

				response$finish()
			}

			# Add your Rook app to the Rhttp object
			svgviewr_env$R.server$add(app = viewer_app, name = "svgViewR")

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
		svgviewr.new(file=file, conn.type='close', layers=file$layers, debug=file$debug, app.dir.src=file$app.dir.src)
	}

	# If wait is TRUE don't return function until signaled
	t1 <- proc.time()[3]
	if(wait){

		# Check every interval seconds whether browser has been closed by javascript
		interval <- 0.1
		while(svgviewr_env$js_var[['browser_open']]){
			#print(proc.time()[3] - t1)
			#if(proc.time()[3] - t1 > 20) break
			Sys.sleep(0.1)
		}

		Sys.sleep(1)
	}

	ret = NULL
}