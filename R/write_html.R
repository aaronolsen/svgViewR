write_HTML <- function(srcs, json, js.var, server = NULL){

	# Set javascript source files
	# THREE.MeshLine.js: https://github.com/spite/THREE.MeshLine , 'THREE.MeshLine.js'
	js_src <- c('render.scene.js', 'three.min.js', 'TrackballControls.js', 'Detector.js', 'jquery-3.2.1.min.js', 'jquery.mousewheel.js')
	
	if(js.var[['show_stats']]) js_src <- c(js_src, 'stats.min.js')

	# Write javascript source files
	if(!is.null(server)){

		# Source through server address
		js_src_str <- paste0('\t\t<script src="', server$full_url('extdata'), '/js/', paste0(js_src, collapse=paste0('"></script>\n\t\t<script src="', server$full_url('extdata'), '/js/')), '"></script>\n')

	} else if(js.var[['src_link']]){

		# Source from computer filepath
		js_src_str <- paste0('\t\t<script src="', js.var[['app_dir']], '/js/', paste0(js_src, collapse=paste0('"></script>\n\t\t<script src="', js.var[['app_dir']], '/js/')), '"></script>\n')

	}else{

		js_src_str <- ''
		for(i in 1:length(js_src)){

			# Read js file
			read_js <- suppressWarnings(readLines(paste0(js.var[['app_dir']], '/js/', js_src[i])))

			# Copy lines into html
			js_src_str <- paste0(js_src_str, paste0('\t\t<script>\n\t\t\t', paste0(read_js, collapse='\n\t\t\t'), '\n\t\t</script>\n'))
		}
	}

	## Start head content
	# Create title and meta tags
	title_and_meta <- paste0('<title>', js.var[['window_title']], '</title>\n\t\t<meta charset="utf-8">\n\t\t<meta name="viewport" content="width=device-width, user-scalable=no, minimum-scale=1.0, maximum-scale=1.0">\n\n')

	# Write stylesheet link
	if(!is.null(server)){

		# Source through server address
		style_str <- paste0('\t\t<link rel="stylesheet" type="text/css" href="', server$full_url('extdata'), '/css/server_stylesheet.css" >\n\n')

	} else if(js.var[['src_link']]){

		# Source from computer filepath
		style_str <- paste0('\t\t<link rel="stylesheet" type="text/css" href="', paste0(js.var[['app_dir']], '/css/', 'server_stylesheet.css'), '" >\n\n')

	}else{

		# Read css file
		read_css <- suppressWarnings(readLines(paste0(js.var[['app_dir']], '/css/', 'server_stylesheet.css')))

		# Create style string
		style_str <- paste0('\t\t<style>\n\t\t\t', paste0(read_css, collapse='\n\t\t\t'), '\n\t\t</style>\n\n')
	}

	# Create on ready script - doesn't work when put before source scripts...
	on_ready_script <- paste0('\n\t\t<script>\n\t\t\t$(document).ready(\n\t\t\t\tfunction() {\n\t\t\t\t\tonReady();\n\t\t\t\t}\n\t\t\t);\n\t\t</script>\n')
	
	# Start body html
	body_html <- paste0('<div id="container" style="background-color:green;" ></div>\n')

	# Add control panel
	if(svgviewr_env$js_var[['panel']]){

		# Get unique names of all objects
		names_unique <- sort(unique(svgviewr_env$ref$names))
		
		# Exclude certain names
		names_unique <- names_unique[!names_unique %in% c('frame.axislabel', 'frame.ticklabel')]
		
		if(length(names_unique) > 0){

			# Set height and width of panel
			height <- length(names_unique)*20
			width <- min(c(300, max(nchar(names_unique))*10 + 15))

			# Open control panel div
			body_html <- paste0(body_html, '\t\t<div id="control_panel" class="control_panel" style="color:', js.var[['panel_col']], ';background-color:', js.var[['panel_bg_col']], ';height:', height, 'px;width:', width, 'px;">\n')
		
			#
			for(i in 1:length(names_unique)){

				# Start row
				body_html <- paste0(body_html, '\t\t\t<div class="visibility_row">\n')

				# Add checkbox and name of element
				body_html <- paste0(body_html, '\t\t\t\t<input type="checkbox" onchange="toggleVisibility(this)" name="visibility_box_1" value="', names_unique[i], '" checked>', names_unique[i], '\n')

				# End row
				body_html <- paste0(body_html, '\t\t\t</div>\n')
			}

			# Close control panel div
			body_html <- paste0(body_html, '\t\t</div>\n')
		}
	}

	# Open bottom frame div
	body_html <- paste0(body_html, '\t\t<div id="bottom_frame" class="bottom_frame" >\n')

	# Start additional style string
	style_str_add <- paste0('\t\t<style>\n')

	# Add timeline
	if(svgviewr_env$js_var[['animate']]){
		
		# Set number of times to display in timeline
		timeline_n_values <- 11
		
		# Check that number of values is odd
		if(timeline_n_values %% 2 == 0) stop('Number of timeline values should be odd.')
		
		# Number of bold ticks
		timeline_n_bold_ticks <- floor(timeline_n_values / 2 - 1)

		# Make sure number of bold ticks is odd		
		if(timeline_n_bold_ticks %% 2 == 0) timeline_n_bold_ticks <- timeline_n_bold_ticks + 1

		# Set ratio, space relative to value div width
		ratio_space_value <- 2.13
		width_total_percent <- 99.4
		width_space_left <- 0.5
		
		# Calculate width of value div
		width_value <- (width_total_percent - width_space_left) / (timeline_n_values + ratio_space_value*(timeline_n_values - 1))
		width_space <- ratio_space_value*width_value
		
		# Create sequence of times, convert from ms to sec
		timeline_time_seq <- seq(svgviewr_env$js_var[['timeline_start']], svgviewr_env$js_var[['timeline_end']], length=timeline_n_values) / 1000
		
		# Format values
		timeline_time_seq_f <- rep(NA, length(timeline_time_seq))

		# Set number of digits to start with
		tseq_n_digits <- 3

		# Try adding digits if first two values are identical
		n_trys <- 0
		i <- 1
		while(i <= length(timeline_time_seq)){

			# Set value		
			value <- signif(timeline_time_seq[i], digits=tseq_n_digits)
		
			# Format value of numeric value
			if(nchar(value) == 1){
				if(!grepl('[.]', value)) value <- paste0(value, '.')
				value <- paste0(value, '0')
			}
			if(nchar(value) >= tseq_n_digits+1 && substr(value, 2, 2) == '.') value <- substr(value, 1, tseq_n_digits)
			
			timeline_time_seq_f[i] <- value
			
			if(i == 2 && n_trys < 5 && as.numeric(timeline_time_seq_f[1]) == as.numeric(timeline_time_seq_f[2])){
				tseq_n_digits <- tseq_n_digits + 1
				n_trys <- n_trys + 1
				i <- 1
			}else{
				i <- i + 1
			}
		}

		# Set which ticks are bold
		tick_is_bold <- rep(FALSE, timeline_n_values)
		tick_is_bold[c(1, timeline_n_values)] <- TRUE
		tick_is_bold[ceiling(seq(1, timeline_n_values, length=timeline_n_bold_ticks))] <- TRUE

		# Add to style string
		css_timeline_axis_space <- paste0('\t\t\t.timeline_axis_space {\n\t\t\t\twidth:', round(width_space, 2), '%;\n\t\t\t\theight:100%;\n\t\t\t}\n')
		style_str_add <- paste0(style_str_add, css_timeline_axis_space)

		# Add to style string
		css_timeline_axis_tick_value <- paste0('\t\t\t.timeline_axis_tick_value, .timeline_axis_tick_value_left, .timeline_axis_tick_value_right {\n\t\t\t\twidth:', round(width_value, 2), '%;\n\t\t\t\theight:100%;\n\t\t\t}\n')
		style_str_add <- paste0(style_str_add, css_timeline_axis_tick_value)
		
		# Set timeline label
		timeline_label <- 'Animation'
		
		# Set timeline units
		timeline_units <- 'sec'

		# Set bottom frame height based on number of timelines
		js.var['bottom_frame_height_px'] <- 28 * svgviewr_env$js_var[['n_timelines']]

		for(tl_num in 1:svgviewr_env$js_var[['n_timelines']]){

			# Add body text
			body_html <- paste0(body_html, '
				<div id="timeline_container_', tl_num, '" class="timeline_container" >
					<div id="timeline_label_', tl_num, '" class="timeline_label" >', timeline_label, '</div>
					<div id="timeline_axis_in_', tl_num, '" class="timeline_axis_in" >in</div>
					<div id="timeline_axis_units_', tl_num, '" class="timeline_axis_units" >', timeline_units, '</div>
					<div id="timeline_cursor_container_', tl_num, '" class="timeline_cursor_container" >
						<div id="timeline_cursor_window_', tl_num, '" class="timeline_cursor_window" >
							<input id="timeline_slider_', tl_num, '" type="range" title="Click to skip to frame" class="timeline_slider" oninput="inputTimelineIndex(this, ', tl_num-1, ', \'slider\');" value="0" >
						</div>
						<div id="timeline_axis_', tl_num, '" class="timeline_axis" >
							<div class="timeline_axis_space_left"></div>')

			for(i in 1:length(timeline_time_seq_f)){
			
				# Create tick divs
				if(tick_is_bold[i]){
					before_value <- '<div class="timeline_axis_tick_tall">&#9475;</div>\n\t\t\t\t\t\t\t<div class="timeline_axis_tall_vspace"></div>'
				}else{
					before_value <- '<div class="timeline_axis_tick">&#9475;</div>\n\t\t\t\t\t\t\t<div class="timeline_axis_vspace"></div>'
				}
			
				# Set value class
				if(i == 1){
					value_class <- 'timeline_axis_tick_value_left'
				}else if(i < length(timeline_time_seq)){
					value_class <- 'timeline_axis_tick_value'
				}else{
					value_class <- 'timeline_axis_tick_value_right'
				}

				body_html <- paste0(body_html, '
							<div class="', value_class, '">\n\t\t\t\t\t\t\t', before_value, '
								<div class="timeline_axis_value">', timeline_time_seq_f[i], '</div>
							</div>'
				)

				if(i < length(timeline_time_seq)){
					body_html <- paste0(body_html, '
							<div class="timeline_axis_space"></div>'
					)
				}
			}

			# Set timeline step
			if(js.var[['interpolate']]){
				timeline_step <- js.var[['timeline_duration_disp']] / 100
			}else{
				timeline_step <- js.var[['timeline_duration_disp']] / (js.var[['animation_ntimes']]-1)
			}
			timeline_step <- signif(timeline_step, 4)

			# Timeline direct value input
			body_html <- paste0(body_html, '
						</div>
					</div>
					<div id="timeline_value_', tl_num, '_div" class="timeline_value_div" >
						<input id="timeline_value_', tl_num, '" type="number" oninput="inputTimelineIndex(this, ', tl_num-1, ', \'value\')" 
							title="Enter number to skip to frame" class="timeline_value_input" 
							min="', js.var[['timeline_start_disp']], '" max="', js.var[['timeline_end_disp']], 
							'" step="', timeline_step, '" value="', js.var[['timeline_start_disp']], '">
					</div>')

			# Timeline playback controls
			body_html <- paste0(body_html, '
					<div id="timeline_playback_buttons_', tl_num, '" class="timeline_playback_buttons" >
						<div title="Skip to the beginning" class="timeline_playback_button" onClick="javascript:skipToAnimationFrame(\'b\', \'', tl_num-1, '\');">
							<a class="timeline_playback_stb">
								<span class="timeline_playback_vb">&#9614;</span>&#9664;
							</a><a class="timeline_playback_play">&#9664;</a>
						</div>
						<div title="Previous (key)frame" class="timeline_playback_button" onClick="javascript:skipToAnimationFrame(\'p\', \'', tl_num-1, '\');">
							<a class="timeline_playback_pf">
								<span class="timeline_playback_vb">&#9614;</span>&#9664;
							</a>
						</div>
						<div title="Play/pause animation" id="timeline_playback_play_', tl_num, '" class="timeline_playback_button" onClick="javascript:playPauseAnimation();">
							<a id="timeline_play_icon_', tl_num, '" class="timeline_playback_play">
								&#9654;
							</a>
						</div>
						<div title="Next (key)frame" class="timeline_playback_button" onClick="javascript:skipToAnimationFrame(\'n\', \'', tl_num-1, '\');">
							<a class="timeline_playback_nf">
								&#9654;<span class="timeline_playback_vb">&#9614;</span>
							</a>
						</div>
						<div title="Skip to the end" class="timeline_playback_button" onClick="javascript:skipToAnimationFrame(\'e\', \'', tl_num-1, '\');">
							<a class="timeline_playback_stel">&#9654;</a><a class="timeline_playback_ste">&#9654;<span class="timeline_playback_vb">&#9614;</span></a>
						</div>
					</div>\n')

			# Animation speed settings
			body_html <- paste0(body_html, '
					<div class="timeline_speed_label" >Speed:</div>
					<div id="timeline_speed_', tl_num, '_div" class="timeline_speed_div" >
						<input id="timeline_speed_', tl_num, '" type="number" oninput="changeAnimationSpeed(this.value, ', tl_num-1, ')" 
							title="Set speed of animation playback" class="timeline_speed_input" 
							min="0.1" max="10" step="0.1" value="', js.var[['play_speed']], '" >
					</div>
				</div>\n')
		}

	}else{

		# Bottom frame height 0 if no timeline
		js.var[['bottom_frame_height_px']] <- 0
	}

	# Close bottom frame div
	body_html <- paste0(body_html, '\t\t</div>\n')

	if(!is.null(server)){
		body_html <- paste0(body_html, '
		<iframe id="iframe" name="iframe" style="display:none;">INIFRAME</iframe>
		<form id="form" action="', server$full_url(),'/custom/svgViewR" onSubmit="javascript:submit();" method="post" target="iframe" >
			<input id="form_input" type="hidden" name="jsonstring" value="">
			<input id="form_input_type" type="hidden" name="type" value="default">
		</form>\n')
	}

	if(js.var[['show_clock']]){
		body_html <- paste0(body_html, '
		<div class="system_time" >\n\t\t\tSystem time (ms): <span id="system_time" ></span>\n\t\t</div>
		<div class="clock" >\n\t\t\telapsed_ms: <span id="clock" ></span>\n\t\t</div>
		<div class="idx" >\n\t\t\tIndex: <span id="idx" ></span>\n\t\t</div>
		<div class="time" >\n\t\t\telapsed_ms*play_speed: <span id="time" ></span>\n\t\t</div>
		\n')
	}

	if(js.var[['debug']]){
		body_html <- paste0(body_html, '
		<div class="inactive_since" >\n\t\t\tInactive since: <span id="inactive_since" ></span>\n\t\t</div>
		\n')
	}

	body_html <- paste0(body_html, '\t\t<div class="alert" ><span id="alert" ></span></div>')
	body_html <- paste0(body_html, '\t\t<div class="alert2" ><span id="alert2" ></span></div>')

	# Create script vars
	script_vars <- ''

	# Write app directories
	if(!is.null(server)){
		#script_vars <- paste0(script_vars, '\t\t\tvar server_url = "', server$full_url(), '/custom;"\n')
		script_vars <- paste0(script_vars, '\t\t\tvar server_url = "', server$full_url(), '";\n')
		script_vars <- paste0(script_vars, '\t\t\tvar app_dir = [\n')
		if(!is.null(srcs)){
			for(i in 1:length(srcs)) script_vars <- paste0(script_vars, '\t\t\t\t"', server$full_url(paste0('app_dir', i)), '",\n')
		}
		script_vars <- paste0(script_vars, '\t\t\t]\n')
	}

	script_vars <- paste0(script_vars, '\t\t\tvar svg_obj = JSON.parse(\'', json, '\');\n')

	# Sort by names alphabetically
	js.var <- js.var[order(names(js.var))]

	# Write lines
	for(i in 1:length(js.var)){
		if(length(js.var[[i]]) > 1 || names(js.var)[i] %in% c('save_as_img_paths')){
			script_vars <- paste0(script_vars, '\t\t\tvar ', names(js.var)[i], ' = [\"', paste0(js.var[[i]], collapse='","'), '\"];\n')
		}else{
			if(grepl('^0x', js.var[[i]]) || is.numeric(js.var[[i]])){
				script_vars <- paste0(script_vars, '\t\t\tvar ', names(js.var)[i], ' = ', js.var[[i]], ';\n')
			}else if(js.var[[i]] == 'FALSE' || js.var[[i]] == 'TRUE'){
				script_vars <- paste0(script_vars, '\t\t\tvar ', names(js.var)[i], ' = ', tolower(js.var[[i]]), ';\n')
			}else{
				script_vars <- paste0(script_vars, '\t\t\tvar ', names(js.var)[i], ' = \"', js.var[[i]], '\";\n')
			}
		}
	}

	# Close additional style string
	style_str_add <- paste0(style_str_add, '\t\t</style>\n')

	## Create page
	# head
	#	title, meta, script, link, style
	# body
	page_html <- paste0('<!DOCTYPE html>\n<html>\n\t', 
		'<head>\n\t\t', title_and_meta, '\t\t<script>\n', script_vars, '\t\t</script>\n\n', 
		style_str, js_src_str, on_ready_script, style_str_add, '\t</head>\n', 
		'\t<body>\n\t\t', body_html, '\n\t</body>\n', '</html>')

	page_html
}