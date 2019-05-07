write_HTML <- function(srcs, json, js.var, server = NULL){

	# Set javascript source files
	js_src <- c('render.scene.js', 'three.min.js', 'TrackballControls.js', 'Detector.js', 'jquery-3.2.1.min.js', 'jquery.mousewheel.js')
	
	if(js.var[['show_stats']]) js_src <- c(js_src, 'stats.min.js')

	# Write javascript source files
	if(!is.null(server)){

		# Source through server address
		js_src_str <- paste0('\t\t<script src="', server$full_url('extdata'), '/js/', paste0(js_src, collapse=paste0('"></script>\n\t\t<script src="', server$full_url('extdata'), '/js/')), '"></script>\n')

	} else if(js.var[['debug']]){

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

	# Start page html
	page_html <- ''

	# Write some header contents
	page_html <- paste0(page_html, '<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<title>', js.var[['window_title']], '</title>\n\t\t<meta charset="utf-8">\n\t\t<meta name="viewport" content="width=device-width, user-scalable=no, minimum-scale=1.0, maximum-scale=1.0">\n\n')

	# Write stylesheet link
	if(!is.null(server)){

		# Source through server address
		style_str <- paste0('\t\t<link rel="stylesheet" type="text/css" href="', server$full_url('extdata'), '/css/server_stylesheet.css" >\n\n')

	}else if(js.var[['debug']]){

		# Source from computer filepath
		style_str <- paste0('\t\t<link rel="stylesheet" type="text/css" href="', paste0(js.var[['app_dir']], '/css/', 'server_stylesheet.css'), '" >\n\n')

	}else{

		# Read css file
		read_css <- suppressWarnings(readLines(paste0(js.var[['app_dir']], '/css/', 'server_stylesheet.css')))

		# Create style string
		style_str <- paste0('\t\t<style>\n\t\t\t', paste0(read_css, collapse='\n\t\t\t'), '\n\t\t</style>\n\n')
	}

	page_html <- paste0(page_html, '\t\t<script>\n')

	# Write app directories
	if(!is.null(server)){
		#page_html <- paste0(page_html, '\t\t\tvar server_url = "', server$full_url(), '/custom;"\n')
		page_html <- paste0(page_html, '\t\t\tvar server_url = "', server$full_url(), '";\n')
		page_html <- paste0(page_html, '\t\t\tvar app_dir = [\n')
		if(!is.null(srcs)){
			for(i in 1:length(srcs)) page_html <- paste0(page_html, '\t\t\t\t"', server$full_url(paste0('app_dir', i)), '",\n')
		}
		page_html <- paste0(page_html, '\t\t\t]\n')
	}

	page_html <- paste0(page_html, '\t\t\tvar svg_obj = JSON.parse(\'', json, '\');\n')
	for(i in 1:length(js.var)){
		if(length(js.var[[i]]) > 1 || names(js.var)[i] %in% c('save_as_img_paths')){
			page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = [\"', paste0(js.var[[i]], collapse='","'), '\"];\n')
		}else{
			if(grepl('^0x', js.var[[i]]) || is.numeric(js.var[[i]])){
				page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = ', js.var[[i]], ';\n')
			}else if(js.var[[i]] == 'FALSE' || js.var[[i]] == 'TRUE'){
				page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = ', tolower(js.var[[i]]), ';\n')
			}else{
				page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = \"', js.var[[i]], '\";\n')
			}
		}
	}

	page_html <- paste0(page_html, '\t\t</script>\n\n')

	# Add style string
	page_html <- paste0(page_html, style_str)

	# Add javascript source files
	page_html <- paste0(page_html, js_src_str)

	page_html <- paste0(page_html, '
		<script>
			$(document).ready(
				function() {
					onReady();
				}
			);
		</script>
	</head>
	<body>\n\t\t')

	# Add container
	page_html <- paste0(page_html, '<div id="container" style="background-color:green;" ></div>\n')

	# Open bottom frame div
	page_html <- paste0(page_html, '\t\t<div id="bottom_frame" class="bottom_frame" >\n')

	# Start additional style string
	style_str_add <- paste0('\t<style>\n')

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
		
		# Set which ticks are bold
		tick_is_bold <- rep(FALSE, timeline_n_values)
		tick_is_bold[c(1, timeline_n_values)] <- TRUE
		tick_is_bold[ceiling(seq(1, timeline_n_values, length=timeline_n_bold_ticks))] <- TRUE

		# Add to style string
		css_timeline_axis_space <- paste0('\t\t.timeline_axis_space {\n\t\t\twidth:', round(width_space, 2), '%;\n\t\t\theight:100%;\n\t\t}\n')
		style_str_add <- paste0(style_str_add, css_timeline_axis_space)

		# Add to style string
		css_timeline_axis_tick_value <- paste0('\t\t.timeline_axis_tick_value, .timeline_axis_tick_value_left, .timeline_axis_tick_value_right {\n\t\t\twidth:', round(width_value, 2), '%;\n\t\t\theight:100%;\n\t\t}\n')
		style_str_add <- paste0(style_str_add, css_timeline_axis_tick_value)
		
		# Set timeline label
		timeline_label <- 'Animation'
		
		# Set timeline units
		timeline_units <- 'sec'

		# Add body text
		page_html <- paste0(page_html, '
			<div id="timeline_container_1" class="timeline_container" >
				<div id="timeline_label_1" class="timeline_label" >', timeline_label, '</div>
				<div id="timeline_axis_in_1" class="timeline_axis_in" >in</div>
				<div id="timeline_axis_units_1" class="timeline_axis_units" >', timeline_units, '</div>
				<div id="timeline_cursor_container_1" class="timeline_cursor_container" >
					<div id="timeline_cursor_window_1" class="timeline_cursor_window" >
						<input id="timeline_cursor_slider_1" type="range" title="Click to skip to frame" class="timeline_cursor_slider" oninput="inputTimelineIndex(this);" value="0" >
					</div>
					<div id="timeline_axis_1" class="timeline_axis" >
						<div class="timeline_axis_space_left"></div>'
		)

		for(i in 1:length(timeline_time_seq)){

			# Set value		
			value <- signif(timeline_time_seq[i], digits=3)
			
			# Format value of numeric value
			if(nchar(value) == 1){
				if(!grepl('[.]', value)) value <- paste0(value, '.')
				value <- paste0(value, '0')
			}
			if(nchar(value) >= 4 && substr(value, 2, 2) == '.') value <- substr(value, 1, 3)
			
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

			page_html <- paste0(page_html, '
						<div class="', value_class, '">\n\t\t\t\t\t\t\t', before_value, '
							<div class="timeline_axis_value">', value, '</div>
						</div>'
			)

			if(i < length(timeline_time_seq)){
				page_html <- paste0(page_html, '
						<div class="timeline_axis_space"></div>'
				)
			}
		}

		page_html <- paste0(page_html, '
					</div>
				</div>
				<div id="timeline_cursor_value_1" class="timeline_cursor_value" >Value</div>
				<div id="timeline_cursor_input_1" class="timeline_cursor_input" >Input</div>
				<div id="timeline_playback_buttons_1" class="timeline_playback_buttons" >Playback</div>
				<div id="timeline_speed_value_1" class="timeline_speed_value" >Speed</div>
				<div id="timeline_speed_input_1" class="timeline_speed_input" >Input</div>
			</div>\n')
	}

	# Close bottom frame div
	page_html <- paste0(page_html, '\t\t</div>\n')

	if(!is.null(server)){
		page_html <- paste0(page_html, '
		<iframe id="iframe" name="iframe" style="display:none;">INIFRAME</iframe>
		<form id="form" action="', server$full_url(),'/custom/svgViewR" onSubmit="javascript:submit();" method="post" target="iframe" >
			<input id="form_input" type="hidden" name="jsonstring" value="">
			<input id="form_input_type" type="hidden" name="type" value="default">
		</form>\n')
	}

	if(js.var[['show_clock']]){
		page_html <- paste0(page_html, '
		<div class="system_time" >\n\t\t\tSystem time (ms): <span id="system_time" ></span>\n\t\t</div>
		<div class="clock" >\n\t\t\tClock: <span id="clock" ></span>\n\t\t</div>
		<div class="idx" >\n\t\t\tIndex: <span id="idx" ></span>\n\t\t</div>
		<div class="time" >\n\t\t\tTime: <span id="time" ></span>\n\t\t</div>
		\n')
	}

	if(js.var[['debug']]){
		page_html <- paste0(page_html, '
		<div class="inactive_since" >\n\t\t\tInactive since: <span id="inactive_since" ></span>\n\t\t</div>
		\n')
	}

	page_html <- paste0(page_html, '\t\t<div class="alert" ><span id="alert" ></span></div>')
	page_html <- paste0(page_html, '\t\t<div class="alert2" ><span id="alert2" ></span></div>')

	page_html <- paste0(page_html, '
	</body>\n')

	# Close additional style string
	style_str_add <- paste0(style_str_add, '\t</style>\n')

	# Add style string to page
	page_html <- paste0(page_html, style_str_add)

	page_html <- paste0(page_html, '</html>')

	page_html
}