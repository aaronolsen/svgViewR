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
		style_str <- paste0('\t\t<link rel="stylesheet" type="text/css" href="', paste0(js.var[['app_dir']], '/css/', 'server_stylesheet.css'), '" ">\n\n')

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

	# Add body text
	if(!is.null(server)){

		# Source through server address
		#page_html <- paste0(page_html, 
		#	paste0(suppressWarnings(readLines(paste0(server$full_url('extdata'), '/html/', 'body.html'))), collapse='\n\t\t'))
		
		page_html <- paste0(page_html, '<div id="container" style="background-color:green;" ></div>
		<div id="bottom_frame" class="bottom_frame" style="height:0px;" ></div>
			<div id="timeline_container_1" class="timeline_container" >
				<div id="timeline_label_1" class="timeline_label" >Label</div>
				<div id="timeline_axis_units_1" class="timeline_axis_units" >Units</div>
				<div id="timeline_cursor_container_1" class="timeline_cursor_container" >
					<div id="timeline_cursor_window_1" class="timeline_cursor_window" >
						<input id="timeline_cursor_slider_1" type="range" title="Click to skip to frame" class="timeline_cursor_slider" oninput="inputTimelineIndex(this);" value="0" >
					</div>
					<div id="timeline_axis_1" class="timeline_axis" >
						<div class="timeline_axis_space_left"></div>
						<div class="timeline_axis_tick_value_left">
							<div class="timeline_axis_tick_tall">&#9475;</div>
							<div class="timeline_axis_value">0.0</div>
						</div>
						<div class="timeline_axis_space"></div>
						<div class="timeline_axis_tick_value">
							<div class="timeline_axis_tick">&#9475;</div>
							<div class="timeline_axis_tvspace"></div>
							<div class="timeline_axis_value">0.17</div>
						</div>
						<div class="timeline_axis_space"></div>
						<div class="timeline_axis_tick_value">
							<div class="timeline_axis_tick">&#9475;</div>
							<div class="timeline_axis_tvspace"></div>
							<div class="timeline_axis_value">0.33</div>
						</div>
						<div class="timeline_axis_space"></div>
						<div class="timeline_axis_tick_value">
							<div class="timeline_axis_tick_tall">&#9475;</div>
							<div class="timeline_axis_value">0.5</div>
						</div>
						<div class="timeline_axis_space"></div>
						<div class="timeline_axis_tick_value">
							<div class="timeline_axis_tick">&#9475;</div>
							<div class="timeline_axis_tvspace"></div>
							<div class="timeline_axis_value">0.67</div>
						</div>
						<div class="timeline_axis_space"></div>
						<div class="timeline_axis_tick_value">
							<div class="timeline_axis_tick">&#9475;</div>
							<div class="timeline_axis_tvspace"></div>
							<div class="timeline_axis_value">0.83</div>
						</div>
						<div class="timeline_axis_space"></div>
						<div class="timeline_axis_tick_value_right">
							<div class="timeline_axis_tick_tall">&#9475;</div>
							<div class="timeline_axis_value">1.0</div>
						</div>
					</div>
				</div>
				<div id="timeline_cursor_value_1" class="timeline_cursor_value" >Value</div>
				<div id="timeline_cursor_input_1" class="timeline_cursor_input" >Input</div>
				<div id="timeline_playback_buttons_1" class="timeline_playback_buttons" >Playback</div>
				<div id="timeline_speed_value_1" class="timeline_speed_value" >Speed</div>
				<div id="timeline_speed_input_1" class="timeline_speed_input" >Input</div>
			</div>
		</div>')

	}else{

		# Source from path
		page_html <- paste0(page_html, 
			paste0(suppressWarnings(readLines(paste0(js.var[['app_dir']], '/html/', 'body.html'))), collapse='\n\t\t'))
	}

	page_html <- paste0(page_html, '\n')

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
		<div class="clock" >\n\t\t\tClock: <span id="clock" ></span>\n\t\t</div>
		<div class="idx" >\n\t\t\tIndex: <span id="idx" ></span>\n\t\t</div>
		<div class="time" >\n\t\t\tTime: <span id="time" ></span>\n\t\t</div>\n')
				}

	page_html <- paste0(page_html, '\t\t<div class="alert" ><span id="alert" ></span></div>')
	page_html <- paste0(page_html, '\t\t<div class="alert2" ><span id="alert2" ></span></div>')

	page_html <- paste0(page_html, '
	</body>\n</html>')

	page_html
}