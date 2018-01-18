write_HTML <- function(srcs, json, js.var, server = NULL){

	# Set javascript source files
	js_src <- c('render.scene.js', 'three.min.js', 'TrackballControls.js', 'Detector.js', 'jquery-3.2.1.min.js', 'jquery.mousewheel.js')
	
	if(js.var[['show_stats']]) js_src <- c(js_src, 'stats.min.js')

	# Write javascript source files
	if(is.null(server)){

		js_src_str <- ''
		for(i in 1:length(js_src)){

			# Read js file
			read_js <- suppressWarnings(readLines(paste0(js.var[['app_dir']], '/js/', js_src[i])))

			# Copy lines into html
			js_src_str <- paste0(js_src_str, paste0('\t\t<script>\n\t\t\t', paste0(read_js, collapse='\n\t\t\t'), '\n\t\t</script>\n'))
		}

	}else{
		js_src_str <- paste0('\t\t<script src="', server$full_url('extdata'), '/js/', paste0(js_src, collapse=paste0('"></script>\n\t\t<script src="', server$full_url('extdata'), '/js/')), '"></script>\n')
	}

	# Start page html
	page_html <- ''

	# Write some header contents
	page_html <- paste0(page_html, '<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<title>svgViewR</title>\n\t\t<meta charset="utf-8">\n\t\t<meta name="viewport" content="width=device-width, user-scalable=no, minimum-scale=1.0, maximum-scale=1.0">\n\n')

	# Write stylesheet link
	if(is.null(server)){

		# Read css file
		read_css <- suppressWarnings(readLines(paste0(js.var[['app_dir']], '/css/', 'server_stylesheet.css')))

		# Create style string
		style_str <- paste0('\t\t<style>\n\t\t\t', paste0(read_css, collapse='\n\t\t\t'), '\n\t\t</style>\n\n')

	}else{
		style_str <- paste0('\t\t<link rel="stylesheet" type="text/css" href="', server$full_url('extdata'), '/css/server_stylesheet.css" >\n\n')
	}

	page_html <- paste0(page_html, '\t\t<script>\n')

	# Write app directories
	if(!is.null(server)){
		page_html <- paste0(page_html, '\t\t\tvar app_dir = [\n')
		if(!is.null(srcs)){
			for(i in 1:length(srcs)) page_html <- paste0(page_html, '\t\t\t\t"', server$full_url(paste0('app_dir', i)), '",\n')
		}
		page_html <- paste0(page_html, '\t\t\t]\n')
	}

	page_html <- paste0(page_html, '\t\t\tvar svg_obj = JSON.parse(\'', json, '\');\n')
	for(i in 1:length(js.var)){
		if(grepl('^0x', js.var[[i]]) || is.numeric(js.var[[i]])){
			page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = ', js.var[[i]], ';\n')
		}else if(js.var[[i]] == 'FALSE' || js.var[[i]] == 'TRUE'){
			page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = ', tolower(js.var[[i]]), ';\n')
		}else{
			page_html <- paste0(page_html, '\t\t\tvar ', names(js.var)[i], ' = \"', js.var[[i]], '\";\n')
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
	<body>
		<div id="container" style="background-color:green;" ></div>')

	if(js.var[['show_clock']]){
		page_html <- paste0(page_html, '
		<div class="clock" >\n\t\t\tClock: <span id="clock" ></span>\n\t\t</div>
		<div class="idx" >\n\t\t\tIndex: <span id="idx" ></span>\n\t\t</div>
		<div class="time" >\n\t\t\tTime: <span id="time" ></span>\n\t\t</div>')
				}

	page_html <- paste0(page_html, '<div class="alert" ><span id="alert" ></span></div>')

	page_html <- paste0(page_html, '
	</body>\n</html>')

	page_html
}