svg.close <- function(){

	# Get connection type
	conn_type <- getOption("svgviewr_glo_type")

	if(conn_type == 'webgl'){

		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))
		
		# Set svg objects with source directories
		src_objs <- c('mesh')
		
		# Get unique source directories
		srcs <- c()
		for(src_obj in src_objs){
			for(i in 1:length(env$svgviewr_env[[src_obj]])){
				if(!is.null(env$svgviewr_env[[src_obj]][[i]]$src) && env$svgviewr_env[[src_obj]][[i]]$src != '') srcs <- c(srcs, env$svgviewr_env[[src_obj]][[i]]$src)
			}
		}

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

		# Get transformations from environment
		tm <- env$svgviewr_env$tm
		
		# Convert to json string
		tm_json <- tm2JSON(tm, time.units=gsub('\"', '', env$svgviewr_env$js_var[['time_units']]))
		
		# Get js variables
		js_var <- env$svgviewr_env$js_var

		# Set elements to NULL that aren't written to json
		env$svgviewr_env$tm <- NULL
		env$svgviewr_env$js_var <- NULL

		# Convert svg objects to json
		svg_json <- rjson::toJSON(x = as.list(env$svgviewr_env))

		# Start server
		R.server$start(quiet=TRUE)

		# Create app to handle requests and responses
		Rook.app <- function(env) {

			request <- Request$new(env)
			response <- Response$new()

			write.HTML(response, R.server)

			response$finish()
		}

		# Write html for page
		write.HTML <- function(response, server){
		
			# Set javascript source files
			js_src <- c('three.min.js', 'OrbitControls.js', 'Detector.js', 'jquery-1.6.4.js', 'jquery.mousewheel.js', 'stats.min.js', 'animation.js', 'render.scene.js')
		
			# Write javascript source files
			js_src_str <- paste0('\t\t<script src="', server$full_url('extdata'), '/js/', paste0(js_src, collapse=paste0('"></script>\n\t\t<script src="', server$full_url('extdata'), '/js/')), '"></script>')

			# Start page html
			page_html <- ''

			# Write some header contents
			page_html <- paste0(page_html, '<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<title>svgViewR</title>\n\t\t<meta charset="utf-8">\n\t\t<meta name="viewport" content="width=device-width, user-scalable=no, minimum-scale=1.0, maximum-scale=1.0">\n\n')

			# Write stylesheet link
			page_html <- paste0(page_html, '\t\t<link rel="stylesheet" type="text/css" href="', server$full_url('extdata'), '/css/server_stylesheet.css" >\n\n')

			page_html <- paste0(page_html, '\t\t<script>\n')
			
			# Write app directories
			page_html <- paste0(page_html, '\t\t\tvar app_dir = [\n')
			for(i in 1:length(unique_srcs)) page_html <- paste0(page_html, '\t\t\t\t"', server$full_url(paste0('app_dir', i)), '",\n')
			page_html <- paste0(page_html, '\t\t\t]\n')
			page_html <- paste0(page_html, '\t\t\tvar svg_obj = JSON.parse(\'', svg_json, '\');\n')
			page_html <- paste0(page_html, '\t\t\tvar tm_str = \'', tm_json, '\';\n')
			for(i in 1:length(js_var)){
				page_html <- paste0(page_html, '\t\t\tvar ', names(js_var)[i], ' = ', js_var[[i]], ';\n')
			}
			
			page_html <- paste0(page_html, '\t\t</script>\n\n')

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
		<div id="container" style="background-color:green;" ></div>

		<div class="clock" >\n\t\t\tClock: <span id="clock" ></span>\n\t\t</div>
		<div class="idx" >\n\t\t\tIndex: <span id="idx" ></span>\n\t\t</div>
		<div class="time" >\n\t\t\tTime: <span id="time" ></span>\n\t\t</div>
	</body>\n</html>')

			response$write(page_html)
		}

		# Add your Rook app to the Rhttp object
		R.server$add(app = Rook.app, name = "svgViewR")

		# view your web app in a browser
		R.server$browse("svgViewR")
		
	}else{

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	
		# Close
		svgviewr.new(file=file, conn.type='close', layers=file$layers, fdir=file$fdir, debug=file$debug)
	}

	ret = NULL
}