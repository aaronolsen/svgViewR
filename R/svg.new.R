svgviewr_env <- new.env(parent = emptyenv())

svg.new <- function(file = NULL, window.title="svgViewR", animate.duration = 1, 
	animate.speed = 1, interpolate = TRUE, timeline = TRUE, 
	mode = c('svg', 'webgl'), animate.reverse = FALSE, animate.repeat = -1, 
	margin = 20, col = "white", times = NULL, clock = FALSE, stats = FALSE, panel = FALSE, 
	show.control = TRUE, start.rotate = TRUE, rotate.speed = 1.2, camera.near = 0.01, fov = 45, 
	zoom.speed = 1, pan.speed = 0.2, layers = NULL, connection = TRUE, 
	close.on.done = TRUE, file.type = NULL, app.dir.src = NULL, debug = FALSE, 
	src.link = NULL){
	
	# src.link=TRUE'Users/aaron/Documents/GitHub/svgViewR/inst/extdata' ???

	digits <- 6
	
	# Old svg mode - default if mode[1] is svg
	if(mode[1] == 'svg') options("svgviewr_glo_type"='svg')
	
	# Whether to save plot as images
	save_as_img <- FALSE
	save_as_img_dir <- NULL
	save_as_img_paths <- NULL

	## Set connection type
	# If file is non NULL
	if(!is.null(file)){

		#
		if(mode[1] == 'webgl') options("svgviewr_glo_type"='html')

		# Is file directory?
		is_file_dir <- FALSE
		if(file[1] == ""){

			#
			is_file_dir <- TRUE

			# Set current working directory as image directory
			save_as_img_dir <- file <- getwd()
			
			# Make sure that file.type is specified
			if(is.null(file.type)) stop('To save plot as an image or series of images please specify a valid image type (e.g. jpeg, jpg, tiff, png) as "file.type".')

		}else{
		
			# Check if file
			if(!grepl('[.](jpeg|jpg|tiff|png|html)$', file[1], ignore.case=TRUE)) is_file_dir <- TRUE

			# If directory, make sure that directory exists
			if(is_file_dir && !file.exists(file[1])) stop(paste0('Input file directory "', file[1], '" not found.'))
		}

		# If file
		if(!is_file_dir){
			
			# If image file
			if(grepl('[.](jpeg|jpg|tiff|png)$', file[1], ignore.case=TRUE)){

				# Set to save plot as image(s)
				save_as_img <- TRUE

				# Set type
				file.type <- tolower(tail(strsplit(file[1], '[.]')[[1]], 1))
			
				# Set directory
				save_as_img_dir <- normalizePath(path=dirname(file[1]))

				# Set image names
				save_as_img_fnames <- basename(file)
				save_as_img_paths <- paste0(save_as_img_dir, '/', save_as_img_fnames)
			}

		}else{
			
			# Directory
			save_as_img_dir <- normalizePath(path=file[1])
		}
		
		# Is directory - will be generating images
		if(is_file_dir) save_as_img <- TRUE

		# If saving as image(s) will use live viewer
		if(save_as_img) options("svgviewr_glo_type"='live')

	}else{

		# If file is NULL, set viewer to live server
		options("svgviewr_glo_type"='live')
	}

	# If viewer type is html or live
	if(options("svgviewr_glo_type") %in% c('html', 'live')){
	
		# Check whether package is loaded from source or library
		if(!is.null(app.dir.src)){

			app_dir <- app.dir.src
			app_dir_src <- app_dir
			pkg_load <- 'source'

		}else{

			app_dir <- tryCatch({
				app_dir <- paste0(path.package("svgViewR"), "/extdata")
			}, warning = function(w) {
			}, error = function(e) {
				if(e[1]$message == 'none of the packages are loaded'){

					if(Sys.info()['login'] == 'xromm18'){
						app_dir_src <- '/Users/xromm18/Documents/Analysis/R/svgViewR/inst/extdata'
					}else{
						app_dir_src <- '/Users/aaron/Documents/GitHub/svgViewR/inst/extdata'
					}
					if(file.exists(app_dir_src)){
						return(app_dir_src)
					}else{
						stop(e)
					}
				}
			}, finally = {
			})

			# Set package load source
			if(app_dir %in% c('/Users/aaron/Documents/GitHub/svgViewR/inst/extdata', '/Users/xromm18/Documents/Analysis/R/svgViewR/inst/extdata')){
				pkg_load <- 'source'
			}else{
				pkg_load <- 'library'
			}
		}		

		# Set app source file directory and viewer environment
		if(pkg_load == 'source'){
			options("svgviewr_glo_env"='.GlobalEnv')
		}else{
			options("svgviewr_glo_env"='package:svgViewR')
		}
	
		# Get viewer environment
		env <- as.environment(getOption("svgviewr_glo_env"))

		# Get objects in parent environment
		parent_env_ls <- ls(envir=env)

		# Remove any objects previously added to the svgviewr environment
		if('svgviewr_env' %in% parent_env_ls) rm(list = ls(envir = svgviewr_env), envir = svgviewr_env)

		#
		svgviewr_env$js_var <- list()
	
		if(debug){
			svgviewr_env$js_var[['show_clock']] <- TRUE
			svgviewr_env$js_var[['show_stats']] <- TRUE
		}else{
			svgviewr_env$js_var[['show_clock']] <- clock
			svgviewr_env$js_var[['show_stats']] <- stats
		}
		
		if(timeline){
			svgviewr_env$js_var[['bottom_frame_hidden']] <- FALSE
		}else{
			svgviewr_env$js_var[['bottom_frame_hidden']] <- TRUE
		}
		
		# Set javascript variables
		svgviewr_env$js_var[['anim_pause']] <- FALSE	# Start with animation playing
		svgviewr_env$js_var[['bg_col']] <- setNames(webColor(col, format='0'), NULL)
		svgviewr_env$js_var[['debug']] <- debug
		if(debug){
			svgviewr_env$js_var[['src_link']] <- TRUE
		}else{
			svgviewr_env$js_var[['src_link']] <- FALSE
		}
		if(!is.null(src.link)) svgviewr_env$js_var[['src_link']] <- src.link
		svgviewr_env$js_var[['file']] <- file[1]
		svgviewr_env$js_var[['interpolate']] <- interpolate
		svgviewr_env$js_var[['panSpeed']] <- pan.speed
		svgviewr_env$js_var[['play_speed']] <- animate.speed
		svgviewr_env$js_var[['rotateSpeed']] <- rotate.speed
		svgviewr_env$js_var[['save_as_img']] <- save_as_img
		svgviewr_env$js_var[['save_as_img_dir']] <- save_as_img_dir
		svgviewr_env$js_var[['save_as_img_type']] <- file.type
		svgviewr_env$js_var[['save_as_img_paths']] <- save_as_img_paths
		svgviewr_env$js_var[['camera_near']] <- camera.near
		svgviewr_env$js_var[['camera_fov']] <- fov
		svgviewr_env$js_var[['save_as_img_close']] <- close.on.done
		svgviewr_env$js_var[['signif_digits']] <- digits
		svgviewr_env$js_var[['time_units']] <- 'sec'
		svgviewr_env$js_var[['window_title']] <- window.title
		svgviewr_env$js_var[['zoomSpeed']] <- zoom.speed
		
		# Add panel if TRUE
		if(panel){
			svg.panel()
		}else{
			svgviewr_env$js_var[['panel']] <- FALSE
		}

		# Create name reference
		svgviewr_env$ref <- list()

		# Create svg list
		svgviewr_env$svg <- list()

		# Create animation reference
		svgviewr_env$svg$animate <- list()

		# Create deformation reference
		svgviewr_env$svg$deform <- list()

		## Create server connection to plot WebGL graphics
		if(options("svgviewr_glo_type") == 'live'){

			# Try stopping server, if running
			tryCatch({ svgviewr_env$R.server$stop() }, error = function(e) {}, warning = function(e) {})

			# Remove R.server, if exists
			tryCatch({ remove(svgviewr_env$R.server) }, error = function(e) {}, warning = function(e) {})

			# Try adding server to package environment instead of declaring as a global variable:
			# 	https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
			# pkg.env$cur.val <- 0
			# pkg.env$times.changed <- 0

			# Create new server
			#R.server <<- Rhttpd2$new()
			svgviewr_env$R.server <- Rhttpd2$new()

			# Add directories that will be accessible to server
			svgviewr_env$R.server$add(app = File$new(app_dir), name = "extdata")
			#File$new(paste0(dirname(getwd()), '/json'))
			#svgviewr_env$R.server$add(app = File$new('/Users/aaron/Documents/Research/R Package Tests/svgViewR/WebGL and three js/json'), name = "json_dir")

		}else{

			svgviewr_env$js_var[['app_dir']] <- app_dir
		}

		if(!is.null(times)){

			# If times do not start at 0, shift to start at 0
			if(times[1] != 0) times <- times - min(times, na.rm=TRUE)

			# Set times
			svgviewr_env$svg[['animate']][['times']] <- times

			# Set number of timelines
			#svgviewr_env$js_var[['n_timelines']] <- 1
		}

	}else{

		## Create a file
		# Set connection type
		conn.type <- 'open'
		if(!connection) conn.type <- 'new'

		# Get basic viewer document lines
		con <- svgviewr.new(file=file, window.title=window.title, 
			animate.duration=animate.duration, animate.reverse=animate.reverse, 
			animate.repeat=animate.repeat, margin=margin, col=col, show.control=show.control, 
			start.rotate=start.rotate, layers=layers, debug=debug, conn.type=conn.type, app.dir.src=app.dir.src)

		if(connection){
			if(!is.null(layers)) con$layers <- layers
			#if(!is.null(fdir)) con$fdir <- fdir
			if(!is.null(debug)) con$debug <- debug
			if(!is.null(app.dir.src)) con$app.dir.src <- app.dir.src
		}

		# Save connection through options to enable global access
		options("svg_glo_con"=con)
	}

	ret = NULL
}