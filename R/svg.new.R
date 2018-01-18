svgviewr_env <- new.env(parent = emptyenv())

svg.new <- function(file = NULL, window.title="svgViewR", animate.duration = 1, 
	animate.speed = 1, animate.reverse = FALSE, animate.repeat = -1, margin = 20, col = "white", 
	time.units = 'sec', clock = FALSE, stats = FALSE, show.control = TRUE, start.rotate = TRUE, 
	rotate.speed = 1.2, zoom.speed = 1, pan.speed = 0.2, layers = NULL, connection = TRUE, 
	mode = c('svg', 'webgl'), debug = FALSE){

	digits <- 6

	# Set connection type
	if(is.null(file)) mode[1] <- 'webgl'
	if(mode[1] == 'webgl' && is.null(file)) options("svgviewr_glo_type"='live')
	if(mode[1] == 'webgl' && !is.null(file)) options("svgviewr_glo_type"='html')
	if(mode[1] == 'svg') options("svgviewr_glo_type"='svg')

	if(mode[1] == 'webgl'){
	
		# Check whether package is loaded from source or library
		app_dir <- tryCatch({
			app_dir <- paste0(path.package("svgViewR"), "/extdata")
		}, warning = function(w) {
		}, error = function(e) {
			if(e[1]$message == 'none of the packages are loaded'){
				app_dir_src <- '/Users/aaron/Documents/Research/github/svgViewR/inst/extdata'
				if(file.exists(app_dir_src)){
					return(app_dir_src)
				}else{
					stop(e)
				}
			}
		}, finally = {
		})
		
		# Set package load source
		if(app_dir == '/Users/aaron/Documents/Research/github/svgViewR/inst/extdata'){
			pkg_load <- 'source'
		}else{
			pkg_load <- 'library'
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
	
		# Set javascript variables
		svgviewr_env$js_var[['bg_col']] <- webColor(col, format='0')
		svgviewr_env$js_var[['play_speed']] <- animate.speed
		svgviewr_env$js_var[['time_units']] <- time.units
		svgviewr_env$js_var[['signif_digits']] <- digits
		svgviewr_env$js_var[['show_clock']] <- clock
		svgviewr_env$js_var[['show_stats']] <- stats
		svgviewr_env$js_var[['rotateSpeed']] <- rotate.speed
		svgviewr_env$js_var[['zoomSpeed']] <- zoom.speed
		svgviewr_env$js_var[['panSpeed']] <- pan.speed
		svgviewr_env$js_var[['file']] <- file
		
		# Create name reference
		svgviewr_env$ref <- list()

		# Create svg list
		svgviewr_env$svg <- list()

		# Create animation reference
		svgviewr_env$svg$animate <- list()

		## Create server connection to plot WebGL graphics
		if(is.null(file)){

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

	}else{

		## Create a file
		# Set connection type
		conn.type <- 'open'
		if(!connection) conn.type <- 'new'

		# Get basic viewer document lines
		con <- svgviewr.new(file=file, window.title=window.title, 
			animate.duration=animate.duration, animate.reverse=animate.reverse, 
			animate.repeat=animate.repeat, margin=margin, col=col, show.control=show.control, 
			start.rotate=start.rotate, layers=layers, debug=debug, conn.type=conn.type)

		if(connection){
			if(!is.null(layers)) con$layers <- layers
			#if(!is.null(fdir)) con$fdir <- fdir
			if(!is.null(debug)) con$debug <- debug
		}

		# Save connection through options to enable global access
		options("svg_glo_con"=con)
	}

	ret = NULL
}