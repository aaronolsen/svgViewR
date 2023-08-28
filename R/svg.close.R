svg.close <- function(wait = FALSE, quiet = TRUE){

	# Get connection type
	conn_type <- getOption("svgviewr_glo_type")

	# Give error if svg.new has not been called
	if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

	# Get current connection
	file <- getOption("svg_glo_con")

	# Close
	svgviewr.new(file=file, conn.type='close', layers=file$layers, debug=file$debug, app.dir.src=file$app.dir.src)

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