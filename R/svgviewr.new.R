svgviewr.new <- function(file, window.title="SVG Viewer", animate.duration = 1, 
	animate.reverse = FALSE, animate.repeat = -1, margin = 20, col = "white", 
	show.control = TRUE, start.rotate = TRUE, layers = NULL, debug = FALSE, 
	fdir = NULL, conn.type = 'new', app.dir.src = NULL){

	# IF CON IS NEW MAKE SURE FILE IS NOT NULL
	#if(is.null(file) && conn.type == 'new') stop("Input file is NULL.")

	# CHECK THAT FILE IS OF TYPE HTML
	if(conn.type == 'new' && !grepl('.html$', file, ignore.case=TRUE)) stop("File must have extension '.html'.")

	# Set file directory for extra files to copy into viewer document
	#  /Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/svgViewR/inst/extdata/

	# Set path to package
	if(is.null(app.dir.src)){
		fdir <- NULL
		if(is.null(fdir)){
			fdir <- tryCatch({
				fdir <- paste0(path.package("svgViewR"), "/extdata/")
			}, warning = function(w) {
			}, error = function(e) {
				if(e[1]$message == 'none of the packages are loaded'){
					fdir_dev <- '/Users/aaron/Documents/Research/github/svgViewR/inst/extdata/'
					if(file.exists(fdir_dev)){
						return(fdir_dev)
					}else{
						stop(e)
					}
				}
			}, finally = {
			})
		}

	}else{
		fdir <- app.dir.src
		fdir_dev <- app.dir.src
	}
	n <- rep(NA, 0)

	if(conn.type %in% c('new', 'open')){

		n <- c(n, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
		n <- c(n, paste("<title>", window.title,"</title>", sep=""))
		n <- c(n, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" >\n")
	
		# Parameters set by user
		n <- c(n, "<script type=\"text/javascript\" >")
		n <- c(n, "\t// Parameters set by user")
		n <- c(n, paste0("\tvar animation_reverse = ", ifelse(animate.reverse, 1, 0), ";"))
		n <- c(n, paste0("\tvar animation_duration = ", animate.duration, ";"))
		n <- c(n, paste0("\tvar animation_repeat = ", animate.repeat, ";"))
		n <- c(n, paste0("\tvar animation_count = ", 0, ";"))
		n <- c(n, paste0("\tvar margin = ", margin, ";"))
		n <- c(n, paste0("\tvar background_color = '", col, "';"))
		if(is.null(fdir)){
			n <- c(n, paste0("\tvar svgviewr_version = '", packageVersion('svgViewR'), "';"))
		}else{
			n <- c(n, paste0("\tvar svgviewr_version = 'NA';"))
		}
		n <- c(n, paste0("\tvar start_rotate = ", ifelse(start.rotate, 1, 0), ";"))
		n <- c(n, paste0("\tvar show_control_panel = ", ifelse(show.control, 1, 0), ";"))
		n <- c(n, "</script>\n")

		n <- c(n, "<svg_doc xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" style=\"visibility:hidden;\" >")

		if(conn.type == 'open'){

			# Create file before opening connection - also clears existing file, if present
			file.create(file)

			# Open file connection (does not create the file)
			con <- file(file, "r+")
	
			# Write lines to connection
			write(n, con)
	
			# Return connection
			rlist <- list('con'=con)
			return(rlist)
		}
	}

	if(conn.type %in% c('new', 'close')){

		n <- c(n, "</svg_doc>\n")

		n <- c(n, "<body style=\"margin:0px;background-color:;overflow:hidden;\" >")
		n <- c(n, "\t<a id='keydown' type='checkbox' onkeydown=\"javascript:;\" ></a>")

		n <- c(n, paste("\t", paste(readLines(paste0(fdir, 'html/control_panel1.html')), collapse="\n\t"), "\n", sep=""))
		if(!is.null(layers)) n <- c(n, layers)
		n <- c(n, paste("\t", paste(readLines(paste0(fdir, 'html/control_panel2.html')), collapse="\n\t"), "\n", sep=""))
		
		n <- c(n, "\t<svg id=\"world\" style=\"background-color:;width:100%;height:100%;position:absolute;top:0;left:0;z-index:-1;\" onload=\"Javascript:onLoadFunctions();\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"></svg>")
		n <- c(n, "</body>\n")

		# Copy in css files
		if(debug){
			for(css_file in c('stylesheet.css'))
				n <- c(n, paste0("<LINK rel=\"stylesheet\" type=\"text/css\" href=\"", paste0(fdir, 'css/', css_file), "\" >"))
		}else{
			for(css_file in c('stylesheet.css'))
				n <- c(n, paste0("<style>\n\t", paste(readLines(paste0(fdir, 'css/', css_file)), collapse="\n\t"), "\n</style>\n"))
		}

		n <- c(n, "\n<script type=\"text/javascript\" >")
		n <- c(n, "\tvar svgDocument = document.getElementById(\"world\");")
		n <- c(n, "</script>\n")

		# Copy in javascript files
		if(debug){
			n <- c(n, paste0("<script type=\"text/javascript\" src=\"", paste0(fdir, 'js/math.js'), "\" ></script>"))
			n <- c(n, paste0("<script type=\"text/javascript\" src=\"", paste0(fdir, 'js/ui_functions.js'), "\" ></script>"))
			n <- c(n, paste0("<script type=\"text/javascript\" src=\"", paste0(fdir, 'js/shape_operations.js'), "\" ></script>"))
			n <- c(n, paste0("<script type=\"text/javascript\" src=\"", paste0(fdir, 'js/shapes.js'), "\" ></script>"))
			n <- c(n, paste0("<script type=\"text/javascript\" src=\"", paste0(fdir, 'js/control_panel.js'), "\" ></script>"))
		}else{
			n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/math.js')), collapse="\n\t"), "\n</script>", sep=""))
			n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/ui_functions.js')), collapse="\n\t"), "\n</script>", sep=""))
			n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/shape_operations.js')), collapse="\n\t"), "\n</script>", sep=""))
			n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/shapes.js')), collapse="\n\t"), "\n</script>", sep=""))
			n <- c(n, paste("<script type=\"text/javascript\" >\n\t", paste(readLines(paste0(fdir, 'js/control_panel.js')), collapse="\n\t"), "\n</script>", sep=""))
		}

		if(conn.type == 'close'){

			# Write closing lines to file
			write(n, file$con)
	
			# Close the connection
			close(file$con)

			# Return NULL
			return(NULL)
		}
	}

	# WRITE TO FILE
	if(!is.null(file)) write(n, file)
	
	file
}