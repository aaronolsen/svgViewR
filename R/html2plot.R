html2plot <- function(svg.file, plot.file, as = c('jpeg'), height = 'default', width = 'default', 
	clear.plot.dir = FALSE, max.iter = NULL){
	## Converts an svg/html file to a plot/image or set of plots/images
	
	require(grid)
	
	# Make sure as is lowercase
	as <- tolower(as)
	
	# Convert jpg to jpeg
	if(as[1] == 'jpg') as[1] <- 'jpeg'

	# Set width and height depending on 'as'
	if(height == 'default'){
		height <- 480
		#if(as[1] %in% c('png', 'jpeg')) height <- 480
		#if(as[1] %in% c('pdf')) height <- 7
	}
	if(width == 'default'){
		width <- 480
		#if(as[1] %in% c('png', 'jpeg')) width <- 480
		#if(as[1] %in% c('pdf')) width <- 7
	}
	
	# Allowable plot.file formats
	plot_file_formats <- c('jpeg', 'jpg', 'png')
	
	# Get last element in plot.file path
	lastelem <- tail(strsplit(plot.file, '/')[[1]], 1)
	
	# Check whether plot.file is file or directory
	is_dir <- FALSE
	if(!grepl(paste0('[.](', paste(plot_file_formats, collapse='|'), ')$'), lastelem, ignore.case=TRUE)) is_dir <- TRUE
	
	# Check that directory exists, if not, prompt to create
	if(is_dir){
		if(!dir.exists(plot.file)){
			response <- readline(prompt=paste0("Directory '", plot.file, "' not found. Do wish to create it? (y/n) : "))
			if(tolower(response) %in% c('yes', 'y')){
				dir.create(plot.file)
			}else{
				return(NULL)
			}
		}
		
		# Remove final / if present
		plot.file <- gsub('/$', '', plot.file)
	}

	# Read svg/html file
	read_html <- readHTML(svg.file)

	# Apply any transformations (translations, rotations)
	applyt <- applyTransformations(read_html)

	# Fit shapes to window
	fit_shapes <- fitShapes(applyt, width=width, height=height)
	
	# Get default graphics parameters
	dgp <- default_gpar()

	# Set the number of iterations
	if(is.null(max.iter)){
		n_iter <- applyt$n.iter
	}else{
		n_iter <- min(applyt$n.iter, max.iter)
	}
	
	# Set number of preceding zeros in plot file names
	num_zeros <- max(ceiling(log(n_iter+1, base=10)) - 1, 1)
	
	# If plot.file is directory, check if directory should be cleared
	if(clear.plot.dir && is_dir){
		list_files <- list.files(plot.file)
		if(length(list_files) > 0) file.remove(paste0(plot.file, '/', list_files))
	}
	
	# For each iteration
	for(iter in 1:n_iter){

		# Open file connection
		#if(as[1] == 'pdf'){
		#	do.call(as[1], args=list('file'=paste0(gsub('/$', '', plot.file), '/', rep(0, num_zeros), iter, '.', as[1]), 'width'=width, 'height'=height))
		#}

		do.call(as[1], args=list('filename'=paste0(gsub('/$', '', plot.file), '/', paste(rep(0, num_zeros-nchar(iter)+1), collapse=''), iter, '.', as[1]), 'width'=width, 'height'=height))

		# Create new page
		grid.newpage()

		# Draw shapes
		plot_svg_shapes(fit_shapes, dgp, as=as[1], iter=iter)

		# Close file connection
		dev.off()
	}
}