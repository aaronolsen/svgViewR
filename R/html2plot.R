html2plot <- function(svg.file, plot.file, as = c('jpeg'), height = 'default', width = 'default', 
	clear.plot.dir = FALSE, max.iter = NULL, margin = NULL, first.frame.only = FALSE){

	## Converts an svg/html file to a plot/image or set of plots/images
	#require(grid)
	
	# Make sure as is lowercase
	as <- tolower(as)
	
	# Convert jpg to jpeg
	if(as[1] == 'jpg') as[1] <- 'jpeg'

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

	# Set width and height depending on 'as'
	if(height == 'default' && width == 'default'){
		if(as[1] %in% c('png', 'jpeg')) height <- width <- 480
		#if(as[1] %in% c('pdf')) height <- 7
	}

	# Read svg/html file
	read_html <- readHTML(svg.file)

	# Apply any transformations (translations, rotations) -- slow for large number of iterations
	applyt <- applyTransformations(read_html)

	# Find plot dimensions for all shapes
	plot_dims <- findPlotDims(applyt, width=width, height=height, margin=margin)

	# Fit shapes to window
	fit_shapes <- fitShapes(applyt, plot.dims=plot_dims, margin=margin)

	# Get default graphics parameters
	dgp <- default_gpar()

	# Set the number of iterations
	if(is.null(max.iter)){n_iter <- applyt$n.iter}else{n_iter <- min(applyt$n.iter, max.iter)}
	
	# Set number of preceding zeros in plot file names
	num_zeros <- max(ceiling(log(n_iter+1, base=10)) - 1, 1)
	
	# If plot.file is directory, check if directory should be cleared
	if(clear.plot.dir && is_dir){
		list_files <- list.files(plot.file)
		if(length(list_files) > 0) file.remove(paste0(plot.file, '/', list_files))
	}
	
	#xy_all <- matrix(NA, nrow=0, ncol=2)
	
	# For each iteration
	for(iter in 1:n_iter){

		# Open file connection
		#if(as[1] == 'pdf'){
		#	do.call(as[1], args=list('file'=paste0(gsub('/$', '', plot.file), '/', rep(0, num_zeros), iter, '.', as[1]), 'width'=width, 'height'=height))
		#}

		do.call(as[1], args=list('filename'=paste0(gsub('/$', '', plot.file), '/', paste(rep(0, num_zeros-nchar(iter)+1), collapse=''), iter, '.', as[1]), 
			'width'=plot_dims$width, 'height'=plot_dims$height))

		# Create new page
		grid.newpage()

		# Draw shapes
		plot_svg_shapes(fit_shapes, dgp, as=as[1], iter=iter)
		#xy_all <- rbind(xy_all, xy)

		# Close file connection
		dev.off()

		if(first.frame.only) break
	}

	# 
	#ranges_xy <- apply(xy_all, 2, 'range', na.rm=TRUE)
	#ranges_xy_diff <- abs(ranges_xy[2, ] - ranges_xy[1, ])
	#hw <- ranges_xy_diff[2] / ranges_xy_diff[1]

	#print(dim(xy_all))
	#print(ranges_xy)
	#print(hw)
}