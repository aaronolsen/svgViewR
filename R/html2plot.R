html2plot <- function(svg.file, plot.file, as = c('jpeg'), height = 480, width = 480){
	## Converts an svg/html file to a plot/image or set of plots/images
	
	# Get plot dimensions

	# Read svg/html file
	read_html <- readHTML(svg.file)
	
	# Fit shapes to window
	fit_shapes <- fitShapes(read_html, width=width, height=height)
#	fit_shapes <- read_html
	
	# Get default graphics parameters
	dgp <- default_gpar()

	#
	n_iter <- 3
	
	# Set number of preceding zeros in plot file names
	num_zeros <- ceiling(log(n_iter+1, base=10))
	
	# For each iteration
	for(iter in 1:n_iter){

		# Open file connection
		do.call(as[1], args=list('filename'=paste0(gsub('/$', '', plot.file), '/', rep(0, num_zeros), iter, '.', as[1]), 'width'=width, 'height'=height))

		# Create new page
		grid.newpage()

		# Draw shapes
		plot_svg_shapes(fit_shapes, dgp, iter=iter)

		# Close file connection
		dev.off()
	}
}