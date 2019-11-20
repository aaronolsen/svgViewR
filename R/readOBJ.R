readOBJ <- function(file, scaling = 1){

	# Check if file exists
	if(!file.exists(file)) stop(paste0("File \"", file, "\" not found."))

	# Read lines
	read_lines <- suppressWarnings(readLines(con=file))
	
	# Paste into string
	obj_str <- paste0(paste0(read_lines, collapse="*"), '*')

	# Use C++ function to read string
	read_obj <- read_obj_str(obj_str)
	
	# Convert to matrix
	vertices <- matrix(read_obj$vertices, nrow=length(read_obj$vertices)/3, ncol=3, byrow=TRUE)*scaling
	
	# Get vertex normals
	normals <- read_lines[grepl('^vn ', read_lines)]

	# Convert to matrix
	normals <- matrix(read_obj$normals, nrow=length(read_obj$normals)/3, ncol=3, byrow=TRUE)
	
	# Convert to matrix
	faces <- matrix(read_obj$faces, nrow=length(read_obj$faces)/6, ncol=6, byrow=TRUE)

	# Get lines
	l_lines <- read_lines[grepl('^l ', read_lines)]
	
	# Convert to matrix
	if(!is.null(read_obj$lines)){
		lines_mat <- matrix(read_obj$lines, nrow=length(read_obj$lines)/2, ncol=2, byrow=TRUE)
	}else{
		lines_mat <- NULL
	}

	obj <- list(
		'vertices'=vertices,
		'normals'=normals,
		'faces'=faces,
		'lines'=lines_mat,
		'uvs'=c(),
		'metadata'=list(
			'version'=1,
			'normals'=nrow(normals),
			'vertices'=nrow(vertices),
			'generator'='svgViewR',
			'uvs'=0,
			'faces'=nrow(faces),
			'type'='Geometry'
		)
	)
	class(obj) <- 'obj'

	obj
}

print.obj <- function(x, ...){
	
	rc <- ''

	rc <- c(rc, paste0('$vertices (', paste0(dim(x$vertices), collapse='x'), ')', '\n'))
	rc <- c(rc, paste0('\t', paste0(capture.output(print(head(x$vertices, n=2))), collapse='\n\t'), '\n'))
	if(nrow(x$vertices) > 2){
		rc <- c(rc, '\t...\n')
		rc <- c(rc, paste0('\t', paste0(capture.output(print(tail(x$vertices, n=min(2,nrow(x$vertices)-2)))), collapse='\n\t'), '\n'))
	}
	rc <- c(rc, paste0('$normals (', paste0(dim(x$normals), collapse='x'), ')', '\n'))
	rc <- c(rc, paste0('\t', paste0(capture.output(print(head(x$normals, n=2))), collapse='\n\t'), '\n'))
	if(nrow(x$normals) > 2){
		rc <- c(rc, '\t...\n')
		rc <- c(rc, paste0('\t', paste0(capture.output(print(tail(x$normals, n=min(2,nrow(x$normals)-2)))), collapse='\n\t'), '\n'))
	}
	rc <- c(rc, paste0('$faces (', paste0(dim(x$faces), collapse='x'), ', range:', min(x$faces), '-', max(x$faces), ')', '\n'))
	rc <- c(rc, paste0('\t', paste0(capture.output(print(head(x$faces, n=2))), collapse='\n\t'), '\n'))
	if(nrow(x$faces) > 2){
		rc <- c(rc, '\t...\n')
		rc <- c(rc, paste0('\t', paste0(capture.output(print(tail(x$faces, n=min(2,nrow(x$faces)-2)))), collapse='\n\t'), '\n'))
	}

	cat(rc, sep='')
}