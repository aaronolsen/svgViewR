readOBJ <- function(file, scaling = 1){

	# Shapes to remove
	shape.rm <- c('l')

	# Read lines
	read_lines <- suppressWarnings(readLines(file))

	# Get vertex lines
	vertices <- read_lines[grepl('^v ', read_lines)]
	
	# Convert to matrix
	vertices <- matrix(as.numeric(unlist(lapply(strsplit(x=vertices, split=' '), tail, 3))), nrow=length(vertices), ncol=3, byrow=TRUE)
	
	# Apply scaling
	vertices <- vertices * scaling

	# Get vertex normals
	normals <- read_lines[grepl('^vn ', read_lines)]

	# Convert to matrix
	normals <- matrix(as.numeric(unlist(lapply(strsplit(x=normals, split=' '), tail, 3))), nrow=length(normals), ncol=3, byrow=TRUE)
	
	# Get face lines
	faces <- read_lines[grepl('^f ', read_lines)]

	# Convert to matrix
	faces <- matrix(as.numeric(unlist(lapply(strsplit(x=faces, split=' |//'), tail, 6))), nrow=length(faces), ncol=6, byrow=TRUE)

	# Take every other - not sure why these are repeated
	faces <- faces[, c(1,3,5)]

	# Get vertex normals
	l_lines <- read_lines[grepl('^l ', read_lines)]
	
	# Convert to matrix
	if(length(l_lines) > 0){

		l_lines <- matrix(as.numeric(unlist(lapply(strsplit(x=l_lines, split=' |//'), tail, 2))), nrow=length(l_lines), ncol=2, byrow=TRUE)

		# 
		if('l' %in% shape.rm){
		
			#
			max_l_lines <- max(l_lines)
			vertices <- vertices[(max_l_lines+1):nrow(vertices), ]
			faces <- faces - max_l_lines
		}
	}

	obj <- list(
		'vertices'=vertices,
		'normals'=normals,
		'faces'=faces,
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
	rc <- c(rc, paste0('\t', paste0(capture.output(print(head(x$vertices))), collapse='\n\t'), '\n'))
	if(nrow(x$vertices) > 6) rc <- c(rc, '\t...\n')
	rc <- c(rc, paste0('$normals (', paste0(dim(x$normals), collapse='x'), ')', '\n'))
	rc <- c(rc, paste0('\t', paste0(capture.output(print(head(x$normals))), collapse='\n\t'), '\n'))
	if(nrow(x$normals) > 6) rc <- c(rc, '\t...\n')
	rc <- c(rc, paste0('$faces (', paste0(dim(x$faces), collapse='x'), ')', '\n'))
	rc <- c(rc, paste0('\t', paste0(capture.output(print(head(x$faces))), collapse='\n\t'), '\n'))
	if(nrow(x$faces) > 6) rc <- c(rc, '\t...\n')

	cat(rc, sep='')
}