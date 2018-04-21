readOBJ <- function(file, scaling = 1, shape.rm = NULL){

	# Read lines
	read_lines <- suppressWarnings(readLines(file[1]))

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

	# Get lines
	l_lines <- read_lines[grepl('^l ', read_lines)]
	
	# Convert to matrix
	if(length(l_lines) > 0){

		lines_mat <- matrix(as.numeric(unlist(lapply(strsplit(x=l_lines, split=' |//'), tail, 2))), nrow=length(l_lines), ncol=2, byrow=TRUE)

		# 
		if(!is.null(shape.rm)){
			if('l' %in% shape.rm){
		
				#
				max_lines_mat <- max(lines_mat)
				vertices <- vertices[(max_lines_mat+1):nrow(vertices), ]
				faces <- faces - max_lines_mat
			}
		}
	}else{
		lines_mat <- NULL
	}

	#if(repair){

		# Only keep faces with all indices greater than 0
		#faces <- faces[rowSums(faces > 0) == ncol(faces), ]

		# Get range of face index values
		#faces_range <- range(faces)

		# Set desired number of vertices
		#num_vertices <- faces_range[2]
	
		# Get number of normals
		#num_normals <- nrow(normals)

		# Add normals to equal desired number
		#if(num_normals < num_vertices) normals <- rbind(normals, matrix(c(1,0,0), nrow=num_vertices-num_normals, ncol=3, byrow=TRUE))

		# Remove normals to equal desired number
		#if(num_normals < num_vertices) normals <- normals[1:num_vertices, ]
	#}

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