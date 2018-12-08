readHTML <- function(file){
	## Reads an svg/html file and returns svg elements as list

	# Set distinctive line break string (not likely to be used in text fields)
	br_str <- '@\n@'

	# Read in lines and collapse into string with distinct line breaks
	read_lines <- paste(readLines(file), collapse=br_str)
	
	# Get javascript parameters
	jsp_start <- regexpr(pattern=paste0('<script type="text/javascript" >', br_str, '[\t]*// Parameters set by user'), text=read_lines)
	jsp_stop <- regexpr(pattern=paste0(br_str, '[\t]*</script>', br_str), text=read_lines)
	
	# Extract string between start and stop
	jsp_vec <- strsplit(substr(x=read_lines, start=jsp_start, stop=jsp_stop+attr(jsp_stop,"match.length")), br_str)[[1]]
	jsp_vec <- gsub('^[\t]+var |;$', '', jsp_vec[3:(length(jsp_vec)-2)])

	# Get start and stop points of svg structure
	svg_start <- regexpr(pattern=paste0('<svg_doc xmlns="http://www.w3.org/2000/svg" version="[0-9.]+" style="visibility:hidden;" >', br_str), text=read_lines)
	svg_stop <- regexpr(pattern=paste0(br_str, '[\t]*</svg_doc>', br_str), text=read_lines)

	# Extract string between start and stop
	svg_vec <- strsplit(substr(x=read_lines, start=svg_start, stop=svg_stop+attr(svg_stop,"match.length")), br_str)[[1]]
	svg_vec <- svg_vec[2:(length(svg_vec)-2)]

	# Properties to split at comma
	split_comma <- c('r', 'stroke', 'fill', 'opacity', 'stroke-width', 'fill-opacity', 'stroke-opacity')

	# Vector of point indices for pathC, if present
	point_idx <- c()
	point_num <- 1

	# Create lists and vectors
	params <- list()
	shapes <- list()
	tform <- list()
	tform[['rotate']] <- list()
	rarr_len <- 0
	tform[['translate']] <- matrix(NA, nrow=0, ncol=3)
	z.index <- c()

	# Read each item into list
	for(lnum in 1:length(jsp_vec)){
		
		# Split at =
		str_split <- gsub(' ', '', strsplit(jsp_vec[lnum], '=')[[1]])
		
		if(str_split[2] == "'NA'"){
			params[[str_split[1]]] <- NA
			next
		}
		if(!grepl("'", str_split[2])){
			params[[str_split[1]]] <- as.numeric(str_split[2])
			next
		}
		params[[str_split[1]]] <- gsub("'", "", str_split[2])
	}
	
	# Add other parameters
	param_names <- c('zoom', 'maxzoom', 'eyez')
	for(param_name in param_names){
		str_search <- regexpr(pattern=paste0('[\t]*var ', param_name, ' = [-0-9]+;', br_str), text=read_lines)
		params[[param_name]] <- as.numeric(strsplit(substr(x=read_lines, start=str_search, stop=str_search+attr(str_search,"match.length")), '=|;')[[1]][2])
	}
	params[['depth']] <- floor(params[['zoom']] * (params[['eyez']] - params[['maxzoom']]) / 100 + params[['eyez']])

	# Read each item into list
	lnum <- 1
	for(vnum in 1:length(svg_vec)){

		# Define as list
		shapes[[lnum]] <- list()

		# Get string in line
		svg_line <- svg_vec[vnum]

		# Identify tag
		shapes[[lnum]]$type <- type <- substr(svg_line, 3, regexpr(pattern=' ', text=svg_line)-1)

		# If text, read text between tags and remove
		if(type == 'text'){

			# Find start of text string
			text_start <- regexpr(pattern=' >', text=svg_line)+2

			# Get text string
			shapes[[lnum]]$value <- gsub('</text>$', '', substr(x=svg_line, start=text_start, stop=nchar(svg_line)))
			
			# Remove end tag
			svg_line <- substr(x=svg_line, start=1, stop=text_start-3)

		}else{

			# Remove end tag
			svg_line <- gsub(paste0(' />| ></', type, '>'), '', svg_line)
		}

		# Remove start tag
		svg_line <- gsub(paste0('[\t]*<', type, '[ ]+'), '', svg_line)
		
		# Split property elements
		line_split <- strsplit(gsub('"[ ]+([A-Za-z1-3-]+=)',"\"@~@\\1",svg_line), '@~@')[[1]]
		
		# Read properties
		for(i in 1:length(line_split)){
			
			# Split between name and value
			str_split <- strsplit(line_split[i], '="')[[1]]

			# Collapse at split string in case =" appears in value
			value <- paste(str_split[2:length(str_split)], collapse='="')
			
			# Remove final "
			shapes[[lnum]][[str_split[1]]] <- substr(value, 1, nchar(value)-1)
			
			# Check if numeric
			if(!is.na(suppressWarnings(as.numeric(shapes[[lnum]][[str_split[1]]])))) shapes[[lnum]][[str_split[1]]] <- as.numeric(shapes[[lnum]][[str_split[1]]])
		}
		
		if(type == 'point'){
			point_idx[point_num] <- lnum
			point_num <- point_num + 1
		}

		if(type == 'pathC'){
			d_str <- shapes[[lnum]][['d']]
			if(grepl(',', d_str)) shapes[[lnum]][['d']] <- as.numeric(strsplit(d_str, ',')[[1]])
		}

		## Copy coordinate elements into matrices
		if(type == 'path'){
			# Split path string by separating letters and numbers
			d_split <- splitAlphaNum(shapes[[lnum]][['d']])
			xyz <- matrix(NA, nrow=length(d_split$numeric)/3, ncol=3)
			for(k in 1:(length(d_split$numeric)/3)) xyz[k ,] <- d_split$numeric[(k*3-2):(k*3)]
			shapes[[lnum]][['dtype']] <- d_split$alpha
			shapes[[lnum]][['xyz']] <- xyz
		}

		if(type %in% c('point', 'text')){
			if(grepl(',', shapes[[lnum]][['x']])){
				shapes[[lnum]][['xyz']] <- cbind(
					as.numeric(strsplit(shapes[[lnum]][['x']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['y']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['z']], ',')[[1]])
				)
			}else{
				shapes[[lnum]][['xyz']] <- matrix(unlist(shapes[[lnum]][c('x', 'y', 'z')]), nrow=1, ncol=3)
			}
		}

		if(type == 'circle'){
			if(grepl(',', shapes[[lnum]][['cx']])){
				shapes[[lnum]][['xyz']] <- cbind(
					as.numeric(strsplit(shapes[[lnum]][['cx']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['cy']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['cz']], ',')[[1]])
				)
			}else{
				shapes[[lnum]][['xyz']] <- matrix(unlist(shapes[[lnum]][c('cx', 'cy', 'cz')]), nrow=1, ncol=3)
			}
		}

		if(type %in% c('line', 'arrow')){
			if(grepl(',', shapes[[lnum]][['x1']])){
				shapes[[lnum]][['xyz']] <- cbind(
					as.numeric(strsplit(shapes[[lnum]][['x1']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['y1']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['z1']], ',')[[1]]),
					as.numeric(strsplit(shapes[[lnum]][['x2']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['y2']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['z2']], ',')[[1]])
				)
			}else{
				shapes[[lnum]][['xyz']] <- matrix(unlist(shapes[[lnum]][c('x1', 'y1', 'z1', 'x2', 'y2', 'z2')]), nrow=1, ncol=6)
			}

			#if(type %in% c('line')){
				#print(shapes[[lnum]][['xyz']])
			#}
		}
		
		if(type == 'translate'){
			if(grepl(',', shapes[[lnum]][['x']])){
				translate <- cbind(
					as.numeric(strsplit(shapes[[lnum]][['x']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['y']], ',')[[1]]), 
					as.numeric(strsplit(shapes[[lnum]][['z']], ',')[[1]])
				)
			}else{
				translate <- matrix(unlist(shapes[[lnum]][c('x', 'y', 'z')]), nrow=1, ncol=3)
			}
			tform[['translate']] <- rbind(tform[['translate']], translate)
		}

		if(type == 'rotate'){
			
			# Read in as vector
			rvec <- as.numeric(strsplit(shapes[[lnum]][['r']], ",")[[1]])

			# Convert into array of rotation matrices
			tform[['rotate']][[length(tform[['rotate']])+1]] <- array(rvec, dim=c(3,3,length(rvec)/9))
			
			# Add length to vector
			rarr_len <- c(rarr_len, length(rvec)/9)
		}

		for(name_split in split_comma){
			if(is.null(shapes[[lnum]][[name_split]])) next
			if(!grepl(',', shapes[[lnum]][[name_split]])) next
			shapes[[lnum]][[name_split]] <- strsplit(shapes[[lnum]][[name_split]], ',')[[1]]
			if(!is.na(suppressWarnings(as.numeric(shapes[[lnum]][[name_split]][1])))) shapes[[lnum]][[name_split]] <- as.numeric(shapes[[lnum]][[name_split]])
		}
		
		# Save z.index
		if(!is.null(shapes[[lnum]][['z-index']])) z.index[lnum] <- shapes[[lnum]][['z-index']]

		lnum <- lnum + 1
	}

	# Copy pathC points to pathC
	for(lnum in 1:length(svg_vec)){
		
		if(shapes[[lnum]]$type != 'pathC') next
		
		# Get point indices
		point_indices <- point_idx[shapes[[lnum]][['d']]]

		# Get coordinates of first point
		point_dims <- matrix(NA, nrow=length(point_indices), ncol=2)
		for(i in 1:length(point_indices)) point_dims[i, ] <- dim(shapes[[point_indices[i]]][['xyz']])

		# Create array for points
		shapes[[lnum]][['xyz']] <- array(NA, dim=c(length(point_indices), rev(apply(point_dims, 2, 'max'))))

		# Fill array
		for(i in 1:length(point_indices)){
			shapes[[lnum]][['xyz']][i, , ] <- t(shapes[[point_indices[i]]][['xyz']])
		}
	}	
	
	# Combine rotate arrays into a single array
	if(length(tform[['rotate']]) > 0){
		rarr_len <- cumsum(rarr_len)
		rotate <- array(NA, dim=c(3,3,tail(rarr_len, 1)))
		for(i in 1:length(tform[['rotate']])){
			rotate[, , (rarr_len[i]+1):(rarr_len[i]+dim(tform[['rotate']][[i]])[3])] <- tform[['rotate']][[i]]
		}
		tform[['rotate']] <- rotate
	}

	# Check results
	#for(lnum in 1:length(svg_vec)) cat(paste0(names(shapes[[lnum]]), '=', shapes[[lnum]]), '\n')

	list(
		'params'=params,
		'shapes'=shapes,
		'tform'=tform,
		'z.index'=z.index
	)
}