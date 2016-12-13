readSVG <- function(file){
	## Reads an svg/html file and returns svg elements as list

	# Set distinctive line break string (not likely to be used in text fields)
	br_str <- '@\n@'

	# Read in lines and collapse into string with distinct line breaks
	read_lines <- paste(readLines(file), collapse=br_str)
	
	# Get start and stop points of svg structure
	svg_start <- regexpr(pattern=paste0('<svg_doc xmlns="http://www.w3.org/2000/svg" version="[0-9.]+" style="visibility:hidden;" >', br_str), text=read_lines)
	svg_stop <- regexpr(pattern=paste0(br_str, '[\t]*</svg_doc>', br_str), text=read_lines)

	# Extract string between start and stop
	svg_vec <- strsplit(substr(x=read_lines, start=svg_start, stop=svg_stop+attr(svg_stop,"match.length")), br_str)[[1]]
	svg_vec <- svg_vec[2:(length(svg_vec)-2)]
	
	# Create lists and vectors
	svg.list <- as.list(rep(NA, length(svg_vec)))
	z.index <- rep(NA, length(svg_vec))

	# Read each item into list
	for(lnum in 1:length(svg_vec)){

		# Define as list
		svg.list[[lnum]] <- list()

		# Get string in line
		svg_line <- svg_vec[lnum]

		# Identify tag
		svg.list[[lnum]]$type <- type <- substr(svg_line, 3, regexpr(pattern=' ', text=svg_line)-1)

		# If text, read text between tags and remove
		if(type == 'text'){

			# Find start of text string
			text_start <- regexpr(pattern=' >', text=svg_line)+2

			# Get text string
			svg.list[[lnum]]$value <- gsub('</text>$', '', substr(x=svg_line, start=text_start, stop=nchar(svg_line)))
			
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
			svg.list[[lnum]][[str_split[1]]] <- substr(value, 1, nchar(value)-1)
			
			# Check if numeric
			if(!is.na(suppressWarnings(as.numeric(svg.list[[lnum]][[str_split[1]]])))) svg.list[[lnum]][[str_split[1]]] <- as.numeric(svg.list[[lnum]][[str_split[1]]])
		}
		
		# Parse path into matrix
		if(type == 'path'){

			# Split path string by separating letters and numbers
			d_split <- strsplit_alphanumeric(xmlAttrs(r[[i]])[['d']])
			xyz <- matrix(NA, nrow=length(d_split$numeric)/3, ncol=3)
			for(k in 1:(length(d_split$numeric)/3)) xyz[k ,] <- d_split$numeric[(k*3-2):(k*3)]
			shapes[[xmlName(r[[i]])]][[j]][['point_type']] <- d_split$alpha

			letter_split <- strsplit(svg.list[[lnum]][['d']], '[A-Za-z]')[[1]]
			dmat <- matrix(NA, nrow=0, ncol=3)
			for(i in 1:length(letter_split)){
				pt_split <- suppressWarnings(as.numeric(strsplit(letter_split[i], ' ')[[1]]))
				if(length(pt_split) == 0) next
				dmat <- rbind(dmat, pt_split[!is.na(pt_split)])
			}
			svg.list[[lnum]][['dmat']] <- dmat
		}

		if(type == c('point', 'text') && grepl(',', svg.list[[lnum]][['x']])){
			svg.list[[lnum]][['xyz']] <- cbind(
				as.numeric(strsplit(svg.list[[lnum]][['x']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['y']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['z']], ',')[[1]])
			)
		}

		if(type == 'circle' && grepl(',', svg.list[[lnum]][['cx']])){
			svg.list[[lnum]][['xyz']] <- cbind(
				as.numeric(strsplit(svg.list[[lnum]][['cx']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['cx']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['cx']], ',')[[1]])
			)
		}

		if(type %in% c('line', 'arrow') && grepl(',', svg.list[[lnum]][['x1']])){
			svg.list[[lnum]][['xyz']] <- cbind(
				as.numeric(strsplit(svg.list[[lnum]][['x1']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['y1']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['z1']], ',')[[1]]),
				as.numeric(strsplit(svg.list[[lnum]][['x2']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['y2']], ',')[[1]]), 
				as.numeric(strsplit(svg.list[[lnum]][['z2']], ',')[[1]])
			)
		}

		# Save z.index
		if(!is.null(svg.list[[lnum]][['z-index']])) z.index[lnum] <- svg.list[[lnum]][['z-index']]
	}
		
	# Check results
	#for(lnum in 1:length(svg_vec)) cat(paste0(names(svg.list[[lnum]]), '=', svg.list[[lnum]]), '\n')

	list(
		'svg.list'=svg.list,
		'z.index'=z.index
	)
}