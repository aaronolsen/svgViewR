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
	svg_vec <- svg_vec[2:(length(svg_vec)-1)]
	
	# Read each item into list
	svg_list <- list()
	for(lnum in 1:length(svg_vec)){

		cat(paste0(svg_vec[lnum], '\n'))
		svg_list[[lnum]] <- list()

		# Get string in line
		svg_line <- svg_vec[lnum]

		# Identify tag
		svg_list[[lnum]]$type <- substr(svg_line, 3, regexpr(pattern=' ', text=svg_line)-1)
		
		# Remove tags
		svg_line <- gsub(paste0('[\t]*<', svg_list[[lnum]]$type, ' | />'), '', svg_line)
		
		# Split property elements
		line_split <- strsplit(gsub('" ([A-Za-z-]+=)',"\"@~@\\1",svg_line), '@~@')[[1]]
		
		# Read properties
		for(i in 1:length(line_split)){
			
			# Split between name and value
			str_split <- strsplit(line_split[i], '="')[[1]]

			# Collapse at split string in case =" appears in value
			value <- paste(str_split[2:length(str_split)], collapse='="')
			
			# Remove final "
			svg_list[[lnum]][[str_split[1]]] <- substr(value, 1, nchar(value)-1)
		}
		#cat(paste0(line_split, '\n'))

		break
	}
	
	print(svg_list)

	
	#
}