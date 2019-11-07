webColor <- function(col, format=c('#hex', '0xhex', '0', '0x'), rev=FALSE){

	# Example inputs
	# webColor(c('green', 'red'), format='0xhex')
	# webColor('#1A1A1A', format='0xhex')
	# webColor(rgb(0.1,0.1,0.1), format='0xhex')
	
	# Color replace
	col_replace <- c('olive'='#808000', 'teal'='#008080')
	
	if(rev) return(paste0('#', substr(col, 3, nchar(col))))
	
	if(length(col) > 1) return(sapply(col, webColor, format=format))
	
	if(grepl(',', col)){
		col_split <- strsplit(col, ',')[[1]]
		return(paste(webColor(col_split, format=format), collapse=','))
	}
	
	if(col == '') return('')
	if(col == 'none') return('none')
	if(substr(col, 1, 1) == '#' && nchar(col) > 7){
		col <- paste0('rgb(', paste(c(col2rgb(col)), collapse=','), ')')
	}
	if(substr(col, 1, 1) != '#' && is.character(col)){

		if(col %in% names(col_replace)) col <- col_replace[col]
		
		col_2_rgb <- col2rgb(col) / 255
		col <- rgb(col_2_rgb[1],col_2_rgb[2],col_2_rgb[3])
	}

	if(format[1] %in% c('0xhex', '0', '0x')) col <- paste0('0x', gsub('#', '', col))

	col
}