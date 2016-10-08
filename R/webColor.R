webColor <- function(col){
	
	if(length(col) > 1) return(sapply(col, webColor))
	
	if(substr(col, 1, 1) == '#' && nchar(col) > 7) col <- paste0('rgb(', paste(c(col2rgb(col)), collapse=','), ')')

	col
}