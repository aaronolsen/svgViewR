viewer_save_image <- function(from, to){

	# Decode 'to'
	to_decode <- URLdecode(to)
	to_decode <- gsub('[+]', ' ', to_decode)

	# Copy image file from temp folder
	file.copy(from=from, to=to_decode, overwrite=TRUE)

	#cat('from: ', from, '\n')
	#cat('to: ', to_decode, '\n')
	
	# Remove version in temp
	if(file.exists(from)) file.remove(from)
}