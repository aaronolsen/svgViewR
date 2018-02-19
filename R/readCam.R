readCam <- function(file){
	
	# Reads MayaCam file exported from XMALab
	
	read_lines <- readLines(file)
	
	image_size <- as.numeric(strsplit(read_lines[2], ',')[[1]])

	intrinsic <- matrix(NA, 3, 3)
	intrinsic[1,] <- as.numeric(strsplit(read_lines[5], ',')[[1]])
	intrinsic[2,] <- as.numeric(strsplit(read_lines[6], ',')[[1]])
	intrinsic[3,] <- as.numeric(strsplit(read_lines[7], ',')[[1]])

	extrinsic <- matrix(NA, 3, 4)
	extrinsic[1,1:3] <- as.numeric(strsplit(read_lines[10], ',')[[1]])
	extrinsic[2,1:3] <- as.numeric(strsplit(read_lines[11], ',')[[1]])
	extrinsic[3,1:3] <- as.numeric(strsplit(read_lines[12], ',')[[1]])
	extrinsic[,4] <- as.numeric(read_lines[15:17])

	list(
		'image.size'=image_size,
		'intrinsic'=intrinsic,
		'extrinsic'=extrinsic
	)
}