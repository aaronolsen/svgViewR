lim2corners <- function(lim){
	
	# Set corners
	corners <- rbind(
		c(lim[1,1], lim[1,2], lim[1,3]),
		c(lim[1,1], lim[2,2], lim[1,3]),
		c(lim[1,1], lim[2,2], lim[2,3]),
		c(lim[1,1], lim[1,2], lim[2,3]),
		c(lim[2,1], lim[1,2], lim[1,3]),
		c(lim[2,1], lim[2,2], lim[1,3]),
		c(lim[2,1], lim[2,2], lim[2,3]),
		c(lim[2,1], lim[1,2], lim[2,3])
	)
	
	# Set any infinity values to NA
	corners[corners == Inf] <- NA
	corners[corners == -Inf] <- NA
	
	corners
}