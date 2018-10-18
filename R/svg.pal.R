svg.pal <- function(n, yellow = TRUE, alpha = 1){
	
	# Set colors
	cols <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF",
		"#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")

	if(alpha < 1){
		for(i in 1:length(cols)){
			as_rgb <- col2rgb(cols[i])
			cols[i] <- rgb(as_rgb[1], as_rgb[2], as_rgb[3], alpha=alpha*255, maxColorValue=255)
		}
	}

	if(!yellow) cols <- cols[c(1:5,7:length(cols))]

	# Return specified number of colors
	cols[1:n]
}