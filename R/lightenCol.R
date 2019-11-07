lightenCol <- function(col, factor){

	if(length(col) > 1){
		rcols <- rep(NA, length(col))
		for(i in 1:length(col)){
			rcols[i] <- lightenCol(col[i], factor)
		}
		names(rcols) <- names(col)
		return(rcols)
	}

	if(factor < 0) stop(paste0("'factor' (", factor, ") must be greater than 0."))
	if(factor > 1) stop(paste0("'factor' (", factor, ") must be less than 1."))
	
	# Add factor to 1 to increase color
	factor <- 1 + factor

	# If color string, convert to vector
	input_str <- FALSE
	if(length(col) == 1){
		input_str <- TRUE
		col <- col2rgb(col)
	}
	
	if(length(col) == 3){
		new_col <- c(min(1, factor*col[1]/255), min(1, factor*col[2]/255), min(1, factor*col[3]/255))
	}else if(length(col) == 4){
		new_col <- c(min(1, factor*col[1]/255), min(1, factor*col[2]/255), min(1, factor*col[3]/255, col[4]))
	}

	if(input_str){
		if(length(new_col) == 3){
			new_col <- rgb(new_col[1], new_col[2], new_col[3])
		}else if(length(new_col) == 4){
			new_col <- rgb(new_col[1], new_col[2], new_col[3], new_col[4])
		}
	}

	new_col
}