darkenCol <- function(col, factor){

	if(factor < 0) stop(paste0("'factor' (", factor, ") must be greater than 0."))
	if(factor > 1) stop(paste0("'factor' (", factor, ") must be less than 1."))
	
	# Subtract factor from 1 so that a larger number darkens the color more
	factor <- 1 - factor

	# If color string, convert to vector
	input_str <- FALSE
	if(length(col) == 1){
		input_str <- TRUE
		col <- col2rgb(col)
	}
	
	if(length(col) == 3){
		darker_col <- c(max(0, factor*col[1]/255), max(0, factor*col[2]/255), max(0, factor*col[3]/255))
	}else if(length(col) == 4){
		darker_col <- c(max(0, factor*col[1]/255), max(0, factor*col[2]/255), max(0, factor*col[3]/255, col[4]))
	}

	if(input_str){
		if(length(darker_col) == 3){
			darker_col <- rgb(darker_col[1], darker_col[2], darker_col[3])
		}else if(length(darker_col) == 4){
			darker_col <- rgb(darker_col[1], darker_col[2], darker_col[3], darker_col[4])
		}
	}

	darker_col
}