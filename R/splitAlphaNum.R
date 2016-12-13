splitAlphaNum <- function(string){

	s <- strsplit(string, "")[[1]]

	r <- list()
	r$alpha <- rep(NA, 0)
	r$numeric <- rep(NA, 0)
	s_as_numeric <- suppressWarnings(as.numeric(s))

	# CHECK IF FIRST CHARACTER IS NUMERIC
	if(!is.na(s_as_numeric) || s[1] == "-" || s[1] == "."){
		previous_numeric <- TRUE
	}else{
		previous_numeric <- FALSE
	}

	for(i in 1:length(s)){
		
		is_numeric <- FALSE
		if(!is.na(s_as_numeric[i]) || s[i] == "-" || s[i] == "." || s[i] == "e" || s[i] == "+") is_numeric <- TRUE

		if(!is_numeric){
			if(s[i] == " " || s[i] == ","){
				previous_numeric <- FALSE
				next
			}
			r$alpha <- c(r$alpha, s[i])
			previous_numeric <- FALSE
		}else{
			if(previous_numeric){
				r$numeric[length(r$numeric)] <- paste(r$numeric[length(r$numeric)], s[i], sep="")
			}else{
				r$numeric <- c(r$numeric, s[i])
			}
			previous_numeric <- TRUE
		}
	}
	
	r$numeric <- suppressWarnings(as.numeric(r$numeric))

	r
}