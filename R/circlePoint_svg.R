circlePoint_svg <- function(circle, T){
	
	if(is.vector(T) && length(T) > 1){
		if(circle$R == 0) return(matrix(circle$C, nrow=length(T), ncol=length(circle$C), byrow=TRUE))
		r <- matrix(NA, nrow=length(T), ncol=length(circle$C))
		for(i in 1:length(T)) r[i, ] <- circle$C + circle$R*cos(T[i])*circle$U + circle$R*sin(T[i])*circle$V		
	}else{
		if(circle$R == 0) return(circle$C)
		r <- circle$C + circle$R*cos(T)*circle$U + circle$R*sin(T)*circle$V
	}
	r
}