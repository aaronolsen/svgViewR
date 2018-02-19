create_sphere_mesh <- function(center = NULL, ends = NULL, radius = NULL, width = NULL, axes = NULL, 
	seg = 40, portion = 1){

	# Set width
	if(is.null(width) && !is.null(radius)) width <- rep(radius*2, 3)

	# Set default axes
	if(is.null(ends) && is.null(axes)) axes <- diag(3)

	if(!is.null(ends) && is.null(axes)){
		axes <- matrix(NA, 2, 3)
		axes[1,] <- ends[2,]-ends[1,]
		axes[2,] <- vorthogonal_svg(axes[1,])
	}
	
	# Make sure axes are unit vectors
	axes <- uvector_svg(axes)
	
	# Make sure axes is matrix
	if(is.vector(axes)) axes <- matrix(axes, 1, 3)

	# Set other variables in first input case
	if(!is.null(center) && !is.null(width)){

		# Set ends
		ends <- matrix(NA, 2, 3)
		ends[1,] <- center - (width[1]/2)*axes[1,]
		ends[2,] <- center + (width[1]/2)*axes[1,]
	}
	
	if(!is.null(center) && nrow(axes) == 1) axes <- rbind(axes, uvector_svg(vorthogonal_svg(axes[1,])))

	# Make sure ends is matrix
	if(is.vector(ends)) ends <- matrix(ends, 1, 3)

	# Add orthogonal axis vector if only one end given
	if(nrow(ends) == 1 && nrow(axes) == 1) axes <- rbind(axes, uvector_svg(vorthogonal_svg(axes[1,])))

	# Add second row to ends if not given
	if(nrow(ends) == 1) ends <- rbind(ends, ends+width[1]*uvector_svg(axes[1,]))

	# Set first axis to ends if only one given
	if(nrow(axes) == 1) axes <- rbind(uvector_svg(ends[2,]-ends[1,]), axes)

	# Set third axis if only two given
	if(nrow(axes) == 2) axes <- rbind(axes, uvector_svg(cprod_svg(axes[1,], axes[2,])))

	# Add first width from ends if width length is 2
	if(is.null(width)) width <- rep(sqrt(sum((ends[2,]-ends[1,])^2)), 3)
	if(length(width) == 1) width <- rep(width, 3)
	if(length(width) == 2) width <- c(sqrt(sum((ends[2,]-ends[1,])^2)), width)

	# Create vertices matrix
	n_vertices <- 2+(seg[1]-1)*seg[2]
	vertices <- matrix(NA, n_vertices, 3)
	
	# Set points along main axis at which to draw circles/ellipses
	a_ends <- seq(0, pi, length=seg[1]+1)

	# Find a_ends value closest to portion (scaled to pi)
	a_ends_diff <- which.min(abs(a_ends - portion*pi))
	
	# Determine which ends and sides to draw
	if(a_ends_diff == length(a_ends) && portion == 1){
		ends_draw <- 2
		sides_draw <- (seg[1]-2)
	}
	if(a_ends_diff == length(a_ends) && portion < 1){
		ends_draw <- 1
		sides_draw <- (seg[1]-2)
	}
	if(a_ends_diff < length(a_ends) && a_ends_diff > 1){
		ends_draw <- 1
		sides_draw <- a_ends_diff-2
	}
	if(a_ends_diff == 1){
		ends_draw <- 1
		sides_draw <- 0
	}

	# Remove end indices
	a_ends <- a_ends[2:(length(a_ends)-1)]

	# Set angle
	T <- seq(0, 2*pi, length=seg[2]+1)[1:seg[2]]

	# Add ends
	vertices[c(1, n_vertices), ] <- ends

	# Fill vertex matrix
	ri <- 2
	for(a_end in a_ends){
	
		# Set center of shape
		t_center <- ends[2, ] - width[1]*((cos(a_end)+1)/2)*axes[1,]

		# Set rows to fill
		fill_rows <- ri:(ri+seg[2]-1)
		
		# Set width
		width_seg <- width[2:3]
		width_seg[1] <- (width[2]/2)*sin(a_end)
		width_seg[2] <- (width[3]/2)*sin(a_end)

		# Add points
		for(i in 1:length(fill_rows)) vertices[fill_rows[i], ] <- t_center + width_seg[1]*cos(T[i])*axes[2,] + width_seg[2]*sin(T[i])*axes[3,]
		
		ri <- tail(fill_rows, 1)+1
	}

	# Create faces matrix
	n_faces <- 2*seg[2]*((seg[1]-2) + 1)
	faces <- matrix(NA, n_faces, 3)

	# Add end faces
	end_faces <- 1:seg[2]
	faces[end_faces, 1] <- 0
	faces[end_faces, 2] <- end_faces
	faces[c(tail(end_faces,1), end_faces[1]:(tail(end_faces,1)-1)), 3] <- end_faces

	if(ends_draw == 2){
		end_faces <- (n_faces-seg[2]+1):n_faces
		end_row <- (n_vertices-seg[2]+1):n_vertices
		faces[end_faces, 1] <- n_vertices-1
		faces[end_faces, 3] <- end_row-2
		faces[c(tail(end_faces,1), end_faces[1]:(tail(end_faces,1)-1)), 2] <- end_row-2
	}

	# Fill faces matrix
	if(sides_draw > 0){

		for(i in 1:sides_draw){

			# Set rows to fill
			fill_start <- ((i-1)*2*seg[2]) + seg[2] + 1
			fill_end <- fill_start + 2*seg[2] - 1
			fill_rows <- fill_start:fill_end
			fill_rows1 <- fill_rows[1:(length(fill_rows)/2)]
			fill_rows2 <- fill_rows[((length(fill_rows)/2)+1):length(fill_rows)]

			# Set indices of first row and second row
			first_row <- (1+(i-1)*seg[2]):(1+(i-1)*seg[2] + seg[2] - 1)
			second_row <- (1+i*seg[2]):((i+1)*seg[2])
			#print(first_row)
			#print(second_row)
		
			# 
			faces[fill_rows1, 1] <- first_row
			faces[fill_rows1, 2] <- second_row
			faces[c(tail(fill_rows1,1), fill_rows1[1]:(tail(fill_rows1,1)-1)), 3] <- second_row

			faces[fill_rows2, 1] <- first_row
			faces[c(tail(fill_rows2,1), fill_rows2[1]:(tail(fill_rows2,1)-1)), 3] <- first_row
			faces[c(tail(fill_rows2,1), fill_rows2[1]:(tail(fill_rows2,1)-1)), 2] <- second_row
		}
	}
	
	# Remove rows with NA values
	faces <- faces[rowSums(is.na(faces)) == 0, ]
	
	# Remove vertices not referenced in faces
	vertices <- vertices[1:max(faces+1), ]

	# Output vertices and faces
	list('vertices'=vertices, 'faces'=faces)
}
