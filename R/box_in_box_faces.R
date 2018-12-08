box_in_box_faces <- function(vertices, faces, icorners, ocorners){
	
	# Add values so can loop around at end
	ocorners <- c(tail(ocorners, 1), ocorners, ocorners[1])
	icorners <- c(icorners, icorners[1])
	
	# Project each inner corner
	new_idx <- c()
	for(i in 1:4){
	
		n <- nrow(vertices)
		
		# Project to one side
		plp1 <- pointLineProj_svg(vertices[icorners[i], ], vertices[ocorners[i+1], ], vertices[ocorners[i+2], ])

		# Project to other side
		plp2 <- pointLineProj_svg(vertices[icorners[i], ], vertices[ocorners[i+1], ], vertices[ocorners[i], ])
		
		# Create faces
		faces <- rbind(faces, 
			c(ocorners[i+1], n+1, icorners[i]), 
			c(icorners[i], ocorners[i+1], n+2)
		)
		
		# Add vertex indices
		new_idx <- c(new_idx, n+1, n+2)

		# Add vertices
		vertices <- rbind(vertices, plp1, plp2)
	}
	
	# Add values so can loop around at end
	new_idx <- c(new_idx, new_idx[1:2])
	
	for(i in 1:4){
		faces <- rbind(faces, 
			c(icorners[i], new_idx[(i-1)*2+1], new_idx[(i-1)*2+4]),
			c(new_idx[(i-1)*2+4], icorners[i], icorners[i+1])
		)
	}
	
	return(list('vertices'=vertices, 'faces'=faces))	
}