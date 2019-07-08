bestAlign_svg <- function(m1, m2, m3 = NULL, sign = NULL){

	## See fit_joint_model for revised svd code that fixes bug-- I think I've updated this, based on procAlign

	# If m1 is 3-d array
	if(length(dim(m1)) == 3 && length(dim(m2)) == 2){

		# Get number of iterations
		n_iter <- dim(m1)[3]

		# Get common points
		common_names <- rownames(m1)[rownames(m1) %in% dimnames(m2)[[1]]]
		
		# Array for transformed m2
		m2r <- array(NA, dim=c(dim(m2), n_iter), dimnames=list(dimnames(m2)[[1]], NULL, NULL))
		
		# Create transformation array
		tmat <- array(NA, dim=c(4,4,n_iter))

		# Not finished
		return(NULL)
	}

	if(length(dim(m2)) == 3){
		
		# Get number of iterations
		n_iter <- dim(m2)[3]
		
		# Get common points
		common_names <- rownames(m1)[rownames(m1) %in% dimnames(m2)[[1]]]

		# Each iteration
		for(iter in 1:n_iter){

			# Find translation and rotation to align m2 to m1
			m2[, , iter] <- bestAlign_svg(m1[common_names, ], m2[common_names, , iter], m2[, , iter])$mc
		}

		return(m2)
	}
	
	# IF INPUTTING A MATRIX WITH ALL ZEROS IN ONE DIMENSION, MAKE IT M2, NOT M1

	if(is.null(m3)) m3r <- NULL

	# SET INITIAL COMMON POINT MATRIX VALUES
	m1o <- m1
	m2o <- m2

	# USE ROWNAMES, IF GIVEN, TO REMOVE NON-CORRESPONDING POINTS
	if(!is.null(rownames(m1)) && !is.null(rownames(m2))){

		m1o <- m1o[sort(rownames(m1o)), ]
		m2o <- m2o[sort(rownames(m2o)), ]

		# REMOVE NA VALUES
		m1o <- m1o[!is.na(m1o[, 1]), ]
		m2o <- m2o[!is.na(m2o[, 1]), ]

		m1o[!rownames(m1o) %in% rownames(m2o), ] <- NA
		m2o[!rownames(m2o) %in% rownames(m1o), ] <- NA

	}else{

		# REPLACE NON-COMMON LANDMARKS BETWEEN TWO MATRICES WITH NA
		m1o[which(is.na(m2o))] <- NA
		m2o[which(is.na(m1o))] <- NA
	}

	# REMOVE NA VALUES
	m1o <- m1o[!is.na(m1o[, 1]), ]
	m2o <- m2o[!is.na(m2o[, 1]), ]

	# CREATE FIRST TRANSLATION TRANSFORMATION MATRIX
	tmat1 <- diag(4);tmat1[1:3, 4] <- -colMeans(m2o, na.rm=TRUE)
	
	# CENTER M2 ABOUT CENTROID OF COMMON POINTS
	m2c <- mtransform_svg(m2, tmat1)

	# APPLY TRANSLATION TO EXTRA MATRIX, IF PROVIDED
	if(!is.null(m3)) m3c <- mtransform_svg(m3, tmat1)
	
	# CENTER COMMON POINTS
	m1oc <- scale(m1o, center=TRUE, scale=FALSE)
	m2oc <- scale(m2o, center=TRUE, scale=FALSE)

	# Find best alignment with just two points
	if(nrow(m1oc) == 2){
		
		# Check if points are identical
		tmat2 <- diag(4)
		if(sum(colMeans(abs(m2oc - m1oc))) > 1e-10){

			m_axis <- cprod_svg(m1oc[2,]-m1oc[1,], m2oc[2,]-m2oc[1,])

			# Find angle between points
			m_avec <- avec_svg(m2oc[2,], m1oc[2,], axis=m_axis, about.axis=TRUE)

			# Find rotation
			RM <- tMatrixEP_svg(v=m_axis, -m_avec)
			
			#rotated <- rotateBody(m2oc, m_axis, -m_avec)
			#cat('-----------\n')
			#print(m1oc)
			#print(rotated)

			# Set rotation matrix			
			tmat2[1:3, 1:3] <- t(RM)
		}

		m2r <- mtransform_svg(m2c, tmat2)

	}else{

		# FIND ROTATION MATRIX TO APPLY TO M2 THAT MINIMIZES DISTANCE BETWEEN M1 AND M2
		SVD <- svd(t(na.omit(m1oc)) %*% na.omit(m2oc))

		# CORRECTION TO ENSURE A RIGHT-HANDED COORDINATE SYSTEM
		S <- diag(3)
		S[3,3] <- sign(det(SVD$v %*% t(SVD$u)))

		# GET ROTATION MATRIX
		# MIGHT CHANGE POINTS RELATIVE TO ONE ANOTHER (VERY SLIGHTLY)
		# I THINK IT ONLY HAPPENS WHEN ONE DIMENSION OF M1 IS ALL ZEROS
		# CAUSES PROBLEM IN DETERMINING FIT PERHAPS
		RM <- SVD$v %*% S %*% t(SVD$u)

		# CREATE ROTATION TRANSFORMATION MATRIX
		tmat2 <- diag(4);tmat2[1:3, 1:3] <- t(RM)

		# TEST ALIGNMENT
		#t2 <- m2c %*% RM
		#print(m1oc[!is.na(m1oc[, 1]), ])
		#print(t2[!is.na(t2[, 1]), ])

		# ROTATE ALL CENTER LANDMARKS IN M2
		m2r <- mtransform_svg(m2c, tmat2)
		if(!is.null(m3)) m3r <- mtransform_svg(m3c, tmat2)

		# TEST WHETHER CHIRALITY OF POINT SET HAS FLIPPED
		if(nrow(m1o) == 3 && sum(!is.na(m2[,1])) > 3){
		
			# IF THE ALIGNMENT FLIPPED THE SET TO ITS MIRROR IMAGE, THE CROSS PRODUCT OF THE 
			#	FIRST THREE POINTS WILL MAINTAIN THE SAME ORIENTATION. BUT ANY OTHER POINTS WILL
			#	BE FLIPPED RELATIVE TO THIS VECTOR. SO IF THE DISTANCE BETWEEN THE CROSS PRODUCT
			#	AND THESE POINTS CHANGES, IT INDICATES THE CHIRALITY HAS BEEN FLIPPED

			# FIND NORMAL VECTORS FOR PRE AND POST ROTATED SETS
			m2c_cprod <- uvector_svg(cprod_svg(m2c[2, ]-m2c[1, ], m2c[3, ]-m2c[1, ]))
			m2r_cprod <- uvector_svg(cprod_svg(m2r[2, ]-m2r[1, ], m2r[3, ]-m2r[1, ]))
		
			# FIND DISTANCE FROM CPROD VECTOR TO OTHER POINTS
			dpp <- dppt_svg(m2c_cprod, m2c[4:min(7,nrow(m2c)), ])
			dpp_r <- dppt_svg(m2r_cprod, m2r[4:min(7,nrow(m2r)), ])
		
			# CHIRALITY HAS FLIPPED, FLIP 3RD COLUMN OF SVD$v AND RE-TRANSFORM
			if(sum(round(abs(dpp - dpp_r), 7)) > 0.001){
				SVD$v[, 3] <- -SVD$v[, 3]
				RM <- SVD$v %*% S %*% t(SVD$u)
				tmat2[1:3, 1:3] <- t(RM)
				m2r <- mtransform_svg(m2c, tmat2)
				if(!is.null(m3)) m3r <- mtransform_svg(m3c, tmat2)
			}else{
			}
		}
	}

	m2or <- mtransform_svg(m2oc, tmat2)

	# CREATE SECOND TRANSLATION TRANSFORMATION MATRIX
	tmat3 <- diag(4);tmat3[1:3, 4] <- colMeans(m1o, na.rm=TRUE)

	# APPLY TRANSLATION PARAMETERS
	m2r <- mtransform_svg(m2r, tmat3)
#	m2r <- m2r + matrix(colMeans(m1o, na.rm=TRUE), nrow=nrow(m2r), ncol=ncol(m2r), byrow=TRUE)
	if(!is.null(m3)) m3r <- mtransform_svg(m3r, tmat3)

	# GET ALIGNMENT ERROR
	errors <- m1oc - m2or
	attr(errors, "scaled:center") <- NULL
	
	dist.errors <- matrix(sqrt(rowSums(errors^2)), ncol=1, dimnames=list(rownames(errors), NULL))

	# GET FINAL TRANSFORMATION MATRIX
	tmat <- tmat3 %*% tmat2 %*% tmat1

	#errors <- m1o - mtransform_svg(m2o, tmat)
	
	list(
		mat=m2r,
		pos.errors=errors,
		dist.errors=dist.errors,
		mc=m3r,
		tmat=tmat
	)
}