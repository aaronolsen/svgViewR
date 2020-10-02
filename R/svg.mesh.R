svg.mesh <- function(file = NULL, name = NULL, col = '#F5F5F5', emissive = 'black', 
	material = c('auto', 'lambert', 'phong')[1], opacity = 1, ontop = FALSE, get.lim = TRUE, 
	scaling = 1, debug = FALSE, vertex.normals = NULL, face.normals = NULL, vertex.labels = NULL, 
	vertex.spheres = NULL, dbl.side = c('auto', TRUE, FALSE)[1]){

	# Make sure that type is webgl
	if('svg' == getOption("svgviewr_glo_type")) stop("'webgl' mode must be used to enable mesh drawing. This can be done by adding the following parameter to the svg.new() function call: mode='webgl'. This will become the default mode by version 1.4.")

	# Get viewer environment
	env <- as.environment(getOption("svgviewr_glo_env"))

	# Get all input parameters as list
	input_params <- mget(names(formals()),sys.frame(sys.nframe()))
	
	# Check for NA color input
	if(is.na(col)) stop("Input color is 'NA'")

	# Set type
	input_params$type <- gsub('svg[.]', '', input_params$fcn)

	# Set where to add object
	add_at <- length(svgviewr_env$svg$mesh)+1
	
	# Set default material
	material_final <- 'lambert'
	double_side <- FALSE
	parse_model <- TRUE

	if(!is.list(file)){

		# Set name
		if(is.null(name)) name <- gsub('[.][A-Za-z]+$', '', tail(strsplit(file, '/')[[1]], 1))

		# Check that file exists
		if(!file.exists(file)) stop(paste0('Input file "', file, '" not found.'))
		
		# Check that file is json format
		if(!grepl('[.](json|obj)$', file)) stop(paste0('Input file "', file, '" is of unrecognized file type. Allowed file types are .obj and .json.'))

		#
		if('html' == getOption("svgviewr_glo_type")){

			# Read mesh file
			if(grepl('[.]json$', file)){
				obj_list <- fromJSON(paste(suppressWarnings(readLines(file)), collapse=""))
			}else{

				# Read OBJ as string
				obj_list <- read_obj_str(paste(paste(suppressWarnings(readLines(file)), collapse="*"), '*'))

				## Format faces for threejs
				# Convert to matrix
				obj_faces <- matrix(obj_list$faces, nrow=length(obj_list$faces)/6, ncol=6, byrow=TRUE)
				faces <- matrix(NA, nrow=nrow(obj_faces), ncol=7)
				faces[, 1] <- 32
				faces[, 2:4] <- obj_faces[, c(1,3,5)] - 1
				faces[, 5:7] <- obj_faces[, c(2,4,6)] - 1
				obj_list$faces <- t(faces)
			}

		}else{

			if(FALSE){

				# Live/server visualization
				if(grepl('[.](obj)$', file)){
				
					# For server webgl visualization
					#json_file <- gsub('[.]obj$', '.json', file)
					cat(paste0('For server ("live") visualization mesh file should be .json format. To convert an .obj file to .json, you can use the svgViewR function objToJSON(). For example:\n'))
					cat(paste0('\tobjToJSON(obj=\'', file, '\', file=\'', gsub('[.]obj$', '.json', file), '\')\n'))
					cat(paste0('\nThen replace the file input to this function with the \'.json\' file.\n\n'))
				
					response <- readline(prompt="Would you like to do this now? (y/n) : ");
					if(tolower(response) %in% c('yes', 'y')){

						response <- readline(prompt=paste0("Enter the file path of the converted .json file (to use '", json_file, "' simply press return): "))

					}else{
						stop('Please input a mesh in the .json format')
					}
				
					if(response != '') json_file <- response

					objToJSON(obj=file, file=json_file)
				
					file <- json_file
				}

				# Get absolute file path (rook app doesn't work with relative paths)
				file <- normalizePath(path=file)

				# Separate directory and filename
				file_strsplit <- strsplit(file, '/')[[1]]

				# Set filename
				input_params$fname <- tail(file_strsplit, 1)

				# Read source file
				if(get.lim) obj_list <- fromJSON(paste(suppressWarnings(readLines(file)), collapse=""))

				# Set directory path
				input_params$src <- ''
				if(length(file_strsplit) > 1) input_params$src <- paste0(paste0(file_strsplit[1:(length(file_strsplit)-1)], collapse='/'), '/')
			}

			# Read mesh file
			if(grepl('[.]json$', file)){
				obj_list <- fromJSON(paste(suppressWarnings(readLines(file)), collapse=""))
			}else{

				# Read OBJ as string
				obj_list <- read_obj_str(paste(paste(suppressWarnings(readLines(file)), collapse="*"), '*'))
			}
		}
		
	}else{

		# Set name
		if(is.null(name)) name <- 'mesh'

		# Check class of input list	
		if(class(file) == 'obj'){

			# Set material if auto
			if(material == 'auto') material <- 'lambert'
			
		}else{
		
			# Check if there are vertices and faces
			if(!'vertices' %in% names(file)) stop("If input object is not of class 'obj' it must have vertices")
			if(!'faces' %in% names(file)) stop("If input object is not of class 'obj' it must have faces")
		}
	
		# Assign as mesh
		obj_list <- file
		
		if(class(file) != 'obj' && material == 'lambert'){
			if(ncol(file$faces) == 3){
				new_faces <- matrix(NA, nrow=nrow(file$faces), ncol=6)
				new_faces[,3:1] <- file$faces
				new_faces[,6:4] <- file$faces
				file$faces <- new_faces + 1
			}
			class(file) <- 'obj'
		}

		if(class(file) == 'obj' && material == 'lambert'){

			## Format faces for threejs
			# Convert to matrix
			obj_faces <- file$faces
			faces <- matrix(NA, nrow=nrow(obj_faces), ncol=7)
			faces[, 1] <- 32
			faces[, 2:4] <- obj_faces[, c(1,3,5)] - 1
			faces[, 5:7] <- obj_faces[, c(2,4,6)] - 1

			# Overwrite faces with re-formatted version
			obj_list$faces <- t(faces)
		
			# Convert to vector for threejs
			obj_list$vertices <- c(t(obj_list$vertices))

			# Convert to vector for threejs
			if('normals' %in% names(obj_list)) obj_list$normals <- c(t(obj_list$normals))
		}
		
		if(class(file) == 'obj' && material == 'phong'){
			obj_list$faces <- obj_list$faces[, c(1,3,5)] - 1
			parse_model <- FALSE
		}
		
		if(class(file) != 'obj'){
			material_final <- 'phong'
			double_side <- TRUE
			parse_model <- FALSE
		}
	}
	
	# Overwrite material if provided in input
	if(material != 'auto') material_final <- material
	
	# Apply scaling to vertices
	obj_list$vertices <- obj_list$vertices*scaling
	
	# If debugging plot mesh vertices, normals
	if(debug){
	
		# Set default
		if(is.null(vertex.spheres)) vertex.spheres <- TRUE
		if(is.null(vertex.normals)) vertex.normals <- TRUE
		if(is.null(face.normals)) face.normals <- TRUE
		if(is.null(vertex.labels)) vertex.labels <- TRUE
	
		# Set to double side faces so that all faces show up regardless of normal direction
		double_side <- TRUE

		# Make sure vertices are matrix
		if(!is.matrix(obj_list$vertices)){
			vertex_mat <- matrix(obj_list$vertices, nrow=length(obj_list$vertices)/3, 3, byrow=TRUE)
		}else{
			vertex_mat <- obj_list$vertices
		}

		# Make sure faces are simple matrix
		if(nrow(obj_list$faces) == 7){
			faces_mat <- t(obj_list$faces[2:4, ]) + 1
		}else if(ncol(obj_list$faces) == 6){
			faces_mat <- obj_list$faces
		}else{
			faces_mat <- obj_list$faces + 1
		}
		
		# Get vertex row sampling indices
		sample_idx <- seq(1, nrow(vertex_mat), length=min(nrow(vertex_mat), 100))
	
		# Get mesh center
		mesh_center <- colMeans(vertex_mat[sample_idx, ])
	
		# Set scale
		mesh_csize <- mean(dppt_svg(mesh_center, vertex_mat[sample_idx,]))
		
		# Plot vertices
		if(vertex.spheres) svg.spheres(vertex_mat, radius=0.01*mesh_csize, name=name)

		# Plot vertex normals (if present)
		if(!is.null(obj_list$normals) && vertex.normals){

			# Make sure vertex normals are matrix
			if(!is.matrix(obj_list$normals)){
				normals_mat <- matrix(obj_list$normals, nrow=length(obj_list$normals)/3, 3, byrow=TRUE)
			}else{
				normals_mat <- obj_list$normals
			}

			for(i in 1:nrow(normals_mat)){
				svg.lines(x=rbind(vertex_mat[i,], vertex_mat[i,] + 0.1*mesh_csize*normals_mat[i,]), name=name)
			}
		}

		# Plot faces
		for(i in 1:nrow(faces_mat)){
		
			face_vertices <- vertex_mat[c(faces_mat[i,], faces_mat[i,1]), ]

			# Plot edges of face
			svg.lines(x=face_vertices, name=name)

			if(face.normals){

				# Add face normals
				face_norm <- uvector_svg(cprod_svg(face_vertices[2,]-face_vertices[1,], face_vertices[3,]-face_vertices[1,]))

				# Plot face normal
				face_center <- colMeans(vertex_mat[faces_mat[i,], ])
				svg.lines(x=rbind(face_center, face_center + 0.1*mesh_csize*face_norm), name=name)
			}
		}

		# Add vertex labels
		if(vertex.labels){
			center_mat <- matrix(mesh_center, nrow(vertex_mat), 3, byrow=TRUE)
			svg.text(vertex_mat + 0.03*mesh_csize*uvector_svg(vertex_mat - center_mat), 
				labels=1:(nrow(vertex_mat)), size=0.05*mesh_csize, name=name)
		}
	}
	
	# Overwrite dbl.side if provided in input
	if(dbl.side != 'auto') double_side <- dbl.side
	
	# Add mesh properties to input parameters
	if(material_final == 'lambert'){
		for(prop_name in names(obj_list)) input_params[[prop_name]] <- obj_list[[prop_name]]
	}else{
		input_params[['vertices']] <- t(obj_list[['vertices']])
		input_params[['faces']] <- t(obj_list[['faces']])
		#parse_model <- FALSE
	}

	#print(input_params[['vertices']])
	#print(input_params[['faces']])

	input_params[['scale']] <- 1

	# Set opacity
	input_params[['opacity']] <- setNames(opacity, NULL)
	input_params[['col']] <- setNames(webColor(col), NULL)
	input_params[['emissive']] <- setNames(webColor(emissive), NULL)
	input_params[['doubleSide']] <- double_side
	input_params[['parseModel']] <- parse_model
	input_params[['material']] <- material_final
	input_params[['itmat']] <- diag(4)
	input_params[['depthTest']] <- !ontop

	# Add to meshes
	svgviewr_env$svg$mesh[[add_at]] <- input_params

	# Add object reference data
	svgviewr_env$ref$names <- c(svgviewr_env$ref$names, name)
	svgviewr_env$ref$num <- c(svgviewr_env$ref$num, add_at)
	svgviewr_env$ref$type <- c(svgviewr_env$ref$type, 'mesh')

	# Get xyz limits of mesh		
	if(get.lim){

		# Get number of vertices
		num_vertices <- length(obj_list$vertices)

		# Get xyz limits
		obj_ranges <- cbind(range(obj_list$vertices[seq(1, num_vertices-2, by=3)], na.rm=TRUE),
			range(obj_list$vertices[seq(2, num_vertices-1, by=3)], na.rm=TRUE),
			range(obj_list$vertices[seq(3, num_vertices, by=3)], na.rm=TRUE))
		colnames(obj_ranges) <- c('x', 'y', 'z')
		
		# Set corners
		corners <- lim2corners(obj_ranges)

		# Add limits to object
		svgviewr_env$svg$mesh[[add_at]][['lim']] <- obj_ranges
		svgviewr_env$svg$mesh[[add_at]][['corners']] <- corners

		return(list('lim'=obj_ranges, 'corners'=corners))
	}
	
	ret = NULL
}