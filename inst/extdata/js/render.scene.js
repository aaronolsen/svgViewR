// Start arrays for drawn objects
var arrows = new Array( );
var lines = new Array( );
var meshes = new Array( );
var spheres = new Array( );
var sprites = new Array( );

// Declare global variables
var animation_times, animations, anim_pause_time, camera, controls, mesh_name, mesh_opacity, renderer, scene, stats;
var update_obj = {
    num: new Array(),
    type: new Array()
};

var animate = false;						// Whether to animate - turned on if animation is loaded
var anim_start = Date.now();				// Set animation start time
var anim_pause = false;						// Start with animation paused
var obj_add_ct = 0;							// Set initial object add count
var mesh_load_ct = 0;						// Set initial mesh load count
var meshes_ready = false;					// Start with meshes not ready

function addLights(scene_center, distance, intensity){

	//
	var i;
	var num_lights = svg_obj.bboxLight.length;
	var off = new Array(0, 0, 0);
	var adjust = 1.1;

	for(i = 0; i < num_lights; i++){

		// Set offset
		off = Array(adjust*svg_obj.bboxLight[i].x[0]*distance, adjust*svg_obj.bboxLight[i].x[1]*distance, adjust*svg_obj.bboxLight[i].x[2]*distance);

		// Set light source
		source = [scene_center[0]+off[0], scene_center[1]+off[1], scene_center[2]+off[2]]

		// Add light
		var light = new THREE.PointLight( svg_obj.bboxLight[i].col , 1.1*svg_obj.bboxLight[i].intensity, svg_obj.bboxLight[i].distance*distance);
		light.position.set(source[0], source[1], source[2]);
		scene.add( light );
		
		// Mark light position with sphere
		if(svg_obj.bboxLight[i].hidden == false){
			var sphereGeometry = new THREE.SphereGeometry(svg_obj.bboxLight[i].intensity*distance/20,10,10);
			var sphereMaterial = new THREE.MeshBasicMaterial({color: 0xffff00,opacity:1});
			var sphereMesh = new THREE.Mesh(sphereGeometry,sphereMaterial);
			sphereMesh.position.set(source[0], source[1], source[2]);
			scene.add(sphereMesh);
		}
	}
}

function addMeshToScene( geometry, materials ) {

//alert(Object.getOwnPropertyNames(geometry));

	var material;

	if(materials == undefined){
		material = new THREE.MeshLambertMaterial( { color: 0xF5F5F5 } );
	}else{
		material = materials;
	}

	// Set model opacity
	if(mesh_opacity < 1){
		material.transparent = true;
		material.opacity = mesh_opacity;
	}

	// Create mesh
	var model = new THREE.Mesh( geometry, material );

	// Set model name
	model.name = mesh_name;
	
	// Add to meshes
	meshes.push(model)

	// Add to scene
	scene.add( meshes[mesh_load_ct] );

	// If additional meshes, load next mesh
	if(mesh_load_ct+1 < svg_obj.mesh.length){

		// Advance count
		mesh_load_ct++;

		// Load next mesh
		loadNextMesh();

	}else{

		// Confirm that meshes are ready
		meshes_ready = true;
	}
}

function indexOfMax(arr) {
    if (arr.length === 0) {
        return -1;
    }

    var max = arr[0];
    var maxIndex = 0;

    for (var i = 1; i < arr.length; i++) {
        if (arr[i] > max) {
            maxIndex = i;
            max = arr[i];
        }
    }

    return maxIndex;
}

function loadAnimation() {

	if(svg_obj.animate == ''){
		animate = false;
		return;
	}

	// Set animate on
	animate = true;

	// Set animation times
	animation_times = svg_obj.animate.times;
}

function loadGeometries(){

	var arrowHelper, canvas, context, dir, geometry, i, j, n, line, material, mesh, num_seg, origin;
	var sprite, spriteMaterial, text, texture, text_length, text_size;
	
	//// Load lines
	if(svg_obj.line != undefined){

		// Get number of line segments
		var lines_length = svg_obj.line.length;

	//	alert(svg_obj.line[2].x);
		// Create each line
		for(i = 0; i < lines_length; i++){

			// Set line material
			material = new THREE.LineBasicMaterial({
				color: svg_obj.line[i].col,
				linewidth: svg_obj.line[i].lwd
			});

			// Set number of line segments
			num_seg = svg_obj.line[i].x.length / 3;
			svg_obj.line[i].nseg = num_seg;
		
			// Create new geometry
			geometry = new THREE.Geometry();

			// Add each segment
			for(j = 0; j < num_seg*3; j = j + 3){
				geometry.vertices.push(new THREE.Vector3( svg_obj.line[i].x[j], svg_obj.line[i].x[j+1], svg_obj.line[i].x[j+2] ));
			}

			// Create line
			line = new THREE.Line( geometry, material );

			// Check if position over time is specified
			if(svg_obj.line[i].x_tm != undefined){

				// Add transformed position over time
				line.x_tm = svg_obj.line[i].x_tm;

				// Add type and number to 
				update_obj.num.push(i);
				update_obj.type.push('line');
			}

			// Set name and number of segments
			line.name = 'line' + i;

			// Add to meshes
			lines.push(line)

			// Add to scene
			scene.add( lines[i] );
		}
	}

	//// Add text
	if(svg_obj.text != undefined){

		// Get number of text elements
		text_length = svg_obj.text.length;
	
		var canvas_text_res;

		for(i = 0; i < text_length; i++){

			// create a canvas element
			canvas = document.createElement('canvas');
			context = canvas.getContext('2d');

			// Set text
			text = svg_obj.text[i].labels;
		
			// Set resolution depending on absolute text size (between 35 and 5, increases with smaller values)
			canvas_text_res = 200;

			// Set text size
			text_size = 1.35*canvas_text_res;

			// Make canvas size a bit larger than font size so there is enough room
			canvas.height = nearestPow2(2*text_size);
			canvas.width = nearestPow2(0.9*text.length*text_size);

			context.font = text_size + "px Arial";
			context.textAlign = "center";
			context.fillStyle = svg_obj.text[i].col;
			context.fillText(text, canvas.width/2, canvas.height/2); 

			// Create text from canvas
			texture = new THREE.Texture(canvas) 
			texture.needsUpdate = true;
	  
			// Create sprite material from texture
			spriteMaterial = new THREE.SpriteMaterial( { map: texture } );

			// Create sprite
			sprite = new THREE.Sprite( spriteMaterial );

			// Set scale and position
			sprite_scale = svg_obj.text[i].size*(1/canvas_text_res)
			sprite.scale.set(canvas.width*sprite_scale,canvas.height*sprite_scale,1);
			sprite.position.set(svg_obj.text[i].x[0], svg_obj.text[i].x[1], svg_obj.text[i].x[2]);

			// Set name
			sprite.name = 'sprite' + i;

			// Add to sprites
			sprites.push(sprite)

			// Add to scene
			scene.add( sprite );
		}
	}

	//// Add arrows
	// Get number of arrows
	if(svg_obj.arrow != undefined){

		arrow_length = svg_obj.arrow.length;

		for(i = 0; i < arrow_length; i++){
		
			// Create direction vector
			dir = new THREE.Vector3( svg_obj.arrow[i].dir[0], svg_obj.arrow[i].dir[1], svg_obj.arrow[i].dir[2]);

			// Create origin vector
			origin = new THREE.Vector3( svg_obj.arrow[i].origin[0], svg_obj.arrow[i].origin[1], svg_obj.arrow[i].origin[2]);
	
			// Create arrow
			arrowHelper = new THREE.ArrowHelper( dir, origin, svg_obj.arrow[i].length, svg_obj.arrow[i].col, 
				svg_obj.arrow[i].len, svg_obj.arrow[i].len*0.7);

			// Set line width
			arrowHelper.line.material.linewidth = svg_obj.arrow[i].lwd;

			// Set name
			arrowHelper.name = 'arrow' + i;
		
			// Add initial rotation
			arrowHelper.initPosition = new Array(arrowHelper.position.x, arrowHelper.position.y, arrowHelper.position.z);
			arrowHelper.initRotation = new Array(arrowHelper.rotation.x, arrowHelper.rotation.y, arrowHelper.rotation.z);

			// Add to arrows
			arrows.push(arrowHelper)

			// Add to scene
			scene.add( arrowHelper );
		}
	}
	
	//// Add spheres
	if(svg_obj.sphere != undefined){

		num_objects = svg_obj.sphere.length;

		n = 0;
		for(i = 0; i < num_objects; i++){
		
			// If position is NA, skip
			if(svg_obj.sphere[i].x[0] == 'NA') continue;

			// Create sphere
			geometry = new THREE.SphereGeometry( radius=svg_obj.sphere[i].radius, widthSegments=svg_obj.sphere[i].wseg, heightSegments=svg_obj.sphere[i].hseg ) ;

			// material describes the surface of the shape
			//material = new THREE.MeshLambertMaterial( {color: svg_obj.sphere[i].col} ) ;
			material = new THREE.MeshPhongMaterial( {
						color: svg_obj.sphere[i].col,
						emissive: svg_obj.sphere[i].emissive
						//flatShading: true
						//side: THREE.DoubleSide,
					} )

			// mesh maps the material onto the geometry to make an object  
			mesh = new THREE.Mesh( geometry, material ) ;

			// position the mesh in space
			mesh.position.set( svg_obj.sphere[i].x[0], svg_obj.sphere[i].x[1], svg_obj.sphere[i].x[2] ) ;

			// Check if position over time is specified
			if(svg_obj.sphere[i].x_tm != undefined){

				// Add transformed position over time
				mesh.x_tm = svg_obj.sphere[i].x_tm;

				// Add type and number to 
				update_obj.num.push(n);
				update_obj.type.push('sphere');
			}

			// Set name
			mesh.name = 'sphere' + n;
		
			// add the mesh to the scene
			scene.add( mesh ) ;

			// Add to arrows
			spheres.push(mesh)

			// Add to scene
			scene.add( mesh );
			
			n++;
		}
	}
}

function loadNextMesh(){

	if(svg_obj.mesh == undefined){
		meshes_ready = true;
		return;
	}

	if(svg_obj.mesh[mesh_load_ct].src_idx == undefined){
		
		var geometry, i, material, mesh, num_faces, num_vertices, vertex, face;
		
		// Get mesh object
		mesh = svg_obj.mesh[mesh_load_ct];
		
		// Check if position over time is specified
		if(svg_obj.mesh[mesh_load_ct].position != undefined){

			// Add type and number to 
			update_obj.num.push(mesh_load_ct);
			update_obj.type.push('mesh');
		}

		// Create geometry
		geometry = new THREE.Geometry();

		if(mesh.parseModel){

			geometry = parseModel( mesh, geometry );
			material = new THREE.MeshLambertMaterial( { color:mesh.col, emissive: mesh.emissive } );

		}else{

			// Get vertices
			num_vertices = mesh.vertices.length;
			i = 0;
			while ( i < num_vertices ) {
				vertex = new THREE.Vector3();
				vertex.x = mesh.vertices[ i++ ];
				vertex.y = mesh.vertices[ i++ ];
				vertex.z = mesh.vertices[ i++ ];
				geometry.vertices.push(vertex);
			}

			num_faces = mesh.faces.length;
			i = 0;
			while ( i < num_faces ) {
				face = new THREE.Face3();
				face.a = mesh.faces[ i++ ];
				face.b = mesh.faces[ i++ ];
				face.c = mesh.faces[ i++ ];
				geometry.faces.push(face);
			}

			geometry.computeFaceNormals();
			if(svg_obj.mesh[mesh_load_ct].computeVN == true) geometry.computeVertexNormals();

			material = new THREE.MeshPhongMaterial( {
						color: mesh.col,
						emissive: mesh.emissive,
						side: THREE.DoubleSide,
						//flatShading: true
					} )
		}

		// Add mesh
		addMeshToScene(geometry, material);

	}else{

		// JSONLoader (buffer Geometry loader was not getting the indices right...)
		// Send next mesh after previous mesh is loaded so that names correspond
		// Couldn't figure out how to send name and other information to addMeshToScene in 
		// a way that ensures correspondence
		var loader = new THREE.JSONLoader();
		loader.load( app_dir[svg_obj.mesh[mesh_load_ct].src_idx] + '/' + svg_obj.mesh[mesh_load_ct].fname, addMeshToScene);
	
		// Set mesh name
		mesh_name = svg_obj.mesh[mesh_load_ct].name;
		mesh_opacity = svg_obj.mesh[mesh_load_ct].opacity;

//alert(svg_obj.mesh[mesh_load_ct].src_idx + ' ' + svg_obj.mesh[mesh_load_ct].fname)

		// Check if position over time is specified
		if(svg_obj.mesh[mesh_load_ct].position != undefined){

			// Add type and number to 
			update_obj.num.push(mesh_load_ct);
			update_obj.type.push('mesh');
		}
	}
}

function nearestPow2( aSize ){
	// https://bocoup.com/blog/find-the-closest-power-of-2-with-javascript
	return Math.pow( 2, Math.round( Math.log( aSize ) / Math.log( 2 ) ) ); 
}

function nearestTimeIndex( time, start, end, duration, nTimes ){
	
	//
	if(time < start) return(0);
	if(time > end) return(nTimes-1);
	
	// Find normalized time (0 to 1)
	var time_norm = (time-start) / duration;
	
	// Scale to -0.5, times-0.5
	var time_idx = Math.round(time_norm*(nTimes)-0.5);
	if(time_idx >= nTimes) time_idx = time_idx - 1;
	
	//document.getElementById("js_out").innerHTML += time_norm*(nTimes)-0.5 + '<br>';
	
	return(time_idx);
}

//
function onObjectsReady(){

	var i, j;

	// Set bounding box
	setBoundingBox();

	// Setup the camera
	camera = new THREE.PerspectiveCamera( fov=30, aspect=window.innerWidth / window.innerHeight, near=0.1, far=bbox_size*20 );

	// Add camera controls
	controls = new THREE.TrackballControls( camera );

	//controls.addEventListener( 'change', render );

	controls.rotateSpeed = 1.0;
	controls.zoomSpeed = 1.2;
	controls.panSpeed = 0.2;

	// Set camera to include all shapes
	updateCameraPosition();

	// Add lights
	addLights(bbox_center, bbox_size, 1.5*0.95)
}

// Set frames per second for motion
function onReady(){

	// Key events
	document.body.onkeyup = function(e){

		// Spacebar event
		if(e.keyCode == 32){
			if(anim_pause) { 
				anim_pause = false; 
			}else{ 
				anim_pause_time = Date.now();
				anim_pause_start = anim_start;
				anim_pause = true; 
			}
		}
	}

	// Setup a new scene
	scene = new THREE.Scene();
	
	// Set background
	scene.background = new THREE.Color( bg_col );

	// Get container
	var container = document.getElementById( "container" );

	// Add stats box in top left corner
	//stats = new Stats();
	//container.appendChild( stats.dom );

	// Setup the renderer
	renderer = new THREE.WebGLRenderer( {antialias: true } );
	renderer.setSize(window.innerWidth, window.innerHeight);
	document.body.appendChild(renderer.domElement);

	// Start mesh loading
	loadNextMesh();

	// Load coordinate objects
	loadGeometries();
	
	// Load animation from string
	loadAnimation();

	// Try rendering every 10 msec until all objects are finished loaded
	try_render_int = setInterval(tryRender, 10);
}

function parseModel( json, geometry ) {

	function isBitSet( value, position ) {

		return value & ( 1 << position );

	}

	var i, j, fi,

		offset, zLength,

		colorIndex, normalIndex, uvIndex, materialIndex,

		type,
		isQuad,
		hasMaterial,
		hasFaceVertexUv,
		hasFaceNormal, hasFaceVertexNormal,
		hasFaceColor, hasFaceVertexColor,

		vertex, face, faceA, faceB, hex, normal,

		uvLayer, uv, u, v,

		faces = json.faces,
		vertices = json.vertices,
		normals = json.normals,
		colors = json.colors,

		scale = json.scale,

		nUvLayers = 0;


	if ( json.uvs !== undefined ) {

		// disregard empty arrays

		for ( i = 0; i < json.uvs.length; i ++ ) {

			if ( json.uvs[ i ].length ) nUvLayers ++;

		}

		for ( i = 0; i < nUvLayers; i ++ ) {

			geometry.faceVertexUvs[ i ] = [];

		}

	}

	offset = 0;
	zLength = vertices.length;

	while ( offset < zLength ) {

		vertex = new THREE.Vector3();

		vertex.x = vertices[ offset ++ ] * scale;
		vertex.y = vertices[ offset ++ ] * scale;
		vertex.z = vertices[ offset ++ ] * scale;

		geometry.vertices.push( vertex );

	}

	offset = 0;
	zLength = faces.length;

	while ( offset < zLength ) {

		type = faces[ offset ++ ];

		isQuad = isBitSet( type, 0 );
		hasMaterial = isBitSet( type, 1 );
		hasFaceVertexUv = isBitSet( type, 3 );
		hasFaceNormal = isBitSet( type, 4 );
		hasFaceVertexNormal = isBitSet( type, 5 );
		hasFaceColor = isBitSet( type, 6 );
		hasFaceVertexColor = isBitSet( type, 7 );

		// console.log("type", type, "bits", isQuad, hasMaterial, hasFaceVertexUv, hasFaceNormal, hasFaceVertexNormal, hasFaceColor, hasFaceVertexColor);
		if ( isQuad ) {

			faceA = new THREE.Face3();
			faceA.a = faces[ offset ];
			faceA.b = faces[ offset + 1 ];
			faceA.c = faces[ offset + 3 ];

			faceB = new THREE.Face3();
			faceB.a = faces[ offset + 1 ];
			faceB.b = faces[ offset + 2 ];
			faceB.c = faces[ offset + 3 ];

			offset += 4;

			if ( hasMaterial ) {

				materialIndex = faces[ offset ++ ];
				faceA.materialIndex = materialIndex;
				faceB.materialIndex = materialIndex;

			}

			// to get face <=> uv index correspondence

			fi = geometry.faces.length;

			if ( hasFaceVertexUv ) {

				for ( i = 0; i < nUvLayers; i ++ ) {

					uvLayer = json.uvs[ i ];

					geometry.faceVertexUvs[ i ][ fi ] = [];
					geometry.faceVertexUvs[ i ][ fi + 1 ] = [];

					for ( j = 0; j < 4; j ++ ) {

						uvIndex = faces[ offset ++ ];

						u = uvLayer[ uvIndex * 2 ];
						v = uvLayer[ uvIndex * 2 + 1 ];

						uv = new THREE.Vector2( u, v );

						if ( j !== 2 ) geometry.faceVertexUvs[ i ][ fi ].push( uv );
						if ( j !== 0 ) geometry.faceVertexUvs[ i ][ fi + 1 ].push( uv );

					}

				}

			}

			if ( hasFaceNormal ) {

				normalIndex = faces[ offset ++ ] * 3;

				faceA.normal.set(
					normals[ normalIndex ++ ],
					normals[ normalIndex ++ ],
					normals[ normalIndex ]
				);

				faceB.normal.copy( faceA.normal );

			}

			if ( hasFaceVertexNormal ) {

				for ( i = 0; i < 4; i ++ ) {

					normalIndex = faces[ offset ++ ] * 3;

					normal = new THREE.Vector3(
						normals[ normalIndex ++ ],
						normals[ normalIndex ++ ],
						normals[ normalIndex ]
					);


					if ( i !== 2 ) faceA.vertexNormals.push( normal );
					if ( i !== 0 ) faceB.vertexNormals.push( normal );

				}

			}


			if ( hasFaceColor ) {

				colorIndex = faces[ offset ++ ];
				hex = colors[ colorIndex ];

				faceA.color.setHex( hex );
				faceB.color.setHex( hex );

			}


			if ( hasFaceVertexColor ) {

				for ( i = 0; i < 4; i ++ ) {

					colorIndex = faces[ offset ++ ];
					hex = colors[ colorIndex ];

					if ( i !== 2 ) faceA.vertexColors.push( new THREE.Color( hex ) );
					if ( i !== 0 ) faceB.vertexColors.push( new THREE.Color( hex ) );

				}

			}

			geometry.faces.push( faceA );
			geometry.faces.push( faceB );

		} else {

			face = new THREE.Face3();
			face.a = faces[ offset ++ ];
			face.b = faces[ offset ++ ];
			face.c = faces[ offset ++ ];

			if ( hasMaterial ) {

				materialIndex = faces[ offset ++ ];
				face.materialIndex = materialIndex;

			}

			// to get face <=> uv index correspondence

			fi = geometry.faces.length;

			if ( hasFaceVertexUv ) {

				for ( i = 0; i < nUvLayers; i ++ ) {

					uvLayer = json.uvs[ i ];

					geometry.faceVertexUvs[ i ][ fi ] = [];

					for ( j = 0; j < 3; j ++ ) {

						uvIndex = faces[ offset ++ ];

						u = uvLayer[ uvIndex * 2 ];
						v = uvLayer[ uvIndex * 2 + 1 ];

						uv = new THREE.Vector2( u, v );

						geometry.faceVertexUvs[ i ][ fi ].push( uv );

					}

				}

			}

			if ( hasFaceNormal ) {

				normalIndex = faces[ offset ++ ] * 3;

				face.normal.set(
					normals[ normalIndex ++ ],
					normals[ normalIndex ++ ],
					normals[ normalIndex ]
				);

			}

			if ( hasFaceVertexNormal ) {

				for ( i = 0; i < 3; i ++ ) {

					normalIndex = faces[ offset ++ ] * 3;

					normal = new THREE.Vector3(
						normals[ normalIndex ++ ],
						normals[ normalIndex ++ ],
						normals[ normalIndex ]
					);

					face.vertexNormals.push( normal );

				}

			}


			if ( hasFaceColor ) {

				colorIndex = faces[ offset ++ ];
				face.color.setHex( colors[ colorIndex ] );

			}


			if ( hasFaceVertexColor ) {

				for ( i = 0; i < 3; i ++ ) {

					colorIndex = faces[ offset ++ ];
					face.vertexColors.push( new THREE.Color( colors[ colorIndex ] ) );

				}

			}

			geometry.faces.push( face );
		}
	}
	
	return geometry;
}

function setBoundingBox () {

	// Geometry is the same as when read in - not updated with position/rotation
	// Compute bounding box (fills 'boundingBox' property, which is null by default)
	// alert(Object.getOwnPropertyNames());
	// e.g. meshes[0].geometry.boundingBox.min.x

	// Set initial values to not return NaN
	var minX = minY = minZ = Infinity;
	var maxX = maxY = maxZ = -Infinity;

	// If no animation, just iterate once through update
	if(animate == false){
		var anim_ntimes = 1;
		var time_int = 1;
	}else{
		var anim_ntimes = animation_ntimes;
		var time_int = Math.round(animation_ntimes/5);
	}

	for(j = 0; j < anim_ntimes; j = j + time_int){

		// Transform shapes to first animation index
		updateShapes(j);

		// Compute bounding box of transformed objects
		for(i = 0; i <= spheres.length-1; i++){

			var bbox = new THREE.Box3().setFromObject( spheres[i] );

			// Update min and max
			minX = Math.min (minX, bbox.min.x);
			minY = Math.min (minY, bbox.min.y);
			minZ = Math.min (minZ, bbox.min.z);
			maxX = Math.max (maxX, bbox.max.x);
			maxY = Math.max (maxY, bbox.max.y);
			maxZ = Math.max (maxZ, bbox.max.z);
		}

		// Compute bounding box of transformed objects
		for(i = 0; i <= meshes.length-1; i++){

			var bbox = new THREE.Box3().setFromObject( meshes[i] );

			// Update min and max
			minX = Math.min (minX, bbox.min.x);
			minY = Math.min (minY, bbox.min.y);
			minZ = Math.min (minZ, bbox.min.z);
			maxX = Math.max (maxX, bbox.max.x);
			maxY = Math.max (maxY, bbox.max.y);
			maxZ = Math.max (maxZ, bbox.max.z);
		}

		// Compute bounding box of transformed objects
		for(i = 0; i <= lines.length-1; i++){

			var bbox = new THREE.Box3().setFromObject( lines[i] );

			// Update min and max
			minX = Math.min (minX, bbox.min.x);
			minY = Math.min (minY, bbox.min.y);
			minZ = Math.min (minZ, bbox.min.z);
			maxX = Math.max (maxX, bbox.max.x);
			maxY = Math.max (maxY, bbox.max.y);
			maxZ = Math.max (maxZ, bbox.max.z);
		}

		// Compute bounding box of transformed objects
		for(i = 0; i <= sprites.length-1; i++){

			var bbox = new THREE.Box3().setFromObject( sprites[i] );

			// Update min and max
			minX = Math.min (minX, bbox.min.x);
			minY = Math.min (minY, bbox.min.y);
			minZ = Math.min (minZ, bbox.min.z);
			maxX = Math.max (maxX, bbox.max.x);
			maxY = Math.max (maxY, bbox.max.y);
			maxZ = Math.max (maxZ, bbox.max.z);
		}
	}

	// Get bounding box dimensions, size, and center
	var bbox_min = new THREE.Vector3 (minX, minY, minZ);
    var bbox_max = new THREE.Vector3 (maxX, maxY, maxZ);
    var bbox_new = new THREE.Box3 (bbox_min, bbox_max);
	bbox_scale = [ bbox_new.max.x-bbox_new.min.x, bbox_new.max.y-bbox_new.min.y, bbox_new.max.z-bbox_new.min.z ];
	bbox_center = [ bbox_scale[0]/2 + bbox_new.min.x, bbox_scale[1]/2 + bbox_new.min.y, bbox_scale[2]/2 + bbox_new.min.z ];
	bbox_size = (bbox_scale[0] + bbox_scale[1] + bbox_scale[2]) / 3;

//alert(minX + ',' + minY + ',' + minZ)
//alert(maxX + ',' + maxY + ',' + maxZ)

	// Draw bounding box as wireframe
	if(false){
		var geometry = new THREE.BoxGeometry( bbox_scale[0], bbox_scale[1], bbox_scale[2] );
		var geo = new THREE.EdgesGeometry( geometry ); // or WireframeGeometry( geometry )
		var mat = new THREE.LineBasicMaterial( { color: 0xffffff, linewidth: 1 } );
		var wireframe = new THREE.LineSegments( geo, mat );
		wireframe.position.set( bbox_center[0], bbox_center[1], bbox_center[2] );
		//scene.add( wireframe );
	}
}

function tryRender () {
	
	// If these are not loaded, do not render
	if(meshes_ready == false) return;

	// Stop running tryRender
	clearInterval(try_render_int);

	// Set viewing frame, camera and lights
	onObjectsReady();

	// Render scene
	render();
}

// After loading JSON from our file, we add it to the scene
var render = function () {

	requestAnimationFrame(render);

	if(animate){

		if(anim_pause){

			// Increase anim_start so that when animation is unpaused it starts where it "left off"
			anim_start = anim_pause_start + (Date.now() - anim_pause_time);

		}else{

			// Get elapsed time in ms
			var elapsed_ms = Date.now() - anim_start;
	
			// If exceeds animation duration, reset clock
			if(elapsed_ms > animation_duration){
				anim_start = Date.now();
				elapsed_ms = 0;
			}

			// Find closest time point in animation
			time_index = nearestTimeIndex( elapsed_ms, animation_start, animation_end, animation_duration, animation_ntimes);

			// Print clock
			if(show_clock){
				document.getElementById( "clock" ).innerHTML = Math.round(elapsed_ms / 100)*100 + " ms";
				document.getElementById( "idx" ).innerHTML = time_index;
				document.getElementById( "time" ).innerHTML = Math.round((elapsed_ms*play_speed) / 100)*100 + " ms";
			}

			// Update shapes
			updateShapes(time_index);
		}
	}

	//stats.update();
	controls.update();

	renderer.render(scene, camera);
};

function updateCameraPosition(){

	// Get screen dimensions
	var screen_width = window.innerWidth;
	var screen_height = window.innerHeight;

	// Set initial camera distance and interval
	var camera_dist = bbox_scale[2]+bbox_size*0.01;
	var camera_dist_int = bbox_size*0.2;
	var margin = 50;
	var n = 0;
	var n_limit = 30;

	// Set initial camera
	// Projection will not use updated camera position without rendering
	camera.aspect = screen_width / screen_height;
	camera.updateProjectionMatrix();
	renderer.setSize( screen_width, screen_height );
	camera.position.set(bbox_center[0], bbox_center[1], bbox_center[2]+camera_dist);
	renderer.render(scene, camera);
	
	// Corners 3d position
	var corner1 = {x:bbox_center[0] + bbox_scale[0]/2, y:bbox_center[1] + bbox_scale[1]/2, z:bbox_center[2] + bbox_scale[2]/2};
	var corner4 = {x:bbox_center[0] - bbox_scale[0]/2, y:bbox_center[1] - bbox_scale[1]/2, z:bbox_center[2] + bbox_scale[2]/2};

	var widthHalf = screen_width / 2, heightHalf = screen_height / 2;
	var box_width, box_height;

	while(n < n_limit){
		
		// Project corners
		var corner1_proj = new THREE.Vector3(corner1.x, corner1.y, corner1.z).project(camera);
		var corner4_proj = new THREE.Vector3(corner4.x, corner4.y, corner4.z).project(camera);

		corner1_proj.x = ( corner1_proj.x * widthHalf ) + widthHalf;
		corner1_proj.y = - ( corner1_proj.y * heightHalf ) + heightHalf;
		corner4_proj.x = ( corner4_proj.x * widthHalf ) + widthHalf;
		corner4_proj.y = - ( corner4_proj.y * heightHalf ) + heightHalf;

		box_width = Math.abs(corner4_proj.x-corner1_proj.x);
		box_height = Math.abs(corner1_proj.y-corner4_proj.y);
		
		// 
		if(box_width > (screen_width-margin) || box_height > (screen_height-margin)){

			// Shape exceeds boundaries, move camera further away
			camera_dist = camera_dist + camera_dist_int;

			// Update camera
			camera.position.set(bbox_center[0], bbox_center[1], bbox_center[2]+camera_dist);
			renderer.render(scene, camera);

		}else{
		
			// If last run and is within boundaries, stop
			if(n == n_limit - 1) break;
			
			// Replace camera to previous distance
			camera_dist = camera_dist - camera_dist_int;
			
			// Shape is within boundaries, try moving camera further away at smaller interval
			camera_dist_int = camera_dist_int / 2;

			// Update camera
			camera.position.set(bbox_center[0], bbox_center[1], bbox_center[2]+camera_dist);
			renderer.render(scene, camera);
		}

		n++;
	}

	//alert(box_width + ' (' + screen_width + '),' + box_height + ' (' + screen_height + ') ' + n);

	// Set where the camera is targeted
	controls.target.set( bbox_center[0], bbox_center[1], bbox_center[2]);
	controls.update();
}

function updateShapes(time_index){

	var num, type, i, j, k, set_prop, set_val, tracks_length;

	//// Apply animation transformations
	if(animate == true){

		//// Apply object updates/transformations
		var update_obj_length = update_obj.num.length;

		for (i = 0; i < update_obj_length; i++){
		
			if(update_obj.type[i] == 'mesh'){
				meshes[update_obj.num[i]].position.x = svg_obj.mesh[update_obj.num[i]].position[time_index][0];
				meshes[update_obj.num[i]].position.y = svg_obj.mesh[update_obj.num[i]].position[time_index][1];
				meshes[update_obj.num[i]].position.z = svg_obj.mesh[update_obj.num[i]].position[time_index][2];

				meshes[update_obj.num[i]].rotation.x = svg_obj.mesh[update_obj.num[i]].rotation[time_index][0];
				meshes[update_obj.num[i]].rotation.y = svg_obj.mesh[update_obj.num[i]].rotation[time_index][1];
				meshes[update_obj.num[i]].rotation.z = svg_obj.mesh[update_obj.num[i]].rotation[time_index][2];
			}

			if(update_obj.type[i] == 'sphere'){
				spheres[update_obj.num[i]].position.x = spheres[update_obj.num[i]].x_tm[time_index][0];
				spheres[update_obj.num[i]].position.y = spheres[update_obj.num[i]].x_tm[time_index][1];
				spheres[update_obj.num[i]].position.z = spheres[update_obj.num[i]].x_tm[time_index][2];
			}

			if(update_obj.type[i] == 'line'){
				// Update each segment
				k = 0;
				for(j = 0; j < svg_obj.line[i].nseg*3; j = j + 3){
					lines[update_obj.num[i]].geometry.vertices[k].x = lines[update_obj.num[i]].x_tm[time_index][j];
					lines[update_obj.num[i]].geometry.vertices[k].y = lines[update_obj.num[i]].x_tm[time_index][j+1];
					lines[update_obj.num[i]].geometry.vertices[k].z = lines[update_obj.num[i]].x_tm[time_index][j+2];
					k++;
				}
				// Update vertices
				lines[update_obj.num[i]].geometry.verticesNeedUpdate = true;
			}
		}
	}
}

window.onresize = function () {
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	controls.handleResize();
	renderer.setSize( window.innerWidth, window.innerHeight );
};