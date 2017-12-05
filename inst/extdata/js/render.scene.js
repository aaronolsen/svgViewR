// Start arrays for drawn objects
var lines = new Array( );
var meshes = new Array( );
var sprites = new Array( );
var arrows = new Array( );

// Declare global variables
var animation, animations, anim_pause_time, camera, controls, mesh_name, renderer, scene, stats;

var screen_width = window.innerWidth;		// Set screen dimensions
var screen_height = window.innerHeight;		// Set screen dimensions
var anim_start = Date.now();				// Set animation start time
var anim_pause = false;						// Start with animation paused
var obj_add_ct = 0;							// Set initial object add count
var canvas_text_res = 5;					// Set canvas text resolution (factor by which canvas is scaled up then down)
var font_scale = 1.38*canvas_text_res;		// Set font scaling
var mesh_load_ct = 0;						// Set initial mesh load count
var meshes_ready = false;					// Start with meshes not ready

function addLights(scene_center, distance, intensity){

	var ldistance = distance*3;
	var i;
	var off = new Array(0, 0, 0);
	
	for(i = 0; i <= 3; i++){
	
		if(i == 0) off = Array(distance, distance, distance);
		if(i == 1) off = Array(-distance, distance, distance);
		if(i == 2) off = Array(-distance, -distance, -distance);
		if(i == 3) off = Array(distance, -distance, -distance);

		// Set light source
		source = [scene_center[0]+off[0], scene_center[1]+off[1], scene_center[2]+off[2]]

		// Add lights lights
		var light = new THREE.PointLight( 0xFFFFDD , intensity, ldistance);
		light.position.set(source[0], source[1], source[2]);
		scene.add( light );

		if(false){
			var sphereGeometry = new THREE.SphereGeometry(distance/20,10,10);
			var sphereMaterial = new THREE.MeshBasicMaterial({color: 0xffff00,opacity:1});
			var sphereMesh = new THREE.Mesh(sphereGeometry,sphereMaterial);
			sphereMesh.position.set(source[0], source[1], source[2]);
			scene.add(sphereMesh);
		}
	}
}

function addMeshToScene( geometry, materials ) {

	//var material = new THREE.MeshFaceMaterial(materials);
	var material = new THREE.MeshLambertMaterial( { color: 0xF5F5F5 } );

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
	
		// Run any script after meshes are loaded
		onMeshesReady();
	}
}

function loadGeometries(){

	var arrowHelper, canvas, context, dir, geometry, i, j, line, material, num_seg, origin;
	var sprite, spriteMaterial, text, texture, text_length, text_size;
	
	//// Load lines
	// Get number of line segments
	var lines_length = svg_obj.lines.length;

	// Create each line
	for(i = 0; i < lines_length; i++){

		// Set line material
		material = new THREE.LineBasicMaterial({
			color: svg_obj.lines[i].col,
			linewidth: svg_obj.lines[i].lwd
		});

		// Set number of line segments
		num_seg = svg_obj.lines[i].x.length / 3;
		
		// Create new geometry
		geometry = new THREE.Geometry();

		// Add each segment
		for(j = 0; j < num_seg*3; j = j + 3){
			geometry.vertices.push(new THREE.Vector3( svg_obj.lines[i].x[j], svg_obj.lines[i].x[j+1], svg_obj.lines[i].x[j+2] ));
		}

		// Create line
		line = new THREE.Line( geometry, material );

		// Set name
		line.name = 'line' + i;

		// Add to meshes
		lines.push(line)

		// Add to scene
		scene.add( lines[i] );
	}

	//// Add text
	// Get number of text elements
	text_length = svg_obj.text.length;

	for(i = 0; i < text_length; i++){

		// create a canvas element
		canvas = document.createElement('canvas');
		context = canvas.getContext('2d');

		// Set text
		text = svg_obj.text[i].labels;
		
		// Set text size
		text_size = font_scale*svg_obj.text[i].size;

		// Make canvas size a bit larger than font size so there is enough room
		canvas.height = 1.5*text_size;
		canvas.width = 0.6*text.length*text_size;

		context.font = text_size + "px Arial";
		context.textAlign = "center";
		context.fillText(text, canvas.width/2, canvas.height/2); 

		// Create text from canvas
		texture = new THREE.Texture(canvas) 
		texture.needsUpdate = true;
	  
		// Create sprite material from texture
		spriteMaterial = new THREE.SpriteMaterial( { map: texture, useScreenCoordinates: false } );

		// Create sprite
		sprite = new THREE.Sprite( spriteMaterial );

		// Set scale and position
		sprite_scale = 2 / canvas.height
		sprite.scale.set(canvas.width*sprite_scale,canvas.height*sprite_scale,1);
		sprite.position.set(svg_obj.text[i].x[0], svg_obj.text[i].x[1], svg_obj.text[i].x[2]);

		// Set name
		sprite.name = 'sprite' + i;

		// Add to sprites
		sprites.push(sprite)

		// Add to scene
		scene.add( sprite );
	}


	//// Add arrows
	// Get number of arrows
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

		// Add to arrows
		arrows.push(arrowHelper)

		// Add to scene
		scene.add( arrowHelper );
	}
}

function loadNextMesh(){

	// JSONLoader (buffer Geometry loader was not getting the indices right...)
	// Send next mesh after previous mesh is loaded so that names correspond
	// Couldn't figure out how to send name and other information to addMeshToScene in 
	// a way that ensures correspondence
	var loader = new THREE.JSONLoader();
	loader.load( app_dir[svg_obj.mesh[mesh_load_ct].src_idx] + '/' + svg_obj.mesh[mesh_load_ct].fname, addMeshToScene);
	
	// Set mesh name
	//mesh_name = mesh_names[mesh_load_ct];
	mesh_name = svg_obj.mesh[obj_add_ct].name;
}

//
function onMeshesReady(){
	
	var i, j;
	
	if(false){
		anim_pause_time = Date.now();
		anim_pause_start = anim_start;
		anim_pause = true; 
	}

	// Geometry is the same as when read in - not updated with position/rotation
	// Compute bounding box (fills 'boundingBox' property, which is null by default)
	// alert(Object.getOwnPropertyNames());
	// e.g. meshes[0].geometry.boundingBox.min.x

	// Set initial values to not return NaN
	var minX = minY = minZ = Infinity;
	var maxX = maxY = maxZ = -Infinity;

	//
	var time_int = Math.round(animation.ntimes/5);
	for(j = 0; j < animation.ntimes; j = j + time_int){

		// Transform shapes to first animation index
		updateShapes(j);

		// Compute bounding box of transformed objects
		for(i = 0; i <= mesh_load_ct; i++){

			var bbox = new THREE.Box3().setFromObject( meshes[i] );

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
	var bbox_scale = [ bbox_new.max.x-bbox_new.min.x, bbox_new.max.y-bbox_new.min.y, bbox_new.max.z-bbox_new.min.z ];
	var bbox_center = [ bbox_scale[0]/2 + bbox_new.min.x, bbox_scale[1]/2 + bbox_new.min.y, bbox_scale[2]/2 + bbox_new.min.z ];
	var bbox_size = (bbox_scale[0] + bbox_scale[1] + bbox_scale[2]) / 3;
	
	// Draw bounding box as wireframe
	var geometry = new THREE.BoxGeometry( bbox_scale[0], bbox_scale[1], bbox_scale[2] );
	var geo = new THREE.EdgesGeometry( geometry ); // or WireframeGeometry( geometry )
	var mat = new THREE.LineBasicMaterial( { color: 0xffffff, linewidth: 1 } );
	var wireframe = new THREE.LineSegments( geo, mat );
	wireframe.position.set( bbox_center[0], bbox_center[1], bbox_center[2] );
	//scene.add( wireframe );

	// Set camera position
	camera_dist = 3*bbox_size;
	camera.position.set(bbox_center[0]+camera_dist/2, bbox_center[1]+camera_dist/2, bbox_center[2]);
	
	// Set where the camera is targeted
	controls.target.set( bbox_center[0], bbox_center[1], bbox_center[2] );
	controls.update();

	// Add lights
	addLights(bbox_center, bbox_size, 1)
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
	
	//
	scene.background = new THREE.Color( bg_col );

	// Setup the camera
	camera = new THREE.PerspectiveCamera( 30, screen_width / screen_height, 1, 10000 );

	// Add camera controls
	controls = new THREE.OrbitControls( camera );

	// Get container
	var container = document.getElementById( "container" );

	// Add stats box in top left corner
	stats = new Stats();
	container.appendChild( stats.dom );

	// Setup the renderer
	renderer = new THREE.WebGLRenderer( {antialias: true } );
	renderer.setSize(window.innerWidth, window.innerHeight);
	document.body.appendChild(renderer.domElement);

	// Start mesh loading
	loadNextMesh();
	
	// Load coordinate objects
	loadGeometries();
	
	// Load animation from file
	//var loader = new THREE.FileLoader();
	//loader.load( mod_dir + '/' + anim_file, load_animation);

	// Load animation from string
	load_animation(tm_str);

	// Try rendering every 10 msec until all objects are finished loaded
	try_render_int = setInterval(tryRender, 10);
}

function tryRender () {
	
	// If these are not loaded, do not render
	if(animation == undefined) return;
	if(meshes_ready == false) return;

	// Stop running tryRender
	clearInterval(try_render_int);

	// Render scene
	render();
}

// After loading JSON from our file, we add it to the scene
var render = function () {

	requestAnimationFrame(render);

	if(anim_pause){

		// Increase anim_start so that when animation is unpaused it starts where it "left off"
		anim_start = anim_pause_start + (Date.now() - anim_pause_time);

	}else{

		// Get elapsed time in ms
		var elapsed_ms = Date.now() - anim_start;
	
		// If exceeds animation duration, reset clock
		if(elapsed_ms > animation.duration){
			anim_start = Date.now();
			elapsed_ms = 0;
		}

		// Find closest time point in animation
		time_index = nearestTimeIndex( elapsed_ms, animation.start, animation.end, animation.duration, animation.ntimes);

		// Print clock
		document.getElementById( "clock" ).innerHTML = Math.round(elapsed_ms / 100)*100 + " ms";
		document.getElementById( "idx" ).innerHTML = time_index;
		document.getElementById( "time" ).innerHTML = Math.round((elapsed_ms*play_speed) / 100)*100 + " ms";

		// Update shapes
		updateShapes(time_index);
	}
	
	stats.update();
	
	renderer.render(scene, camera);
};

function updateShapes(time_index){

	var apply_idx, set_prop, set_val;

	// Go through each track
	var tracks_length = animation.tracks.length;
	for (var i = 0; i < tracks_length; i++){

		// Get name of element to apply animation to
		apply_idx = animation.tracks[i].apply_idx;

		// If mesh name in animation file not found, continue
		if(apply_idx == undefined) continue;

		// Get property to set
		set_prop = animation.tracks[i].set;

		// Get value to set
		set_val = animation.tracks[i].keys[time_index].value;

		if(set_prop == 'position'){
			meshes[apply_idx].position.x = set_val[0];
			meshes[apply_idx].position.y = set_val[1];
			meshes[apply_idx].position.z = set_val[2];
		}

		if(set_prop == 'rotation'){
			meshes[apply_idx].rotation.x = set_val[0];
			meshes[apply_idx].rotation.y = set_val[1];
			meshes[apply_idx].rotation.z = set_val[2];
		}
	}
}

window.onresize = function () {
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	renderer.setSize( window.innerWidth, window.innerHeight );
};