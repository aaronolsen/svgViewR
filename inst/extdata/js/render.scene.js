// Start arrays for drawn objects
var animation_times = new Array( );
var arrows = new Array( );
var lines = new Array( );
var meshes = new Array( );
var images = new Array( );
var textures = new Array( );
var spheres = new Array( );
var sprites = new Array( );

// Declare global variables
var anim_index, anim_start, animations, anim_pause_time, camera, controls;
var elapsed_ms, image_name, image_obj;
var image_opacity, mesh_name, mesh_opacity, mesh_color, renderer, scene, stats;
var update_obj = {
    num: new Array(),
    type: new Array()
};
var deform_obj = {
    num: new Array(),
    type: new Array()
};

var bottom_frame_start_y = 0;
var deform = false;							// Whether to deform any shapes
var obj_add_ct = 0;							// Set initial object add count
var mesh_load_ct = 0;						// Set initial mesh load count
var image_idx_ct = 0;						// 
var inactive_since = 0;						// Keep track of time for which browser is inactive
var inactive_start_time = Date.now();		// Set initial inactive start time
var max_anim_plays_to_pause = 10;			// Max number of animation plays before pausing render
var n_anim_plays = 0;						// Count number of animation plays since render unpaused
var render_pause = false;					// Whether to pause renderer
var texture_idx_ct = 0;						// 
var texture_load_ct = 0;					// 
var save_as_img_ct = 0;
var meshes_ready = false;					// Initially meshes not ready
var images_ready = false;					// Initially images not ready
var image_textures_ready = false;

function fillArray(value, len) {
  if (len == 0) return [];
  var a = [value];
  while (a.length * 2 <= len) a = a.concat(a);
  if (a.length < len) a = a.concat(a.slice(0, len - a.length));
  return a;
}

var anim_index = fillArray(0, n_timelines)

function addFirstTexture(texture){

	var geometry, material, plane;

	// Add to textures
	textures[texture_idx_ct].push(texture);

	// Parse mesh geometry
	geometry = parseMeshGeometry(svg_obj.image[image_idx_ct]);
		
	// Get material from loaded texture
	material = new THREE.MeshBasicMaterial({map: texture, side: THREE.DoubleSide});

	plane = new THREE.Mesh( geometry, material );
	scene.add( plane );

	// Set plane opacity
	if(svg_obj.image[image_idx_ct].opacity.length > 1){
		material.transparent = true;
		material.opacity = svg_obj.image[image_idx_ct].opacity[1];
	}else{
		if(svg_obj.image[image_idx_ct].opacity < 1){
			material.transparent = true;
			material.opacity = svg_obj.image[image_idx_ct].opacity;
		}
	}

	// Set plane name
	plane.name = svg_obj.image[image_idx_ct].name;
	
	// Add to images
	images.push(plane)
	
	// Add to scene
	scene.add( images[image_idx_ct] );

	// Advance indices once all are loaded
	texture_idx_ct++;
	image_idx_ct++;

	// Move to next image
	loadFirstImageTexture();
}

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
			var sphereGeometry = new THREE.SphereGeometry(svg_obj.bboxLight[i].intensity*distance/10,200,200);
			//var sphereGeometry = new THREE.SphereGeometry(10,200,200);
			var sphereMaterial = new THREE.MeshBasicMaterial({color: 0xffff00,opacity:1});
			var sphereMesh = new THREE.Mesh(sphereGeometry,sphereMaterial);
			sphereMesh.position.set(source[0], source[1], source[2]);
			scene.add(sphereMesh);
		}
	}
}

function addTexture( texture ) {

	// Add to textures
	textures[texture_idx_ct].push(texture);

	// If additional textures, load next texture
	if(texture_load_ct+1 < svg_obj.image[image_idx_ct].fname.length){

		// Advance count
		texture_load_ct++;

		// Load next texture
		loadNextTexture();

	}else{

		// Advance indices once all are loaded
		texture_load_ct = 1;
		texture_idx_ct++;
		image_idx_ct++;
		
		// Move to next image
		loadSubsequentTextures();
		
	}
}

function addMeshToScene( geometry, materials ) {

//alert(Object.getOwnPropertyNames(geometry));

	var material;

	if(materials == undefined){
		material = new THREE.MeshLambertMaterial( { color: mesh_color } );
	}else{
		material = materials;
	}

	// Set model opacity
	if(mesh_opacity.length > 1){
		material.transparent = true;
		material.opacity = mesh_opacity[1];
		material.depthTest = mesh_depthTest;
	}else{
		if(mesh_opacity < 1){
			material.transparent = true;
			material.opacity = mesh_opacity;
			material.depthTest = mesh_depthTest;
		}
	}

	// Create mesh
	var model = new THREE.Mesh( geometry, material );

	// Set model name
	model.name = mesh_name;
	
	// Add to meshes
	meshes.push(model)

	// Add to scene
	scene.add( meshes[mesh_load_ct] );
	
	// Apply any initial transformations
	if(svg_obj.mesh[mesh_load_ct].iposition != undefined){

		meshes[mesh_load_ct].position.x = svg_obj.mesh[mesh_load_ct].iposition[0];
		meshes[mesh_load_ct].position.y = svg_obj.mesh[mesh_load_ct].iposition[1];
		meshes[mesh_load_ct].position.z = svg_obj.mesh[mesh_load_ct].iposition[2];

		meshes[mesh_load_ct].rotation.x = svg_obj.mesh[mesh_load_ct].irotation[0];
		meshes[mesh_load_ct].rotation.y = svg_obj.mesh[mesh_load_ct].irotation[1];
		meshes[mesh_load_ct].rotation.z = svg_obj.mesh[mesh_load_ct].irotation[2];
	}
	
	// Set visibility to true by default
	svg_obj.mesh[mesh_load_ct].visible = true;

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

function changeAnimationSpeed(speed, num){

	var i;

	if(speed == 0) return;

	// Set play speed
	play_speed = speed;
	
	// Update times based on new play speed
	for(i=0; i < animation_times.length; i++) animation_times[i] = svg_obj.animate.times[i] * (1 / play_speed);
	
	// Update animation time parameters
	animation_start = Math.min(...animation_times)
	animation_end = Math.max(...animation_times)
	animation_duration = animation_end - animation_start

    // Set elapsed time to match
    elapsed_ms = ((anim_index[0]) / (animation_ntimes-1))*animation_duration;

    // Update animation start timing so that animation resumes from same position whether playing or paused
	if(anim_pause){
		anim_start = Date.now() + elapsed_ms;
		anim_pause_start = anim_start;
		anim_pause_time = anim_pause_start + elapsed_ms;
	}else{
		anim_start = Date.now() - elapsed_ms;
	}
}

function dataURItoBlob(dataURI) {

    // convert base64/URLEncoded data component to raw binary data held in a string
    var byteString;
    if (dataURI.split(',')[0].indexOf('base64') >= 0)
        byteString = atob(dataURI.split(',')[1]);
    else
        byteString = unescape(dataURI.split(',')[1]);

    // separate out the mime component
    var mimeString = dataURI.split(',')[0].split(':')[1].split(';')[0];

    // write the bytes of the string to a typed array
    var ia = new Uint8Array(byteString.length);
    for (var i = 0; i < byteString.length; i++) {
        ia[i] = byteString.charCodeAt(i);
    }

    return new Blob([ia], {type:mimeString});
}

function distQuat(a, b){
	return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) +  Math.pow(a.z - b.z, 2) +  Math.pow(a.w - b.w, 2))
}

function getStyle(el, styleProp) {
  var value, defaultView = (el.ownerDocument || document).defaultView;
  // W3C standard way:
  if (defaultView && defaultView.getComputedStyle) {
	// sanitize property name to css notation
	// (hypen separated words eg. font-Size)
	styleProp = styleProp.replace(/([A-Z])/g, "-$1").toLowerCase();
	return defaultView.getComputedStyle(el, null).getPropertyValue(styleProp);
  } else if (el.currentStyle) { // IE
	// sanitize property name to camelCase
	styleProp = styleProp.replace(/\-(\w)/g, function(str, letter) {
	  return letter.toUpperCase();
	});
	value = el.currentStyle[styleProp];
	// convert other units to pixels on IE
	if (/^\d+(em|pt|%|ex)?$/i.test(value)) { 
	  return (function(value) {
		var oldLeft = el.style.left, oldRsLeft = el.runtimeStyle.left;
		el.runtimeStyle.left = el.currentStyle.left;
		el.style.left = value || 0;
		value = el.style.pixelLeft + "px";
		el.style.left = oldLeft;
		el.runtimeStyle.left = oldRsLeft;
		return value;
	  })(value);
	}
	return value;
  }
}

function getWindowDims() {

	var width = window.innerWidth;
	var height = window.innerHeight;

	// Subtract bottom frame height from window height
	if(!bottom_frame_hidden) height = height - bottom_frame_height_px;

	return {
        width: width,
        height: height
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

function inputTimelineIndex(index, num, type) {

	// Pause animation
	playPauseAnimation('pause');
	
	// Play render
	playPauseRender('play');
	
	// Create copy
	index_value = index.value;
	var index_value_input = index_value;

	// Do nothing if empty input string
	if(index_value == '') return;

	// Value (text) input
	if(type == 'value'){
	
		// If value is outside of min/max range set to min/max
		if(index_value < timeline_start_disp) index_value = timeline_start_disp;
		if(index_value > timeline_end_disp) index_value = timeline_end_disp;
		
		// Convert value to range from 0 to 100 to match slider output
		index_value = Math.round(((index_value - timeline_start_disp) / timeline_duration_disp) * 100);
		//return;
	}
	
	// Set to index
	if(interpolate){

		// Find proportional time index value
		anim_index[num] = (index_value / 100)*(animation_ntimes - 1);

	}else{

		// Set elapsed time based on index		
		//elapsed_ms = animation_start + ((index_value-1)/98) * animation_duration;
		elapsed_ms = (index_value / 100) * animation_duration;

		//printAlert2(index_value_input + ',' + anim_index[num] + ',' + nearestTimeIndex(elapsed_ms, animation_duration, animation_ntimes))

		// Find closest time index in animation
		anim_index[num] = nearestTimeIndex(elapsed_ms, animation_duration, animation_ntimes);
	}

    // Set elapsed time to match
    elapsed_ms = ((anim_index[0]) / (animation_ntimes-1))*animation_duration;

    // Update animation start time
    anim_start = Date.now() + elapsed_ms;

    // Update shapes
    updateShapes(anim_index);

    //printAlert2(anim_index);

    // Animation pause time just needs to be greater than animation pause start by the amount of elapsed time
    // Then when animation starts again, it will start from input index
    anim_pause_start = anim_start;
    anim_pause_time = anim_pause_start + elapsed_ms;
}

//Interpolate between two given quaternions or positions
function interpolateVec(q0, q1, t){
//printAlert2("Quaternion1 " + q0.x + "," + q0.y + "," + q0.z + "," + q0.w + "  Quaternion2 " + q1.x + "," + q1.y + "," + q1.z + "," + q1.w);
//printAlert2("Distance1: " + distQuat(q0, q1) + "  Distance2: " + distQuat(q0, invertQuat(q1)))
  var n , value
  var q; 
  if (t <= 0){
    return q0;
  }
  if (t >= 1){
    return q1;
  }
  //Check if the passed object q is a position
  if (typeof q0 == 'object' && q0.w == undefined){
  	q = {x: 0, y:0, z:0}
  	q.x = (1 - t)*(q0.x) + t*(q1.x);
	q.y = (1 - t)*(q0.y) + t*(q1.y);
	q.z = (1 - t)*(q0.z) + t*(q1.z);
	return q;
  } 
  //If q is a quaternion and not a position:
  else{
  	q = {w: 0, x: 0, y:0, z:0}
  	if (distQuat(q0, invertQuat(q1)) < distQuat(q0, q1)){
  		q1 = invertQuat(q1);
  	}
  	q.w = (1 - t)*q0.w + t*q1.w;
  	q.x = (1 - t)*q0.x + t*q1.x;
  	q.y = (1 - t)*q0.y + t*q1.y;
  	q.z = (1 - t)*q0.z + t*q1.z;
  
  	value = q.w*q.w + q.x*q.x + q.y*q.y + q.z*q.z;
  
  	if (value > 0){
    	if (q.w < 0){
      	n = -Math.sqrt(value);
    	} else{
      	n = Math.sqrt(value);
    	}
  		q.w = q.w / n
  		q.x = q.x / n
  		q.y = q.y / n
  		q.z = q.z / n
  	} else {
    		q.w = 1
    		q.x = 0
    		q.y = 0
    		q.z = 0
  		}
  		return q;
	}
}

//Linear Interpolation
function interpolate1D(q1,q2,x){
	return interpolateVec(q1,q2,x);					
}

//Bilinear Interpolation
function interpolate2D(q1,q2,q3,q4,x,y){
	var s = interpolate1D(q1,q2,x);
	var t = interpolate1D(q3,q4,x);			
	return interpolate1D(s,t,y);	
}

//Trilinear Interpolation
function interpolate3D(q1,q2,q3,q4,q5,q6,q7,q8,x,y,z){
	var s = interpolate2D(q1,q2,q3,q4,x,y);		
	var t = interpolate2D(q5,q6,q7,q8,x,y);
	return interpolate1D(s,t,z);
}

//Given a quaternion, invert its x, y, z, and w values
function invertQuat(quat){
 var q = {w: 0, x: 0, y:0, z:0}
	q.w = -quat.w;
    q.x = -quat.x;
    q.y = -quat.y;
    q.z = -quat.z;
	return q;
}

function loadAnimation() {

	// Fill animation times
	var i;
	for(i=0; i < svg_obj.animate.times.length; i++) animation_times[i] = svg_obj.animate.times[i];
}

function loadDeformation() {

	if(svg_obj.deform == ''){
		deform = false;
		return;
	}

	// Set deform on
	deform = true;
}

function loadFirstImageTexture(){

	if(svg_obj.image == undefined){
		images_ready = true;
		image_textures_ready = true;
		return;
	}
	
	// If no additional images, start loading any/all textures after first iteration
	if(image_idx_ct+1 > svg_obj.image.length){

		// Confirm that images are ready
		images_ready = true;

		// Reset indices
		image_idx_ct = 0;
		texture_idx_ct = 0;
		texture_load_ct = 1;
		
		// Load subsequent textures
		loadSubsequentTextures();
		
		return;
	}

	// Set index in texture array to find loaded textures
	svg_obj.image[image_idx_ct].texture_idx = texture_idx_ct;

	// Add array at look-up index
	textures[texture_idx_ct] = new Array( );

	// Add to animated objects
	if(typeof(svg_obj.image[image_idx_ct].fname) == 'object'){
		
		// Add type and number
		update_obj.num.push(image_idx_ct);
		update_obj.type.push('image');
	}

	// Load texture
	var loader = new THREE.TextureLoader();
	if(typeof(svg_obj.image[image_idx_ct].fname) == 'string'){
		loader.load( app_dir[svg_obj.image[image_idx_ct].src_idx] + '/' + svg_obj.image[image_idx_ct].fname, addFirstTexture);
	}else{
		loader.load( app_dir[svg_obj.image[image_idx_ct].src_idx] + '/' + svg_obj.image[image_idx_ct].fname[texture_load_ct], addFirstTexture);
	}
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
			if(false){
				//material = new MeshLineMaterial({
				//	color: svg_obj.line[i].col,
				//	linewidth: svg_obj.line[i].lwd
				//});
			}else{
				material = new THREE.LineBasicMaterial({
					color: svg_obj.line[i].col,
					linewidth: svg_obj.line[i].lwd,
					depthTest: svg_obj.line[i].depthTest
				});
			}

			// Set line opacity
			if(svg_obj.line[i].opacity < 1){
				material.transparent = true;
				material.opacity = svg_obj.line[i].opacity;
			}

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
			if(true){
				line = new THREE.Line( geometry, material );
			}else{
				mesh_line = new MeshLine();
				mesh_line.setGeometry( geometry );
				line = new THREE.Mesh( mesh_line.geometry, material );
			}

			// Check if position over time is specified
			if(svg_obj.line[i].x_tm != undefined){

				// Add transformed position over time
				line.x_tm = svg_obj.line[i].x_tm;

				// Add type and number to 
				update_obj.num.push(i);
				update_obj.type.push('line');
			}

			// Set name and number of segments
			line.name = svg_obj.line[i].name;

			// Add to lines
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
			//if(svg_obj.sphere[i].x[0] == 'NA') continue;

			// Create sphere
			geometry = new THREE.SphereGeometry( radius=svg_obj.sphere[i].radius, widthSegments=svg_obj.sphere[i].wseg, heightSegments=svg_obj.sphere[i].hseg ) ;

			// material describes the surface of the shape
			//material = new THREE.MeshLambertMaterial( {color: svg_obj.sphere[i].col} ) ;
			material = new THREE.MeshPhongMaterial( {
						color: svg_obj.sphere[i].col,
						emissive: svg_obj.sphere[i].emissive,
						depthTest: svg_obj.sphere[i].depthTest
						//transparent: true,
						//depthWrite: false,
						//polygonOffset: true,
						//polygonOffsetFactor: 4
						//flatShading: true
						//side: THREE.DoubleSide,
					} )

			if(svg_obj.sphere[i].opacity < 1){
				material.transparent = true;
				material.opacity = svg_obj.sphere[i].opacity;
			}

			// mesh maps the material onto the geometry to make an object  
			mesh = new THREE.Mesh( geometry, material ) ;

			// position the mesh in space
			mesh.position.set( svg_obj.sphere[i].x[0], svg_obj.sphere[i].x[1], svg_obj.sphere[i].x[2] ) ;

			// Check if position over time is specified
			if(svg_obj.sphere[i].x_animated != undefined){

				// Add type and number to 
				update_obj.num.push(n);
				update_obj.type.push('sphere');
			}

			// Set name
			mesh.name = svg_obj.sphere[i].name;
		
			// Apply any initial transformations
			if(svg_obj.sphere[i].iposition != undefined){
				mesh.position.x = svg_obj.sphere[i].iposition[0];
				mesh.position.y = svg_obj.sphere[i].iposition[1];
				mesh.position.z = svg_obj.sphere[i].iposition[2];
			}

			// add the mesh to the scene
			scene.add( mesh ) ;

			// Add to spheres
			spheres.push(mesh)

			// Add to scene
			scene.add( mesh );
			
			n++;
		}
	}
}

function loadNextMesh(){

	var i

	if(svg_obj.mesh == undefined){
		meshes_ready = true;
		return;
	}

	// Check if position over time is specified
	if(svg_obj.mesh[mesh_load_ct].position != undefined){

		// Add type and number to 
		update_obj.num.push(mesh_load_ct);
		update_obj.type.push('mesh');
	}

	// Check if deformation over time is specified
	if(svg_obj.mesh[mesh_load_ct].deform != undefined){

		// Add type and number to 
		deform_obj.num.push(mesh_load_ct);
		deform_obj.type.push('mesh');
	}

	if(svg_obj.mesh[mesh_load_ct].src_idx == undefined){

		var i, material, mesh, num_faces, num_vertices, vertex, face, double_side;
		
		// Get mesh object
		mesh = svg_obj.mesh[mesh_load_ct];

		if(mesh.doubleSide == undefined) svg_obj.mesh[mesh_load_ct].doubleSide = true;
		if(mesh.doubleSide == true){ double_side = THREE.DoubleSide; }else{ double_side = false; }

		// Create geometry
		if(mesh.parseModel){

			var geometry = new THREE.Geometry();
			geometry = parseModel( mesh, geometry );

		}else{
			// Get mesh vertices and faces
			geometry = parseMeshGeometry(mesh);

			geometry.computeFaceNormals();
			if(mesh.computeVN == true) geometry.computeVertexNormals();
		}

		// Set material
		if(mesh.material == 'lambert'){

			material = new THREE.MeshLambertMaterial( { 
					color:mesh.col, 
					emissive: mesh.emissive,
					side: double_side
				} );

		}else{

			material = new THREE.MeshPhongMaterial( {
						color: mesh.col,
						emissive: mesh.emissive,
						side: double_side,
						//flatShading: true
					} )
		}

		// Set mesh name
		mesh_name = mesh.name;
		mesh_opacity = mesh.opacity;
		mesh_color = mesh.col;
		mesh_depthTest = mesh.depthTest;
		//mesh_depthWrite = false; //mesh.depthWrite;

		// Check if there's a deformation
		if(mesh.deform != undefined){

			// Copy vertices
			deform_obj.num.push(mesh_load_ct);
			deform_obj.type.push('mesh');

			var nVertices = geometry.vertices.length;
			mesh.clone_vertices = new Array();

			for(i = 0; i < nVertices; i++) {
				svg_obj.mesh[mesh_load_ct].clone_vertices[i] = geometry.vertices[i].clone();
			}

			//document.getElementById( "alert" ).innerHTML = svg_obj.mesh[mesh_load_ct].clone_vertices[3].y;
		}

		// Add mesh -- this advances mesh_load_ct!
		addMeshToScene(geometry, material);

		//alert(svg_obj.mesh[mesh_load_ct].position + ',' + svg_obj.mesh[mesh_load_ct].rotation)

	}else{

		// Set mesh name
		mesh_name = svg_obj.mesh[mesh_load_ct].name;
		mesh_opacity = svg_obj.mesh[mesh_load_ct].opacity;
		mesh_color = svg_obj.mesh[mesh_load_ct].col;
		mesh_depthTest = svg_obj.mesh[mesh_load_ct].depthTest;
		//mesh_depthWrite = false; //svg_obj.mesh[mesh_load_ct].depthWrite;

		// JSONLoader (buffer Geometry loader was not getting the indices right...)
		// Send next mesh after previous mesh is loaded so that names correspond
		// Couldn't figure out how to send name and other information to addMeshToScene in 
		// a way that ensures correspondence
		var loader = new THREE.JSONLoader();
		loader.load( app_dir[svg_obj.mesh[mesh_load_ct].src_idx] + '/' + svg_obj.mesh[mesh_load_ct].fname, addMeshToScene);

		//alert(svg_obj.mesh[mesh_load_ct].src_idx + ' ' + svg_obj.mesh[mesh_load_ct].fname)
	}
}

function loadNextTexture(){

	if(typeof(svg_obj.image[image_idx_ct].fname) == 'string'){

		// Advance indices for next image
		image_idx_ct++;
		texture_idx_ct++;
		texture_load_ct = 1;
		
		// Try to load next texture
		loadSubsequentTextures();

	}else{
		
		//printAlert2(image_idx_ct + ',' + texture_load_ct + ',' + app_dir[svg_obj.image[image_idx_ct].src_idx] + '/' + svg_obj.image[image_idx_ct].fname[texture_load_ct])

		//printAlert2(svg_obj.image[image_idx_ct].fname[texture_load_ct]);

		// Load texture
		var loader = new THREE.TextureLoader();
		loader.load( app_dir[svg_obj.image[image_idx_ct].src_idx] + '/' + svg_obj.image[image_idx_ct].fname[texture_load_ct], addTexture);
	}
}

function loadSubsequentTextures(){

	// If no additional textures, end
	if(image_idx_ct+1 > svg_obj.image.length){

		// Confirm that images are ready
		image_textures_ready = true;

		return;
	}

	// Start loading textures
	loadNextTexture();
}

function nearestPow2( aSize ){
	// https://bocoup.com/blog/find-the-closest-power-of-2-with-javascript
	return Math.pow( 2, Math.round( Math.log( aSize ) / Math.log( 2 ) ) ); 
}

function nearestTimeIndex( time, duration, nTimes ){
	
	//
	//if(time < start) return(0);
	//if(time > end) return(nTimes-1);

	if(time < 0) return(0);
	if(time > duration) return(nTimes-1)
	
	// Find normalized time (0 to 1)
	//var time_norm = (time-start) / duration;
	var time_norm = time / duration;
	
	// Convert to time index
	var time_idx = Math.round(time_norm*(nTimes-1));
	
	return(time_idx);
}

//
function onObjectsReady(){

	var i, j;

	// Get window dimensions
	var window_dims = getWindowDims();

	// Set bounding box
	setBoundingBox();

	if(svg_obj.camera != undefined){

		// Get number of cameras
		var cameras_length = svg_obj.camera.length;

		// For each camera
		for(i = 0; i < cameras_length; i++){
			
			// Setup the camera
			camera = new THREE.PerspectiveCamera( fov=45, aspect=window_dims.width / window_dims.height, near=camera_near, far=svg_obj.camera[i].far );

			// Set camera position
			camera.position.set(svg_obj.camera[i].x[0], svg_obj.camera[i].x[1], svg_obj.camera[i].x[2]);

			// Set camera focal length
			camera.setFocalLength(svg_obj.camera[i].focal);

			// Add camera controls
			if(svg_obj.camera[i].set == true){

				// Set camera to controls
				controls = new THREE.TrackballControls( camera );

				// Set where the camera is targeted
				controls.target.set(svg_obj.camera[i].target[0], svg_obj.camera[i].target[1], svg_obj.camera[i].target[2]);
			}

			// Set target as bounding box center
			bbox_center = svg_obj.camera[i].target;
		}

	}else{

		// Setup a camera
		camera = new THREE.PerspectiveCamera( fov=camera_fov, aspect=window_dims.width / window_dims.height, near=camera_near, far=bbox_size*20 );

		// Set camera to controls
		controls = new THREE.TrackballControls( camera );

		// Set look at to scene
		camera.lookAt(scene.position);

		// Set camera to include all shapes
		updateCameraPosition();
	}

	// Set controls
	controls.rotateSpeed = 1.0;
	controls.zoomSpeed = 1.2;
	controls.panSpeed = 0.2;

	renderer.render(scene, camera);
	
	controls.target.set( bbox_center[0], bbox_center[1], bbox_center[2]);
	controls.update();

	// Add lights
	addLights(bbox_center, bbox_size, 1.5*0.95)
}

// Set frames per second for motion
function onReady(){

	var i, elem_id;

	// Set bottom frame height from R write
	if(bottom_frame_hidden){
		document.getElementById( "bottom_frame" ).style.height = '0px';
	}else{
		document.getElementById( "bottom_frame" ).style.height = bottom_frame_height_px + 'px';
	}

	// Set where bottom frame starts
	bottom_frame_start_y = window.innerHeight - bottom_frame_height_px;

	// Key events
	document.body.onkeyup = function(e){

		// On spacebar keyup, start-stop animation
		if(e.keyCode == 32) playPauseAnimation();
	}
	
	// If no animation, set animate to pause
	if(animate == false) anim_pause = true;

	// Setup a new scene
	scene = new THREE.Scene();
	
	// Set background
	scene.background = new THREE.Color( bg_col );

	// Get container
	var container = document.getElementById( "container" );

	// Add stats box in top left corner
	if(show_stats){
		stats = new Stats();
		container.appendChild( stats.dom );
	}

	// Setup the renderer
	renderer = new THREE.WebGLRenderer( {
		antialias: true,
		preserveDrawingBuffer: true
	} );
	
	// Get window dimensions
	var window_dims = getWindowDims();
	renderer.setSize(window_dims.width, window_dims.height);
	document.body.appendChild(renderer.domElement);

	// Start mesh loading
	loadNextMesh();

	// Load first texture of each image and images
	loadFirstImageTexture();

	// Load all textures for any images, after all of these are loaded images will be loaded
	//loadNextImageTexture();

	// Load coordinate objects
	loadGeometries();
	
	if(animate){

		// Load animation
		loadAnimation();
	
		// Set animation speed at start
		changeAnimationSpeed(play_speed, 0)
	}

	// Load deformation
	loadDeformation();

	// Try rendering every 10 msec until all objects are finished loaded
	try_render_int = setInterval(tryRender, 10);
}

function parseMeshGeometry( mesh ){

	// Create geometry
	var geometry = new THREE.Geometry();

	// Get vertices
	var num_vertices = mesh.vertices.length;
	var i = 0;
	while ( i < num_vertices ) {
		vertex = new THREE.Vector3();
		vertex.x = mesh.vertices[ i++ ];
		vertex.y = mesh.vertices[ i++ ];
		vertex.z = mesh.vertices[ i++ ];
		geometry.vertices.push(vertex);
	}

	var num_faces = mesh.faces.length;
	i = 0;
	while ( i < num_faces ) {
		face = new THREE.Face3();
		face.a = mesh.faces[ i++ ];
		face.b = mesh.faces[ i++ ];
		face.c = mesh.faces[ i++ ];
		geometry.faces.push(face);
	}

	if(mesh.uvs != undefined){

		var num_uvs = mesh.uvs.length;

		i = 0;
		while ( i < num_uvs ) {
			geometry.faceVertexUvs[0].push([
				new THREE.Vector2(mesh.uvs[ i++ ], mesh.uvs[ i++ ]),
				new THREE.Vector2(mesh.uvs[ i++ ], mesh.uvs[ i++ ]),
				new THREE.Vector2(mesh.uvs[ i++ ], mesh.uvs[ i++ ])
			]);
		}
	}
	
	return geometry
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

function playPauseAnimation(state) {

	// If no animation, return
	if(!animate) return;

	// Set state based on toggle if undefined
	if(state == undefined) {
		var state;
		if(anim_pause){
			state = 'play'
		}else{
			state = 'pause'
		}
	}else{
		if(state == 'pause' && anim_pause) return;
		if(state == 'play' && !anim_pause) return;
	}

	if(state == 'play') { 

		anim_pause = false; 

		// Set anim_start so that when animation is unpaused it starts where it "left off"
		anim_start = anim_pause_start + (Date.now() - anim_pause_time);

	}else{

		anim_pause_time = Date.now();	// time at which the animation was paused
		anim_pause_start = anim_start; 	// start time when animation was paused
		anim_pause = true;

		// Set play/pause icon to play
		for (i = 1; i <= n_timelines; i++){
			control_icon_play = document.getElementById('timeline_play_icon_' + i);
			control_icon_play.innerHTML = '&#9654;';
		}
	}

	// Update animation timeline icons
	updateAnimationIcons(state);
}

function playPauseRender(state) {

	// Set state based on toggle if undefined
	if(state == undefined) {
		var state;
		if(render_pause){
			state = 'play'
		}else{
			state = 'pause'
		}
	}

	if(state == 'pause' && render_pause) return;
	if(state == 'play' && !render_pause){

		// Reset inactive since
		inactive_start_time = Date.now();

		return;
	}

	if(state == 'play') { 

		// Turn off pause
		render_pause = false;

		// Reset inactive since
		inactive_start_time = Date.now();
		
		// Reset number of animation plays
		n_anim_plays = 0;

		// Re-start callback
		requestAnimationFrame(render);

	}else{ 
		render_pause = true;
	}
}

function printAlert2(text){
	document.getElementById( "alert2" ).innerHTML = text;
}

function printObject(object){

	if(typeof(object) == 'undefined'){
		document.getElementById("alert2").innerHTML = object;
	}else if(typeof(object) == 'number'){
		document.getElementById("alert2").innerHTML = object;
	}else{
		document.getElementById("alert2").innerHTML = Object.values(object);
	}
}

function setBoundingBox () {

	// Geometry is the same as when read in - not updated with position/rotation
	// Compute bounding box (fills 'boundingBox' property, which is null by default)
	// alert(Object.getOwnPropertyNames());
	// e.g. meshes[0].geometry.boundingBox.min.x

	// Set initial values to not return NaN
	var minX = minY = minZ = Infinity;
	var maxX = maxY = maxZ = -Infinity;
	var time_index_bb = fillArray(0, n_timelines)

	// If no animation, just iterate once through update
	if(animate == false){
		var anim_ntimes = 1;
		var time_int = 1;
	}else{

		// Set number of times
		var anim_ntimes = animation_ntimes;

		// Get time interval to take bounding measurements from 5 even distributed time points
		var time_int = Math.round(animation_ntimes/5);

		// Make sure interval is not 0
		if(time_int == 0) time_int = 1;
	}

	for(j = 0; j < anim_ntimes; j = j + time_int){

		// Just use first index
		time_index_bb[0] = j;

		// Transform shapes to first animation index
		updateShapes(time_index_bb);

		// Compute bounding box of transformed objects
		for(i = 0; i <= spheres.length-1; i++){

			var bbox = new THREE.Box3().setFromObject( spheres[i] );
			
			if(isNaN(bbox.min.x)) continue;

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

			if(isNaN(bbox.min.x)) continue;

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

			if(isNaN(bbox.min.x)) continue;

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

			if(isNaN(bbox.min.x)) continue;

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

	//alert(minX + ',' + minY + ',' + minZ + ',' + maxX + ',' + maxY + ',' + maxZ)

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
	if(images_ready == false) return;
	if(image_textures_ready == false) return;

	// Stop running tryRender
	clearInterval(try_render_int);
	
	// Set viewing frame, camera and lights
	onObjectsReady();
	
	// Reset inactive start time to once all objects are loaded
	inactive_start_time = Date.now();

	if(save_as_img == true){

		// Render to scene
		renderer.render(scene, camera);
		
		// Start count of saved images
		save_as_img_ct = 0;
		
		// Send rendered visualization to server as image
		saveFramesAsImages(save_as_img_ct);

	}else{

		if(animate == true){

			// Set animation start time
			if(!anim_pause) anim_start = Date.now();

			// Update animation icons
			updateAnimationIcons();
		}

		// Render scene
		render();
	}
}

function receiveObjectFromR(object){

	// Parse object
	var json = JSON.parse(object);

	// If there is a function call, evaluate call	
	if(json.call != undefined) eval(json.call);
	
	return;
}

// After loading JSON from our file, we add it to the scene
var render = function () {

	var i;

	// Check whether render is paused
	if(!render_pause) requestAnimationFrame(render);
	
	// Update inactive since (in seconds)
	inactive_since = (Date.now() - inactive_start_time) / 1000;

	// If there is an animation
	if(animate){

		// If animation is playing 
		if(!anim_pause){

			// Get elapsed time in ms
			elapsed_ms = Date.now() - anim_start;

			// If exceeds animation duration, reset clock
			if(elapsed_ms > animation_duration){
				anim_start = Date.now();
				elapsed_ms = 0;
				
				// Increase count of animation plays
				n_anim_plays = n_anim_plays + 1;
				
				// Check whether number of animation plays exceeds max to pause render
				if(n_anim_plays > max_anim_plays_to_pause) playPauseRender('pause');
			}
			
			for (i = 1; i <= n_timelines; i++){
				if(interpolate){
					// Find proportional time index
					anim_index[i-1] = (elapsed_ms / animation_duration)*(animation_ntimes-1);
				}else{

					// Find closest time point in animation
					anim_index[i-1] = nearestTimeIndex(elapsed_ms, animation_duration, animation_ntimes);
				}
			}

			// Set anim_index as vector with same length as number of timelines
			//anim_index = fillArray(anim_index, n_timelines);

			// Update shapes
			updateShapes(anim_index);

			// Reset inactive since start time because animation is playing
			inactive_start_time = Date.now();
		}
	
		// Update timeline slider and value entry box
		for (i = 1; i <= n_timelines; i++){

			// Update slider position
			document.getElementById('timeline_slider_' + i).value = ((anim_index[i-1]) / (animation_ntimes-1))*100;

			// Update input value
			document.getElementById('timeline_value_' + i).value = timeline_start_disp + Math.round(timeline_duration_disp*((anim_index[i-1]) / (animation_ntimes-1))*1000)/1000;
		}
	}

	// If no activity for a period of time, pause rendering
	if(inactive_since >= 1) playPauseRender('pause');
 
	// Update clock
	if(debug){
		document.getElementById( "inactive_since" ).innerHTML = (Math.round(inactive_since*100) / 100) + " sec";
	}

	if(show_clock){
		document.getElementById( "clock" ).innerHTML = Math.round(elapsed_ms / 100)*100 + " ms";
		document.getElementById( "idx" ).innerHTML = anim_index;
		document.getElementById( "time" ).innerHTML = Math.round((elapsed_ms*play_speed) / 100)*100 + " ms";
		document.getElementById( "system_time" ).innerHTML = Date.now();
	}

	if(show_stats) stats.update();
	controls.update();
	renderer.render(scene, camera);
};

function saveFramesAsImages(save_as_img_ct){

	// Check if all images have already been submitted
	if(save_as_img_ct == save_as_img_paths.length){
	
		//
		sendObjectToR({'parsejson': 'true', 'function':'close'});
		
		//setTimeout(function() { alert(Object.getOwnPropertyNames(document.getElementById( "iframe" ))); }, 1000);
		
		
		// Close on all loaded if specified
		//if(save_as_img_close) window.close();
		
		//document.getElementById( "alert" ).innerHTML = 'Done';
		return;
	}
	
	// If animation, find closest time point in animation
	if(animate){

		// Update shapes using count as time index (should be same as number of frames)
		updateShapes([save_as_img_ct]);

		// Update renderer
		renderer.render(scene, camera);
	}

	// Set animation to frame

	// Save current frame as image
	var dataUrl = renderer.domElement.toDataURL('image/jpeg', 1.0);

	var fd = new FormData(document.getElementById( "form" ));
	var blob = dataURItoBlob(dataUrl);
	var fd = new FormData(document.forms[0]);

	// Creates an image at $tempfile - should be removed after copying over
	fd.append("image", blob);
	fd.append("function", 'save_image');
	fd.append("save_image_as", decodeURI(save_as_img_paths[save_as_img_ct]));

	// Create new request
	var xhr = new XMLHttpRequest();

	// Function to call on receipt (can also be used to receive text from server after request)
	xhr.onreadystatechange = function() {
		if (xhr.readyState == XMLHttpRequest.DONE) {

			//document.getElementById( "alert" ).innerHTML = xhr.response;

			// Advance count
			save_as_img_ct++;
			
			// Call on next image
			saveFramesAsImages(save_as_img_ct);
		}
	};

	xhr.open('POST', server_url + '/custom/svgViewR', true);
	xhr.send(fd);

	document.getElementById( "form_input" ).value = fd;
}

function sendObjectToR(object){

	var fd = new FormData(document.getElementById( "form" ));
	var fd = new FormData(document.forms[0]);
	fd.append("type", 'jsonstring');
	fd.append("jsonstring", JSON.stringify(object));

	// Create new request
	var xhr = new XMLHttpRequest();

	// Function to call on receipt
	xhr.onreadystatechange = function() {
		if (xhr.readyState == XMLHttpRequest.DONE) {
			receiveObjectFromR(xhr.response);
		}
	};

	xhr.open('POST', server_url + '/custom/svgViewR', true);
	xhr.send(fd);
}

function skipToAnimationFrame(to, num){

	// Check that there is an animation
	if(animate == false) return;
	
	// Pause animation
	playPauseAnimation('pause');
	
	// Play render
	playPauseRender('play');
	
	// Get current closest time index
	if(to == 'p' || to == 'n'){

		var current_idx = nearestTimeIndex(elapsed_ms, animation_duration, animation_ntimes);

		// Set new index
		if(to == 'p') anim_index[num] = current_idx - 1;
		if(to == 'n') anim_index[num] = current_idx + 1;
	
		// Make sure new index is within bounds
		if(anim_index[num] < 0) anim_index[num] = 0;
		if(anim_index[num] > animation_ntimes-1) anim_index[num] = animation_ntimes-1;

	}else{

		if(to == 'b') anim_index[num] = 0;
		if(to == 'e') anim_index[num] = animation_ntimes - 1;
	}
	
    // Set elapsed time to match
    elapsed_ms = (anim_index[num] / (animation_ntimes-1))*animation_duration;

    // Update animation start time
    anim_start = Date.now() + elapsed_ms;

    // Update shapes
    updateShapes(anim_index);

    // Animation pause time just needs to be greater than animation pause start by the amount of elapsed time
    // Then when animation starts again, it will start from input index
    anim_pause_start = anim_start;
    anim_pause_time = anim_pause_start + elapsed_ms;
}

function toggleVisibility(box_elem){

	var i, opacity;

	if(box_elem.checked){
		change_to = true;
	}else{
		change_to = false;
	}

	if(svg_obj.mesh != undefined){
		for (i = 0; i < meshes.length; i++){
			if(meshes[i].name == box_elem.value){

				meshes[i].visible = change_to;
				svg_obj.mesh[i].visible = change_to;
			}
		}
	}
	if(svg_obj.line != undefined){
		for (i = 0; i < lines.length; i++){
			if(lines[i].name == box_elem.value) lines[i].visible = change_to;
		}
	}
	if(svg_obj.sphere != undefined){
		for (i = 0; i < spheres.length; i++){
			if(spheres[i].name == box_elem.value) spheres[i].visible = change_to;
		}
	}
}

function updateAnimationIcons(state){

	var i;
	var control_icon_play;

	// Set state based on toggle if undefined
	if(state == undefined) {
		var state;
		if(anim_pause){
			state = 'pause'
		}else{
			state = 'play'
		}
	}

	if(state == 'play') { 

		// Set play/pause icon to pause
		for (i = 1; i <= n_timelines; i++){

			control_icon_play = document.getElementById('timeline_play_icon_' + i);
			control_icon_play.innerHTML = '&#9613;&#9613;';						// From thick to thin: 9612, 9613, 9614

			control_icon_play.style.fontSize = '0.8em';
			control_icon_play.style.verticalAlign = 'top';
			control_icon_play.style.margin = '0% 0% 0% 10%';
			control_icon_play.style.lineHeight = '1.6em';
			control_icon_play.style.letterSpacing = '-1.6em';
		}

	}else{

		// Set play/pause icon to play
		for (i = 1; i <= n_timelines; i++){

			control_icon_play = document.getElementById('timeline_play_icon_' + i);
			control_icon_play.innerHTML = '&#9654;';

			control_icon_play.style.fontSize = '1.2em';
			control_icon_play.style.margin = '0em';
			control_icon_play.style.verticalAlign = 'baseline';
			control_icon_play.style.lineHeight = '0em';
			control_icon_play.style.letterSpacing = '0em';
		}
	}
}

function updateCameraPosition(){

	// Get screen dimensions
	var window_dims = getWindowDims();

	// Set initial camera distance and interval
	var camera_dist = bbox_scale[2]+bbox_size*0.01;
	var camera_dist_int = bbox_size*0.2;
	var margin = 50;
	var n = 0;
	var n_limit = 30;

	// Set initial camera
	// Projection will not use updated camera position without rendering
	camera.aspect = window_dims.width / window_dims.height;
	camera.updateProjectionMatrix();
	renderer.setSize( window_dims.width, window_dims.height );
	camera.position.set(bbox_center[0], bbox_center[1], bbox_center[2]+camera_dist);
	renderer.render(scene, camera);
	
	// Corners 3d position
	var corner1 = {x:bbox_center[0] + bbox_scale[0]/2, y:bbox_center[1] + bbox_scale[1]/2, z:bbox_center[2] + bbox_scale[2]/2};
	var corner4 = {x:bbox_center[0] - bbox_scale[0]/2, y:bbox_center[1] - bbox_scale[1]/2, z:bbox_center[2] + bbox_scale[2]/2};

	var widthHalf = window_dims.width / 2, heightHalf = window_dims.height / 2;
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
		if(box_width > (window_dims.width-margin) || box_height > (window_dims.height-margin)){

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

	//alert(box_width + ' (' + window_dims.width + '),' + box_height + ' (' + window_dims.height + ') ' + n);

	// Set where the camera is targeted
	//controls.target.set( bbox_center[0], bbox_center[1], bbox_center[2]);
	//controls.update();
}


function updateShapes(time_index){

	//document.getElementById( "alert" ).innerHTML = document.getElementById( "alert" ).innerHTML + ',' + time_index;
	var def_vars, num, type, i, j, k, nVertices, obj_num, obj_type, set_prop, set_val, tracks_length;

	//// Apply animation transformations
	if(animate == true){

		//// Apply object updates/transformations
		var update_obj_length = update_obj.num.length;
		var new_quat;
		var new_pos;
		var time_index_floor = new Array(n_timelines);
		var time_index_ceil = new Array(n_timelines);
   		var ratio_x, ratio_y, ratio_z;
   		var opacity_t;
   		
		// Set floor and ceiling from time indices
		for (j = 0; j < n_timelines; j++){

			// Set the floor of time indices
			time_index_floor[j] = Math.floor(time_index[j])

			// Update the floor of the specified time index if at final iteration
			if(time_index_floor[j] == animation_ntimes - 1) time_index_floor[j] = time_index_floor[j] - 1

			// Set the ceiling of the specified time index
			time_index_ceil[j] = time_index_floor[j] + 1; 	
		}

		// Set ratios for interpolation
		ratio_x = time_index[0] - time_index_floor[0];
		if(n_timelines > 1) ratio_y = time_index[1] - time_index_floor[1];
		if(n_timelines > 2) ratio_z = time_index[2] - time_index_floor[2];

		for (i = 0; i < update_obj_length; i++){

			// Set object number and type
			obj_num = update_obj.num[i];
			obj_type = update_obj.type[i];

			if(obj_type == 'mesh'){

				// Create a quaternion based on user specified time index
				if(n_timelines == 1){
					new_quat = interpolate1D(svg_obj.mesh[obj_num].quaternion[time_index_floor[0]], 
									svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]], ratio_x)
					new_pos = interpolate1D(svg_obj.mesh[obj_num].position[time_index_floor[0]], 
									svg_obj.mesh[obj_num].position[time_index_ceil[0]], ratio_x)

				}else if(n_timelines == 2){	

					// Find the new quaternion
					new_quat = interpolate2D(svg_obj.mesh[obj_num].quaternion[time_index_floor[0]][time_index_floor[1]], 
										svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]][time_index_floor[1]],
										svg_obj.mesh[obj_num].quaternion[time_index_floor[0]][time_index_ceil[1]],
										svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]][time_index_ceil[1]], 
										ratio_x, ratio_y);
					// Find the new position
					new_pos = interpolate2D(svg_obj.mesh[obj_num].position[time_index_floor[0]][time_index_floor[1]], 
										svg_obj.mesh[obj_num].position[time_index_ceil[0]][time_index_floor[1]],
										svg_obj.mesh[obj_num].position[time_index_floor[0]][time_index_ceil[1]],
										svg_obj.mesh[obj_num].position[time_index_ceil[0]][time_index_ceil[1]], 
										ratio_x, ratio_y);	
										
				} else if(n_timelines == 3){
				
					new_quat = interpolate3D(svg_obj.mesh[obj_num].quaternion[time_index_floor[0]][time_index_floor[1]][time_index_floor[2]], 
										svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]][time_index_floor[1]][time_index_floor[2]],
										svg_obj.mesh[obj_num].quaternion[time_index_floor[0]][time_index_ceil[1]][time_index_floor[2]],
										svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]][time_index_ceil[1]][time_index_floor[2]], 
										svg_obj.mesh[obj_num].quaternion[time_index_floor[0]][time_index_floor[1]][time_index_ceil[2]], 
										svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]][time_index_floor[1]][time_index_ceil[2]],
										svg_obj.mesh[obj_num].quaternion[time_index_floor[0]][time_index_ceil[1]][time_index_ceil[2]],
										svg_obj.mesh[obj_num].quaternion[time_index_ceil[0]][time_index_ceil[1]][time_index_ceil[2]], 
										ratio_x, ratio_y, ratio_z);
			
					new_pos = interpolate3D(svg_obj.mesh[obj_num].position[time_index_floor[0]][time_index_floor[1]][time_index_floor[2]], 
										svg_obj.mesh[obj_num].position[time_index_ceil[0]][time_index_floor[1]][time_index_floor[2]],
										svg_obj.mesh[obj_num].position[time_index_floor[0]][time_index_ceil[1]][time_index_floor[2]],
										svg_obj.mesh[obj_num].position[time_index_ceil[0]][time_index_ceil[1]][time_index_floor[2]], 
										svg_obj.mesh[obj_num].position[time_index_floor[0]][time_index_floor[1]][time_index_ceil[2]], 
										svg_obj.mesh[obj_num].position[time_index_ceil[0]][time_index_floor[1]][time_index_ceil[2]],
										svg_obj.mesh[obj_num].position[time_index_floor[0]][time_index_ceil[1]][time_index_ceil[2]],
										svg_obj.mesh[obj_num].position[time_index_ceil[0]][time_index_ceil[1]][time_index_ceil[2]], 
										ratio_x, ratio_y, ratio_z);
				}
				
				meshes[obj_num].position.set(new_pos.x, new_pos.y, new_pos.z);
				meshes[obj_num].quaternion.set(new_quat.x, new_quat.y, new_quat.z, new_quat.w);

				// If opacity is animated
				if(svg_obj.mesh[obj_num].opacity.length == animation_ntimes){

					// Interpolate opacity value
					opacity_t = (1 - ratio_x)*svg_obj.mesh[obj_num].opacity[time_index_floor[0]] + 
						ratio_x*svg_obj.mesh[obj_num].opacity[time_index_ceil[0]];
					
					// User input mesh visibility takes priority over animation
					if(svg_obj.mesh[i].visible == true){
						if(opacity_t == 0){
							meshes[obj_num].visible = false;
						}else{
							meshes[obj_num].visible = true;
						}
					}

					meshes[obj_num].material.opacity = opacity_t;
				}
			}

			if(obj_type == 'image'){

				// Get texture index
				texture_idx = svg_obj.image[obj_num].texture_idx;
			
				//if(images[obj_num] == undefined) continue;
				if(texture_idx + 1 > textures[texture_idx].length){
					continue;
				}

				//printAlert2('Alert: ' + obj_num + ',' + obj_type + ',' + time_index[0] + ',' + textures[texture_idx].length)

				// Get material from loaded texture
				//material = new THREE.MeshBasicMaterial({map: textures[texture_idx][time_index[0]], side: THREE.DoubleSide});
				images[obj_num].material.map = textures[texture_idx][time_index[0]];

				if(obj_num == 0){
					//printAlert2('Alert: ' + obj_num + ',' + obj_type + ',' + texture_idx + ',' + time_index[0] + ',' + images[obj_num].material.map)
				}
				
				if(svg_obj.image[texture_idx].opacity.length == animation_ntimes){
					images[obj_num].material.opacity = svg_obj.image[obj_num].opacity[time_index[0]];
				}
			}

			if(obj_type == 'sphere'){
			
				// Get new position (treat as simple point positions, transformation is done in R)
				new_pos = interpolate1D(svg_obj.sphere[obj_num].x_animated[time_index_floor[0]], 
					svg_obj.sphere[obj_num].x_animated[time_index_ceil[0]], ratio_x)
				
				// Set new position and quaternion
				spheres[obj_num].position.set(new_pos.x, new_pos.y, new_pos.z);
				
				// Change opacity with time
				if(svg_obj.sphere[obj_num].opacity.length == animation_ntimes){
					//spheres[obj_num].material.opacity = svg_obj.sphere[obj_num].opacity[time_index[0]];
				}
			}

			if(obj_type == 'line'){
			
				// Update each segment
				k = 0;
				for(j = 0; j < svg_obj.line[obj_num].nseg*3; j = j + 3){
				
					// Get new position (treat as simple point positions, transformation is done in R)
					new_pos = interpolate1D({x:lines[obj_num].x_tm[time_index_floor[0]][j], y:lines[obj_num].x_tm[time_index_floor[0]][j+1], z:lines[obj_num].x_tm[time_index_floor[0]][j+2]}, 
						{x:lines[obj_num].x_tm[time_index_ceil[0]][j], y:lines[obj_num].x_tm[time_index_ceil[0]][j+1], z:lines[obj_num].x_tm[time_index_ceil[0]][j+2]}, ratio_x)

					if(true){
						lines[obj_num].geometry.vertices[k].x = new_pos.x;
						lines[obj_num].geometry.vertices[k].y = new_pos.y;
						lines[obj_num].geometry.vertices[k].z = new_pos.z;
					}else{
						lines[obj_num].geometry.attributes.position.array[j*2+0] = new_pos.x;
						lines[obj_num].geometry.attributes.position.array[j*2+1] = new_pos.y;
						lines[obj_num].geometry.attributes.position.array[j*2+2] = new_pos.z;
						lines[obj_num].geometry.attributes.position.array[j*2+3] = new_pos.x;
						lines[obj_num].geometry.attributes.position.array[j*2+4] = new_pos.y;
						lines[obj_num].geometry.attributes.position.array[j*2+5] = new_pos.z;
					}

					k++;
				}

				// Update vertices
				if(true){
					lines[obj_num].geometry.verticesNeedUpdate = true;
				}else{
					lines[obj_num].geometry.attributes.position.needsUpdate = true;
				}
			}
		}
	}

	//// Apply deformation transformations
	if(deform == true){
		
		// Apply object updates/transformations
		var deform_obj_length = deform_obj.num.length;

		for (i = 0; i < deform_obj_length; i++){
			
			// Set object number and type
			obj_num = deform_obj.num[i];
			obj_type = deform_obj.type[i];

			if(obj_type == 'mesh'){

				//var nVertices = Object.getOwnPropertyNames(meshes[obj_num].geometry.vertices);
				nVertices = meshes[obj_num].geometry.vertices.length;
//document.getElementById( "alert" ).innerHTML = svg_obj.mesh[obj_num].deform[time_index[0]];

				// deform variables
				def_vars = svg_obj.mesh[obj_num].deform[time_index[0]];

//document.getElementById( "alert" ).innerHTML = def_vars;

				for(i = 0; i < nVertices; i++) {
					meshes[obj_num].geometry.vertices[i].x = svg_obj.mesh[obj_num].clone_vertices[i].x + def_vars[1]*(svg_obj.mesh[obj_num].clone_vertices[i].x - def_vars[0]);
					meshes[obj_num].geometry.vertices[i].y = svg_obj.mesh[obj_num].clone_vertices[i].y + def_vars[3]*(svg_obj.mesh[obj_num].clone_vertices[i].y - def_vars[2]);
					meshes[obj_num].geometry.vertices[i].z = svg_obj.mesh[obj_num].clone_vertices[i].z + def_vars[5]*(svg_obj.mesh[obj_num].clone_vertices[i].z - def_vars[4]);
				}

				//svg_obj.mesh[obj_num].clone_vertices[i] = geometry.vertices[i].clone();

				// Mark the vertices as needing update
				meshes[obj_num].geometry.verticesNeedUpdate = true;		

				//anim_pause_time = Date.now();
				//anim_pause_start = anim_start;
				//anim_pause = true; 
			}
		}
	}
}

//xhr_close_on_change = function() {
//	if (xhr.readyState == XMLHttpRequest.DONE) {
//		window.close()
//	}
//}

window.onresize = function () {

	// Get window dimensions
	var window_dims = getWindowDims();

	// Update where bottom frame starts
	bottom_frame_start_y = window.innerHeight - bottom_frame_height_px;

	camera.aspect = window_dims.width / window_dims.height;
	camera.updateProjectionMatrix();
	controls.handleResize();
	renderer.setSize( window_dims.width, window_dims.height );
	renderer.render(scene, camera);
};