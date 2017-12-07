function load_animation( str ) {

	// Parse animations
	animations = JSON.parse(str).animations;

	// Select first animation
	animation = animations[0];
	
	// Get time units
	animation.units = 'msec';
	if(JSON.parse(str).units == 'sec' || JSON.parse(str).units == 's') animation.units = 'sec';

	// Set animate	
	if(animation.tracks.length == 0){

		// Get animation times/length if only update_obj is used (no transformations)
		// if(update_obj.num.length > 0){
		// if(update_obj.type[0] == 'sphere') spheres[update_obj.num[0]].x_tm.length

		animate = false;
		return;
	}

	animate = true;

	// Get times
	animation.times = getUniqueTimes( animation );
	
	// Apply play speed (factor by which to slow or speed up playback)
	animation.times = animation.times;
	
	// Get start and end times
	animation.start = Math.min(...animation.times);
	animation.end = Math.max(...animation.times);
	animation.duration = (animation.end - animation.start);

	// Set number of time points
	animation.ntimes = animation.times.length;
}

function getUniqueTimes( animation ){

	var i, j, keys_length;
	var times = [];
	var num, apply_to, apply_type;
	
	var factor = 1;
	if(animation.units == 'sec') factor = 1000;
	
	var tracks_length = animation.tracks.length;
	for (i = 0; i < tracks_length; i++){

		// Get index of apply
		apply_to = animation.tracks[i].apply;

		animation.tracks[i].num = new Array();
		animation.tracks[i].type = new Array();

		//// Find objects with matching name
		for(j = 0; j < svg_obj.mesh.length; j++){
			if(apply_to == svg_obj.mesh[j].name){
				animation.tracks[i].num.push(j);
				animation.tracks[i].type.push('mesh');
			}
		}
		
		// Find corresponding mesh
		for(j = 0; j < svg_obj.mesh.length; j++){
			if(apply_to == svg_obj.mesh[j].name){
				animation.tracks[i].apply_idx = j;
				break;
			}
		}
		
		//alert(animation.tracks[i].num + '\n' + animation.tracks[i].type);
		
		// Add times
		keys_length = animation.tracks[i].keys.length;
		for (j = 0; j < keys_length; j++){
			times.push(animation.tracks[i].keys[j].time * factor * (1/play_speed));
		}
	}
	
	times = times.filter( onlyUnique );

	times.sort(function(a, b){return a - b});

	return(times);
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

function onlyUnique(value, index, self) { 
	return self.indexOf(value) === index;
}