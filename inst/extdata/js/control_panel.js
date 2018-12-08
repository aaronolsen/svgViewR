function changeAnimationDuration(element){
	
	if(!is_numeric(element.value)) return;

	// Set new animation duration
	animation_duration = parseFloat(element.value);

	// If animation is already stopped, return
	if(stop_anim == 1) return;

	// If animation is running, stop and re-start
	clearInterval(intervalID);
	startAnimation();
}

function find_layer_children(container, layers){

	var container_class;
	var header;
	var header_class;
	var main_class;
	var i, j;
	var main;

	if(layers == undefined) layers = [];

	for(i = 0; i < container.childNodes.length; i++){

		container_class = container.childNodes[i].className;

		if(container_class == undefined) continue

		if(container_class == 'control_layer_header'){
		
			header = container.childNodes[i];

			for(j = 0; j < header.childNodes.length; j++){
			
				header_class = header.childNodes[j].className;

				if(header_class != 'control_layer_text' && header_class != 'control_layer_text_nochild') continue

				layers.push(header.childNodes[j].innerHTML)
			}
		}

		if(container_class == 'control_layer_main') main = container.childNodes[i];
	}

	if(main == undefined) return layers;

	for(i = 0; i < main.childNodes.length; i++){

		main_class = main.childNodes[i].className;

		if(main_class == undefined || main_class != 'control_layer_container') continue

		layers.concat(find_layer_children(main.childNodes[i], layers))
	}

	return layers;
}

function inputAnimationState(element){
	
	// Pause animation
	playPauseAnimation('stop');
	
	if(element.id == 'animation_frame_range_input'){
		
		anim_n = Math.round(((element.value) / 100)*animation_length);

		animateShapes();
	}

	if(element.id == 'animation_frame_count_input'){
	
		if(!is_numeric(element.value)) return;

		anim_n = parseFloat(element.value) - 1;
		
		animateShapes();
	}
}

function layer_visibility(element){

	var class_name;
	var div_header = element.parentNode.parentNode;
	var div_container = element.parentNode.parentNode.parentNode;
	var i;
	
	// Get parent layer
	for(i = 0; i < div_header.childNodes.length; i++){

		class_name = div_header.childNodes[i].className;
		if(class_name == undefined) continue
		if(class_name != 'control_layer_text' && class_name != 'control_layer_text_nochild') continue

		var parent_layer = div_header.childNodes[i].innerHTML;
	}

	// Find all child layers
	layers = find_layer_children(div_container)

	// Remove duplicate layer names
	layers = array_unique(layers)

	// Get opacity to apply to layers
	var opacity = element.value

	// Show hide all child layers and layers of the same name
	layer_visibility_children(document.getElementById("control_layers_container"), layers, opacity)
}

function layer_visibility_children(container, layers, opacity){

	var container_class;
	var header;
	var header_class;
	var layer_text;
	var main_class;
	var i, j, k;
	var idx_input;
	var idx_vis;
	var main;

	for(i = 0; i < container.childNodes.length; i++){

		container_class = container.childNodes[i].className;

		if(container_class == undefined) continue

		if(container_class == 'control_layer_header'){

			// Clear layer text
			layer_text;

			header = container.childNodes[i];

			for(j = 0; j < header.childNodes.length; j++){
				header_class = header.childNodes[j].className;
				if(header_class == 'control_layer_shape_vis_range') idx_vis = j;
				if(header_class == 'control_layer_text' || header_class == 'control_layer_text_nochild') layer_text = header.childNodes[j].innerHTML;
			}
			
			if(layer_text == undefined) continue
			
			if(layers.indexOf(layer_text) > -1){

				// Change opacity for any shapes that list layer
				changeLayerOpacity(layer_text, (opacity/100));

				// Find slider element
				for(k = 0; k < header.childNodes[idx_vis].childNodes.length; k++){
					if(header.childNodes[idx_vis].childNodes[k].tagName == 'INPUT') idx_input = k;
				}
				
				// Change opacity slider in control panel
				header.childNodes[idx_vis].childNodes[idx_input].value = opacity;
			}
		}

		if(container_class == 'control_layer_main' || container_class == 'control_layers_main' || container_class == 'control_animation_main') main = container.childNodes[i];
	}

	if(main == undefined) return;

	for(i = 0; i < main.childNodes.length; i++){

		main_class = main.childNodes[i].className;

		if(main_class == undefined || main_class != 'control_layer_container') continue

		layer_visibility_children(main.childNodes[i], layers, opacity)
	}
}

function show_hide_control(element, action){

	if(element == null) return;

	var class_action;
	var class_name;
	var div_container = element.parentNode.parentNode;
	var i;
	
	var win_show_sym = '&#8853;';
	var win_hide_sym = '&#8854;';

	for(i = 0; i < div_container.childNodes.length; i++){

		class_name = div_container.childNodes[i].className;
		
		if(class_name == undefined) continue

		if(class_name == 'control_layers_main' || class_name == 'control_layer_main' || class_name == 'control_animation_main'){

			if(action == undefined) if(element.innerHTML.charCodeAt(0) == 9660){action = 'hide';}else{action = 'show';}

			if(action == 'hide'){
				// Hide
				element.innerHTML = '&#9658;';
				div_container.childNodes[i].style.display = 'none';
			}else{
				// Show
				element.innerHTML = '&#9660;';
				div_container.childNodes[i].style.display = '';
			}
			
			if(element.id !== ''){
				if(action == 'hide'){
					deleteCookie('show_hide_control(document.getElementById("' + element.id + '"),"show")');
					createCookie('show_hide_control(document.getElementById("' + element.id + '"),"hide")', 'eval', 3600);
				}else{
					deleteCookie('show_hide_control(document.getElementById("' + element.id + '"),"hide")');
					createCookie('show_hide_control(document.getElementById("' + element.id + '"),"show")', 'eval', 3600);
				}
			}
		}

		if(class_name == 'control_layer_subheader'){

			if(action == undefined) if(element.innerHTML.charCodeAt(0) == 9660){action = 'hide';}else{action = 'show';}

			if(action == 'hide'){
				div_container.childNodes[i].style.display = 'none';
			}else{
				div_container.childNodes[i].style.display = '';
			}
		}
	}
}

function showHideControlPanel(action, write_cookie){

	if(write_cookie == undefined) var write_cookie = true;

	// Check whether any containers within control panel are visible
	var none_visible = true;
	if(document.getElementById('control_layers_container') !== null && document.getElementById('control_layers_container').style.display == '') none_visible = false;
	if(document.getElementById('control_animation_container') !== null && document.getElementById('control_animation_container').style.display == '') none_visible = false;

	if(none_visible){
		document.getElementById('control_container').style.display = 'none';
		writeControlIcon('control', 'disable');
		return;
	}

	if(action == undefined){
		if(control_panel_visible){control_panel_visible = false;}else{control_panel_visible = true;}
	}else{
		if(action == 'show'){
			control_panel_visible = true;
		}else{
			control_panel_visible = false;
		}
	}

	var control_container = document.getElementById('control_container');

	if(control_container == null) return;

	if(control_panel_visible){
		// Show
		control_container.style.width = '300px';
		control_container.style.display = '';
		writeControlIcon('control', 'show');
		
		if(write_cookie){
			deleteCookie('showHideControlPanel("hide")');
			createCookie('showHideControlPanel("show")', 'eval', 3600);
		}
	}else{
		// Hide
		control_container.style.width = '85px';
		control_container.style.display = 'none';
		writeControlIcon('control', 'hide');

		if(write_cookie){
			deleteCookie('showHideControlPanel("show")');
			createCookie('showHideControlPanel("hide")', 'eval', 3600);
		}
	}
}

function toggleAnimationReverse(){
	if(animation_reverse){animation_reverse = 0;}else{animation_reverse = 1;}
}

function toggle_rotate_mode(element, write_cookie){

	if(write_cookie == undefined) var write_cookie = true;

	if(element == 'translate' && rotate_mode == false) return;
	if(element == 'rotate' && rotate_mode == true) return;

	if(rotate_mode){
		rotate_mode = false;
		document.getElementById("control_icon_rotate").title = "Turn on rotate mode or press and hold 'r'";
		document.getElementById("control_icon_translate").title = 'Translate mode on, click and drag to move object';
		document.getElementById("control_icon_rotate").style.border = '1px solid #cccccc';
		document.getElementById("control_icon_rotate_arrow").style.color = '#cccccc';
		document.getElementById("control_icon_translate").style.border = '1px solid black';
		document.getElementById("control_icon_translate").style.color = 'black';

		if(write_cookie){
			deleteCookie('toggle_rotate_mode("rotate")');
			createCookie('toggle_rotate_mode("translate")', 'eval', 3600);
		}
	}else{
		rotate_mode = true;
		document.getElementById("control_icon_rotate").title = 'Rotate mode on, click and drag to rotate object';
		document.getElementById("control_icon_translate").title = 'Turn on translate mode';
		document.getElementById("control_icon_rotate").style.border = '1px solid #000000';
		document.getElementById("control_icon_rotate_arrow").style.color = 'black';
		document.getElementById("control_icon_translate").style.border = '1px solid #cccccc';
		document.getElementById("control_icon_translate").style.color = '#cccccc';

		if(write_cookie){
			deleteCookie('toggle_rotate_mode("translate")');
			createCookie('toggle_rotate_mode("rotate")', 'eval', 3600);
		}
	}
}

function writeControlIcon(name, state){

	if(name == undefined) name = new Array('control', 'rotate', 'translate');
	if(Array.isArray(name) == false) name = new Array(name);

	var i;

	for(i = 0; i < name.length; i++){
		if(name[i] == 'control'){
			if(state == undefined) continue;
			
			if(state == 'show'){
				document.getElementById("control_icon_control").title = 'Hide control panel';
				document.getElementById("control_icon_control_vertical").innerHTML = '&#8213;';
				document.getElementById("control_icon_control_horizontal").innerHTML = '';
			}
			if(state == 'hide'){
				document.getElementById("control_icon_control").title = 'Show control panel';
				document.getElementById("control_icon_control_vertical").innerHTML = '&#124;';
				document.getElementById("control_icon_control_horizontal").innerHTML = '&#8213;';
			}
			if(state == 'disable'){
				document.getElementById("control_icon_control").title = 'No control panel contents to display';
				document.getElementById("control_icon_control").style.color = '#cccccc';
				document.getElementById("control_icon_control").style.border = '1px solid #cccccc';
				document.getElementById("control_icon_control_vertical").innerHTML = '&#124;';
				document.getElementById("control_icon_control_horizontal").innerHTML = '&#8213;';
			}
		}

		if(name[i] == 'rotate'){

			if(state == undefined){
				if(BrowserDetect.browser == "Firefox"){
					document.getElementById("control_icon_rotate_arrow").style.fontSize = '0.9em';
					document.getElementById("control_icon_rotate_arrow").style.lineHeight = '16px';
					document.getElementById("control_icon_rotate_arrow").style.fontWeight = '100';
				}
			}
		}
	}

}
