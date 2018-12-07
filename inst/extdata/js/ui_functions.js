var initialX;
var initialY;
function_name = new Array();
url_path = new Array();
innerHTML_id = new Array();
request_status = new Array();
xmlHttp = new Array();
time1 = new Array();
time2 = new Array();
var req_ct = 0;
var skip_buffer = 10;
var skip_to = 0;
var ajax_output;
var auto_view_r_result = 0;

String.prototype.firstToUpperCase = function() {
	return this.charAt(0).toUpperCase() + this.slice(1);
}

Array.prototype.firstToUpperCase = function() {
	for (var i = 0, len = this.length; i < len; i++) this[i] = this[i].firstToUpperCase();
	return this;
}

function about_alert(){
	alert('svgViewR is a browser-based viewer that allows users to interactively visualize 3D vector graphics and animations. Visualizations can be made in R, using the package svgViewR, and easily distributed as a single HTML file with nearly universal compatibility across browsers and operating systems. This visualization was made using svgViewR version ' + svgviewr_version + '.\n\nsvgViewR was created by Aaron Olsen and is freely available under a GPL-3 license. For more information, please visit:\n\nhttp://cran.r-project.org/web/packages/svgViewR');
}

function ajax_alert_return(j, xmlHttp_object){
	alert(xmlHttp_object[j].responseText);
}

function ajax_fill_element(j, xmlHttp_object){
	document.getElementById(innerHTML_id[j]).innerHTML = xmlHttp_object[j].responseText;
	window.status= '';
}

function ajax_fill_element_return_call(j, xmlHttp_object){
	document.getElementById(innerHTML_id[j]).innerHTML = xmlHttp_object[j].responseText;
	on_ajax_request_return();
}

function ajax_fill_element_return_call2(j, xmlHttp_object){
	document.getElementById(innerHTML_id[j]).innerHTML = xmlHttp_object[j].responseText;
	on_ajax_request_return2(url_path[j]);
}

function ajax_get_xml_http_object(){
	var xmlHttp=null;
	try{ // Firefox, Opera 8.0+, Safari
		xmlHttp = new XMLHttpRequest();}
	catch(e){ // Internet Explorer		
		try{
			xmlHttp = new ActiveXObject("Msxml2.XMLHTTP");}
		catch(e){
			xmlHttp = new ActiveXObject("Microsoft.XMLHTTP");}
	}
	return xmlHttp;
}

function ajax_request(url, send_content, call_function, element_id){
	url_path[req_ct] = url;
	function_name[req_ct] = call_function;
	innerHTML_id[req_ct] = element_id;
	request_status[req_ct] = 0;

	xmlHttp[req_ct]=ajax_get_xml_http_object();
	if(xmlHttp[req_ct]==null){
		alert ("Browser does not support HTTP Request");
		return;}
	xmlHttp[req_ct].onreadystatechange=ajax_state_changed;
	xmlHttp[req_ct].open("POST", url, true);
	xmlHttp[req_ct].setRequestHeader("Content-type","application/x-www-form-urlencoded");
	xmlHttp[req_ct].send(send_content);	
	window.status= 'Command to ' + url + ' sent.';

	time1[req_ct] = new Date().getTime() / 1000;

	req_ct++;
}

function ajax_state_changed(){
	var j;
	for(j = skip_to;j < xmlHttp.length; j++){
		if(request_status[j] == 1){continue;}
		if(xmlHttp[j].readyState==4 || xmlHttp[j].readyState=="complete"){
			//alert(skip_to + ' -- ' + j);
			eval(function_name[j] + '(j, xmlHttp)');

			time2[j] = new Date().getTime() / 1000;
			if(document.getElementById("run_time")) document.getElementById("run_time").innerHTML = "Run-time: " + Math.round((time2[j] - time1[j])*100)/100 + " sec";
			
			request_status[j] = 1;
			if(j - skip_buffer > 0){skip_to = j - skip_buffer;}
		}	
	}
}

function array_unique(arr) {
	var hash = {}, result = [];
	for(var i = 0, l = arr.length;i < l;++i){
    	if(hash[ arr[i] ] === true) continue;
		hash[ arr[i] ] = true;
		result.push(arr[i]);    	
    }
    return result;
}

function array_unique_properties(arr, property) {
	var hash = {}, result = [];
	for(var i = 0, l = arr.length;i < l;++i){
    	if(hash[ arr[i][property] ] === true) continue;
		hash[ arr[i][property] ] = true;
		result.push(arr[i][property]);    	
    }
    return result;
}

function baseName(ev) {
	var id = ev.target.getAttribute("id");
	return id;
}

var BrowserDetect = {
	//Source: http://www.quirksmode.org/js/detect.html

	init: function () {
		this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
		this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
		this.OS = this.searchString(this.dataOS) || "an unknown OS";
	},
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera",
			versionSearch: "Version"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
			string: navigator.userAgent,
			subString: "Mozilla",
			identity: "Netscape",
			versionSearch: "Mozilla"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
			   string: navigator.userAgent,
			   subString: "iPhone",
			   identity: "iPhone/iPod"
	    },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	]

};

BrowserDetect.init();

function clean(array, deleteValue){

	var array_clean = new Array();

	if(deleteValue == undefined) var deleteValue = '';

	var j = 0;
	for(var i = 0; i < array.length; i++) {

		if(array[i] == deleteValue) continue;
		if(isNaN(deleteValue) && isNaN(array[i])) continue;

		array_clean[j] = array[i];

		j++;
	}

	return array_clean;
}

function createCookie(name, value, sec){

	if (sec) {
		var date = new Date();
		date.setTime(date.getTime()+(sec*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function degToRad(d){
	return d*(Math.PI/180);
}

function detectKeyUp(e){
	var evt = e || window.event;
	if(evt.keyCode==82 && globalkeypress == "r"){globalkeypress = '';return;}
}

function deleteCookie(name) {
	createCookie(name,"",-1);
}

function detectKeyDown(e){
	var evt = e || window.event;
	if(evt.keyCode==82){globalkeypress = 'r';} // r
	if(evt.shiftKey === true){
		if(globalkeypress == 'r'){
			if(evt.keyCode==37){setKeyEvent("rotateYaxis", 45);} // left arrow
			if(evt.keyCode==39){setKeyEvent("rotateYaxis", -45);} // right arrow
			if(evt.keyCode==38){setKeyEvent("rotateXaxis", 45);} // up arrow
			if(evt.keyCode==40){setKeyEvent("rotateXaxis", -45);} // down arrow
			if(evt.keyCode==74){setKeyEvent("rotateZaxis", -45);} // j
			if(evt.keyCode==75){setKeyEvent("rotateZaxis", 45);} // k
		}else{
			if(evt.keyCode==37){setKeyEvent("translateX", -10);} // left arrow
			if(evt.keyCode==39){setKeyEvent("translateX", 10);} // right arrow
			if(evt.keyCode==38){setKeyEvent("translateY", 10);} // up arrow
			if(evt.keyCode==40){setKeyEvent("translateY", -10);} // down arrow
			if(evt.keyCode==74){setKeyEvent("translateZ", 10);} // j
			if(evt.keyCode==75){setKeyEvent("translateZ", -10);} // k
		}
	}else{
		if(evt.metaKey === true){
		}else{
			if(globalkeypress == 'r'){
				if(evt.keyCode==37){setKeyEvent("rotateYaxis", 10);} // left arrow
				if(evt.keyCode==39){setKeyEvent("rotateYaxis", -10);} // right arrow
				if(evt.keyCode==38){setKeyEvent("rotateXaxis", 10);} // up arrow
				if(evt.keyCode==40){setKeyEvent("rotateXaxis", -10);} // down arrow
				if(evt.keyCode==74){setKeyEvent("rotateZaxis", -10);} // j
				if(evt.keyCode==75){setKeyEvent("rotateZaxis", 10);} // k
			}else{
				if(evt.keyCode==37){setKeyEvent("translateX", -1);} // left arrow
				if(evt.keyCode==39){setKeyEvent("translateX", 1);} // right arrow
				if(evt.keyCode==38){setKeyEvent("translateY", 1);} // up arrow
				if(evt.keyCode==40){setKeyEvent("translateY", -1);} // down arrow
				if(evt.keyCode==74){setKeyEvent("translateZ", 1);} // j
				if(evt.keyCode==75){setKeyEvent("translateZ", -1);} // k
				//if(evt.keyCode==80){} // p
				if(evt.keyCode==32){setKeyEvent("playPauseAnimation");} // space-bar
			}
		}
	}
}

function doEvent(){

	var i, len;
	var dx = 0;
	var dy = 0;
	var dz = 0;

	if (currentEvent == "callZoomIn"){
		if(zoom <= 400){
			zoom = zoom + 4;
			depth = Math.floor(zoom * (eyez - maxzoom) / 100 + eyez);
		}
	}

	if (currentEvent == "callZoomOut"){
		if(zoom >= 175){
			zoom = zoom - 4;
			depth = Math.floor(zoom * (eyez - maxzoom) / 100 + eyez);
		}
	}

	if(currentEvent == "translateZ") d = -d;

	// Apply translation
	if (currentEvent == "translateX" || currentEvent == "translateY" || currentEvent == "translateZ")
		for (i = 0, len = shape_transform.length; i < len; i++) eval(currentEvent + '(' + shape_transform[i] + ', d)');

	// Apply rotation
//	var t1 = new Date().getTime();
	if (currentEvent == "rotateXaxis" || currentEvent == "rotateYaxis" || currentEvent == "rotateZaxis")
		for (i = 0, len = shape_transform.length; i < len; i++) eval(currentEvent + '(' + shape_transform[i] + ', degToRad(d))');
//	var t2 = new Date().getTime();
//	alert(t2 - t1)

	if (currentEvent == "playPauseAnimation"){
		playPauseAnimation();
	}
}

function extract_number(strString){
	var strValidChars = "0123456789.-";
	var strChar;
	var returnString = "";
	var i;
	
	if (strString.length == 0) return false;

	for (i = 0; i < strString.length; i++){
		strChar = strString.charAt(i);
		if(strValidChars.indexOf(strChar) > -1) returnString += strChar;
	}
	return parseFloat(returnString);
}

function extract_alpha(strString){
	var strNonAlphaChars = "0123456789.-";
	var strChar;
	var returnString = "";
	var i;
	
	if (strString.length == 0) return false;

	for (i = 0; i < strString.length; i++){
		strChar = strString.charAt(i);
		if(strNonAlphaChars.indexOf(strChar) == -1) returnString += strChar;
	}
	return returnString;
}

function firstToUpperCase(string){
	if(typeof string === 'string'){
		return string.charAt(0).toUpperCase() + string.slice(1);
	}else{
		var string_array = new Array();
		for (var i = 0, len = string.length; i < len; i++) string_array[i] = firstToUpperCase(string[i]);
		return string_array;
	}
}

function getCookie(Name){ 
	var re=new RegExp(Name+"=[^;]+", "i"); //construct RE to search for target name/value pair
	if (document.cookie.match(re)) //if cookie found
		return document.cookie.match(re)[0].split("=")[1] //return its value
	return false;
}

function getMouseXYLocal(e){
	if(IE){return new Array(event.clientX,event.clientY);}else{return new Array(e.pageX,e.pageY);}
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

function getWindowHeight(){
	var y = 0;
	if (self.innerHeight) {
		y = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) {
		y = document.documentElement.clientHeight;
	} else if (document.body) {
		y = document.body.clientHeight;
	}

	return y;
}

function getWindowWidth(){
	var x = 0;
	if (self.innerHeight) {
		x = self.innerWidth;
	} else if (document.documentElement && document.documentElement.clientHeight) {
		x = document.documentElement.clientWidth;
	} else if (document.body) {
		x = document.body.clientWidth;
	}
	
	return x;
}

function has_numeric(strString){
	var strValidChars = "0123456789.-";
	var strChar;
	var blnResult = false;
	var i;
	
	if (strString.length == 0) return false;

	//  test strString consists of valid characters listed above
	for (i = 0; i < strString.length && blnResult == false; i++){
		strChar = strString.charAt(i);
		if (strValidChars.indexOf(strChar) > -1){
			blnResult = true;
		}
	}
	return blnResult;
}

function is_array(input){
	return typeof(input)=='object'&&(input instanceof Array);
}

function is_numeric(strString){
	var strValidChars = "0123456789.-";
	var strChar;
	var blnResult = true;
	var i;
	
	if (strString.length == 0) return false;

	//  test strString consists of valid characters listed above
	for (i = 0; i < strString.length && blnResult == true; i++){
		strChar = strString.charAt(i);
		if (strValidChars.indexOf(strChar) == -1){
			blnResult = false;
		}
	}
	return blnResult;
}

function listCookies(){

    var all_cookies = document.cookie.split(';');
    var cookies = new Array();

    for (var i = 0; i < all_cookies.length; i++) {
		var cookie_split = all_cookies[i].split('=');
		cookies[i] = {name : cookie_split[0], value : cookie_split[1]};
    }

    return cookies;
}

function mouseDownEvent(e){

	e.preventDefault();

	initialX = e.clientX;
	initialY = e.clientY;
	mousedown = 1;
}

function mouseUpEvent(e){	

	e.preventDefault();

	mousedown = 0;
}

function on_ajax_request_return2(url){
	ajax_request(url, 'function=print_ui', 'ajax_fill_element', 'ui_div');
	if(auto_view_r_result){
		document.getElementById('r_call_view').submit();
	}
	window.status= '';
}

function open_text_in_new_window(){
	window.open('A.html','A','width=300, height=200');
}

function parse_style_string(string){

	style_array = new Array();
	string_split1 = string.split(";");

	for(i in string_split1){
		if(string_split1[i] == "") continue;
		string_split2 = string_split1[i].split(":");
		style_array[string_split2[0]] = string_split2[1];
	}

	return style_array; 
}

function scrollEvent(e){
	var id = baseName(e);
	var delta = 0;

	//var start_a = new Date().getTime();

	if(IE){
		s_initialX = event.clientX + document.body.scrollLeft;
		s_initialY = event.clientY + document.body.scrollTop;
		if(!event){event = window.event;}
    	var delta = event.wheelDelta / 40;
	}else{
		s_initialX = e.pageX;
		s_initialY = e.pageY;
	}

	if(e.detail){
		var delta = -e.detail*5;
		var max = 8;
		if(delta > max){delta = max;}
		if(delta < -max){delta = -max;}
		zoom_shape(Math.round(delta*0.2)/20,s_initialX,s_initialY);
	}

	if(e.wheelDelta){ // SAFARI
		var delta = e.wheelDelta;
		var max = 60;
		if(delta > max){delta = max;}
		if(delta < -max){delta = -max;}
		zoom_shape(Math.round(delta*0.1)/20,s_initialX,s_initialY);
	}

	//var end_a = new Date().getTime();
	//document.getElementById("div_status").innerHTML += (end_a - start_a) + ' msec<br />';

}

function setKeyEvent(eventin,dist){
	currentEvent = eventin;
	d = dist;

	if (eventin !== "nothing" && eventin !== "refreshtozero"){

		doEvent();
		updateShapes();

		set_key_event_calls += 1;

		//document.getElementById("div_status").innerHTML = set_key_event_calls;

//	var start_b = new Date().getTime();
//	var end_b = new Date().getTime();
//	alert('A: ' + (end_a - start_a) + ' msec' + '\n' + 'B: ' + (end_b - start_b) + ' msec');
	}
}

function setEvents(e) {
	var eventHandler = detectKeyDown;
	document['on'+this.id] = eventHandler;
}

function setViewboxProperties(){
	window_properties.height = getWindowHeight();
	window_properties.width = getWindowWidth();

	view_box.height = window_properties.height;
	view_box.width = window_properties.width;

	x_window_shift = view_box.width/2;
	y_window_shift = view_box.height/2;
}

function sortasc(a,b){
	return a - b;
}

function string_to_array_cs(string){

	var i, j;
	var t_arr = string.split(",");

	r_arr = new Array();
	for(i = 0;i < t_arr.length;i++){
		i_arr = trim(t_arr[i]).split(" ");
		
		r_arr[i] = new Array();
		for(j = 0;j < i_arr.length;j++) r_arr[i][j] = parseFloat(i_arr[j])
	}

	return r_arr;
}

function updateVisibility(){

	var i,j;
	var visibility;
	var layers = new Array();

	// CYCLE THROUGH TABLE ELEMENTS
	var div_control_panel_layers = document.getElementById("control_panel_layers");
	var items = div_control_panel_layers.getElementsByTagName("*");
	for (i = items.length; i--;) {

		if(items[i].tagName == 'TD' && items[i].name == 'label') layer = items[i].innerHTML;

		if(items[i].tagName == 'INPUT'){
			layers[i] = new Array();
			layers[i].name = layer;
			if(items[i].checked){layers[i].visibility = '';}else{layers[i].visibility = 'hidden';}
		}
	}	

	var items = svgDocument.getElementsByTagName("*");
	for (i = items.length; i--;) {

		visibility = '';
		for(j in layers) if(items[i].getAttribute("layer") == layers[j].name) visibility = layers[j].visibility;
		items[i].setAttribute("visibility", visibility);
	}
}

function toggle_div_visibility(id){
	div = document.getElementById(id);
	if(div.style.visibility == 'hidden'){
		div.style.visibility = '';
		div.style.height = '';
		//div.style.width = new_width + 'px';
	}else{
		div.style.visibility = 'hidden';
		div.style.height = '0px';
		div.style.width = '0px';
	}
}

function trim(stringToTrim){
	//http://www.somacon.com/p355.php
	return stringToTrim.replace(/^\s+|\s+$/g,"");
}

function ltrim(stringToTrim){
	//http://www.somacon.com/p355.php
	return stringToTrim.replace(/^\s+/,"");
}

function rtrim(stringToTrim){
	//http://www.somacon.com/p355.php
	return stringToTrim.replace(/\s+$/,"");
}
