function add(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] + b[i];
	return s;
}

function avec(a, b){
	s = 0;
	var d = dot(a,a) * dot(b,b);
	if(d < 0 || d > 0) s = Math.acos(dot(a,b)) / d;
	return s;
}

function cprod(u, v){
	s = new Array(3);
	s[0] = u[1]*v[2] - u[2]*v[1];
	s[1] = u[2]*v[0] - u[0]*v[2];
	s[2] = u[0]*v[1] - u[1]*v[0];
	return s;	
}

function dist(a, b){
	var i;
	var s = 0;
	for(i = 0;i < a.length;i++) s += Math.pow(b[i] - a[i], 2)
	return Math.sqrt(s);
}

function dot(a, b){
	var i;
	var s = 0;
	for(i = 0;i < a.length;i++) s += a[i]*b[i];
	return s;
}

function addC(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] + b;
	return s;
}

function inv(a){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = 1 / a[i];
	return s;
}

function maxC(numArray) {
  return Math.max.apply(null, numArray);
}

function minC(numArray) {
  return Math.min.apply(null, numArray);
}

function max() {
    var args = Array.prototype.slice.call(arguments);
    return Math.max.apply(Math, args.filter(function(val) {
       return !isNaN(val);
    }));
}

function min() {
    var args = Array.prototype.slice.call(arguments);
    return Math.min.apply(Math, args.filter(function(val) {
       return !isNaN(val);
    }));
}

function multC(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] * b;
	return s;
}

function multM(a, b){
	var i, j, k;
	var s = new Array();
	for (var i = 0, len = a.length; i < len; i++){
		if(!a[0].length){
			s[i] = 0;
			for (var j = 0, len = a.length; j < len; j++){
				s[i] += a[j]*b[j][i];
			}
		}else{
			s[i] = new Array(a[i].length);
			for (var k = 0, len = a[i].length; k < len; k++){
				s[i][k] = 0;
				for (var j = 0, len = a[i].length; j < len; j++){
					s[i][k] += a[i][j]*b[j][k];
				}
			}
		}
	}
	return s;
}

function rotateTrig(r){return {sin : -Math.sin(r), cos : Math.cos(r)};}

function rotateVector(v, deg){
	var rad = degToRad(deg);
	var vec = new Array(v[0]*Math.cos(rad) - v[1]*Math.sin(rad), v[0]*Math.sin(rad) + v[1]*Math.cos(rad));
	return vec;
}

function rotateX1(a, t) {
	var tmp = a.y;
	a.y = (t.cos * a.y) - (t.sin * a.z);
	a.z = (t.sin * tmp) + (t.cos * a.z);
	return a;
}

function rotateY1(a, t) {
	var tmp = a.x;
	a.x = (t.cos * a.x) + (t.sin * a.z);
	a.z = - (t.sin * tmp) + (t.cos * a.z);
	return a;
}

function rotateZ1(a, t) {
	var tmp = a.x;
	a.x = (t.cos * a.x) - (t.sin * a.y);
	a.y = (t.sin * tmp) + (t.cos * a.y);
	return a;
}

function rotateX2(a, t) {
	var tmp = a.y;
	a.y = sub(multC(a.y, t.cos), multC(a.z, t.sin));
	a.z = add(multC(tmp, t.sin), multC(a.z, t.cos));
	return a;
}

function rotateY2(a, t) {
	var tmp = a.x;
	a.x = add(multC(a.x, t.cos), multC(a.z, t.sin));
	a.z = sub(multC(a.z, t.cos), multC(tmp, t.sin));
	return a;
}

function rotateZ2(a, t) {
	var tmp = a.x;
	a.x = sub(multC(a.x, t.cos), multC(a.y, t.sin));
	a.y = add(multC(tmp, t.sin), multC(a.y, t.cos));
	return a;
}

function sub(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] - b[i];
	return s;
}

function tMatrixEP(v, a){
	a = -a;
	v = uvector(v);

	var t0 = Math.cos(a/2);
	var t1 = v[0]*Math.sin(a/2)
	var t2 = v[1]*Math.sin(a/2)
	var t3 = v[2]*Math.sin(a/2)

	var r = new Array(3);
	r[0] = new Array(2*(Math.pow(t0, 2) + Math.pow(t1, 2)) - 1, 2*(t1*t2 - t0*t3), 2*(t1*t3 + t0*t2));
	r[1] = new Array(2*(t1*t2 + t0*t3), 2*(Math.pow(t0, 2) + Math.pow(t2, 2)) - 1, 2*(t2*t3 - t0*t1));
	r[2] = new Array(2*(t1*t3 - t0*t2), 2*(t2*t3 + t0*t1), 2*(Math.pow(t0, 2) + Math.pow(t3, 2)) - 1);

	return r;
}

function uvector(v){
	var i, r;
	var s = 0;
	for(i = 0;i < v.length;i++) s += Math.pow(v[i], 2)
	var d = Math.sqrt(s);
	if(d == 0) return v;

	r = new Array();
	for(i = 0;i < v.length;i++) r[i] = v[i]/d
	return r;
}
