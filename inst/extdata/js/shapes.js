//******************************** Shape constructors **********************************//

function circle(x, y, z, r) {
	this.x = x;
	this.y = y;
	this.z = z;
	this.r = r;
}

function circleAnimate(x, y, z, r) {
	this.x = x;
	this.y = y;
	this.z = z;
	this.r = r;
}

function circleProject(x, y, r) {
	this.x = x;
	this.y = y;
	this.r = r;
}

function line(x1, y1, z1, x2, y2, z2) {
	this.x1 = x1;
	this.y1 = y1;
	this.z1 = z1;
	this.x2 = x2;
	this.y2 = y2;
	this.z2 = z2;
}

function lineAnimate(x1, y1, z1, x2, y2, z2) {
	this.x1 = x1;
	this.y1 = y1;
	this.z1 = z1;
	this.x2 = x2;
	this.y2 = y2;
	this.z2 = z2;
}

function lineProject(x1, y1, x2, y2) {
	this.x1 = x1;
	this.y1 = y1;
	this.x2 = x2;
	this.y2 = y2;
}

function arrow(x1, y1, z1, x2, y2, z2, l, a) {
	this.x1 = x1;
	this.y1 = y1;
	this.z1 = z1;
	this.x2 = x2;
	this.y2 = y2;
	this.z2 = z2;
	this.l = l;
	this.a = a;
}

function arrowAnimate(x1, y1, z1, x2, y2, z2, l, a) {
	this.x1 = x1;
	this.y1 = y1;
	this.z1 = z1;
	this.x2 = x2;
	this.y2 = y2;
	this.z2 = z2;
	this.l = l;
	this.a = a;
}

function arrowProject(x1, y1, x2, y2, hax, hay, hbx, hby) {
	this.x1 = x1;
	this.y1 = y1;
	this.x2 = x2;
	this.y2 = y2;
	this.hax = hax;
	this.hay = hay;
	this.hbx = hbx;
	this.hby = hby;
}

function path(x, y, z, d, close) {
	this.x = x;
	this.y = y;
	this.z = z;
	this.d = d;
	this.close = close;
}

function pathProject(x, y, d, close) {
	this.x = x;
	this.y = y;
	this.d = d;
	this.close = close;
}

function point(x, y, z) {
	this.x = x;
	this.y = y;
	this.z = z;
}

function pointAnimate(x, y, z) {
	this.x = x;
	this.y = y;
	this.z = z;
}

function pointProject(x, y) {
	this.x = x;
	this.y = y;
}

function text(x, y, z, s) {
	this.x = x;
	this.y = y;
	this.z = z;
	this.s = s;
}

function textAnimate(x, y, z, s) {
	this.x = x;
	this.y = y;
	this.z = z;
	this.s = s;
}

function textProject(x, y, s) {
	this.x = x;
	this.y = y;
	this.s = s;
}

function ZOrder(z, i, type){
	this.z = z;
	this.i = i;
	this.type = type;
}

function Layer(layer, i, type){
	this.layer = layer;
	this.i = i;
	this.type = type;
}

//*************************** Shape attribute constructors *****************************//

function imageAttributes(src, preserveAspectRatio, opacity, z_index, animate) {
	this.src = src;
	this.preserveAspectRatio = preserveAspectRatio;
	this.opacity = opacity;
	this.z_index = z_index;
	this.animate = animate;
}

function imageAttributesAnimate(i, src, preserveAspectRatio, opacity, z_index) {
	this.i = i;
	this.src = src;
	this.preserveAspectRatio = preserveAspectRatio;
	this.opacity = opacity;
	this.z_index = z_index;
}

function shapeAttributes(fill, fill_opacity, stroke, stroke_opacity, stroke_width, r, z_index, animate) {
	this.fill = fill;
	this.fill_opacity = fill_opacity;
	this.stroke = stroke;
	this.stroke_opacity = stroke_opacity;
	this.stroke_width = stroke_width;
	this.z_index = z_index;
	this.r = r;
	this.animate = animate;
}

function shapeAttributesAnimate(i, fill, fill_opacity, stroke, stroke_opacity, stroke_width, r, z_index) {
	this.i = i;
	this.fill = fill;
	this.fill_opacity = fill_opacity;
	this.stroke = stroke;
	this.stroke_opacity = stroke_opacity;
	this.stroke_width = stroke_width;
	this.r = r;
	this.z_index = z_index;
}

function textAttributes(text, text_anchor, fill, opacity, font_family, font_style, font_weight, letter_spacing, writing_mode, glyph_orientation_vertical, z_index, animate) {
	this.text = text;
	this.text_anchor = text_anchor;
	this.fill = fill;
	this.opacity = opacity;
	this.font_family = font_family;
	this.font_style = font_style;
	this.font_weight = font_weight;
	this.letter_spacing = letter_spacing;
	this.writing_mode = writing_mode;
	this.glyph_orientation_vertical = glyph_orientation_vertical;
	this.z_index = z_index;
	this.animate = animate;
}

function textAttributesAnimate(i, text_anchor, fill, opacity, font_family, font_style, font_weight, letter_spacing, writing_mode, glyph_orientation_vertical, z_index) {
	this.i = i;
	this.text_anchor = text_anchor;
	this.fill = fill;
	this.opacity = opacity;
	this.font_family = font_family;
	this.font_style = font_style;
	this.font_weight = font_weight;
	this.letter_spacing = letter_spacing;
	this.writing_mode = writing_mode;
	this.glyph_orientation_vertical = glyph_orientation_vertical;
	this.z_index = z_index;
}

//*********************************** Shape methods ************************************//

circle.prototype = {
    scale: function (s) {
		this.x *= s;
		this.y *= s;
		this.z *= s;
		this.r *= s;
    },

    rotateXaxis: function (t) {rotateX1(this, t);},
    rotateYaxis: function (t) {rotateY1(this, t);},
    rotateZaxis: function (t) {rotateZ1(this, t);},

	translateX: function (d) {this.x += d;},
    translateY: function (d) {this.y += d;},
    translateZ: function (d) {this.z += d;},

    translate: function (dx, dy, dz) {
		this.x += dx;
		this.y += dy;
		this.z += dz;
    },

    project: function (d, e, xs, ys) {
		var u = -(d - e) / (this.z - e);
		return new circleProject((u * this.x) + xs, -(u * this.y) + ys, this.r);
    },

    limits: function () {return {x:[this.x + this.r, this.x - this.r], y:[this.y + this.r, this.y - this.r], z:[this.z]};}
}

line.prototype = {
    scale: function (s) {
		this.x1 *= s;
		this.y1 *= s;
		this.z1 *= s;
		this.x2 *= s;
		this.y2 *= s;
		this.z2 *= s;
    },

    rotateXaxis: function (t) {
		var tmp = this.y1;
		this.y1 = (t.cos * this.y1) - (t.sin * this.z1);
		this.z1 = (t.sin * tmp) + (t.cos * this.z1);
		var tmp = this.y2;
		this.y2 = (t.cos * this.y2) - (t.sin * this.z2);
		this.z2 = (t.sin * tmp) + (t.cos * this.z2);
    },
    rotateYaxis: function (t) {
		var tmp = this.x1;
		this.x1 = (t.cos * this.x1) + (t.sin * this.z1);
		this.z1 = - (t.sin * tmp) + (t.cos * this.z1);
		var tmp = this.x2;
		this.x2 = (t.cos * this.x2) + (t.sin * this.z2);
		this.z2 = - (t.sin * tmp) + (t.cos * this.z2);
    },
    rotateZaxis: function (t) {
		var tmp = this.x1;
		this.x1 = (t.cos * this.x1) - (t.sin * this.y1);
		this.y1 = (t.sin * tmp) + (t.cos * this.y1);
		var tmp = this.x2;
		this.x2 = (t.cos * this.x2) - (t.sin * this.y2);
		this.y2 = (t.sin * tmp) + (t.cos * this.y2);
    },

    translateX: function (d) {this.x1 += d;this.x2 += d;},
    translateY: function (d) {this.y1 += d;this.y2 += d;},
    translateZ: function (d) {this.z1 += d;this.z2 += d;},

    translate: function (dx, dy, dz) {
		this.x1 += dx;
		this.y1 += dy;
		this.z1 += dz;
		this.x2 += dx;
		this.y2 += dy;
		this.z2 += dz;
    },

    project: function (d, e, xs, ys) {
		var u1 = -(d - e) / (this.z1 - e);
		var u2 = -(d - e) / (this.z2 - e);
		return new lineProject((u1 * this.x1) + xs,-(u1 * this.y1) + ys, (u2 * this.x2) + xs, -(u2 * this.y2) + ys);
    },

    limits: function () {return {x:[this.x1, this.x2], y:[this.y1, this.y2], z:[this.z1, this.z2]}}
}

arrow.prototype = {
    scale: function (s) {
		this.x1 *= s;
		this.y1 *= s;
		this.z1 *= s;
		this.x2 *= s;
		this.y2 *= s;
		this.z2 *= s;
		this.l *= s;
    },

    rotateXaxis: function (t) {
		var tmp = this.y1;
		this.y1 = (t.cos * this.y1) - (t.sin * this.z1);
		this.z1 = (t.sin * tmp) + (t.cos * this.z1);
		var tmp = this.y2;
		this.y2 = (t.cos * this.y2) - (t.sin * this.z2);
		this.z2 = (t.sin * tmp) + (t.cos * this.z2);
    },
    rotateYaxis: function (t) {
		var tmp = this.x1;
		this.x1 = (t.cos * this.x1) + (t.sin * this.z1);
		this.z1 = - (t.sin * tmp) + (t.cos * this.z1);
		var tmp = this.x2;
		this.x2 = (t.cos * this.x2) + (t.sin * this.z2);
		this.z2 = - (t.sin * tmp) + (t.cos * this.z2);
    },
    rotateZaxis: function (t) {
		var tmp = this.x1;
		this.x1 = (t.cos * this.x1) - (t.sin * this.y1);
		this.y1 = (t.sin * tmp) + (t.cos * this.y1);
		var tmp = this.x2;
		this.x2 = (t.cos * this.x2) - (t.sin * this.y2);
		this.y2 = (t.sin * tmp) + (t.cos * this.y2);
    },

    translateX: function (d) {this.x1 += d;this.x2 += d;},
    translateY: function (d) {this.y1 += d;this.y2 += d;},
    translateZ: function (d) {this.z1 += d;this.z2 += d;},

    translate: function (dx, dy, dz) {
		this.x1 += dx;
		this.y1 += dy;
		this.z1 += dz;
		this.x2 += dx;
		this.y2 += dy;
		this.z2 += dz;
    },

    project: function (d, e, xs, ys) {

		var u1 = -(d - e) / (this.z1 - e);
		var u2 = -(d - e) / (this.z2 - e);
		var v21 = [this.x2-this.x1,this.y2-this.y1,this.z2-this.z1];
		var v12 = [this.x1-this.x2,this.y1-this.y2,this.z1-this.z2];
		var v12u = uvector(v12);
		var c_prod1 = uvector(cprod(v21, [0,0,1]));
		var c_prod2 = uvector(cprod(v21, [c_prod1[0],c_prod1[1],c_prod1[2]]));
		if(c_prod2[0] == 0 && c_prod2[1] == 0 && c_prod2[2] == 0) c_prod2 = [0,1,0];
		if(Math.abs(c_prod2[0]) > 0.98) c_prod2 = [0,1,0];
		var rm1 = tMatrixEP(c_prod2, this.a);
		var rm2 = tMatrixEP(c_prod2, -this.a);

		// Adjust arrowhead length so that it is longer when z-component is higher (or else front on view hides most of arrowhead)
		var ahl = this.l+(Math.pow(Math.abs(v12u[2]),10)/1)*0.6*this.l -(Math.abs(v12u[0]*v12u[1])/0.5)*0.2*this.l
		
		var ha = add(multM(multC(v12u, ahl), rm1), [this.x2, this.y2, this.z2]);
		var hb = add(multM(multC(v12u, ahl), rm2), [this.x2, this.y2, this.z2]);
		var hau = -(d - e) / (ha[2] - e);
		var hbu = -(d - e) / (hb[2] - e);
		
		return new arrowProject((u1 * this.x1) + xs,-(u1 * this.y1) + ys, (u2 * this.x2) + xs, -(u2 * this.y2) + ys,
			(hau * ha[0]) + xs, -(hau * ha[1]) + ys, (hbu * hb[0]) + xs, -(hbu * hb[1]) + ys);
    },

    limits: function () {return {x:[this.x1, this.x2], y:[this.y1, this.y2], z:[this.z1, this.z2]}}
}

path.prototype = {
 
    scale: function (s) {
		this.x = multC(this.x, s);
		this.y = multC(this.y, s);
		this.z = multC(this.z, s);
    },

    rotateXaxis: function (t) {rotateX2(this, t);},
    rotateYaxis: function (t) {rotateY2(this, t);},
    rotateZaxis: function (t) {rotateZ2(this, t);},

    translateX: function (d) {this.x = addC(this.x, d);},
    translateY: function (d) {this.y = addC(this.y, d);},
    translateZ: function (d) {this.z = addC(this.z, d);},

    translate: function (dx, dy, dz) {
		this.x = addC(this.x, dx);
		this.y = addC(this.y, dy);
		this.z = addC(this.z, dz);
    },

	project: function(d, e, xs, ys){
		var u;
		var x = new Array(this.x.length);
		var y = new Array(this.x.length);
		for (var i = 0, len = this.x.length; i < len; i++){
			u = -(d - e) / (this.z[i] - e);
			x[i] = (u * this.x[i]) + xs;
			y[i] = -(u * this.y[i]) + ys;
		}
		//var u = multC(inv(addC(this.z, -e)), -(d - e));
		//return {x:addC(multC(this.x, u), xs), y:addC(multC(this.y, -u), ys), d:this.d, close:this.close};
		return new pathProject(x, y, this.d, this.close);
	},

    limits: function () {
    	return {
    		x:[min.apply(Math, this.x), max.apply(Math, this.x)], 
    		y:[min.apply(Math, this.y), max.apply(Math, this.y)], 
    		z:[min.apply(Math, this.z), max.apply(Math, this.z)]
		};
    }
}

point.prototype = {
    scale: function (s) {
		this.x *= s;
		this.y *= s;
		this.z *= s;
    },

    rotateXaxis: function (t) {rotateX1(this, t);},
    rotateYaxis: function (t) {rotateY1(this, t);},
    rotateZaxis: function (t) {rotateZ1(this, t);},

    translateX: function (d) {this.x += d;},
    translateY: function (d) {this.y += d;},
    translateZ: function (d) {this.z += d;},

    translate: function (dx, dy, dz) {
		this.x += dx;
		this.y += dy;
		this.z += dz;
    },

	project: function (d, e, xs, ys) {
		var u = -(d - e) / (this.z - e);
		return new pointProject((u * this.x) + xs, -(u * this.y) + ys);
	},

    limits: function () {return {x:[this.x], y:[this.y], z:[this.z]};}
}

text.prototype = {
    scale: function (s) {
		this.x *= s;
		this.y *= s;
		this.z *= s;
		this.s *= s;
    },

    rotateXaxis: function (t) {rotateX1(this, t);},
    rotateYaxis: function (t) {rotateY1(this, t);},
    rotateZaxis: function (t) {rotateZ1(this, t);},

    translateX: function (d) {this.x += d;},
    translateY: function (d) {this.y += d;},
    translateZ: function (d) {this.z += d;},

    translate: function (dx, dy, dz) {
		this.x += dx;
		this.y += dy;
		this.z += dz;
    },

	project: function (d, e, xs, ys) {
		var u = -(d - e) / (this.z - e);
		return new textProject((u * this.x) + xs, -(u * this.y) + ys, this.s);
	},

    limits: function () {return {x:[this.x], y:[this.y], z:[this.z]};}
}

circleAnimate.prototype = {
    scale: function (s) {
		this.x = multC(this.x, s);
		this.y = multC(this.y, s);
		this.z = multC(this.z, s);
		this.r = multC(this.r, s);
    },

    rotateXaxis: function (t) {rotateX2(this, t);},
    rotateYaxis: function (t) {rotateY2(this, t);},
    rotateZaxis: function (t) {rotateZ2(this, t);},

    translateX: function (d) {this.x = addC(this.x, d);},
    translateY: function (d) {this.y = addC(this.y, d);},
    translateZ: function (d) {this.z = addC(this.z, d);},

    translate: function (dx, dy, dz) {
		this.x = addC(this.x, dx);
		this.y = addC(this.y, dy);
		this.z = addC(this.z, dz);
    },

    limits: function () {
    	return {
    		x:[min.apply(Math, add(this.x, this.r)), min.apply(Math, sub(this.x, this.r)), max.apply(Math, add(this.x, this.r)), max.apply(Math, sub(this.x, this.r))], 
    		y:[min.apply(Math, add(this.y, this.r)), min.apply(Math, sub(this.y, this.r)), max.apply(Math, add(this.y, this.r)), max.apply(Math, sub(this.y, this.r))], 
    		z:[min.apply(Math, add(this.z, this.r)), min.apply(Math, sub(this.z, this.r)), max.apply(Math, add(this.z, this.r)), max.apply(Math, sub(this.z, this.r))], 
		};
    }
}

pointAnimate.prototype = {
    scale: function (s) {
		this.x = multC(this.x, s);
		this.y = multC(this.y, s);
		this.z = multC(this.z, s);
    },

    rotateXaxis: function (t) {rotateX2(this, t);},
    rotateYaxis: function (t) {rotateY2(this, t);},
    rotateZaxis: function (t) {rotateZ2(this, t);},

    translateX: function (d) {this.x = addC(this.x, d);},
    translateY: function (d) {this.y = addC(this.y, d);},
    translateZ: function (d) {this.z = addC(this.z, d);},

    translate: function (dx, dy, dz) {
		this.x = addC(this.x, dx);
		this.y = addC(this.y, dy);
		this.z = addC(this.z, dz);
    },

    limits: function () {
    	return {
    		x:[min.apply(Math, this.x), max.apply(Math, this.x)], 
    		y:[min.apply(Math, this.y), max.apply(Math, this.y)], 
    		z:[min.apply(Math, this.z), max.apply(Math, this.z)]
		};
    }
}

lineAnimate.prototype = {
    scale: function (s) {
		this.x1 = multC(this.x1, s);
		this.y1 = multC(this.y1, s);
		this.z1 = multC(this.z1, s);
		this.x2 = multC(this.x2, s);
		this.y2 = multC(this.y2, s);
		this.z2 = multC(this.z2, s);
    },

    rotateXaxis: function (t) {
		var tmp = this.y1;
		this.y1 = sub(multC(this.y1, t.cos), multC(this.z1, t.sin));
		this.z1 = add(multC(tmp, t.sin), multC(this.z1, t.cos));
		var tmp = this.y2;
		this.y2 = sub(multC(this.y2, t.cos), multC(this.z2, t.sin));
		this.z2 = add(multC(tmp, t.sin), multC(this.z2, t.cos));
    },
    rotateYaxis: function (t) {
		var tmp = this.x1;
		this.x1 = add(multC(this.x1, t.cos), multC(this.z1, t.sin));
		this.z1 = sub(multC(this.z1, t.cos), multC(tmp, t.sin));
		var tmp = this.x2;
		this.x2 = add(multC(this.x2, t.cos), multC(this.z2, t.sin));
		this.z2 = sub(multC(this.z2, t.cos), multC(tmp, t.sin));
    },
    rotateZaxis: function (t) {
		var tmp = this.x1;
		this.x1 = sub(multC(this.x1, t.cos), multC(this.y1, t.sin));
		this.y1 = add(multC(tmp, t.sin), multC(this.y1, t.cos));
		var tmp = this.x2;
		this.x2 = sub(multC(this.x2, t.cos), multC(this.y2, t.sin));
		this.y2 = add(multC(tmp, t.sin), multC(this.y2, t.cos));
    },

    translateX: function (d) {
    	this.x1 = addC(this.x1, d);
    	this.x2 = addC(this.x2, d);
    },
    translateY: function (d) {
    	this.y1 = addC(this.y1, d);
    	this.y2 = addC(this.y2, d);
    },
    translateZ: function (d) {
    	this.z1 = addC(this.z1, d);
    	this.z2 = addC(this.z2, d);
    },

    translate: function (dx, dy, dz) {
		this.x1 = addC(this.x1, dx);
		this.y1 = addC(this.y1, dy);
		this.z1 = addC(this.z1, dz);
		this.x2 = addC(this.x2, dx);
		this.y2 = addC(this.y2, dy);
		this.z2 = addC(this.z2, dz);
    },

   limits: function () {
    	return {
    		x:[min.apply(Math, this.x1), min.apply(Math, this.x2), max.apply(Math, this.x1), max.apply(Math, this.x2)], 
    		y:[min.apply(Math, this.y1), min.apply(Math, this.y2), max.apply(Math, this.y1), max.apply(Math, this.y2)], 
    		z:[min.apply(Math, this.z1), min.apply(Math, this.z2), max.apply(Math, this.z1), max.apply(Math, this.z2)]
		};
    }
}

arrowAnimate.prototype = {
    scale: function (s) {
		this.x1 = multC(this.x1, s);
		this.y1 = multC(this.y1, s);
		this.z1 = multC(this.z1, s);
		this.x2 = multC(this.x2, s);
		this.y2 = multC(this.y2, s);
		this.z2 = multC(this.z2, s);
    },

    rotateXaxis: function (t) {
		var tmp = this.y1;
		this.y1 = sub(multC(this.y1, t.cos), multC(this.z1, t.sin));
		this.z1 = add(multC(tmp, t.sin), multC(this.z1, t.cos));
		var tmp = this.y2;
		this.y2 = sub(multC(this.y2, t.cos), multC(this.z2, t.sin));
		this.z2 = add(multC(tmp, t.sin), multC(this.z2, t.cos));
    },
    rotateYaxis: function (t) {
		var tmp = this.x1;
		this.x1 = add(multC(this.x1, t.cos), multC(this.z1, t.sin));
		this.z1 = sub(multC(this.z1, t.cos), multC(tmp, t.sin));
		var tmp = this.x2;
		this.x2 = add(multC(this.x2, t.cos), multC(this.z2, t.sin));
		this.z2 = sub(multC(this.z2, t.cos), multC(tmp, t.sin));
    },
    rotateZaxis: function (t) {
		var tmp = this.x1;
		this.x1 = sub(multC(this.x1, t.cos), multC(this.y1, t.sin));
		this.y1 = add(multC(tmp, t.sin), multC(this.y1, t.cos));
		var tmp = this.x2;
		this.x2 = sub(multC(this.x2, t.cos), multC(this.y2, t.sin));
		this.y2 = add(multC(tmp, t.sin), multC(this.y2, t.cos));
    },

    translateX: function (d) {
    	this.x1 = addC(this.x1, d);
    	this.x2 = addC(this.x2, d);
    },
    translateY: function (d) {
    	this.y1 = addC(this.y1, d);
    	this.y2 = addC(this.y2, d);
    },
    translateZ: function (d) {
    	this.z1 = addC(this.z1, d);
    	this.z2 = addC(this.z2, d);
    },

    translate: function (dx, dy, dz) {
		this.x1 = addC(this.x1, dx);
		this.y1 = addC(this.y1, dy);
		this.z1 = addC(this.z1, dz);
		this.x2 = addC(this.x2, dx);
		this.y2 = addC(this.y2, dy);
		this.z2 = addC(this.z2, dz);
    },

   limits: function () {
    	return {
    		x:[min.apply(Math, this.x1), min.apply(Math, this.x2), max.apply(Math, this.x1), max.apply(Math, this.x2)], 
    		y:[min.apply(Math, this.y1), min.apply(Math, this.y2), max.apply(Math, this.y1), max.apply(Math, this.y2)], 
    		z:[min.apply(Math, this.z1), min.apply(Math, this.z2), max.apply(Math, this.z1), max.apply(Math, this.z2)]
		};
    }
}

textAnimate.prototype = {
    scale: function (s) {
		this.x = multC(this.x, s);
		this.y = multC(this.y, s);
		this.z = multC(this.z, s);
		this.s = multC(this.s, s);
    },

    rotateXaxis: function (t) {rotateX2(this, t);},
    rotateYaxis: function (t) {rotateY2(this, t);},
    rotateZaxis: function (t) {rotateZ2(this, t);},

    translateX: function (d) {this.x = addC(this.x, d);},
    translateY: function (d) {this.y = addC(this.y, d);},
    translateZ: function (d) {this.z = addC(this.z, d);},

    translate: function (dx, dy, dz) {
		this.x = addC(this.x, dx);
		this.y = addC(this.y, dy);
		this.z = addC(this.z, dz);
    },

    limits: function () {
    	return {
    		x:[min.apply(Math, this.x), max.apply(Math, this.x)], 
    		y:[min.apply(Math, this.y), max.apply(Math, this.y)], 
    		z:[min.apply(Math, this.z), max.apply(Math, this.z)]
		};
    }
}

//********************************** Shape functions ***********************************//

function attributesToString(attributes){

	s = "";

	if(attributes.stroke !== undefined) s += "stroke:" + attributes.stroke + ";";
	if(attributes.stroke_width !== undefined) s += "stroke-width:" + attributes.stroke_width + ";";
	if(attributes.stroke_opacity !== undefined) s += "stroke-opacity:" + attributes.stroke_opacity + ";";
	if(attributes.fill !== undefined) s += "fill:" + attributes.fill + ";";
	if(attributes.fill_opacity !== undefined) s += "fill-opacity:" + attributes.fill_opacity + ";";
	if(attributes.r !== undefined) s += "r:" + attributes.r + ";";

	if(attributes.opacity !== undefined) s += "opacity:" + attributes.opacity + ";";
	if(attributes.font_family !== undefined) s += "font-family:" + attributes.font_family + ";";
	if(attributes.font_size !== undefined) s += "font-size:" + attributes.font_size + ";";
	if(attributes.font_weight !== undefined) s += "font-weight:" + attributes.font_weight + ";";
	if(attributes.font_style !== undefined) s += "font-style:" + attributes.font_style + ";";
	if(attributes.letter_spacing !== undefined) s += "letter-spacing:" + attributes.letter_spacing + ";";
	if(attributes.text_anchor !== undefined) s += "text-anchor:" + attributes.text_anchor + ";";
	if(attributes.writing_mode !== undefined) s += "writing-mode:" + attributes.writing_mode + ";";
	if(attributes.glyph_orientation_vertical !== undefined) s += "glyph-orientation-vertical:" + attributes.glyph_orientation_vertical + ";";

	return s;
}

function fillArray(value, len) {
	var i, j, jlen;
	var value_length = value.length;
	if(typeof(value) == 'string') value_length = 1;
	if(value_length > 1){ // Check for multiple values
		var mv = true;
		var nlen = Math.floor(len / value_length);
		var add = len - nlen*value_length
		len = nlen;
		jlen = value_length;
	}
	var arr = [];
	if(mv){
		for (i = 0; i < len; i++) for (j = 0; j < jlen; j++) arr.push(value[j]);
		for (i = 0; i < add; i++) arr.push(value[i]);
	}else{
		for (i = 0; i < len; i++) arr.push(value);
	}
	return arr;
}

function parseSVGPath(path_in){

	// Add spaces between numbers and letters
	var str = path_in.replace(/((\.|-|\d)+)/g, function (_, num){return ' ' + num + ' ';}).trim();

	// Split path
	str_split = str.split(/[\s|,]/);

	// Remove empty values
	str_split = clean(str_split);

	// Check for z at the end (closed path), and if found remove from end
	var close = false;
	if(str_split[str_split.length-1] == 'Z' || str_split[str_split.length-1] == 'z'){
		close = true;
		str_split.pop();
	}

	var x = new Array();
	var y = new Array();
	var z = new Array();
	var d = new Array();

	// Sort into x,y,z coordinates and path type characters
	var num_ct = 0;
	for (var i = 0, len = str_split.length; i < len; i++){

		if(is_numeric(str_split[i])){
			if(num_ct == 0) x.push(parseFloat(str_split[i]));
			if(num_ct == 1) y.push(parseFloat(str_split[i]));
			if(num_ct == 2) z.push(parseFloat(str_split[i]));
			if(num_ct == 3){
				d.push(',');
				x.push(parseFloat(str_split[i]));
				num_ct = 1;
				continue;
			}
			num_ct++;
		}else{
			d.push(str_split[i]);
			num_ct = 0;
		}
	}
	
	//alert(str_split)
	//alert(x + '\n' + y + '\n' + z + '\n' + d + '\n' + close)

	return new path(x, y, z, d, close);
}

function pathToString(path){

	var str = "";
	for (var i = 0, len = path.x.length; i < len; i++){
		str += path.d[i] + ' ' + path.x[i] + ' ' + path.y[i] + ' ';
	}
	str = str.trim();
	if(path.close) str += ' Z';

	return str;
}

function pointsToPath(points, d){

	var i;
	var str = "", letter = "";

	for (i = 0, len = d.length; i < len; i++){
		if(i == 0) letter = "M";
		if(i > 0) letter = "L";
		str += letter + " " + points[d[i]-1].x + " " + points[d[i]-1].y + " ";
	}

	str = str.trim();

	return str;
}

function project(object, d, e, xs, ys){
	var projection = new Array();
	for (var i = 0, len = object.length; i < len; i++) projection[i] = object[i].project(d, e, xs, ys);
	return projection;
}

function scale(object, s){
	for (var i = 0, len = object.length; i < len; i++) object[i].scale(s);
}

function translate(object, dx, dy, dz){
	for (var i = 0, len = object.length; i < len; i++) object[i].translate(dx, dy, dz);
}

function rotateXaxis(object, d){for (var i = 0, len = object.length; i < len; i++) object[i].rotateXaxis(rotateTrig(d));}
function rotateYaxis(object, d){for (var i = 0, len = object.length; i < len; i++) object[i].rotateYaxis(rotateTrig(d));}
function rotateZaxis(object, d){for (var i = 0, len = object.length; i < len; i++) object[i].rotateZaxis(rotateTrig(d));}

function translateX(object, d){for (var i = 0, len = object.length; i < len; i++) object[i].translateX(d);}
function translateY(object, d){for (var i = 0, len = object.length; i < len; i++) object[i].translateY(d);}
function translateZ(object, d){for (var i = 0, len = object.length; i < len; i++) object[i].translateZ(d);}

function limits(object){

	if(object.length == 0) return {NaN,NaN, NaN,NaN, NaN,NaN}

	var shape_limits;
	var x = [], y = [], z = [];

	for (var i = 0, len = object.length; i < len; i++){

		shape_limits = object[i].limits();

		x.push.apply(x, shape_limits.x);
		y.push.apply(y, shape_limits.y);
		z.push.apply(z, shape_limits.z);
	}

	// Remove NaN values
	x = clean(x, NaN);
	y = clean(y, NaN);
	z = clean(z, NaN);
	
	if(x.length == 0) return {NaN,NaN, NaN,NaN, NaN,NaN}
	
	return {
		xmin:min.apply(Math, x), xmax:max.apply(Math, x),
		ymin:min.apply(Math, y), ymax:max.apply(Math, y),
		zmin:min.apply(Math, z), zmax:max.apply(Math, z)
	}
}
