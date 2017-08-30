/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2007-2016. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * %CopyrightEnd%
 */

function size_image(img, src) {
	percept_content = document.getElementById("content");
	var width  = percept_content.offsetWidth - 120;
        var imgfile = "/cgi-bin/percept_graph/" + src + "&width=" + width;
	img.src = imgfile;
	img.onload = '';
}

function load_image() {
	var percept_graph = document.getElementById("percept_graph");
	if (percept_graph) {
		percept_content = document.getElementById("content");
		var width  = percept_content.offsetWidth - 50;
		var height = max(screen.height - 550, 600); 
		var rmin   = document.form_area.data_min.value;
		var rmax   = document.form_area.data_max.value;

		percept_graph.style.backgroundImage = "url('/cgi-bin/percept_graph/graph" +
			"?range_min=" + rmin + 
			"&range_max=" + rmax +
			"&width=" + width + 
			"&height=" + height + "')";
		percept_graph.style.width = width;
		percept_graph.style.height = height;
	}
}

function select_image() {
	var Graph = document.getElementById("percept_graph");
	if (Graph) {
	    var GraphIndex = document.form_area.graph_select.selectedIndex;
	    var GraphSelectValue = document.form_area.graph_select.options[GraphIndex].value;
	    Graph.style.backgroundImage = "url('" + GraphSelectValue +"')";
	}
}
   
function select_down(event) {
	var Graf = document.getElementById("percept_graph");
        var Area = document.getElementById("percept_areaselect");
        var x =  event.offsetX?(event.offsetX):event.pageX-Graf.offsetLeft;
	x = x - 60;
        
        var width  = Graf.offsetWidth;
	var height = Graf.offsetHeight;
	var margin = 20;

	var Xmin   = document.form_area.data_min.value;
        var Xmax   = document.form_area.data_max.value;

	// Trim edges

	if ( x < margin ) {
	    x = margin;
	}

        if ( x > width - margin ) {
	    x = width - margin;
	}

        Area.style.left = x;
        Area.style.top = height - margin;
        Area.style.width = 1;
        Area.style.height = margin;
        Area.moving = true;
        Area.bgcolor = "#00ff00";
	Area.style.visibility = "visible";
	Area.style.borderRight = "1px solid #000"
	Area.style.borderLeft = "1px solid #000"
        Area.style.opacity = 0.65;
        Area.style.filter = 'alpha(opacity=65)';
	var RangeMin = convert_image2graph(x, Xmin, Xmax, margin, width - margin);
        if (RangeMin == 0) document.form_area.range_min.value = 0.0;
	else document.form_area.range_min.value = RangeMin;
}

 function select_move(event) {
        var Graf = document.getElementById("percept_graph");
        var Area = document.getElementById("percept_areaselect");
        var x =  event.offsetX?(event.offsetX):event.pageX-Graf.offsetLeft;
	x = x - 60;
        if (Area.moving == true) {
	    
	    var width = Graf.offsetWidth;
	    var height = Graf.offsetHeight;
	    var margin = 20;
            var Xmin = document.form_area.data_min.value;
            var Xmax = document.form_area.data_max.value;
	    
	    // Trim edges
	
	    if ( x < margin ) {
	    	x = margin;
	    }

            if ( x > width - margin ) {
	    	x = width - margin;
	    }

            var x0 = min(x, Area.offsetLeft);
            var x1 = max(x, Area.offsetLeft);
            var w = (x1 - x0);
            Area.style.left = x0;
            Area.style.width = w;
	    var RangeMin = convert_image2graph(x0, Xmin, Xmax, margin, width - margin);
	    var RangeMax = convert_image2graph(x1, Xmin, Xmax, margin, width - margin);
	    Area.style.visibility = "visible";
           
	    if (RangeMin == 0) document.form_area.range_min.value = 0.0;
	    else document.form_area.range_min.value = RangeMin;
            if (RangeMax == 0) document.form_area.range_max.value = 0.0;
	    else document.form_area.range_max.value = RangeMax;
	}
}

function select_up(event) {
        var Graf = document.getElementById("percept_graph");
        var Area = document.getElementById("percept_areaselect");
        var x =  event.offsetX?(event.offsetX):event.pageX-Graf.offsetLeft;

	x = x - 60;
	var width = Graf.offsetWidth;
	var height = Graf.offsetHeight;
	var margin = 20;
        var Xmin = document.form_area.data_min.value;
        var Xmax = document.form_area.data_max.value;
	
	// Trim edges
	
	if ( x < margin ) {
	    x = margin;
	}

        if ( x > width - margin ) {
	    x = width - margin;
	}

	var w = (x - Area.style.offsetLeft);

        Area.moving = false;
        Area.style.width = w;
	var RangeMax = convert_image2graph(x, Xmin, Xmax, margin, width - margin);
        if (RangeMax == 0) document.form_area.range_max.value = 0.0;
	else document.form_area.range_max.value = RangeMax;
}

function min(A, B) {
        if (A > B) return B;
        else return A;
}

function max(A,B) {
        if (A > B) return A;
        else return B;
}

function convert_image2graph(X, Xmin, Xmax, X0, X1) {
	var ImageWidth = X1 - X0;
	var RangeWidth = Xmax - Xmin;
	var DX = RangeWidth/ImageWidth;
	var Xprime = (X - X0)*DX + Xmin*1.0;
	return Xprime;
}
