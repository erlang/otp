// ######################################################################

// ## flipMenu 5.0.0 (c) J. Reijers
// ## Last modifications: 23 March 2007

// ######################################################################

// ## Degree of indentation from the left.
	flipIndentation = "5px";

// ## Padding inbetween menu items.
	flipVerticalPadding = "4px";

// ## Margin between the left of the browser and the menu.
	flipLeftMargin = "16px";

// ## Margin between the top of the browser and the menu.
	flipTopMargin = "10px";

// ## Allow multiple menus to fold out without closing all the other open ones.
	flipOpenMultipleMenus = false;

// ## Preserve the current state of the menu (requires cookies).
	flipSaveMenuState = false;

// ## Open entries where the attribute expanded is true
	flipMarked = true;

// ## Use custom images for bullets
	flipImages = true;

// ## Images to use (specify full path)
	flipImg_open   = "flip_open.gif";
	flipImg_closed = "flip_closed.gif";
	flipImg_static = "flip_static.gif";

// ## Initialise all flipMenus onload
	flipInitOnLoad = true;

// ## Message to display in status bar while loading
	flipLoadingMessage = "Loading...";

// ######################################################################

function alterSize(someSize, alterAmount) {
	someSize = String(someSize);
	var tmpNr = parseFloat(someSize.replace(/\D/g, ""));
	var tmpChar = someSize.replace(/\d/g, "");
	return isNaN(tmpNr) ? someSize : ((tmpNr + alterAmount) + tmpChar);
}

isIE = (String(navigator.appVersion).indexOf("MSIE") > -1);
if (!isIE) flipIndentation = alterSize(flipIndentation, -16);
if (!isIE) flipLeftMargin = alterSize(flipLeftMargin, -16);

document.write(
	"<style type=\"text/css\">" +

	"ul.flipMenu { margin-top: " + flipTopMargin + "; margin-left: " + flipLeftMargin + "; " + (flipImages ? "" : "list-style-type: none;") + " }" +
	"ul.flipMenu ul, ul.flipMenu li { padding-top: " + flipVerticalPadding + "; margin-left: " + flipIndentation + "; margin-right: 0px; " + (flipImages ? "" : "list-style-type: none;") + " }" +

	"li.flipFolderOpen { cursor: pointer; " + (flipImages ? "list-style-image: url(" + document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_open + ");" : "") + " }" +
	"li.flipFolderClosed { cursor: pointer; " + (flipImages ? "list-style-image: url(" + document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_closed + ");" : "") + " }" +

	"</style>"
);
         


if (flipImages) {
	aFlipPreloads = [];
	aFlipPreloads[0] = new Image;
	aFlipPreloads[0].src = document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_open;
	aFlipPreloads[1] = new Image;
	aFlipPreloads[1].src = document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_closed;
	aFlipPreloads[2] = new Image;
	aFlipPreloads[2].src = document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_static;
}

function addEvent(someObj, someEvent, someFunction) {
	if (someObj.addEventListener) { someObj.addEventListener(someEvent, someFunction, true); return true; } else if (someObj.attachEvent) return someObj.attachEvent("on" + someEvent, someFunction); else return false;
}

function openCloseFlip(theItem, newSetting, openParents) {
	if (theItem.flipID) {
		if (openParents) {
			var tmpItem = theItem;
			while (tmpItem.parentElement || tmpItem.parentNode) {
				tmpItem = (tmpItem.parentElement) ? tmpItem.parentElement : tmpItem.parentNode;
				openCloseFlip(tmpItem, newSetting);
			}
		}
		if ((theItem.className == "flipFolderOpen" && newSetting == "closed") || (theItem.className == "flipFolderClosed" && newSetting == "open")) {
			if (!theItem.childrenInitialised) {
				for (var j = 0; j < theItem.childNodes.length; j++) if (theItem.childNodes[j].nodeName == "UL" && !theItem.childNodes[j].initialised) initFlip(theItem.childNodes[j]);
				theItem.childrenInitialised = true;
			}
			theItem.getElementsByTagName("UL")[0].style.display = (newSetting == "open") ? "" : "none";
			theItem.className = newSetting == "open" ? "flipFolderOpen" : "flipFolderClosed";
		}
	}
}

function openFlip(theItem, openParents) {
	openCloseFlip(theItem, "open", openParents);
}

function closeFlip(theItem, closeParents) {
	openCloseFlip(theItem, "closed", closeParents);
}

function toggleFlip(theElement) {
	if (theElement.flipID) {
		var theItem = theElement;
		var isContained = true;
	} else {
		if (theElement && theElement.button > 0) return false;
		var theItem = (isIE) ? event.srcElement : theElement.target;

		var isContained = false;
		if (theItem.className == "flipFolderOpen" || theItem.className == "flipFolderClosed") isContained = true; else while (theItem.parentElement || theItem.parentNode) {
			if (theItem.className == "flipStatic" || theItem.className == "flipFolderOpen" || theItem.className == "flipFolderClosed") {
				isContained = (theItem.className == "flipFolderOpen" || theItem.className == "flipFolderClosed");
				break;
			}
			theItem = (theItem.parentElement) ? theItem.parentElement : theItem.parentNode;
		}
	}

	var toOpenFlip = (isContained && theItem.className == "flipFolderClosed");

	if (!flipOpenMultipleMenus && (toOpenFlip || theItem.className == "flipStatic")) {
		if (theItem.parentElement || theItem.parentNode) {
			var parentUL = (theItem.parentElement) ? theItem.parentElement : theItem.parentNode;
			for (var i = 0; i < parentUL.childNodes.length; i++) closeFlip(parentUL.childNodes[i]);
		}
	}

	if (isContained) {
		if (toOpenFlip) openFlip(theItem); else closeFlip(theItem);
	}
}

function setAllFlips(startElement, newSetting) {
	if (typeof startElement == "undefined") var startElement = document;
	if (typeof newSetting == "undefined") var newSetting = "closed";

	var aUL = startElement.getElementsByTagName("UL");
	for (var i = 0; i < aUL.length; i++) {
		var parentFlip = aUL[i].parentElement ? aUL[i].parentElement : aUL[i].parentNode;
		openCloseFlip(parentFlip, newSetting);
	}
}

function openAllFlips(startElement) {
	setAllFlips(startElement, "open");
}

function closeAllFlips(startElement) {
	setAllFlips(startElement, "closed");
}

function initFlip(startElement) {
	if (!document.createElement) return false;

	if (!startElement || !startElement.nodeName) {
		var aUL = document.getElementsByTagName("UL");
		for (var i = 0; i < aUL.length; i++) {
			if (flipLoadingMessage != "") window.status = flipLoadingMessage + " " + parseInt((i / (aUL.length - 1)) * 100, 10) + "%";
			var curUL = aUL[i];
			if (curUL.className == "flipMenu") {
				initFlip(curUL);

				// ## Fix text selecting problem in Mozilla
				curUL.onselectstart = new Function("return false");
				curUL.onmousedown = new Function("return false");
				curUL.onclick = new Function("return true");
			}
		}

		if (flipSaveMenuState) loadMenuState();

		if (flipLoadingMessage != "") window.status = "";
		return true;
	}

	if (startElement.className == "flipMenu") startElement.style.display = "";

	if (!startElement.childNodes || startElement.childNodes.length == 0) return false;

	if (typeof flipIDCur == "undefined") flipIDCur = 0;
	if (!startElement.initialised) {
		var aUL = startElement.getElementsByTagName("UL");
		for (var i = 0; i < aUL.length; i++) aUL[i].style.display = "none";
	}

	for (var i = 0; i < startElement.childNodes.length; i++) {
		var curNode = startElement.childNodes[i];
		if (curNode.nodeName == "LI") {
			flipIDCur++;
			curNode.flipID = flipIDCur;

			var nodeHasChildren = curNode.getElementsByTagName("UL").length > 0;
			if (nodeHasChildren) {
				if (flipImages && curNode.flipClosed) curNode.style.listStyleImage = "url(" + curNode.flipClosed + ")";

				if (curNode.className == null || curNode.className == "") curNode.className = "flipFolderClosed";
			} else {
				curNode.className = "flipStatic";
				if (flipImages && !curNode.style.listStyleImage) {
					if (!curNode.flipStatic) curNode.flipStatic = document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_static;
					curNode.style.listStyleImage = "url(" + curNode.flipStatic + ")";
				}
			}


			if (!curNode.flipOpen) curNode.flipOpen = document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_open;
			if (!curNode.flipClosed) curNode.flipClosed = document.getElementById("js").getAttribute("src").match( /.*\// ) + flipImg_closed;

			if (flipMarked) {
			  if (curNode.getAttribute("expanded") == "true") curNode.flipIsOpen=1;
			}

			if (curNode.flipIsOpen) openFlip(curNode);
		}
	}

	startElement.initialised = true;
}

function rootOfFlip(flipID, startElement) {

	function containsFlip(startElement, flipID) {
		var flipFound = false;
		var i = 0;
		while (i < startElement.childNodes.length && !flipFound) {
			var curNode = startElement.childNodes[i];
			flipFound = (curNode.flipID == flipID) ? true : containsFlip(curNode, flipID);
			i++;
		}
		return flipFound;
	}

	var rootFlip = null;

	if (!startElement || !startElement.nodeName) {
		var aUL = document.getElementsByTagName("UL");
		var i = 0;
		while (rootFlip == null && i < aUL.length) {
			var curUL = aUL[i];
			if (curUL.nodeName == "UL" && curUL.className == "flipMenu") rootFlip = rootOfFlip(flipID, curUL);
			i++;
		}
		return rootFlip;
	}

	if (startElement.childNodes) for (var i = 0; i < startElement.childNodes.length; i++) {
		var curNode = startElement.childNodes[i];
		if (curNode.flipID == flipID || containsFlip(curNode, flipID)) rootFlip = curNode;
	}

	return rootFlip;
}

function getCookie(cookieName) {
	var allCookies = document.cookie;
	var indexStr = allCookies.indexOf(cookieName + "=");
	if (indexStr == -1) return "";
	indexStr = allCookies.indexOf("=", indexStr) + 1;
	var endStr = allCookies.indexOf(";", indexStr);
	if (endStr == -1) endStr = allCookies.length;
	return unescape(allCookies.substring(indexStr, endStr));
}

function inArray(someID, someArray) {
	for (var i = 0; i < someArray.length; i++) if (someArray[i] == someID) return true;
	return false;
}

function getMenuState(startElement) {
	if (!startElement.childNodes || startElement.childNodes.length == 0) return "";

	var openItems = "";
	var aUL = startElement.getElementsByTagName("UL");
	for (var i = 0; i < aUL.length; i++) {
		var curNode = aUL[i];
		var parentFlip = (curNode.parentElement) ? curNode.parentElement : curNode.parentNode;
		if (curNode.style.display == "" && parentFlip.flipID) openItems += " " + parentFlip.flipID;		
	}
	return openItems;
}

function putMenuState(startElement) {
	if (!startElement.childNodes || startElement.childNodes.length == 0) return false;

	var aUL = startElement.getElementsByTagName("UL");
	for (var i = 0; i < aUL.length; i++) {
		var curNode = aUL[i];
		var parentFlip = (curNode.parentElement) ? curNode.parentElement : curNode.parentNode;
		if (inArray(parentFlip.flipID, aOpenItems)) {
			openFlip(parentFlip);
			if (typeof prevFlipRoot == "undefined") {
				var testRoot = rootOfFlip(parentFlip.flipID);
				if (testRoot.flipID == parentFlip.flipID) prevFlipRoot = testRoot;
			}
		}
	}
}

function saveMenuState() {
	if (flipSaveMenuState) {
		var aUL = document.getElementsByTagName("UL");
		for (var i = 0; i < aUL.length; i++) {
			var curUL = aUL[i];
			var curID = curUL.id ? curUL.id : i;
			if (curUL.className == "flipMenu") document.cookie = cookiePrefix + curID + "=" + getMenuState(curUL) + ";";
		}
	}
}

function loadMenuState() {
	var aUL = document.getElementsByTagName("UL");
	for (var i = 0; i < aUL.length; i++) {
		var curUL = aUL[i];
		var curID = curUL.id ? curUL.id : i;
		if (curUL.className == "flipMenu") {
			var savedState = String(getCookie(cookiePrefix + curID));
			if (savedState != "") {
				aOpenItems = savedState.split(" ");
				putMenuState(curUL);
			}
		}
	}

	addEvent(window, "unload", saveMenuState);
}

function clearMenuState(flipMenuID) {
	if (typeof flipMenuID == "undefined") {
		var aUL = document.getElementsByTagName("UL");
		for (var i = 0; i < aUL.length; i++) {
			var curUL = aUL[i];
			var curID = curUL.id ? curUL.id : i;
			if (curUL.className == "flipMenu") document.cookie = cookiePrefix + curID + "=;";
		}
	} else document.cookie = cookiePrefix + flipMenuID + "=;";
}

cookiePrefix = document.location.pathname + "_";

addEvent(document, "click", toggleFlip);
if (flipInitOnLoad) addEvent(window, "load", initFlip);
var hljs=new function(){function m(p){return p.replace(/&/gm,"&amp;").replace(/</gm,"&lt;")}function c(r,q,p){return RegExp(q,"m"+(r.cI?"i":"")+(p?"g":""))}function j(r){for(var p=0;p<r.childNodes.length;p++){var q=r.childNodes[p];if(q.nodeName=="CODE"){return q}if(!(q.nodeType==3&&q.nodeValue.match(/\s+/))){break}}}function g(t,s){var r="";for(var q=0;q<t.childNodes.length;q++){if(t.childNodes[q].nodeType==3){var p=t.childNodes[q].nodeValue;if(s){p=p.replace(/\n/g,"")}r+=p}else{if(t.childNodes[q].nodeName=="BR"){r+="\n"}else{r+=g(t.childNodes[q])}}}if(/MSIE [678]/.test(navigator.userAgent)){r=r.replace(/\r/g,"\n")}return r}function a(s){var q=s.className.split(/\s+/);q=q.concat(s.parentNode.className.split(/\s+/));for(var p=0;p<q.length;p++){var r=q[p].replace(/^language-/,"");if(d[r]||r=="no-highlight"){return r}}}function b(p){var q=[];(function(s,t){for(var r=0;r<s.childNodes.length;r++){if(s.childNodes[r].nodeType==3){t+=s.childNodes[r].nodeValue.length}else{if(s.childNodes[r].nodeName=="BR"){t+=1}else{q.push({event:"start",offset:t,node:s.childNodes[r]});t=arguments.callee(s.childNodes[r],t);q.push({event:"stop",offset:t,node:s.childNodes[r]})}}}return t})(p,0);return q}function l(y,z,x){var r=0;var w="";var t=[];function u(){if(y.length&&z.length){if(y[0].offset!=z[0].offset){return(y[0].offset<z[0].offset)?y:z}else{return z[0].event=="start"?y:z}}else{return y.length?y:z}}function s(C){var D="<"+C.nodeName.toLowerCase();for(var A=0;A<C.attributes.length;A++){var B=C.attributes[A];D+=" "+B.nodeName.toLowerCase();if(B.nodeValue!=undefined&&B.nodeValue!=false&&B.nodeValue!=null){D+='="'+m(B.nodeValue)+'"'}}return D+">"}while(y.length||z.length){var v=u().splice(0,1)[0];w+=m(x.substr(r,v.offset-r));r=v.offset;if(v.event=="start"){w+=s(v.node);t.push(v.node)}else{if(v.event=="stop"){var q=t.length;do{q--;var p=t[q];w+=("</"+p.nodeName.toLowerCase()+">")}while(p!=v.node);t.splice(q,1);while(q<t.length){w+=s(t[q]);q++}}}}w+=x.substr(r);return w}function i(){function p(u,t,v){if(u.compiled){return}if(!v){u.bR=c(t,u.b?u.b:"\\B|\\b");if(!u.e&&!u.eW){u.e="\\B|\\b"}if(u.e){u.eR=c(t,u.e)}}if(u.i){u.iR=c(t,u.i)}if(u.r==undefined){u.r=1}if(u.k){u.lR=c(t,u.l||hljs.IR,true)}for(var s in u.k){if(!u.k.hasOwnProperty(s)){continue}if(u.k[s] instanceof Object){u.kG=u.k}else{u.kG={keyword:u.k}}break}if(!u.c){u.c=[]}u.compiled=true;for(var r=0;r<u.c.length;r++){p(u.c[r],t,false)}if(u.starts){p(u.starts,t,false)}}for(var q in d){if(!d.hasOwnProperty(q)){continue}p(d[q].dM,d[q],true)}}function e(J,D){if(!i.called){i();i.called=true}function z(r,M){for(var L=0;L<M.c.length;L++){if(M.c[L].bR.test(r)){return M.c[L]}}}function w(L,r){if(C[L].e&&C[L].eR.test(r)){return 1}if(C[L].eW){var M=w(L-1,r);return M?M+1:0}return 0}function x(r,L){return L.iR&&L.iR.test(r)}function A(O,N){var M=[];for(var L=0;L<O.c.length;L++){M.push(O.c[L].b)}var r=C.length-1;do{if(C[r].e){M.push(C[r].e)}r--}while(C[r+1].eW);if(O.i){M.push(O.i)}return c(N,"("+M.join("|")+")",true)}function s(M,L){var N=C[C.length-1];if(!N.t){N.t=A(N,H)}N.t.lastIndex=L;var r=N.t.exec(M);if(r){return[M.substr(L,r.index-L),r[0],false]}else{return[M.substr(L),"",true]}}function p(O,r){var L=H.cI?r[0].toLowerCase():r[0];for(var N in O.kG){if(!O.kG.hasOwnProperty(N)){continue}var M=O.kG[N].hasOwnProperty(L);if(M){return[N,M]}}return false}function F(M,O){if(!O.k){return m(M)}var N="";var P=0;O.lR.lastIndex=0;var L=O.lR.exec(M);while(L){N+=m(M.substr(P,L.index-P));var r=p(O,L);if(r){t+=r[1];N+='<span class="'+r[0]+'">'+m(L[0])+"</span>"}else{N+=m(L[0])}P=O.lR.lastIndex;L=O.lR.exec(M)}N+=m(M.substr(P,M.length-P));return N}function K(r,M){if(M.sL&&d[M.sL]){var L=e(M.sL,r);t+=L.keyword_count;return L.value}else{return F(r,M)}}function I(M,r){var L=M.cN?'<span class="'+M.cN+'">':"";if(M.rB){q+=L;M.buffer=""}else{if(M.eB){q+=m(r)+L;M.buffer=""}else{q+=L;M.buffer=r}}C.push(M);B+=M.r}function E(O,L,Q){var R=C[C.length-1];if(Q){q+=K(R.buffer+O,R);return false}var M=z(L,R);if(M){q+=K(R.buffer+O,R);I(M,L);return M.rB}var r=w(C.length-1,L);if(r){var N=R.cN?"</span>":"";if(R.rE){q+=K(R.buffer+O,R)+N}else{if(R.eE){q+=K(R.buffer+O,R)+N+m(L)}else{q+=K(R.buffer+O+L,R)+N}}while(r>1){N=C[C.length-2].cN?"</span>":"";q+=N;r--;C.length--}var P=C[C.length-1];C.length--;C[C.length-1].buffer="";if(P.starts){I(P.starts,"")}return R.rE}if(x(L,R)){throw"Illegal"}}var H=d[J];var C=[H.dM];var B=0;var t=0;var q="";try{var v=0;H.dM.buffer="";do{var y=s(D,v);var u=E(y[0],y[1],y[2]);v+=y[0].length;if(!u){v+=y[1].length}}while(!y[2]);if(C.length>1){throw"Illegal"}return{r:B,keyword_count:t,value:q}}catch(G){if(G=="Illegal"){return{r:0,keyword_count:0,value:m(D)}}else{throw G}}}function f(t){var r={keyword_count:0,r:0,value:m(t)};var q=r;for(var p in d){if(!d.hasOwnProperty(p)){continue}var s=e(p,t);s.language=p;if(s.keyword_count+s.r>q.keyword_count+q.r){q=s}if(s.keyword_count+s.r>r.keyword_count+r.r){q=r;r=s}}if(q.language){r.second_best=q}return r}function h(r,q,p){if(q){r=r.replace(/^((<[^>]+>|\t)+)/gm,function(t,w,v,u){return w.replace(/\t/g,q)})}if(p){r=r.replace(/\n/g,"<br>")}return r}function o(u,x,q){var y=g(u,q);var s=a(u);if(s=="no-highlight"){return}if(s){var w=e(s,y)}else{var w=f(y);s=w.language}var p=b(u);if(p.length){var r=document.createElement("pre");r.innerHTML=w.value;w.value=l(p,b(r),y)}w.value=h(w.value,x,q);var t=u.className;if(!t.match("(\\s|^)(language-)?"+s+"(\\s|$)")){t=t?(t+" "+s):s}if(/MSIE [678]/.test(navigator.userAgent)&&u.tagName=="CODE"&&u.parentNode.tagName=="PRE"){var r=u.parentNode;var v=document.createElement("div");v.innerHTML="<pre><code>"+w.value+"</code></pre>";u=v.firstChild.firstChild;v.firstChild.cN=r.cN;r.parentNode.replaceChild(v.firstChild,r)}else{u.innerHTML=w.value}u.className=t;u.result={language:s,kw:w.keyword_count,re:w.r};if(w.second_best){u.second_best={language:w.second_best.language,kw:w.second_best.keyword_count,re:w.second_best.r}}}function k(){if(k.called){return}k.called=true;var r=document.getElementsByTagName("pre");for(var p=0;p<r.length;p++){var q=j(r[p]);if(q){o(q,hljs.tabReplace)}}}function n(){if(window.addEventListener){window.addEventListener("DOMContentLoaded",k,false);window.addEventListener("load",k,false)}else{if(window.attachEvent){window.attachEvent("onload",k)}else{window.onload=k}}}var d={};this.LANGUAGES=d;this.highlight=e;this.highlightAuto=f;this.fixMarkup=h;this.highlightBlock=o;this.initHighlighting=k;this.initHighlightingOnLoad=n;this.IR="[a-zA-Z][a-zA-Z0-9_]*";this.UIR="[a-zA-Z_][a-zA-Z0-9_]*";this.NR="\\b\\d+(\\.\\d+)?";this.CNR="\\b(0x[A-Za-z0-9]+|\\d+(\\.\\d+)?)";this.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|\\.|-|-=|/|/=|:|;|<|<<|<<=|<=|=|==|===|>|>=|>>|>>=|>>>|>>>=|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~";this.BE={b:"\\\\.",r:0};this.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[this.BE],r:0};this.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[this.BE],r:0};this.CLCM={cN:"comment",b:"//",e:"$"};this.CBLCLM={cN:"comment",b:"/\\*",e:"\\*/"};this.HCM={cN:"comment",b:"#",e:"$"};this.NM={cN:"number",b:this.NR,r:0};this.CNM={cN:"number",b:this.CNR,r:0};this.inherit=function(p,s){var r={};for(var q in p){r[q]=p[q]}if(s){for(var q in s){r[q]=s[q]}}return r}}();hljs.LANGUAGES.bash=function(){var d={"true":1,"false":1};var b={cN:"variable",b:"\\$([a-zA-Z0-9_]+)\\b"};var a={cN:"variable",b:"\\$\\{(([^}])|(\\\\}))+\\}",c:[hljs.CNM]};var c={cN:"string",b:'"',e:'"',i:"\\n",c:[hljs.BE,b,a],r:0};var e={cN:"test_condition",b:"",e:"",c:[c,b,a,hljs.CNM],k:{literal:d},r:0};return{dM:{k:{keyword:{"if":1,then:1,"else":1,fi:1,"for":1,"break":1,"continue":1,"while":1,"in":1,"do":1,done:1,echo:1,exit:1,"return":1,set:1,declare:1},literal:d},c:[{cN:"shebang",b:"(#!\\/bin\\/bash)|(#!\\/bin\\/sh)",r:10},hljs.HCM,hljs.CNM,c,b,a,hljs.inherit(e,{b:"\\[ ",e:" \\]",r:0}),hljs.inherit(e,{b:"\\[\\[ ",e:" \\]\\]"})]}}}();hljs.LANGUAGES.erlang=function(){var g="[a-z'][a-zA-Z0-9_']*";var l="("+g+":"+g+"|"+g+")";var d={keyword:{after:1,and:1,andalso:10,band:1,begin:1,bnot:1,bor:1,bsl:1,bzr:1,bxor:1,"case":1,"catch":1,cond:1,div:1,end:1,fun:1,let:1,not:1,of:1,orelse:10,query:1,receive:1,rem:1,"try":1,when:1,xor:1},literal:{"false":1,"true":1}};var j={cN:"comment",b:"%",e:"$",r:0};var c={b:"fun\\s+"+g+"/\\d+"};var m={b:l+"\\(",e:"\\)",rB:true,r:0,c:[{cN:"function_name",b:l,r:0},{b:"\\(",e:"\\)",eW:true,rE:true,r:0}]};var f={cN:"tuple",b:"{",e:"}",r:0};var a={cN:"variable",b:"\\b_([A-Z][A-Za-z0-9_]*)?",r:0};var k={cN:"variable",b:"[A-Z][a-zA-Z0-9_]*",r:0};var h={b:"#",e:"}",i:".",r:0,rB:true,c:[{cN:"record_name",b:"#"+hljs.UIR,r:0},{b:"{",eW:true,r:0}]};var i={k:d,b:"(fun|receive|if|try|case)",e:"end"};i.c=[j,c,hljs.inherit(hljs.ASM,{cN:""}),i,m,hljs.QSM,hljs.CNM,f,a,k,h];var b=[j,c,i,m,hljs.QSM,hljs.CNM,f,a,k,h];m.c[1].c=b;f.c=b;h.c[1].c=b;var e={cN:"params",b:"\\(",e:"\\)",eW:true,c:b};return{dM:{k:d,i:"(</|\\*=|\\+=|-=|/=|/\\*|\\*/|\\(\\*|\\*\\))",c:[{cN:"function",b:"^"+g+"\\(",e:";|\\.",rB:true,c:[e,{cN:"title",b:g},{k:d,b:"->",eW:true,c:b}]},j,{cN:"pp",b:"^-",e:"\\.",r:0,eE:true,rB:true,l:"-"+hljs.IR,k:{"-module":1,"-record":1,"-undef":1,"-export":1,"-ifdef":1,"-ifndef":1,"-author":1,"-copyright":1,"-doc":1,"-vsn":1,"-import":1,"-include":1,"-include_lib":1,"-compile":1,"-define":1,"-else":1,"-endif":1,"-file":1,"-behaviour":1,"-behavior":1},c:[e]},hljs.CNM,hljs.QSM,h,a,k,f]}}}();hljs.LANGUAGES.css=function(){var a={cN:"function",b:hljs.IR+"\\(",e:"\\)",c:[{eW:true,eE:true,c:[hljs.NM,hljs.ASM,hljs.QSM]}]};return{cI:true,dM:{i:"[=/|']",c:[hljs.CBLCLM,{cN:"id",b:"\\#[A-Za-z0-9_-]+"},{cN:"class",b:"\\.[A-Za-z0-9_-]+",r:0},{cN:"attr_selector",b:"\\[",e:"\\]",i:"$"},{cN:"pseudo",b:":(:)?[a-zA-Z0-9\\_\\-\\+\\(\\)\\\"\\']+"},{cN:"at_rule",b:"@(font-face|page)",l:"[a-z-]+",k:{"font-face":1,page:1}},{cN:"at_rule",b:"@",e:"[{;]",eE:true,k:{"import":1,page:1,media:1,charset:1},c:[a,hljs.ASM,hljs.QSM,hljs.NM]},{cN:"tag",b:hljs.IR,r:0},{cN:"rules",b:"{",e:"}",i:"[^\\s]",r:0,c:[hljs.CBLCLM,{cN:"rule",b:"[^\\s]",rB:true,e:";",eW:true,c:[{cN:"attribute",b:"[A-Z\\_\\.\\-]+",e:":",eE:true,i:"[^\\s]",starts:{cN:"value",eW:true,eE:true,c:[a,hljs.NM,hljs.QSM,hljs.ASM,hljs.CBLCLM,{cN:"hexcolor",b:"\\#[0-9A-F]+"},{cN:"important",b:"!important"}]}}]}]}]}}}();hljs.LANGUAGES.erlang_repl={dM:{k:{special_functions:{spawn:10,spawn_link:10,self:2},reserved:{after:1,and:1,andalso:5,band:1,begin:1,bnot:1,bor:1,bsl:1,bsr:1,bxor:1,"case":1,"catch":0,cond:1,div:1,end:1,fun:0,"if":0,let:1,not:0,of:1,or:1,orelse:5,query:1,receive:0,rem:1,"try":0,when:1,xor:1}},c:[{cN:"input_number",b:"^[0-9]+> ",r:10},{cN:"comment",b:"%",e:"$"},hljs.NM,hljs.ASM,hljs.QSM,{cN:"constant",b:"\\?(::)?([A-Z]\\w*(::)?)+"},{cN:"arrow",b:"->"},{cN:"ok",b:"ok"},{cN:"exclamation_mark",b:"!"},{cN:"function_or_atom",b:"(\\b[a-z'][a-zA-Z0-9_']*:[a-z'][a-zA-Z0-9_']*)|(\\b[a-z'][a-zA-Z0-9_']*)",r:0},{cN:"variable",b:"[A-Z][a-zA-Z0-9_']*",r:0}]}};hljs.LANGUAGES.cpp=function(){var b={keyword:{"false":1,"int":1,"float":1,"while":1,"private":1,"char":1,"catch":1,"export":1,virtual:1,operator:2,sizeof:2,dynamic_cast:2,typedef:2,const_cast:2,"const":1,struct:1,"for":1,static_cast:2,union:1,namespace:1,unsigned:1,"long":1,"throw":1,"volatile":2,"static":1,"protected":1,bool:1,template:1,mutable:1,"if":1,"public":1,friend:2,"do":1,"return":1,"goto":1,auto:1,"void":2,"enum":1,"else":1,"break":1,"new":1,extern:1,using:1,"true":1,"class":1,asm:1,"case":1,typeid:1,"short":1,reinterpret_cast:2,"default":1,"double":1,register:1,explicit:1,signed:1,typename:1,"try":1,"this":1,"switch":1,"continue":1,wchar_t:1,inline:1,"delete":1,alignof:1,char16_t:1,char32_t:1,constexpr:1,decltype:1,noexcept:1,nullptr:1,static_assert:1,thread_local:1},built_in:{std:1,string:1,cin:1,cout:1,cerr:1,clog:1,stringstream:1,istringstream:1,ostringstream:1,auto_ptr:1,deque:1,list:1,queue:1,stack:1,vector:1,map:1,set:1,bitset:1,multiset:1,multimap:1,unordered_set:1,unordered_map:1,unordered_multiset:1,unordered_multimap:1,array:1,shared_ptr:1}};var a={cN:"stl_container",b:"\\b(deque|list|queue|stack|vector|map|set|bitset|multiset|multimap|unordered_map|unordered_set|unordered_multiset|unordered_multimap|array)\\s*<",e:">",k:b,r:10};a.c=[a];return{dM:{k:b,i:"</",c:[hljs.CLCM,hljs.CBLCLM,hljs.QSM,{cN:"string",b:"'",e:"[^\\\\]'",i:"[^\\\\][^']"},hljs.CNM,{cN:"preprocessor",b:"#",e:"$"},a]}}}();

$(document).ready(function(){
    $("pre code").each(function(i, e){
	if($(e).text().substring(0, 1) == "\n"){
	    $(e).text($(e).text().substring(1, $(e).text().length));
	}
	var pattern = new RegExp(/    /g);
	if(pattern.test($(e).text())){
	    $(e).text($(e).text().replace(/    /g, "\t"));
	}
	if($(e).text().substring($(e).text().length-1, $(e).text().length) == "\t"){
	    $(e).text($(e).text().substring(0, $(e).text().length-2));
	}
	hljs.highlightBlock(e, '&nbsp;&nbsp;&nbsp;&nbsp;');
    });
});