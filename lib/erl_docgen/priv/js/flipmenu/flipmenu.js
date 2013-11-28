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
