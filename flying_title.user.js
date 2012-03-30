// ==UserScript==
// @name           Flying Title
// @namespace      https://github.com/RichardBarrell/snippets
// @description    Paints a flying div at the bottom right of the screen with the contents of the <title> element, cropped to 128 characters.
// @include        *
// @version        0.6
// ==/UserScript==

var silly_name = "title-title-title-title-mushroom-mushroom";
var maxlen = 128;
var telem = document.getElementsByTagName("title");
if ((window === window.top) && (telem.length>0) &&
	(document.getElementById(silly_name)===null)) {
	var title = telem[0].textContent;
	if (title.length>maxlen) {
		title = title.substring(0,maxlen) + String.fromCharCode(8230);
	}
	var flyer = document.createElement("span"), s=flyer.style;
	s.position = "fixed";
	s.right = "0px";
	s.bottom = "0px";
	s.backgroundColor = "#faf04e";
	s.border = "1px solid black";
	s.color = "black";
	s.textAlign = "left";
	s.textDecoration = "non";
	s.textTransform = "none";
	s.display = "inline";
	s.fontFamily = "sans-serif";
	s.fontStyle = "normal";
	s.fontSize = "14px";
	s.padding = "2px";
	s.zIndex = (1 << 30).toString();
	flyer.id = silly_name;
	flyer.appendChild(document.createTextNode(title));
	document.body.appendChild(flyer);
	flyer.addEventListener("click", function(event) {
		event.preventDefault();
		event.stopPropagation();
		flyer.parentNode.removeChild(flyer);
		return false;
	});
}
