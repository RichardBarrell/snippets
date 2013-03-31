// ==UserScript==
// @name           Achewood Lazy
// @namespace      https://github.com/RichardBarrell/snippets
// @description    Keyboard nav for Achewood
// @include        http://www.achewood.com/*
// @version        0.0.1
// ==/UserScript==

var prevLink = document.querySelector("a.dateNav[title='Previous comic']");
var nextLink = document.querySelector("a.dateNav[title='Next comic']");
var comic_body = document.getElementById("comic_body");

if (prevLink && nextLink) {
	document.addEventListener("keydown", function(event) {
		if (event.keyCode === 37) {
			window.location = prevLink.getAttribute("href");
		}
		if (event.keyCode === 39) {
			window.location = nextLink.getAttribute("href");
		}
	});
}
if (comic_body) {
	var comic = comic_body.querySelector("img.comic");
	comic_body.appendChild(document.createTextNode(comic.getAttribute("title")));
}

