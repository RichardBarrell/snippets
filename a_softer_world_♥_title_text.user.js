// ==UserScript==
// @name        a softer world ♥ title text
// @namespace   https://github.com/RichardBarrell/snippets
// @description Copies the comic title text on asofterworld to a spot where I can see it without mousing over.
// @include     http://www.asofterworld.com/
// @include     http://www.asofterworld.com/index.php*
// @include     http://asofterworld.com/index.php*
// @version     1.0
// @grant       none
// ==/UserScript==

var comic = document.querySelector("#thecomic > img");
var heart = document.getElementById("hex");
if (comic && heart) {
    var s = document.createElement("div");
    s.appendChild(document.createTextNode(comic.title));
    heart.parentNode.insertBefore(s, heart);
    s.style.backgroundColor = '#eeeeaa';
    s.style.padding = '5px';
}

var next = document.querySelector("#nextbutton > a");
var prev = document.querySelector("#backbutton > a");

if (next && prev) {
    window.addEventListener("keydown", function(event) {
        if (event.keyCode === 39) {
            next.click();
        } else if (event.keyCode == 37) {
            prev.click();
        }
    }, false);
}
