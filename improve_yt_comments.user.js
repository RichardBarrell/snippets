// ==UserScript==
// @name           Improved YouTube Comments
// @namespace      http://robarr.co.uk/
// @description    Makes YouTube comments better
// @include        http://www.youtube.com/*
// @version        0.0.1
// ==/UserScript==

var c = document.getElementById("comments-view");
c.parentElement.removeChild(c);


