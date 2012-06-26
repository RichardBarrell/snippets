// ==UserScript==
// @name           Improved Arbitrary Text
// @namespace      https://github.com/RichardBarrell/snippets
// @description    Makes arbitrary text better
// @include        *
// @version        0.0.1
// ==/UserScript==

function xplor(n, f) {
	f(n);
	var c = n.firstChild;
	while (c) {
		xplor(c, f);
		c = c.nextSibling;
	}
}

function improve(n) {
	if (n.nodeName !== '#text') { return; }
	var t = n.nodeValue.replace(/minister/gi, "Nemesis");
	if (t === n.nodeValue) { return; }
	var tn = document.createTextNode(t);
	var p = n.parentNode;
	p.insertBefore(tn, n);
	p.removeChild(n);
}

xplor(document.body, improve);
