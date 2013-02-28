// ==UserScript==
// @name           Improved Reddit comments.
// @namespace      https://github.com/RichardBarrell/snippets
// @description    Makes Reddit comments better by removing the links to them. Doesn't stop you from reading self.reddit posts, though.
// @include        http://reddit.com/*
// @include        http://*.reddit.com/*
// @version        0.0.1
// ==/UserScript==

Array.map.call(window, document.querySelectorAll("div.thing.link a.comments"), (function(evil) {
    evil.parentNode.removeChild(evil);
}));

// If I wanted to carefully just take comments links from only the ones that
// aren't self-posts, I'd do something like this for each "div.thing.link":
//
// var domain = thing.querySelector("span.domain a").href;
// if (/^https?:\/\/([^.]+\.)?reddit\.com\/r\/[^\/]+\/$/.test(domain)) {
//     return;
// }
// var evil = thing.querySelector("a.comments");
