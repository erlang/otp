/*globals document, window*/
window.addEventListener("load", function () {
    "use strict";
    var body = document.body,
        base = window.__otpTopDocDir || "/doc/js/",
        cssLink = document.createElement('link'),
        script = document.createElement('script'),
        intervalId, attempts = 0;

    cssLink.rel = "stylesheet";
    cssLink.href = base + "../highlight.css";
    script.src = base + "highlight.pack.js";

    body.appendChild(cssLink);
    body.appendChild(script);

    function doHighlight() {
        attempts += 1;

        if (attempts > 20) {
            window.clearInterval(intervalId);
            return;
        }

        if (!window.hljs) {
            return;
        }

        window.clearInterval(intervalId);

        var i, len, nodes = document.querySelectorAll('.example');
        for (i = 0, len = nodes.length; i < len; i += 1) {
            window.hljs.highlightBlock(nodes[i]);
        }

    }

    intervalId = window.setInterval(doHighlight, 50);
});
