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

        function highlightLanguage(css, languages) {
            var i, len, nodes = document.querySelectorAll(css);
            for (i = 0, len = nodes.length; i < len; i += 1) {
                hljs.configure({languages: languages});
                window.hljs.highlightBlock(nodes[i]);
            }
        }

        highlightLanguage('.example-erl',["erlang"]);
        highlightLanguage('.example-erl-repl',["erlang-repl"]);
        highlightLanguage('.example-c',["cpp"]);
        highlightLanguage('.example-cpp',["cpp"]);
        highlightLanguage('.example-sh',["bash"]);
        highlightLanguage('.example-diff',["diff"]);
        highlightLanguage('.example-none',["erlang","erlang-repl","bash","cpp","diff"]);

    }

    intervalId = window.setInterval(doHighlight, 50);
});
