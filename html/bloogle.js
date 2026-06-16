
// Vanilla JS, no framework dependencies.

var instant = true; // should we search on key presses
var query = parseQuery(); // what is the current query string

var bloogleEl; // document.getElementById("bloogle") after load


/////////////////////////////////////////////////////////////////////
// SEARCHING

function on_arrow_press(ev) {
    var offset = 0;
    if (ev.keyCode == Key.Up) {
        offset = -1;
    } else if (ev.keyCode == Key.Down) {
        offset = +1;
    } else if (ev.keyCode != Key.Return) {
        return;
    }

    // Figure out where we are
    var results = document.querySelectorAll("div#body .result");
    var activeResults = document.querySelectorAll("div#body .result.active");
    var activeRow = -1;
    if (activeResults.length == 1)
        activeRow = Array.prototype.indexOf.call(results, activeResults[0]);

    if (ev.keyCode == Key.Return) {
        if (activeRow >= 0) {
            var a = activeResults[0].querySelector("a");
            if (a) document.location.href = a.getAttribute("href");
        }
    } else {
        var newRow = activeRow + offset;
        var activeEl = results[activeRow];
        if (newRow < 0) {
            if (activeEl) activeEl.classList.remove("active");
            bloogleEl.focus();
        } else if (newRow < results.length) {
            var newEl = results[newRow];
            if (activeRow >= 0 && activeEl) activeEl.classList.remove("active");
            newEl.classList.add("active");
            bloogleEl.blur();
        }
    }
}

ready(function() {
    document.addEventListener("keyup", on_arrow_press);
});

ready(function(){
    bloogleEl = document.getElementById("bloogle");
    var form = bloogleEl.closest("form");
    var scopeEl = form.querySelector("[name=scope]");

    var self = newReal();

    var active = bloogleEl.value + " " + scopeEl.value; // What is currently being searched for (may not yet be displayed)
    var past = cache(100); // Cache of previous searches
    var watch = watchdog(500, function(){self.showWaiting();}); // Timeout of the "Waiting..." callback

    function hit(){
        if (!instant) return;
        function getScope(){
            var v = scopeEl ? scopeEl.value : "";
            return v == null ? "" : v;
        }

        var nowBloogle = bloogleEl.value;
        var nowScope = getScope();
        var now = nowBloogle + " " + nowScope;
        if (now == active) return;
        active = now;

        var title = now + (now == " " ? "" : " - ") + "Bloogle";
        query["bloogle"] = nowBloogle;
        query["scope"] = nowScope;
        if (window.history)
            window.history.replaceState(null, title, renderQuery(query));
        document.title = title;

        var old = past.ask(now);
        if (old != undefined){self.showResult(old); return;}

        watch.stop();
        watch.start();

        var url = "?" + new URLSearchParams({bloogle:nowBloogle, scope:nowScope, mode:"body"});
        fetch(url, {headers: {"Accept": "text/html"}})
            .then(function(resp){
                return resp.text().then(function(text){ return {status:resp.status, text:text}; });
            })
            .then(function(r){
                watch.stop();
                var current = bloogleEl.value + " " + getScope() == now;
                if (r.status == 200){
                    past.add(now, r.text);
                    if (current) self.showResult(r.text);
                } else if (current) {
                    self.showError(r.status, r.text);
                }
            })
            .catch(function(){
                watch.stop();
                // Network/connection failure: clear the "Still working..." state
                // and surface an error, rather than leaving the page frozen.
                if (bloogleEl.value + " " + getScope() == now)
                    self.showError(0, "Could not reach the server — check your connection and try again.");
            });
    };
    bloogleEl.addEventListener("keyup", hit);
    scopeEl.addEventListener("change", hit);
})

function newReal()
{
    bloogleEl.select();
    var body = document.getElementById("body");

    return {
        showWaiting: function(){document.querySelector("h1").textContent = "Still working...";},
        // Build the error DOM with textContent so a server message (e.g. a 500
        // body that echoes the query) can never be interpreted as HTML.
        showError: function(status,text){
            body.textContent = "";
            var h1 = document.createElement("h1");
            var b = document.createElement("b");
            b.textContent = "Error:";
            h1.appendChild(b);
            if (status) h1.appendChild(document.createTextNode(" status " + status));
            var p = document.createElement("p");
            p.textContent = text;
            body.appendChild(h1);
            body.appendChild(p);
        },
        showResult: function(text){body.innerHTML = text; newDocs();}
    }
}


/////////////////////////////////////////////////////////////////////
// DOCUMENTATION

ready(function(){
    window.addEventListener("resize", resizeDocs);
    newDocs();
});

function resizeDocs()
{
    document.querySelectorAll("#body .doc").forEach(function(el){
        // If a segment is open, it should remain open forever.
        // .doc/.shut clip to max-height with overflow:hidden, so an overflowing
        // element (content taller than the clipped box) needs the expand icon.
        var overflowing = el.classList.contains("newline") || el.scrollHeight > el.clientHeight;
        if (overflowing && !el.classList.contains("open"))
            el.classList.add("shut");
        else if (!overflowing && el.classList.contains("shut"))
            el.classList.remove("shut");
    });
}

function newDocs()
{
    resizeDocs();
    document.querySelectorAll("#body .doc").forEach(function(el){
        el.addEventListener("click", function(){
            if (el.classList.contains("open") || el.classList.contains("shut")){
                el.classList.toggle("open");
                el.classList.toggle("shut");
            }
        });
    });
}


/////////////////////////////////////////////////////////////////////
// iOS TWEAKS

ready(function(){
    if (inputSearch)
        bloogleEl.type = "search";

    var qphone = query["phone"];
    var phone =
        qphone == "0" ? false :
        qphone == "1" ? true :
        phoneSupport;

    if (!phone) return;
    document.body.classList.add("phone");
    var meta = document.createElement("meta");
    meta.name = "viewport";
    meta.content = "width=device-width";
    document.head.appendChild(meta);
});


/////////////////////////////////////////////////////////////////////
// LIBRARY BITS

function ready(fn) // run fn once the DOM is parsed
{
    if (document.readyState != "loading") fn();
    else document.addEventListener("DOMContentLoaded", fn);
}

function parseQuery() // :: IO (Dict String String)
{
    // From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    var params = {},
        e,
        a = /\+/g,  // Regex for replacing addition symbol with a space
        r = /([^&=]+)=?([^&]*)/g,
        d = function (s) { return decodeURIComponent(s.replace(a, " ")); },
        q = window.location.search.substring(1);

    while (e = r.exec(q))
        params[d(e[1])] = d(e[2]);

    return params;
}

function renderQuery(query) // Dict String String -> IO String
{
    var s = "";
    for (var i in query)
    {
        if (query[i] != "")
            s += (s == "" ? "?" : "&") + i + "=" + encodeURIComponent(query[i]);
    }
    return window.location.href.substring(0, window.location.href.length - window.location.search.length) + s;
}


var iOS =
    (navigator.userAgent.indexOf("iPhone") != -1) ||
    (navigator.userAgent.indexOf("iPod") != -1) ||
    (navigator.userAgent.indexOf("iPad") != -1);

var phoneSupport =
    (navigator.userAgent.indexOf("iPhone") != -1) ||
    (navigator.userAgent.indexOf("iPod") != -1) ||
    (navigator.userAgent.indexOf("Android") != -1);

// Supports <input type=search />
var inputSearch = iOS;

var Key = {
    Up: 38,
    Down: 40,
    Return: 13,
    Escape: 27
};

function cache(maxElems)
{
    var contents = {}; // what we have in the cache, with # prepended
    // note that contents[toString] != undefined, since it's a default method
    // hence the leading #
    var order = []; // prefixed keys in insertion order, oldest first

    return {
        add: function(key,val)
        {
            var k = "#" + key;
            if (!(k in contents)) order.push(k);
            contents[k] = val;
            // Evict oldest entries once we exceed the requested capacity.
            while (order.length > maxElems)
                delete contents[order.shift()];
        },

        ask: function(key)
        {
            return contents["#" + key];
        }
    };
}

function watchdog(time, fun)
{
    var id = undefined;
    function stop(){if (id == undefined) return; window.clearTimeout(id); id = undefined;}
    function start(){stop(); id = window.setTimeout(function(){id = undefined; fun();}, time);}
    return {start:start, stop:stop}
}
