// Evolving layout, tried on a live article (dev). A live article lends the
// page everything but its sections; those come either from a <template> in
// the page or from the live article's own sections, and are turned here
// into the markup an evolving article has (hypermedia/evolving.lisp). The
// site's own script and styles do the rest.
//
// The page sets window.SCROLLY before loading this file:
//   shell     the live article to borrow, e.g. "/magazine/almighty-bakers"
//   template  id of a <template> holding the sections; left out, the live
//             article's sections are converted instead
//   title     replaces the headline (optional)
//   dek       replaces the dek (optional)
(function () {
  var config = window.SCROLLY || {};
  var STAMP = Date.now();
  var status = document.getElementById("status");

  function esc(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/"/g, "&quot;");
  }
  function pad(n) { return n < 10 ? "0" + n : "" + n; }
  function each(list, fn) { Array.prototype.forEach.call(list, fn); }

  // One live section, as the markup scrolly.js reads. Paragraphs and lists
  // become stops. A note or a code block goes to the left side for as long
  // as the paragraph before it is being read; a subheading labels the stop
  // after it. Code blocks arrive as individual blocks: nothing in the live
  // markup says how one would evolve.
  function convert(doc, live) {
    var out = doc.createElement("section");
    out.className = "neo-piece ev";
    out.id = live.id;
    var title = doc.createElement("h2");
    title.className = "ev-title";
    title.textContent = live.querySelector(".piece-name").textContent;
    out.appendChild(title);

    var last = null, count = 0, head = null;
    function attach(item) {
      item.id = live.id + "-left-" + (++count);
      out.appendChild(item);
      if (!last || last.getAttribute("data-block")) {
        last = doc.createElement("p"); // nothing to hold it: a stop of its own
        out.appendChild(last);
      }
      last.setAttribute("data-block", item.id);
    }
    each(live.querySelector(".piece-text .piece-body").children, function (child) {
      if (child.matches("p.prose, ul, ol")) {
        var stop = doc.createElement(child.tagName);
        stop.innerHTML = child.innerHTML;
        if (last && last.getAttribute("data-block")) stop.setAttribute("data-block", "");
        if (head) { stop.setAttribute("data-head", head); head = null; }
        out.appendChild(stop);
        last = stop;
      } else if (child.matches("aside.inline-note")) {
        var note = doc.createElement("aside");
        note.className = "ev-note";
        var label = child.querySelector(".margin-note-label");
        note.setAttribute("data-name", label ? label.textContent : "Note");
        each(child.querySelectorAll(".margin-note-body"), function (body) {
          var p = doc.createElement("p");
          p.innerHTML = body.innerHTML;
          note.appendChild(p);
        });
        attach(note);
      } else if (child.matches("figure.src")) {
        var block = doc.createElement("figure");
        block.className = "ev-block";
        var pre = doc.createElement("pre");
        pre.textContent = Array.prototype.map.call(child.querySelectorAll(".src-line"), function (line) {
          return line.textContent;
        }).join("\n");
        block.appendChild(pre);
        attach(block);
      } else if (child.matches(".subhead-group")) {
        head = child.querySelector(".subhead").textContent;
      }
    });
    return out;
  }

  fetch(config.shell).then(function (r) {
    if (!r.ok) throw new Error(config.shell + " answered " + r.status);
    return r.text();
  }).then(function (html) {
    var doc = new DOMParser().parseFromString(html, "text/html");
    var list = doc.querySelector(".piece-list");
    if (!list) throw new Error("no .piece-list in " + config.shell);
    var ids = function () {
      return Array.prototype.map.call(list.querySelectorAll("section"), function (s) { return s.id; }).join(" ");
    };
    var before = ids();
    if (config.template) {
      list.innerHTML = document.getElementById(config.template).innerHTML
        .replace(/\bsx-/g, "ev-").replace(/neo-piece sx\b/g, "neo-piece ev");
    } else {
      each(list.querySelectorAll("section.neo-piece"), function (live) {
        live.parentNode.replaceChild(convert(doc, live), live);
      });
    }
    // Each paragraph or list goes in a div.ev-stop that says what is beside
    // it. The site writes that out for every stop; here one that says
    // nothing keeps what the stop before it had.
    each(list.querySelectorAll("section.ev"), function (section) {
      var beside = "";
      each(Array.prototype.slice.call(section.children), function (child) {
        if (!/^(P|UL|OL)$/.test(child.tagName)) return;
        var stop = doc.createElement("div");
        stop.className = "ev-stop";
        if (child.hasAttribute("data-block")) beside = child.getAttribute("data-block");
        stop.setAttribute("data-block", beside);
        ["data-frame", "data-head"].forEach(function (name) {
          if (child.hasAttribute(name)) stop.setAttribute(name, child.getAttribute(name));
          child.removeAttribute(name);
        });
        child.removeAttribute("data-block");
        section.replaceChild(stop, child);
        stop.appendChild(child);
        var last = child.querySelectorAll("mark[data-block]");
        if (last.length) beside = last[last.length - 1].getAttribute("data-block");
      });
    });
    var sections = Array.prototype.map.call(list.querySelectorAll("section.ev"), function (s) {
      return { id: s.id, title: s.querySelector(".ev-title").textContent };
    });

    var set = function (sel, text) { var el = doc.querySelector(sel); if (el && text) el.textContent = text; };
    var title = config.title || (doc.querySelector(".art-headline") || {}).textContent || "Article";
    doc.title = title + " (scroll-driven mock)";
    doc.body.removeAttribute("hx-boost");
    doc.body.classList.add("page-evolving");
    set(".art-headline", config.title);
    set(".neo-dek", config.dek);
    set(".toc-article-title", config.title);
    set(".contents-row.current .contents-t", config.title);

    // Contents in the lede and in the strip, when the sections aren't the
    // live article's.
    if (ids() !== before) {
      each(doc.querySelectorAll(".contents-row.sub"), function (a) { a.parentNode.remove(); });
      var current = doc.querySelector(".contents-row.current");
      if (current) {
        current.parentNode.insertAdjacentHTML("afterend", sections.map(function (s, i) {
          return '<li><a class="contents-row sub" href="#' + s.id + '"><span class="contents-n">' + (i + 1) +
            '</span><span class="contents-t">' + esc(s.title) + "</span></a></li>";
        }).join(""));
      }
      each(doc.querySelectorAll(".toc-cell"), function (a) { a.remove(); });
      var strip = doc.querySelector(".toc-strip");
      if (strip) {
        strip.insertAdjacentHTML("beforeend", sections.map(function (s, i) {
          return '<a class="toc-cell" tabindex="-1" href="#' + s.id + '" data-section="' + s.id +
            '"><span class="strip-n">' + pad(i + 1) + '</span><span class="strip-t">' + esc(s.title) +
            '</span><span class="strip-bar"></span></a>';
        }).join(""));
      }
    }

    each(doc.querySelectorAll(".toc-cell .strip-n"), function (n, i, all) {
      n.setAttribute("data-total", pad(all.length));
    });
    var script = doc.createElement("script");
    script.src = "/static/js/magazine-evolving.js?" + STAMP;
    script.defer = true;
    doc.head.appendChild(script);
    // The page's own stylesheet link is versioned by the server; ask again
    // so an edit to it shows without restarting the site.
    each(doc.querySelectorAll('link[href*="magazine.css"]'), function (link) {
      link.href = "/static/css/magazine.css?" + STAMP;
    });

    document.open();
    document.write("<!DOCTYPE html>" + doc.documentElement.outerHTML);
    document.close();
  }).catch(function (e) {
    if (status) status.textContent = "Could not load the article shell (" + e.message + "). Is the dev server running?";
  });
})();
