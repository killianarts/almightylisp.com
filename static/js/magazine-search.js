// The search palette's own behaviour. hypermedia/search.lisp draws the
// palette and describes it, and htmx fetches what a search changes. This
// opens and closes it, presses the controls its keys stand for, keeps what
// a search chose in view, and marks the match in an article opened from it.
//
// Everything hangs off the document, so a page htmx swaps in works without
// being set up again.
(function () {
  "use strict";

  function palette() {
    return document.getElementById("search");
  }

  function open() {
    var search = palette();
    if (!search) return;
    search.classList.add("open");
    var input = search.querySelector(".search-input");
    input.focus();
    input.select();
  }

  function close() {
    var search = palette();
    if (search) search.classList.remove("open");
  }

  // The match an article is being opened at: its text, which of the places
  // that text is in its section it is, and where the link goes.
  var wanted = null;

  document.addEventListener("click", function (e) {
    if (!e.target.closest) return;
    var link = e.target.closest("a[data-find]");
    if (link) {
      wanted = { text: link.dataset.find, nth: Number(link.dataset.nth) || 0,
                 path: link.pathname, id: decodeURIComponent(link.hash.slice(1)) };
      return;
    }
    // The strip's Search cell opens it; the scrim and the Esc key close it.
    var control = e.target.closest("[data-search]");
    if (control) {
      if (control.dataset.search === "open") open();
      else close();
      return;
    }
    // Typing goes on wherever a click landed.
    var inside = e.target.closest(".search-palette");
    if (inside && !e.target.closest("a")) inside.querySelector(".search-input").focus();
  });

  document.addEventListener("keydown", function (e) {
    var search = palette();
    // Not while a word is being composed (Japanese): the keys are its own.
    if (!search || e.isComposing) return;
    var isOpen = search.classList.contains("open");
    var typing = e.target.closest && e.target.closest("input, textarea, select, [contenteditable]");
    // / and Ctrl or Cmd+K open it.
    if ((e.key === "/" && !e.metaKey && !e.ctrlKey && !e.altKey && !typing) ||
        (e.key && e.key.toLowerCase() === "k" && (e.metaKey || e.ctrlKey))) {
      e.preventDefault();
      open();
      return;
    }
    if (!isOpen) return;
    if (e.key === "Escape") {
      close();
      return;
    }
    // Left and Right are the caret's until it is at the end of the query.
    if ((e.key === "ArrowLeft" || e.key === "ArrowRight") &&
        e.target.selectionEnd < (e.target.value || "").length) return;
    // A key presses the control that names it in data-keys, if one does.
    var name = (e.shiftKey ? "Shift" : "") + (e.key === " " ? "Space" : e.key);
    var pressed = search.querySelector("[data-keys~=" + CSS.escape(name) + "]");
    if (pressed) {
      e.preventDefault();
      pressed.click();
    }
  });

  // The NTH place TEXT is in ROOT, as a range, or the last there is before
  // it. Space is left out of the comparison: the page breaks its lines, and
  // sets its code, where the search's text has single spaces.
  function find(root, text, nth) {
    var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    var all = "", places = [], node;
    while ((node = walker.nextNode())) {
      for (var i = 0; i < node.nodeValue.length; i++) {
        if (/\s/.test(node.nodeValue[i])) continue;
        all += node.nodeValue[i].toLowerCase();
        places.push([node, i]);
      }
    }
    var needle = text.toLowerCase().replace(/\s+/g, "");
    var at = -1, found = -1;
    for (var n = 0; needle && n <= nth; n++) {
      at = all.indexOf(needle, at + 1);
      if (at < 0) break;
      found = at;
    }
    // Lower case is a different length for a very few letters.
    if (found < 0 || !places[found + needle.length - 1]) return null;
    var first = places[found], last = places[found + needle.length - 1];
    var range = document.createRange();
    range.setStart(first[0], first[1]);
    range.setEnd(last[0], last[1] + 1);
    return range;
  }

  // The page a match was opened in has arrived, magazine-justify.js has set
  // its text and htmx has scrolled to the match's section: find the match,
  // bring it into view and mark it, where the browser can mark a range
  // (::highlight(search-match) in magazine.css). The mark lasts until the
  // next page. On the way down (capture), so the page is where it will stay
  // before magazine-fx.js, which listens on the way up, settles it in.
  document.addEventListener("htmx:finally:swap", function (e) {
    if (e.detail.ctx.target !== document.body) return;
    if (window.CSS && CSS.highlights) CSS.highlights.delete("search-match");
    var match = wanted;
    wanted = null;
    if (!match || match.path !== location.pathname) return;
    var range = find(document.getElementById(match.id) || document.querySelector(".sheet"),
                     match.text, match.nth);
    if (!range) return;
    var box = range.getBoundingClientRect();
    window.scrollTo(0, Math.max(0, window.scrollY + box.top - window.innerHeight / 3));
    if (window.CSS && CSS.highlights) CSS.highlights.set("search-match", new Highlight(range));
  }, true);

  // A search's answer is in: bring the article and the match it chose into
  // view. A new article starts from its top.
  var shown = null;
  document.addEventListener("htmx:after:swap", function () {
    var search = palette();
    if (!search || !search.classList.contains("open")) return;
    var preview = search.querySelector(".search-preview");
    if (preview.dataset.article !== shown) {
      shown = preview.dataset.article;
      preview.scrollTop = 0;
    }
    [".search-item[aria-selected=true]", ".search-match.on"].forEach(function (chosen) {
      var el = search.querySelector(chosen);
      if (el) el.scrollIntoView({ block: "nearest" });
    });
  });
})();
