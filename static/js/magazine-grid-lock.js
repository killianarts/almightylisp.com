// The grid lock (see magazine.css): puts each box of graph paper on a whole
// pixel and rounds its height up to whole rows.
//
// A box starts wherever the text above it ends, which is as often as not
// part of the way through a pixel, and a 1px line drawn there blurs over
// two. CSS has no way to round where a box starts, so this moves each box
// down the part of a pixel it needs (a margin under 1px), in the order they
// come, as each moves the ones after it.
//
// magazine.css builds the footer and the book panel beside an article from
// whole rows, so their heights are whole rows already and the rounding does
// nothing. It is the net under that: a fallback font with other metrics, an
// image that loads late, text that wraps differently. CSS can't round a
// height that comes from the content (calc-size() will, in time).
//
// The box's ::before is the lattice; a box without one (under 1180px, where
// neither has paper) is left alone. data-gl-nudged and data-gl-snapped say by
// how much a box was moved and rounded (the lab, /static/dev/grid-lock/,
// shows them).
(function () {
  "use strict";

  var CELL = 28;
  var BOXES = ".site-footer, .book-ad.compact";
  var queued = false;

  function fix() {
    queued = false;
    var dpr = window.devicePixelRatio || 1;
    var boxes = document.querySelectorAll(BOXES);
    boxes.forEach(function (box) {
      box.style.marginBlockStart = "";
      box.style.minHeight = "";
      delete box.dataset.glNudged;
      delete box.dataset.glSnapped;
    });
    // In document order, measuring each after the ones above it have moved.
    boxes.forEach(function (box) {
      if (getComputedStyle(box, "::before").content === "none") return;

      var top = (box.getBoundingClientRect().top + window.scrollY) * dpr;
      var part = Math.ceil(top - 0.001) - top;
      if (part > 0.001) {
        box.style.marginBlockStart = part / dpr + "px";
        box.dataset.glNudged = (part / dpr).toFixed(2);
      }

      var cs = getComputedStyle(box);
      var pad = parseFloat(cs.paddingBlockStart) + parseFloat(cs.paddingBlockEnd);
      var inner = box.getBoundingClientRect().height - pad;
      var whole = Math.ceil(inner / CELL - 0.01) * CELL;
      if (whole - inner > 0.01) {
        box.style.minHeight = whole + pad + "px";
        box.dataset.glSnapped = (whole - inner).toFixed(2);
      }
    });
    // Moving a box doesn't resize it, so say so for whatever measures them.
    document.dispatchEvent(new CustomEvent("gridlock:fixed"));
  }

  function queue() {
    if (queued) return;
    queued = true;
    window.requestAnimationFrame(fix);
  }

  // What's inside each box, and the sheet: the boxes' own margins and
  // heights are what this sets.
  var observer = new ResizeObserver(queue);

  function init() {
    observer.disconnect();
    document.querySelectorAll(BOXES).forEach(function (box) {
      Array.prototype.forEach.call(box.children, function (child) { observer.observe(child); });
    });
    var sheet = document.querySelector(".sheet");
    if (sheet) observer.observe(sheet);
    queue();
  }

  // Pages arrive by htmx swap as well as by normal loads. Not the search
  // palette's results: there is no paper in them.
  document.addEventListener("htmx:after:swap", function (e) {
    if (e.target.closest && e.target.closest("#search")) return;
    init();
  });
  window.addEventListener("resize", queue);
  window.addEventListener("load", queue);
  if (document.fonts) {
    document.fonts.ready.then(queue);
    if (document.fonts.addEventListener) document.fonts.addEventListener("loadingdone", queue);
  }

  init();
})();
