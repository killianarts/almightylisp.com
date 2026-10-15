// Grid lock specimen: measures the graph paper against its 28px lattice.
//
// The specimen has two places with paper, measured apart: the book panel
// beside the article (.book-ad.compact) and the footer (.site-footer). In
// each, a lattice box is anything drawn with graph-reversed.svg: before, the
// panel itself, and each of the footer's three cells; after, the ::before of
// the panel and of the footer. For each box it asks:
//
//   Edges   is it 28N + 1 wide and tall, closed by a line on every side?
//           (28N is whole cells with the last line missing; anything else
//           cuts a cell.)
//   Pixels  does it start on a whole device pixel, so its 1px lines are
//           sharp?
//   Shared  is there one lattice for the footer, or one per cell, whose
//           lines don't meet?
//   Buttons do the buttons' borders lie on the box's lines?
//
// With ?overlay=1 it draws what it finds: red over cut cells and off-grid
// buttons, green on what holds. It posts the findings to the page that
// framed it (index.html), and writes them into #gl-report.
(function () {
  "use strict";

  var CELL = 28;
  var EPS = 0.05;
  var REGIONS = [
    { name: "book", label: "Book beside the article", root: ".book-ad.compact", cells: false },
    { name: "footer", label: "Footer", root: ".site-footer", cells: true }
  ];
  var root = document.documentElement;
  var mode = root.classList.contains("grid-lock") ? "after" : "before";
  var layer = null;

  function mod(a, n) { return ((a % n) + n) % n; }
  function near(a, b) { return Math.abs(a - b) < EPS; }
  function onLine(v) { var m = mod(v, CELL); return m < EPS || m > CELL - EPS; }
  function whole(v) { var m = mod(v, 1); return m < EPS || m > 1 - EPS; }
  function px(n) { return Math.round(n * 100) / 100; }

  // "closed": 28N + 1. "open": 28N, the last line missing. "cut": neither.
  function edge(len) {
    var m = mod(len, CELL);
    if (near(m, 1)) return { fit: "closed", cells: Math.round((len - 1) / CELL), over: 0 };
    if (m < EPS || m > CELL - EPS) return { fit: "open", cells: Math.round(len / CELL), over: 0 };
    return { fit: "cut", cells: Math.floor(len / CELL), over: px(m) };
  }

  function isPaper(cs) { return cs.backgroundImage.indexOf("graph-reversed") >= 0; }

  // The region's lattices: its root's ::before, its root, and (the footer)
  // its cells, whichever carry the paper.
  function lattices(el, cells) {
    var found = [];
    var r = el.getBoundingClientRect();
    var before = getComputedStyle(el, "::before");
    if (before.content !== "none" && isPaper(before)) {
      found.push({ name: "lattice", x: r.left + parseFloat(before.left), y: r.top + parseFloat(before.top),
                   w: parseFloat(before.width), h: parseFloat(before.height) });
    }
    if (isPaper(getComputedStyle(el))) found.push({ name: "panel", x: r.left, y: r.top, w: r.width, h: r.height });
    if (cells) {
      el.querySelectorAll(":scope > *").forEach(function (cell) {
        if (!isPaper(getComputedStyle(cell))) return;
        var q = cell.getBoundingClientRect();
        found.push({ name: cell.className.split(" ").pop(), x: q.left, y: q.top, w: q.width, h: q.height });
      });
    }
    return found;
  }

  function boxAt(boxes, r) {
    var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
    for (var i = 0; i < boxes.length; i++) {
      var b = boxes[i];
      if (cx >= b.x && cx <= b.x + b.w && cy >= b.y && cy <= b.y + b.h) return b;
    }
    return null;
  }

  function draw(css, text, cls) {
    var d = document.createElement("div");
    d.className = "gl-mark " + (cls || "");
    d.style.cssText = css;
    if (text) d.textContent = text;
    layer.appendChild(d);
  }

  function say(e) {
    return e.fit === "closed" ? e.cells + " cells, closed" : e.fit === "open" ? e.cells + " cells, open" : e.cells + " cells + " + e.over + "px";
  }

  function region(def, el, dpr, drawing) {
    var boxes = lattices(el, def.cells);
    var out = { name: def.name, label: def.label, lattices: boxes.length, boxes: [],
                buttons: { on: 0, total: 0 }, dividers: { on: 0, total: 0 },
                snapped: el.dataset.glSnapped || null, nudged: el.dataset.glNudged || null };
    var sx = scrollX, sy = scrollY;

    boxes.forEach(function (b) {
      var ex = edge(b.w), ey = edge(b.h);
      var sharp = whole(b.x * dpr) && whole(b.y * dpr);
      out.boxes.push({ name: b.name, left: px(b.x), top: px(b.y + sy), w: px(b.w), h: px(b.h), x: ex, y: ey, sharp: sharp });
      if (!drawing) return;
      var X = b.x + sx, Y = b.y + sy;
      var good = ex.fit === "closed" && ey.fit === "closed" && sharp;
      draw("left:" + X + "px;top:" + Y + "px;width:" + b.w + "px;height:" + b.h + "px", null, good ? "gl-box ok" : "gl-box bad");
      if (ex.fit === "cut") draw("left:" + (X + b.w - ex.over) + "px;top:" + Y + "px;width:" + ex.over + "px;height:" + b.h + "px", null, "gl-cut");
      if (ey.fit === "cut") draw("left:" + X + "px;top:" + (Y + b.h - ey.over) + "px;width:" + b.w + "px;height:" + ey.over + "px", null, "gl-cut");
      draw("left:" + (X + 4) + "px;top:" + (Y + b.h - 22) + "px",
        px(b.w) + " × " + px(b.h) + "  ■  " + say(ex) + "  ×  " + say(ey) + (sharp ? "" : "  ■  half-pixel"),
        good ? "gl-tag ok" : "gl-tag bad");
    });

    // The rules between the footer's cells, when they share one lattice:
    // on its lines?
    if (def.cells && boxes.length === 1) {
      el.querySelectorAll(":scope > * + *").forEach(function (cell) {
        out.dividers.total++;
        if (onLine(cell.getBoundingClientRect().left - boxes[0].x)) out.dividers.on++;
      });
    }

    el.querySelectorAll(".neo-button").forEach(function (btn) {
      var r = btn.getBoundingClientRect();
      if (!r.width) return;
      var b = boxAt(boxes, r);
      out.buttons.total++;
      var ok = !!b && onLine(r.left - b.x) && onLine(r.top - b.y) && onLine(r.width - 1) && onLine(r.height - 1);
      if (ok) out.buttons.on++;
      if (drawing) {
        draw("left:" + (r.left + sx) + "px;top:" + (r.top + sy) + "px;width:" + r.width + "px;height:" + r.height + "px",
          null, ok ? "gl-btn ok" : "gl-btn bad");
      }
    });
    return out;
  }

  // ?probe=1: each region's computed layout too, for reading the report from
  // a headless browser (chrome --headless --dump-dom).
  function probe(el) {
    var r = el.getBoundingClientRect(), cs = getComputedStyle(el), bcs = getComputedStyle(el, "::before");
    return {
      box: [px(r.left), px(r.width), px(r.height)],
      pad: [cs.paddingTop, cs.paddingRight, cs.paddingBottom, cs.paddingLeft],
      cols: cs.gridTemplateColumns,
      before: [bcs.left, bcs.width, bcs.height],
      kids: Array.prototype.map.call(el.children, function (k) {
        var q = k.getBoundingClientRect();
        return k.className + " @" + px(q.left - r.left) + "," + px(q.top - r.top) + " " + px(q.width) + "x" + px(q.height);
      }),
      buttons: Array.prototype.map.call(el.querySelectorAll(".neo-button"), function (b) {
        var q = b.getBoundingClientRect();
        return b.textContent + " @" + px(q.left - r.left) + "," + px(q.top - r.top) + " " + px(q.width) + "x" + px(q.height);
      })
    };
  }

  function measure() {
    var dpr = window.devicePixelRatio || 1;
    var drawing = root.classList.contains("gl-overlay");
    if (layer) layer.remove();
    layer = null;
    if (drawing) {
      layer = document.createElement("div");
      layer.className = "gl-layer";
      document.body.appendChild(layer);
    }

    var probing = /probe=1/.test(location.search);
    var report = {
      type: "grid-lock", mode: mode, width: innerWidth, dpr: dpr,
      // The sheet's, not the document's: a document is never shorter than
      // its frame, so the frame could grow but never shrink back.
      height: Math.ceil(document.querySelector(".sheet").getBoundingClientRect().bottom + scrollY),
      sheet: px(document.querySelector(".sheet").getBoundingClientRect().width),
      regions: []
    };
    REGIONS.forEach(function (def) {
      var el = document.querySelector(def.root);
      if (!el || !el.getBoundingClientRect().width) {
        report.regions.push({ name: def.name, label: def.label, hidden: true });
        return;
      }
      var out = region(def, el, dpr, drawing);
      if (probing) out.probe = probe(el);
      report.regions.push(out);
    });

    var pre = document.getElementById("gl-report");
    if (pre) pre.textContent = JSON.stringify(report, null, 1);
    if (parent !== window) parent.postMessage(report, "*");
  }

  var queued = false;
  function queue() {
    if (queued) return;
    queued = true;
    // Two frames: after magazine-grid-lock.js has rounded the heights.
    requestAnimationFrame(function () { requestAnimationFrame(function () { queued = false; measure(); }); });
  }

  var style = document.createElement("style");
  style.textContent =
    ".gl-layer { position: absolute; inset: 0 auto auto 0; z-index: 100000; pointer-events: none; }" +
    ".gl-mark { position: absolute; box-sizing: border-box; }" +
    ".gl-box.ok { outline: 1px solid #2bd46a; }" +
    ".gl-box.bad { outline: 1px dashed #ff3b3b; }" +
    ".gl-cut { background: repeating-linear-gradient(135deg, rgb(255 59 59 / 0.75) 0 2px, rgb(255 59 59 / 0.2) 2px 5px); }" +
    ".gl-btn.ok { outline: 2px solid rgb(43 212 106 / 0.8); outline-offset: -1px; }" +
    ".gl-btn.bad { outline: 2px dashed #ff3b3b; outline-offset: -1px; background: rgb(255 59 59 / 0.18); }" +
    ".gl-tag { padding: 2px 6px; font: 11px/1.4 ui-monospace, Menlo, monospace; color: #000; white-space: nowrap; }" +
    ".gl-tag.ok { background: #2bd46a; } .gl-tag.bad { background: #ff3b3b; color: #fff; }";
  document.head.appendChild(style);

  addEventListener("resize", queue);
  addEventListener("load", queue);
  document.addEventListener("gridlock:fixed", queue);
  if (document.fonts) document.fonts.ready.then(queue);
  var watch = new ResizeObserver(queue);
  REGIONS.forEach(function (def) {
    var el = document.querySelector(def.root);
    if (el) watch.observe(el);
  });
  addEventListener("message", function (e) {
    if (e.data && e.data.type === "grid-lock-overlay") {
      root.classList.toggle("gl-overlay", !!e.data.on);
      queue();
    }
  });
  queue();
})();
