// Evolving articles (#+LAYOUT: evolving). Each section.ev becomes a tall
// track with one sticky stage: the heading, a note or a code block on the
// left, one paragraph at a time on the right. The markup it reads is written
// by hypermedia/evolving.lisp, which describes it.
//
// A section's scroll is divided into stops: one for each paragraph, and one
// more for each marked expression inside a paragraph. Every stop says what
// the left side shows (the heading, or a block at a frame), so the stage can
// be set from any scroll position, in either direction.
//
// Pages arrive by htmx swap as well as by normal loads, and each evolving
// article brings this script with it. The window's listeners are set up
// once; each new page only builds its sections.
(function () {
  if (window.MagazineEvolving) {
    window.MagazineEvolving.init();
    return;
  }
  var STOP_VH = 0.62;       // scroll per paragraph, in window heights
  var TRIGGER_WEIGHT = 0.6; // an expression's share of that
  var LEAD_VH = 0.14;       // scroll a tall paragraph rests for at each end
  var TRIGGER_LEAD = 280;   // ms an expression is lit before its block moves
  var RETURN_LEAD = 450;    // ms a block that comes back is seen before it moves
  var built = [];

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text != null) e.textContent = text;
    return e;
  }
  function pad(n) { return n < 10 ? "0" + n : "" + n; }

  // --- Code blocks ----------------------------------------------------------

  function parse(line) {
    var text = line.replace(/^\s+/, "");
    return { indent: line.length - text.length, text: text };
  }

  // Two different lines are the same line edited when they start alike.
  function alike(a, b) {
    var n = Math.min(a.length, b.length), i = 0;
    while (i < n && a[i] === b[i]) i++;
    return n >= 4 && i >= n * 0.4;
  }

  // For each line of b, the line of a it came from (or -1), by the longest
  // common subsequence of their texts; then leftover lines between matches
  // are paired when they are alike.
  function match(a, b) {
    var n = a.length, m = b.length, i, j;
    var t = [];
    for (i = 0; i <= n; i++) { t.push([]); for (j = 0; j <= m; j++) t[i].push(0); }
    for (i = n - 1; i >= 0; i--) {
      for (j = m - 1; j >= 0; j--) {
        t[i][j] = a[i].text === b[j].text ? t[i + 1][j + 1] + 1 : Math.max(t[i + 1][j], t[i][j + 1]);
      }
    }
    var from = b.map(function () { return -1; });
    var gaps = [], gap = { a: [], b: [] };
    i = 0; j = 0;
    while (i < n || j < m) {
      if (i < n && j < m && a[i].text === b[j].text) {
        from[j] = i; i++; j++;
        gaps.push(gap); gap = { a: [], b: [] };
      } else if (j < m && (i >= n || t[i][j + 1] >= t[i + 1][j])) {
        gap.b.push(j++);
      } else {
        gap.a.push(i++);
      }
    }
    gaps.push(gap);
    gaps.forEach(function (g) {
      var next = 0;
      g.b.forEach(function (bj) {
        for (var k = next; k < g.a.length; k++) {
          if (alike(a[g.a[k]].text, b[bj].text)) { from[bj] = g.a[k]; next = k + 1; break; }
        }
      });
    });
    return from;
  }

  function Block(figure) {
    var self = this;
    this.id = figure.id;
    var pres = Array.prototype.slice.call(figure.querySelectorAll("pre:not(.ev-returns)"));
    this.frames = pres.map(function (pre) {
      return pre.textContent.replace(/\n$/, "").split("\n").map(parse);
    });
    // What each frame returns (#+RESULTS:), or "" when it isn't given.
    this.results = pres.map(function (pre) {
      var next = pre.nextElementSibling;
      return next && next.classList.contains("ev-returns") ? next.textContent.replace(/\n$/, "") : "";
    });
    this.shown = 0;   // frame on show, 0 before the first
    this.lines = [];  // the elements for that frame's lines, in order
    this.el = el("figure", "ev-block" + (this.frames.length > 1 ? " has-steps" : ""));
    this.el.appendChild(el("p", "ev-name", figure.getAttribute("data-name") || ""));
    this.code = el("div", "ev-code");
    this.nums = el("div", "ev-nums");
    this.code.appendChild(this.nums);
    // The code is seen through a window as wide and as tall as the panel
    // has room for. Wider code scrolls sideways in it; a taller frame is
    // moved up through it (see fit, centre).
    this.view = el("div", "ev-view");
    this.view.appendChild(this.code);
    this.el.appendChild(this.view);
    this.roomH = Infinity;
    this.returns = el("pre", "ev-returns");
    this.el.appendChild(this.returns);
    this.code.style.setProperty("--rows", this.frames[0].length);
    // For find in page: the lines that move are drawn by CSS, which a search
    // doesn't read, and every frame's code lies over them as still, unseen
    // text. Only the layer for the frame on show is laid out, so a match is
    // marked over the line it belongs to; a match in another frame's layer
    // is announced (it is "hidden until found") and we go to that frame.
    this.layers = !FINDABLE ? null : this.frames.map(function (lines) {
      var layer = el("div", "ev-find");
      lines.forEach(function (line) {
        var row = el("div", "", line.text);
        row.style.setProperty("--indent", line.indent);
        layer.appendChild(row);
      });
      conceal(layer, true);
      self.code.appendChild(layer);
      return layer;
    });
    self.show(1, false);
  }

  // Rows the frame takes.
  Block.prototype.height = function (frame) {
    return this.frames[frame - 1].length;
  };
  // How far, in px, FRAME is taller than the window it is seen through.
  Block.prototype.over = function (frame) {
    var size = parseFloat(getComputedStyle(this.code).fontSize);
    return Math.max(0, Math.ceil(this.height(frame) * 1.76 * size - this.roomH));
  };
  // Fit the block to its panel, WIDTH by HEIGHT in px for the code alone.
  // Code is never broken to fit: Lisp is read by its alignment. A block
  // wider than its panel scrolls sideways in its window (.ev-view), under a
  // bar of its own. One too tall is set smaller, as far as CODE_FLOOR px,
  // and one still too tall after that is moved through its window as the
  // reader scrolls the page (see centre).
  var CODE_FLOOR = 9, SCROLLBAR = 26;
  Block.prototype.fit = function (width, height) {
    var scale = 1, probe = el("span", "", "0000000000");
    probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
    this.code.appendChild(probe);
    // The widest line of any frame, in characters, indent included.
    var widest = 0, rows = 0;
    this.frames.forEach(function (lines) {
      rows = Math.max(rows, lines.length);
      lines.forEach(function (line) { widest = Math.max(widest, line.indent + line.text.length); });
    });
    this.code.style.setProperty("--cols", widest);
    // Digits in the last line's number: the numbers' column is that wide.
    var digits = String(rows).length;
    this.el.style.setProperty("--digits", digits);
    for (;;) {
      this.code.style.setProperty("--code-fit", scale.toFixed(2));
      var size = parseFloat(getComputedStyle(this.code).fontSize);
      var ch = probe.getBoundingClientRect().width / 10 || size * 0.6;
      // beside the line numbers' column and the marker's (see --gutter, .ev-m)
      var wide = 0.2 * size + (digits + 1.4 + widest) * ch > width;
      this.roomH = height - (wide ? SCROLLBAR : 0);
      if (rows * 1.76 * size <= this.roomH || size * (scale - 0.05) / scale < CODE_FLOOR) break;
      scale -= 0.05;
    }
    probe.remove();
  };

  // Plug the lines NODES into the block CODE and power them on: the motion
  // and the flare are CSS (.is-powering and .is-jolted in magazine.css, which
  // has the effect's measures as custom properties). At the hit each line's
  // colours split and let go as the site's links' do (MagazineFx.burst);
  // where that can't run, the CSS has a split of its own, in text shadows.
  // RATE slows it all, for the test page (dev/evolving-fx/).
  function power(code, nodes, rate) {
    rate = rate || 1;
    code.classList.remove("is-jolted");
    if (!nodes.length) return;
    var set = getComputedStyle(code);
    function measure(name) { return parseFloat(set.getPropertyValue(name)) || 0; }
    var flare = measure("--plug-flare-ms"), split = measure("--plug-split"), tear = measure("--plug-tear");
    var hit = 140 + measure("--plug-ms") * measure("--plug-hit");
    var bursts = !!(window.MagazineFx && window.MagazineFx.burst);
    nodes.forEach(function (node, i) {
      node.style.setProperty("--i", Math.min(i, 10));
      // No shadows where the site's split will play.
      if (bursts) node.style.setProperty("--plug-split", "0px");
      else node.style.removeProperty("--plug-split");
      node.classList.add("is-powering");
    });
    // The lines already there take the knock.
    void code.offsetWidth;
    code.classList.add("is-jolted");
    if (!split || !bursts) return;
    nodes.forEach(function (node, i) {
      // After the instant the text is black on the flare.
      var at = hit + Math.min(i, 10) * measure("--plug-stagger") + flare * 0.09;
      setTimeout(function () {
        if (!node.isConnected || !node.classList.contains("is-powering")) return;
        var played = window.MagazineFx.burst(node.querySelector(":scope > code"), {
          ms: flare * 0.91 / rate, split: split, tear: tear, rgb: [255, 255, 255]
        });
        if (!played) node.style.setProperty("--plug-split", split + "px");
      }, at / rate);
    });
  }

  Block.prototype.show = function (frame, animate) {
    frame = Math.max(1, Math.min(this.frames.length, frame));
    if (frame === this.shown) return;
    var target = this.frames[frame - 1];
    var before = this.shown ? this.frames[this.shown - 1] : [];
    var from = match(before, target);
    // What to mark is what this frame changed from the one before it,
    // whichever frame we happen to be arriving from.
    var prev = frame > 1 ? this.frames[frame - 2] : null;
    var since = prev ? match(prev, target) : null;

    var old = this.lines, kept = {}, code = this.code, plugged = [];
    var lines = target.map(function (line, row) {
      var node = from[row] >= 0 ? old[from[row]] : null;
      // An edited line isn't kept: its old text leaves and the new text comes
      // in, or a step that only edits a line would change nothing to the eye.
      if (node && animate && before[from[row]].text !== line.text) node = null;
      var fresh = !node;
      if (node) {
        kept[from[row]] = true;
      } else {
        node = el("div", "ev-line");
        node.appendChild(el("span", "ev-m"));
        node.appendChild(el("code"));
        if (animate) node.classList.add("is-entering");
        // The power-on (below) ends with its last animation, the text's.
        node.addEventListener("animationend", function (e) {
          if (e.animationName === "ev-aberrate") node.classList.remove("is-powering");
        });
        code.appendChild(node);
      }
      var kind = "plain";
      if (since) {
        var was = since[row] >= 0 ? prev[since[row]] : null;
        kind = !was ? "new" : (was.text !== line.text || was.indent !== line.indent) ? "moved" : "same";
      }
      node.classList.remove("k-plain", "k-same", "k-moved", "k-new");
      node.classList.add("k-" + kind);
      // A line this step adds is plugged in and powered on (see power),
      // each a moment after the one above it.
      node.classList.remove("is-powering");
      if (animate && fresh && kind === "new") {
        node.classList.remove("is-entering");
        plugged.push(node);
      }
      node.firstChild.textContent = kind === "new" ? "+" : kind === "moved" ? "›" : "";
      var text = node.querySelector(":scope > code");
      if (FINDABLE) text.setAttribute("data-text", line.text);
      else text.textContent = line.text;
      node.style.setProperty("--row", row);
      node.style.setProperty("--indent", line.indent);
      return node;
    });
    old.forEach(function (node, i) {
      if (kept[i]) return;
      if (!animate) { node.remove(); return; }
      node.classList.add("is-leaving");
      setTimeout(function () { node.remove(); }, 500);
    });
    this.nums.innerHTML = target.map(function (_, i) { return "<span>" + (i + 1) + "</span>"; }).join("");
    code.style.setProperty("--rows", target.length);
    this.lines = lines;
    this.shown = frame;
    power(code, plugged);
    if (animate) {
      void code.offsetWidth; // start entering lines from their offset
      lines.forEach(function (node) { node.classList.remove("is-entering"); });
    }
    this.returns.textContent = this.results[frame - 1];
    this.returns.classList.toggle("is-given", !!this.results[frame - 1]);
  };

  // A note on the left side: one frame, nothing to animate.
  function Note(aside) {
    this.id = aside.id;
    this.frames = [[]];
    this.el = el("aside", "ev-block ev-note");
    this.el.appendChild(el("p", "ev-name", aside.getAttribute("data-name") || "Note"));
    var body = el("div", "ev-note-body");
    while (aside.firstChild) body.appendChild(aside.firstChild);
    this.el.appendChild(body);
  }
  Note.prototype.show = function () {};
  // Height its content wants, for sizing the panel when the halves stack.
  Note.prototype.need = function () {
    return this.el.firstChild.offsetHeight + 10 + this.el.lastChild.offsetHeight;
  };
  // Lines of #+RESULTS: under the frame that has the most.
  Block.prototype.returned = function () {
    return Math.max.apply(null, this.results.map(function (r) { return r ? r.split("\n").length : 0; }));
  };
  // Height under the code for what it returns, in px.
  Block.prototype.under = function () {
    var returned = this.returned();
    return returned ? 27 + returned * 1.6 * parseFloat(getComputedStyle(this.returns).fontSize) : 0;
  };
  Block.prototype.need = function () {
    var rows = Math.max.apply(null, this.frames.map(function (f) { return f.length; }));
    var size = parseFloat(getComputedStyle(this.code).fontSize);
    var bar = this.view.scrollWidth > this.view.clientWidth ? SCROLLBAR : 0;
    return this.el.firstChild.offsetHeight + 10 + rows * 1.76 * size + bar + this.under();
  };
  Note.prototype.fit = function () {};

  // --- Sections -------------------------------------------------------------

  // Find in page. Text that isn't in focus is put away as "hidden until
  // found": the browser still searches it, and says so (beforematch) before
  // it shows a match there, which is when we go to that text's stop. That
  // has to include the faded paragraphs either side of the one in focus, or
  // a match in them would be shown where it is, faded. So what is seen there
  // is a ghost: a copy of the paragraph, list or table with every run of
  // text taken out and drawn by CSS instead, which a search doesn't read.
  // The copy keeps the markup (list items, code, links), so it wraps and
  // sits exactly as the real text does.
  // A browser without "until found" leaves the text as it is and is caught,
  // less exactly, by where it scrolls the panel to (see listen).
  var FINDABLE = "onbeforematch" in document.documentElement;
  function conceal(node, on) {
    if (!FINDABLE) return;
    if (on) node.setAttribute("hidden", "until-found");
    else node.removeAttribute("hidden");
  }
  function ghostOf(step, reel) {
    var ghost = el("div", "ev-ghost");
    ghost.setAttribute("aria-hidden", "true");
    Array.prototype.forEach.call(step.children, function (child) {
      ghost.appendChild(child.cloneNode(true));
    });
    // The copies are only for show: nothing that marks the real ones.
    Array.prototype.forEach.call(ghost.querySelectorAll("*"), function (node) {
      Array.prototype.slice.call(node.attributes).forEach(function (attr) {
        if (attr.name !== "class") node.removeAttribute(attr.name);
      });
    });
    var walker = document.createTreeWalker(ghost, NodeFilter.SHOW_TEXT);
    var texts = [], node;
    while ((node = walker.nextNode())) texts.push(node);
    texts.forEach(function (text) {
      var drawn = el("span");
      drawn.setAttribute("data-text", text.nodeValue);
      text.parentNode.replaceChild(drawn, text);
    });
    reel.appendChild(ghost);
    return ghost;
  }

  function build(section, index) {
    var title = section.querySelector(".ev-title");
    var blocks = {};
    Array.prototype.forEach.call(section.querySelectorAll("figure.ev-block"), function (f) {
      blocks[f.id] = new Block(f);
    });
    Array.prototype.forEach.call(section.querySelectorAll("aside.ev-note"), function (a) {
      blocks[a.id] = new Note(a);
    });
    var paras = Array.prototype.filter.call(section.children, function (c) {
      return c.classList.contains("ev-stop");
    });

    var stage = el("div", "ev-stage is-jump" + (FINDABLE ? " is-findable" : ""));
    var left = el("div", "ev-left"), right = el("div", "ev-right");
    var number = pad(index + 1);
    left.appendChild(el("p", "ev-eyebrow", "Section " + number));
    var label = el("p", "ev-label");
    label.appendChild(el("span", "ev-label-n", number));
    // The label repeats the heading; drawn by CSS, a search finds only one.
    var labelText = el("span", "ev-label-t", FINDABLE ? "" : title.textContent);
    if (FINDABLE) labelText.setAttribute("data-text", title.textContent);
    label.appendChild(labelText);
    left.appendChild(label);
    var big = el("h2", "ev-big", title.textContent);
    left.appendChild(big);
    // The subheading being read under: beneath the heading, and, once a
    // block has the heading's place, on the label's line in its stead (the
    // contents strip still names the section). Drawn by CSS, like the label.
    var subhead = el("div", "ev-subhead");
    subhead.setAttribute("aria-hidden", "true");
    subhead.appendChild(el("i", "ev-head-dash"));
    subhead.appendChild(el("span", "ev-head-n"));
    subhead.appendChild(el("span", "ev-head-t"));
    left.appendChild(subhead);
    Object.keys(blocks).forEach(function (id) { left.appendChild(blocks[id].el); });
    var meter = el("div", "ev-meter"), meterText = el("span"), meterBar = el("span", "ev-meter-bar");
    meter.appendChild(meterText);
    meter.appendChild(meterBar);
    left.appendChild(meter);

    // The stops, and what the left side shows at each.
    var reel = el("div", "ev-reel"), rail = el("div", "ev-rail");
    // The subheading being read under, kept at the top of the panel once
    // its own paragraph, which it stands over, has gone by. Drawn by CSS:
    // the one over the paragraph is the one a search finds.
    var sub = el("div", "ev-sub");
    sub.setAttribute("aria-hidden", "true");
    sub.appendChild(el("span", "ev-sub-n"));
    sub.appendChild(el("span", "ev-sub-t"));
    var stops = [], steps = [], block = null, frame = 1;
    paras.forEach(function (p, i) {
      var step = el("div", "ev-step"), tag = null;
      if (p.hasAttribute("data-block")) {
        var next = blocks[p.getAttribute("data-block")] || null;
        // A block that comes back is at the frame it was left at.
        if (next !== block) frame = (next && next.at) || 1;
        block = next;
      }
      if (p.hasAttribute("data-frame") && block) {
        frame = block.at = +p.getAttribute("data-frame");
        if (block.frames.length > 1) tag = el("div", "ev-tag", "Block step " + frame + " / " + block.frames.length);
      }
      stops.push({ step: i, trigger: -1, block: block, frame: frame, weight: 1 });
      var marks = Array.prototype.slice.call(p.querySelectorAll("mark[data-frame]"));
      marks.forEach(function (mark, j) {
        if (mark.hasAttribute("data-block")) block = blocks[mark.getAttribute("data-block")] || block;
        frame = +mark.getAttribute("data-frame");
        if (block) block.at = frame;
        stops.push({ step: i, trigger: j, block: block, frame: frame, weight: TRIGGER_WEIGHT });
      });
      if (marks.length && block) {
        var frames = marks.map(function (m) { return m.getAttribute("data-frame"); });
        tag = el("div", "ev-tag is-inline", "Inline step" + (frames.length > 1 ? "s " + frames[0] + " – " + frames[frames.length - 1] : " " + frames[0]));
      }
      // A subheading: a dash, its number as an eyebrow, and its words.
      var under = !p.hasAttribute("data-sub") ? null
        : { n: number + "." + p.getAttribute("data-sub-n"), title: p.getAttribute("data-sub"),
            first: p.hasAttribute("data-head") };
      if (p.hasAttribute("data-head")) {
        var head = el("div", "ev-head");
        head.appendChild(el("i", "ev-head-dash"));
        head.appendChild(el("span", "ev-head-n", "Section " + (under ? under.n : number)));
        head.appendChild(el("span", "ev-head-t", p.getAttribute("data-head")));
        step.appendChild(head);
      }
      if (tag) step.appendChild(tag);
      step.appendChild(p);
      reel.appendChild(step);
      steps.push({ el: step, marks: marks, sub: under, ghost: FINDABLE ? ghostOf(step, reel) : null });
    });
    var total = 0;
    stops.forEach(function (s) {
      s.at = total;
      total += s.weight;
      rail.appendChild(el("i", s.trigger >= 0 ? "is-trigger" : ""));
    });
    right.appendChild(reel);
    right.appendChild(sub);
    right.appendChild(rail);
    stage.appendChild(left);
    stage.appendChild(right);
    section.textContent = "";
    section.appendChild(stage);

    return { section: section, stage: stage, left: left, big: big, right: right, reel: reel, rail: rail, steps: steps,
             sub: sub, subhead: subhead, label: label, number: number, title: title.textContent,
             stops: stops, total: total, blocks: blocks, meterText: meterText, meterBar: meterBar,
             current: -1 };
  }

  // A paragraph taller than its panel is scrolled through, so it gets
  // scroll in proportion to its height.
  //
  // Under 900px the halves stack, and the contents strip above the stage is
  // the heading: it already names the section. So the top half is only as
  // tall as what it holds: nothing beside a plain paragraph, a block or a
  // note when there is one, up to a share of the stage. A note that wants
  // more than that is set smaller.
  var stacked = window.matchMedia("(max-width: 899px)");
  var TOP_SHARE = 0.56, MARGIN = 48;
  // Space over a block in the stacked top half, under the strip: the
  // block's top padding there (.mag .ev-block in magazine.css) and a little.
  var HEADROOM = 18;
  // The foot of the text's panel, kept clear for the dock (.ev-dock).
  var DOCK = 62;

  // Everything is sized against a window height that holds still. A phone's
  // window grows and shrinks as its toolbars hide and show, and following
  // that would move every stop while the reader is scrolling.
  // The size is the page's own (the root element's), not window.innerHeight:
  // that one is in the reader's zoom, and a phone turned on its side and
  // back reports it zoomed out for a moment, with no word when it settles.
  var view = { w: 0, h: 0 };
  function measure() {
    var root = document.documentElement;
    var w = root.clientWidth, h = root.clientHeight;
    if (w === view.w && Math.abs(h - view.h) < 160) return false;
    view.w = w;
    view.h = h;
    return true;
  }

  function topHeight(s, stop) {
    var most = (view.h - s.topPx) * TOP_SHARE;
    if (!stop.block) return 0;
    var chrome = HEADROOM + 18 + (stop.block.frames.length > 1 ? 34 : 0);
    if (stop.block instanceof Note) {
      var scale = 1;
      stop.block.el.style.setProperty("--note-fit", scale);
      while (scale > 0.72 && chrome + stop.block.need() > most) {
        scale -= 0.04;
        stop.block.el.style.setProperty("--note-fit", scale.toFixed(2));
      }
    }
    return Math.min(most, chrome + stop.block.need());
  }

  // Where each stop starts and how long it is, in pixels of scroll. A
  // paragraph taller than its panel is scrolled through at the speed of the
  // scroll itself, with a short rest at its top and bottom.
  function weigh(s) {
    MARGIN = stacked.matches ? 22 : 48;
    // Measured with everything laid out; each paragraph then keeps its
    // height while its text is put away (see tidy).
    s.steps.forEach(function (step) { conceal(step.el, false); step.el.style.height = ""; });
    Object.keys(s.blocks).forEach(function (id) { conceal(s.blocks[id].el, false); });
    s.topPx = parseFloat(getComputedStyle(s.stage).top) || 0;
    // Each block is fitted to the room it will have: its box less its
    // padding, or, where the halves stack, the most the top half may take.
    // With the block given the whole stage (see full) that room is the
    // stage's, and .is-sizing gives the left side the stage to be measured in.
    s.stage.classList.add("is-sizing");
    Object.keys(s.blocks).forEach(function (id) {
      var block = s.blocks[id], box = getComputedStyle(block.el);
      var width = block.el.clientWidth - parseFloat(box.paddingLeft) - parseFloat(box.paddingRight);
      var height = stacked.matches && mode !== "code"
        ? (view.h - s.topPx) * TOP_SHARE - HEADROOM - 18 - (block.frames.length > 1 ? 34 : 0)
        : block.el.clientHeight - parseFloat(box.paddingTop) - parseFloat(box.paddingBottom);
      block.fit(width, height - block.el.firstChild.offsetHeight - 22 - (block.under ? block.under() : 0));
    });
    s.stage.classList.remove("is-sizing");
    s.steps.forEach(function (step) { step.h = step.el.offsetHeight; });
    s.steps.forEach(function (step) {
      step.el.style.height = step.h + "px";
      if (step.ghost) {
        step.ghost.style.top = step.el.offsetTop + "px";
        step.ghost.style.height = step.h + "px";
      }
    });
    var unit = STOP_VH * view.h;
    s.total = 0;
    s.stops.forEach(function (stop) {
      stop.top = !stacked.matches ? null : mode === "text" ? 0 : Math.round(topHeight(s, stop));
      stop.panel = view.h - s.topPx - (stop.top || 0) - DOCK;
      if (stop.trigger < 0) {
        var h = s.steps[stop.step].el.offsetHeight;
        stop.travel = Math.max(0, h + MARGIN * 2 - stop.panel);
        stop.tall = stop.travel > 0;
        // A block taller than its window is scrolled through as well.
        stop.over = stop.block && stop.block.over ? stop.block.over(stop.frame) : 0;
        stop.scrub = Math.max(stop.travel, stop.over);
        stop.len = Math.max(unit, stop.scrub + 2 * LEAD_VH * view.h);
        stop.lead = (stop.len - stop.scrub) / 2;
      } else {
        stop.len = unit * TRIGGER_WEIGHT;
      }
      stop.at = s.total;
      s.total += stop.len;
    });
  }

  // A section's stage is on show while any of it is in the window, and
  // pinned while it is stuck under the strip.
  function seen(s) {
    var box = s.stage.getBoundingClientRect();
    return box.bottom > 0 && box.top < window.innerHeight;
  }
  function pinned(s) {
    var box = s.section.getBoundingClientRect();
    return box.top <= s.topPx + 1 && box.bottom > s.topPx + 1;
  }
  // Put away whatever isn't on show: every paragraph but the one in focus,
  // every block but the one on the left, and, in a section that is out of
  // the window, all of it with its heading. A match in any of that is then
  // announced, and we go to its stop; left as it was, the browser would
  // scroll just far enough to show the match, with the section half in view.
  function tidy(s) {
    var stop = s.stops[Math.max(0, s.current)];
    var on = s.onscreen = seen(s);
    s.steps.forEach(function (step, i) { conceal(step.el, !on || i !== stop.step); });
    Object.keys(s.blocks).forEach(function (id) {
      var block = s.blocks[id], shown = on && block === stop.block;
      if (!block.layers) conceal(block.el, !shown);
      else block.layers.forEach(function (layer, i) { conceal(layer, !shown || i + 1 !== stop.frame); });
    });
    // The heading keeps its height while put away, so nothing watching its
    // size sees a change.
    s.big.style.height = on || !FINDABLE ? "" : s.bigH + "px";
    conceal(s.big, !on);
  }
  function reveal(s, index) {
    if (index >= 0 && (index !== s.current || !pinned(s))) go(built.indexOf(s) + 1, index + 1);
  }
  function stopFor(s, node) {
    var index = s.big.contains(node) ? 0 : -1;
    s.steps.forEach(function (step, i) {
      if (index < 0 && step.el.contains(node)) {
        s.stops.some(function (stop, j) { if (stop.step === i) { index = j; return true; } });
      }
    });
    Object.keys(s.blocks).forEach(function (id) {
      var block = s.blocks[id];
      if (index >= 0 || !block.el.contains(node)) return;
      // A match in one frame of an evolving block: the first stop that
      // shows that frame, or failing that the first to show the block.
      var frame = 0;
      (block.layers || []).forEach(function (layer, i) { if (layer.contains(node)) frame = i + 1; });
      s.stops.forEach(function (stop, j) {
        if (index < 0 && stop.block === block && stop.frame === frame) index = j;
      });
      s.stops.forEach(function (stop, j) {
        if (index < 0 && stop.block === block) index = j;
      });
    });
    return index;
  }
  function listen() {
    document.addEventListener("beforematch", function (e) {
      built.forEach(function (s) { reveal(s, stopFor(s, e.target)); });
    });
    // A selection made in text that isn't in focus (some browsers select
    // what they find) brings that text into focus.
    document.addEventListener("selectionchange", function () {
      var sel = document.getSelection();
      if (!sel || sel.isCollapsed || !sel.anchorNode) return;
      built.forEach(function (s) {
        var index = stopFor(s, sel.anchorNode);
        if (index < 0) return;
        var now = s.stops[Math.max(0, s.current)], then = s.stops[index];
        var inText = s.reel.contains(sel.anchorNode);
        if (inText ? then.step !== now.step : then.block !== now.block) reveal(s, index);
      });
    });
  }
  // To show a match the browser may scroll the panels themselves, which
  // never scroll otherwise. Undo that, and turn it into our own scroll:
  // through a tall paragraph, or to the paragraph it brought to the middle.
  function watch(s) {
    s.stage.addEventListener("scroll", function (e) {
      var box = e.target, moved = box.scrollTop;
      // A block's window scrolls sideways, and is left to.
      var view = box.classList && box.classList.contains("ev-view");
      if (!moved && (view || !box.scrollLeft)) return;
      var mid = s.right.getBoundingClientRect().top + s.right.clientHeight / 2, found = -1;
      if (box === s.right && !FINDABLE) {
        s.steps.forEach(function (step, i) {
          var r = step.el.getBoundingClientRect();
          if (r.top <= mid && r.bottom >= mid) found = i;
        });
      }
      box.scrollTop = 0;
      if (!view) box.scrollLeft = 0;
      var stop = s.stops[Math.max(0, s.current)];
      if (found >= 0 && found !== stop.step) reveal(s, stopFor(s, s.steps[found].el));
      else if (box === s.right && stop.tall) window.scrollBy(0, moved);
    }, true);
  }

  function size(s) {
    s.section.style.height = "calc(" + Math.round(s.total) + "px + var(--ev-h))";
  }

  // The heading at the largest size its longest word fits the panel at.
  function fit(s) {
    var scale = 1;
    conceal(s.big, false);
    s.big.style.height = "";
    s.big.style.setProperty("--fit", scale);
    var room = stacked.matches ? Infinity : s.left.clientHeight * 0.66;
    while (scale > 0.3 && (s.big.scrollWidth > s.big.clientWidth + 1 || s.big.offsetHeight > room)) {
      scale -= 0.04;
      s.big.style.setProperty("--fit", scale.toFixed(2));
    }
    s.bigH = s.big.offsetHeight;
    s.left.style.setProperty("--big-h", s.bigH + "px");
  }

  // The paragraph in focus sits in the middle of its panel; one too tall for
  // that moves from its top to its bottom as its stop is scrolled through.
  function centre(s, scrubbing) {
    var stop = s.stops[Math.max(0, s.current)];
    var step = s.steps[stop.step].el;
    var room = stop.panel, h = step.offsetHeight;
    if (stop.top == null) s.stage.style.removeProperty("--ev-top-h");
    else s.stage.style.setProperty("--ev-top-h", stop.top + "px");
    var y = room / 2 - (step.offsetTop + h / 2);
    var first = s.stops.filter(function (x) { return x.step === stop.step; })[0];
    var through = stop !== first ? 1
      : first.scrub ? Math.max(0, Math.min(1, ((s.pos || 0) - first.lead) / first.scrub)) : 0;
    if (first.tall) y = MARGIN - step.offsetTop - through * first.travel;
    // A block taller than its window goes up through it with the scroll.
    Object.keys(s.blocks).forEach(function (id) {
      var block = s.blocks[id];
      if (!block.code) return;
      var up = block === stop.block && stop === first && stop.over ? through * stop.over : 0;
      block.code.classList.toggle("is-scrub", !!scrubbing);
      block.code.style.transform = up ? "translateY(" + (-up).toFixed(1) + "px)" : "";
    });
    // While a tall paragraph is being scrolled through it follows the scroll
    // directly, not on the glide used between stops.
    s.reel.classList.toggle("is-scrub", !!scrubbing);
    s.reel.style.transform = "translateY(" + y.toFixed(1) + "px)";
  }

  function apply(s, index, animate) {
    var stop = s.stops[index];
    var far = s.current < 0 || Math.abs(index - s.current) > 2 || !animate;
    s.current = index;
    s.stage.classList.toggle("is-jump", far);

    var before = s.steps.filter(function (step) { return step.el.classList.contains("is-active"); })[0];
    s.steps.forEach(function (step, i) {
      var near = Math.abs(i - stop.step) === 1;
      step.el.classList.toggle("is-active", i === stop.step);
      step.el.classList.toggle("is-near", near && !step.ghost);
      if (step.ghost) {
        // The paragraph leaving focus hands over to its ghost at full
        // strength, and the ghost dims.
        step.ghost.classList.remove("was-active");
        if (near && step === before && !far) {
          void step.ghost.offsetWidth;
          step.ghost.classList.add("was-active");
        }
        step.ghost.classList.toggle("is-near", near);
      }
      step.marks.forEach(function (mark, j) {
        mark.classList.toggle("is-active", i === stop.step && j === stop.trigger);
        mark.classList.toggle("is-passed", i === stop.step && j < stop.trigger);
      });
    });
    centre(s);

    // The subheading this paragraph is under, unless it is the paragraph
    // the subheading stands over.
    var under = s.steps[stop.step].sub;
    if (under) {
      s.sub.firstChild.setAttribute("data-text", under.n);
      s.sub.lastChild.setAttribute("data-text", under.title);
      s.subhead.children[1].setAttribute("data-text", "Section " + under.n);
      s.subhead.lastChild.setAttribute("data-text", under.title);
    }
    s.sub.classList.toggle("is-on", !!under && !under.first);
    s.subhead.classList.toggle("is-on", !!under);
    var named = under ? under.title : s.title;
    s.label.firstChild.textContent = under ? under.n : s.number;
    if (FINDABLE) s.label.lastChild.setAttribute("data-text", named);
    else s.label.lastChild.textContent = named;

    s.stage.classList.toggle("is-code", !!stop.block);
    // A block coming back at a new step is first seen as it was left, or its
    // change would be over before the block had faded in.
    var back = !!stop.block && !stop.block.el.classList.contains("is-on") &&
      !!stop.block.shown && stop.frame !== stop.block.shown;
    Object.keys(s.blocks).forEach(function (id) {
      var shown = s.blocks[id] === stop.block;
      s.blocks[id].el.classList.toggle("is-on", shown);
      // A block that has gone comes back at its left edge.
      if (!shown && s.blocks[id].view) s.blocks[id].view.scrollLeft = 0;
    });
    if (stop.block) {
      // An expression is highlighted first, and then the block follows.
      if (!far && (stop.trigger >= 0 || back)) {
        setTimeout(function () {
          if (s.current === index) stop.block.show(stop.frame, true);
        }, stop.trigger >= 0 ? TRIGGER_LEAD : RETURN_LEAD);
      } else {
        stop.block.show(stop.frame, !far);
      }
      var count = stop.block.frames.length;
      s.stage.classList.toggle("has-steps", count > 1);
      s.meterText.textContent = "Step " + stop.frame + " / " + count;
      while (s.meterBar.children.length < count) s.meterBar.appendChild(el("i"));
      while (s.meterBar.children.length > count) s.meterBar.lastChild.remove();
      Array.prototype.forEach.call(s.meterBar.children, function (seg, i) {
        seg.classList.toggle("is-on", i < stop.frame);
      });
    }
    Array.prototype.forEach.call(s.rail.children, function (dot, i) {
      dot.classList.toggle("is-past", i < index);
      dot.classList.toggle("is-current", i === index);
    });
    tidy(s);
    if (far) {
      void s.stage.offsetWidth;
      s.stage.classList.remove("is-jump");
    }
  }

  // The stop a section is at, from how far its track has gone under the
  // stage's sticking point.
  function where(s) {
    var pos = Math.max(0, Math.min(s.total - 1, s.topPx - s.section.getBoundingClientRect().top));
    var index = 0;
    s.stops.forEach(function (stop, i) { if (stop.at <= pos) index = i; });
    s.pos = pos - s.stops[index].at;
    return index;
  }

  var ticking = false;
  var reading = null; // the section whose stage is pinned
  function update() {
    ticking = false;
    if (!built.length) return;
    var at = "";
    reading = null;
    built.forEach(function (s, i) {
      var index = where(s);
      if (index !== s.current) apply(s, index, true);
      else if (s.stops[index].scrub) centre(s, true);
      if (seen(s) !== s.onscreen) tidy(s);
      // The section whose stage is pinned is the one being read.
      if (pinned(s)) {
        at = (i + 1) + "." + (index + 1);
        reading = s;
      }
    });
    address(at);
    attend();
  }

  // Keep ?at= in the address in step with the scroll, so a reload or a
  // copied link comes back to the same stop. It waits for the scroll to
  // rest: browsers limit how often the address may be rewritten.
  var addressed = null, addressing = 0;
  function address(at) {
    if (at === addressed) return;
    addressed = at;
    clearTimeout(addressing);
    addressing = setTimeout(function () {
      var url = new URL(location.href);
      if (at) url.searchParams.set("at", at);
      else url.searchParams.delete("at");
      try { history.replaceState(history.state, "", url); } catch (e) {}
    }, 400);
  }
  function request() {
    if (!ticking) { ticking = true; window.requestAnimationFrame(update); }
  }
  // Lay out again, as when the fonts arrive or the window changes, and stay
  // on the stop being read: the lengths of the stops before it may change.
  // That is the stop read at the last scroll, not whichever section is under
  // the strip now: a window that has changed has moved the page already.
  function layout() {
    var held = null, s = reading;
    if (s && s.section.isConnected && s.current >= 0) {
      held = { s: s, stop: s.stops[s.current], part: s.pos / s.stops[s.current].len };
    }
    built.forEach(function (s) { fit(s); weigh(s); size(s); });
    if (held) {
      var top = held.s.section.getBoundingClientRect().top;
      var to = window.scrollY + top - held.s.topPx + held.stop.at + held.part * held.stop.len;
      if (Math.abs(to - window.scrollY) > 1) window.scrollTo(0, to);
      where(held.s);
    }
    built.forEach(function (s) { centre(s); tidy(s); });
    request();
  }

  function resized() {
    if (measure()) layout();
  }

  function go(sectionNumber, stopNumber) {
    var s = built[sectionNumber - 1];
    var stop = s && s.stops[stopNumber - 1];
    if (!stop) return;
    var top = s.section.getBoundingClientRect().top;
    window.scrollTo(0, window.scrollY + top - s.topPx + stop.at + (stop.scrub ? 4 : stop.len / 2));
    s.current = -1; // arrive without animating
    update();
  }

  // The dock, at the foot of the window while a stage is in it: a step back
  // and a step on through the stops, and two switches that give the whole
  // stage to the block (while there is one) or to the text.
  var dock = null, mode = "";
  function raise() {
    if (dock && dock.el.isConnected) return;
    dock = { el: el("div", "ev-dock") };
    dock.el.setAttribute("role", "toolbar");
    dock.el.setAttribute("aria-label", "Article controls");
    function button(name, label, path, act) {
      var b = el("button", "ev-dock-b");
      b.type = "button";
      b.title = label;
      b.setAttribute("aria-label", label);
      b.innerHTML = '<svg viewBox="0 0 20 20" aria-hidden="true"><path d="' + path + '"/></svg>';
      b.addEventListener("click", act);
      dock.el.appendChild(dock[name] = b);
    }
    button("back", "Step back", "M4 13l6-6 6 6", function () { step(-1); });
    button("on", "Step forward", "M4 7l6 6 6-6", function () { step(1); });
    dock.el.appendChild(el("i", "ev-dock-rule"));
    button("code", "Code, full screen", "M7 5l-5 5 5 5M13 5l5 5-5 5", function () { full("code"); });
    button("text", "Text, full screen", "M3 5h14M3 10h14M3 15h9", function () { full("text"); });
    document.body.appendChild(dock.el);
  }
  function lower() {
    if (dock) dock.el.remove();
    dock = null;
    mode = "";
    document.body.classList.remove("ev-full-code", "ev-full-text");
  }
  // Where a step in direction DIR (1 or -1) goes: the next stop of the
  // section being read, the nearest stop of the section beyond it, or, on
  // from the last stop of all, out of the section into what follows.
  function aim(dir) {
    var here = -1, found = null;
    built.forEach(function (s, i) { if (pinned(s)) here = i; });
    if (here >= 0) {
      var s = built[here], n = s.current + dir;
      if (n >= 0 && n < s.stops.length) return { i: here, n: n, near: true };
      var beyond = built[here + dir];
      if (beyond) return { i: here + dir, n: dir > 0 ? 0 : beyond.stops.length - 1 };
      return dir > 0 ? { i: here, out: true } : null;
    }
    built.forEach(function (s, i) {
      var box = s.section.getBoundingClientRect();
      if (dir > 0 ? !found && box.top > s.topPx + 1 : box.bottom <= s.topPx + 1) {
        found = { i: i, n: dir > 0 ? 0 : s.stops.length - 1 };
      }
    });
    return found;
  }
  function step(dir) {
    var to = aim(dir);
    if (!to) return;
    var s = built[to.i], box = s.section.getBoundingClientRect();
    if (to.out) window.scrollTo(0, window.scrollY + box.bottom - s.topPx);
    else if (!to.near) go(to.i + 1, to.n + 1);
    else {
      // Within a section the page jumps and the stage makes the change.
      var stop = s.stops[to.n];
      window.scrollTo(0, window.scrollY + box.top - s.topPx + stop.at + (stop.scrub ? 4 : stop.len / 2));
      update();
    }
  }
  // Give the whole stage to the block ("code") or the text ("text"), or
  // give it back. Everything is measured again for the room it now has.
  function full(which) {
    mode = mode === which ? "" : which;
    built.forEach(function (s) { s.stage.classList.add("is-jump"); });
    document.body.classList.toggle("ev-full-code", mode === "code");
    document.body.classList.toggle("ev-full-text", mode === "text");
    layout();
    void document.body.offsetWidth;
    built.forEach(function (s) { s.stage.classList.remove("is-jump"); });
    attend();
  }
  function attend() {
    if (!dock) return;
    // On while a stage shows under the strip.
    dock.el.classList.toggle("is-on", built.some(function (s) {
      var box = s.stage.getBoundingClientRect();
      return box.bottom > s.topPx + 1 && box.top < window.innerHeight;
    }));
    dock.back.disabled = !aim(-1);
    dock.on.disabled = !aim(1);
    dock.code.setAttribute("aria-pressed", mode === "code");
    dock.text.setAttribute("aria-pressed", mode === "text");
  }

  // Build the sections of the page that has just arrived. Sections of a page
  // that has gone are dropped.
  var started = false;
  function init() {
    built = built.filter(function (s) { return s.section.isConnected; });
    var fresh = [];
    Array.prototype.forEach.call(document.querySelectorAll("section.ev"), function (section, index) {
      if (!section.querySelector(".ev-stage")) fresh.push(build(section, index));
    });
    built = built.concat(fresh);
    addressed = null;
    if (built.length) raise();
    else lower();
    if (!started) {
      started = true;
      listen();
      window.addEventListener("scroll", request, { passive: true });
      window.addEventListener("resize", resized);
      window.addEventListener("load", layout);
      document.addEventListener("keydown", function (e) {
        if (e.key === "Escape" && mode) full(mode);
      });
      // A page without this script can take this one's place: drop what was
      // built for it.
      document.addEventListener("htmx:after:swap", init);
    }
    if (!fresh.length) return;
    view.w = 0;
    measure();
    fresh.forEach(function (s) { fit(s); weigh(s); size(s); apply(s, where(s), false); watch(s); });
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(layout);
    // The display face can arrive after all of that, and the headings are
    // fitted to it: lay out again whenever a heading's box changes.
    if (window.ResizeObserver) {
      fresh.forEach(function (s) {
        new ResizeObserver(layout).observe(s.big);
      });
    }
    // ?at=2.5 opens the page at section 2, stop 5.
    var at = /[?&]at=(\d+)\.(\d+)/.exec(location.search);
    if (at) go(+at[1], +at[2]);
  }

  window.MagazineEvolving = { init: init, layout: layout, go: go, power: power,
                              sections: function () { return built; } };
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init);
  else init();
})();
