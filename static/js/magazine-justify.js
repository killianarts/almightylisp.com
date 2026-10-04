// Justified text: article paragraphs, the lead's dek and the book panel's
// pitch.
//
// The browser justifies a paragraph a line at a time, taking as many words
// as fit and stretching the spaces to fill, so one line comes out tight and
// the next full of holes. Here the breaks are chosen for the paragraph as a
// whole (as TeX does), so the spaces come out as even as they can.
//
// Pretext (pretext-0.0.9.min.js) measures the words, on a canvas, without
// touching the page. The breaks are then written into the paragraph: each
// line's text is wrapped in a span that can't wrap (.just-line), leaving
// the space between two lines as the only place to break. The browser does
// the stretching itself (text-align: justify, on .just), so the right edge
// is exact whatever the measuring was off by. A line that has to be set
// tight gets a negative word-spacing, since the browser only stretches.
//
// Links, emphasis and code stay as they are: only their text is wrapped.
// Without JavaScript, in a narrow column, or where a paragraph can't be
// handled, the browser justifies the text itself, hyphenating as it goes
// (.prose in magazine.css).
(function () {
  "use strict";

  var P = window.Pretext;
  if (!P || !window.ResizeObserver || !window.WeakMap) return;

  var SELECTOR = ".prose, .book-ad-pitch p, .neo-dek";

  // Narrower than this many spaces of its own font, a column has too few
  // words a line to justify well without hyphenating, and is left to the
  // browser, which can (article text on a phone is about 85 wide; beside
  // the lead, the book panel's pitch is about 100).
  var MIN_SPACES = 90;

  // How far a line's spaces may shrink, and the stretch that costs as much,
  // as fractions of a space.
  var SHRINK = 0.2;
  var STRETCH = 0.5;

  // Costs, beside a line's own (1 for a line at its natural width, about
  // 10,000 for one at the limit of SHRINK or STRETCH).
  var LONE_WORD = 1e9;     // a line of one word can't be justified
  var WIDOW = 3000;        // a last line of one word

  // Taken off a tight line's spaces (px), so it's sure to fit.
  var SLACK = 0.02;

  var BOX = 0, SPACE = 1, DEAD = 2;

  var widths = new WeakMap();   // paragraph -> the width it was set to
  var spaces = {};              // font -> width of a space
  var observer = new ResizeObserver(resized);

  // ------------------------------------------------------------ measuring

  function px(value) {
    return parseFloat(value) || 0;
  }

  function fontOf(el) {
    var cs = getComputedStyle(el);
    return {
      font: cs.fontStyle + " " + cs.fontWeight + " " + cs.fontSize + " " + cs.fontFamily,
      options: { letterSpacing: px(cs.letterSpacing) }
    };
  }

  function spaceWidth(f) {
    var key = f.font + "|" + f.options.letterSpacing;
    if (!(key in spaces)) {
      spaces[key] = P.prepareWithSegments("a b", f.font, f.options).widths[1];
    }
    return spaces[key];
  }

  // The paragraph's text as a list of items: boxes (text that stays
  // together), spaces, and dead spaces (ones the browser collapses away).
  // Each remembers where it is in its text node. Null if the paragraph
  // holds anything but text in plain inline elements.
  function collect(p) {
    var items = [];
    var ok = true;

    function space(node, start, f) {
      var last = items[items.length - 1];
      var dead = !last || last.kind !== BOX;
      items.push({ kind: dead ? DEAD : SPACE, node: node, start: start, end: start + 1,
                   width: dead ? 0 : spaceWidth(f) });
    }

    function text(node, f) {
      var data = node.data;
      var at = 0;
      if (data.charAt(0) === " ") {
        space(node, 0, f);
        at = 1;
      }
      var prepared = P.prepareWithSegments(data, f.font, f.options);
      prepared.segments.forEach(function (segment, i) {
        if (data.substr(at, segment.length) !== segment) {
          ok = false;
        } else if (segment === " ") {
          space(node, at, f);
        } else {
          items.push({ kind: BOX, node: node, start: at, end: at + segment.length,
                       width: prepared.widths[i] });
        }
        at += segment.length;
      });
      if (at === data.length - 1 && data.charAt(at) === " ") space(node, at, f);
      else if (at !== data.length) ok = false;
    }

    (function walk(el) {
      var f = fontOf(el);
      for (var child = el.firstChild; ok && child; child = child.nextSibling) {
        if (child.nodeType === 3) {
          if (child.data) text(child, f);
        } else if (child.nodeType === 1) {
          var cs = getComputedStyle(child);
          // A line break, an image, a box of its own, or padding that the
          // measuring wouldn't know about.
          if (!child.firstChild || cs.display !== "inline" ||
              px(cs.paddingLeft) || px(cs.paddingRight) ||
              px(cs.borderLeftWidth) || px(cs.borderRightWidth) ||
              px(cs.marginLeft) || px(cs.marginRight)) {
            ok = false;
          } else {
            walk(child);
          }
        } else if (child.nodeType !== 8) {
          ok = false;
        }
      }
    })(p);

    return ok ? items : null;
  }

  // Words: runs of boxes, each with the space that follows it.
  function wordsOf(items) {
    var words = [];
    var word = null;
    items.forEach(function (item, i) {
      if (item.kind === BOX) {
        if (!word || word.space) {
          word = { first: i, last: i, width: 0, space: 0 };
          words.push(word);
        }
        word.last = i;
        word.width += item.width;
      } else if (item.kind === SPACE) {
        word.space = item.width;
      }
    });
    return words;
  }

  // ------------------------------------------------------------- breaking

  // Where to break WORDS into lines of WIDTH, at the least total cost: the
  // index of each line's first word. Null if a word is wider than a line.
  function breaks(words, width) {
    var n = words.length;
    // before[k]: the width of everything ahead of word k, spaces included.
    var before = [0];
    var gaps = [0];
    var i, j;
    for (i = 0; i < n; i++) {
      if (words[i].width > width) return null;
      before.push(before[i] + words[i].width + words[i].space);
      gaps.push(gaps[i] + words[i].space);
    }

    // best[j]: the least cost of setting words 0..j-1 as whole lines;
    // from[j]: the first word of the last of them.
    var best = [0];
    var from = [0];
    for (j = 1; j <= n; j++) {
      best[j] = Infinity;
      var last = j === n;
      for (i = j - 1; i >= 0; i--) {
        var natural = before[j] - before[i] - words[j - 1].space;
        var room = gaps[j] - gaps[i] - words[j - 1].space;
        // More words only make it longer.
        if (natural - room * SHRINK > width) break;
        var cost = lineCost(width - natural, room, last, j - i) + best[i];
        if (cost < best[j]) {
          best[j] = cost;
          from[j] = i;
        }
      }
    }

    var starts = [];
    for (j = n; j > 0; j = from[j]) starts.unshift(from[j]);
    return starts;
  }

  // The cost of a line with SPARE px to fill (negative: to lose) and ROOM
  // px of spaces to do it with.
  function lineCost(spare, room, last, count) {
    if (last && spare >= 0) return count === 1 ? WIDOW : 0;
    if (!room) return LONE_WORD + spare * spare;
    var ratio = spare / room;
    var badness = 100 * Math.pow(Math.abs(ratio) / (ratio < 0 ? SHRINK : STRETCH), 3);
    return (1 + badness) * (1 + badness);
  }

  // ------------------------------------------------------------- the page

  // Back to the paragraph as it was written.
  function reset(p) {
    var lines = p.querySelectorAll(".just-line");
    if (!lines.length) return;
    Array.prototype.forEach.call(lines, function (line) {
      line.replaceWith.apply(line, Array.prototype.slice.call(line.childNodes));
    });
    p.normalize();
    p.classList.remove("just");
  }

  // Line breaks and runs of spaces in the source: one space each, as the
  // browser shows them, so the measured text is the text in the node.
  function tidy(p) {
    var walker = document.createTreeWalker(p, NodeFilter.SHOW_TEXT);
    var node;
    while ((node = walker.nextNode())) {
      if (/[\t\n\r\f]| {2}/.test(node.data)) {
        node.data = node.data.replace(/[ \t\n\r\f]+/g, " ");
      }
    }
  }

  // The spans to make for P: the piece of each text node in each line, last
  // first, so that splitting one node leaves the offsets before it alone.
  // Null where the paragraph is left to the browser.
  function plan(p) {
    var cs = getComputedStyle(p);
    var width = p.getBoundingClientRect().width -
      px(cs.paddingLeft) - px(cs.paddingRight) -
      px(cs.borderLeftWidth) - px(cs.borderRightWidth);
    widths.set(p, width);
    if (width < MIN_SPACES * spaceWidth(fontOf(p))) return null;

    var items = collect(p);
    if (!items) return null;
    var words = wordsOf(items);
    var starts = words.length && breaks(words, width);
    if (!starts || starts.length < 2) return null;

    var spans = [];
    starts.forEach(function (start, l) {
      var end = l + 1 < starts.length ? starts[l + 1] : words.length;
      var natural = 0;
      var count = end - start - 1;
      for (var w = start; w < end; w++) {
        natural += words[w].width + (w < end - 1 ? words[w].space : 0);
      }
      var tight = natural > width && count
        ? ((width - natural) / count - SLACK).toFixed(3) + "px"
        : "";
      var span = null;
      for (var i = words[start].first; i <= words[end - 1].last; i++) {
        var item = items[i];
        if (span && span.node === item.node) {
          span.end = item.end;
        } else {
          span = { node: item.node, start: item.start, end: item.end, tight: tight };
          spans.push(span);
        }
      }
    });
    return spans.reverse();
  }

  function apply(p, spans) {
    if (!spans) return;
    spans.forEach(function (span) {
      var node = span.node;
      if (span.end < node.length) node.splitText(span.end);
      if (span.start > 0) node = node.splitText(span.start);
      var line = document.createElement("span");
      line.className = "just-line";
      if (span.tight) line.style.wordSpacing = span.tight;
      node.replaceWith(line);
      line.appendChild(node);
    });
    p.classList.add("just");
  }

  // All the writing, then all the reading, then all the writing: the page
  // is laid out once, however many paragraphs there are.
  function justify(paragraphs) {
    paragraphs.forEach(function (p) {
      reset(p);
      tidy(p);
    });
    var plans = paragraphs.map(plan);
    paragraphs.forEach(function (p, i) { apply(p, plans[i]); });
  }

  function all() {
    return Array.prototype.slice.call(document.querySelectorAll(SELECTOR));
  }

  // Columns change width with the window, and the book panel comes and
  // goes. The work waits for the next frame: setting a paragraph changes
  // its height, which the observer would have to report in the same turn.
  function resized(entries) {
    var changed = entries.filter(function (entry) {
      return Math.abs(entry.contentRect.width - widths.get(entry.target)) > 0.5;
    }).map(function (entry) { return entry.target; });
    if (!changed.length) return;
    window.requestAnimationFrame(function () {
      justify(changed.filter(function (p) { return p.isConnected; }));
    });
  }

  function init() {
    var paragraphs = all();
    observer.disconnect();
    justify(paragraphs);
    paragraphs.forEach(function (p) { observer.observe(p); });
  }

  // The fonts changed: everything measured so far is wrong.
  function refresh() {
    P.clearCache();
    spaces = {};
    justify(all());
  }

  // Pages arrive by htmx swap as well as by normal loads, and come back
  // from htmx's history with their line spans still in them.
  document.addEventListener("htmx:afterSettle", init);
  document.addEventListener("htmx:historyRestore", init);
  // Fonts the browser loads when the page first needs them (see
  // magazine-fonts.js, which calls refresh itself for the ones it holds
  // back).
  if (document.fonts && document.fonts.addEventListener) {
    document.fonts.addEventListener("loadingdone", refresh);
  }

  window.MagazineJustify = { init: init, refresh: refresh };
  init();
})();
