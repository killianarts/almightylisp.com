// Design variants for the magazine, for the comparison page beside this file
// (index.html). The page loads a live magazine page in a frame, adds
// variants.css and this script to it, and sets window.__design to one of
// "v30" to "v34", or to "current" for the site as it is. window.__book, when
// set, moves the homepage's book panel out of the plate into a band of its
// own under it, with any of them: "band" is a low strip, "brass" is set
// after brasshands.com and "agento" after Creative Mints' Agento project.
//
// variants.css does what CSS can. This adds what it can't: it sets a
// section's text and notes in rows, one per subheading; builds the headings'
// eyebrows and rules; and wraps the lead's headline for v31 and v32. The
// server's markup is otherwise left as it is, so a variant that is kept can
// be moved into the templates a piece at a time.

(function () {
  var design = window.__design;
  if (!design) return;

  // v33 and v34 are v31 and v32 with the dek and byline set to match the
  // lead. LEAD is the lead a design has; variants.css keys its lead rules on
  // it, and the dek and byline rules on data-design-meta.
  var LEADS = { v31: "v31", v32: "v32", v33: "v31", v34: "v32" };
  var META = { v33: "panels", v34: "grid" };
  var lead = LEADS[design] || null;

  var root = document.documentElement;
  var restyled = design !== "current";
  if (restyled) {
    root.setAttribute("data-design", lead || design);
    root.setAttribute("data-design-base", "v30");
    if (META[design]) root.setAttribute("data-design-meta", META[design]);
  }
  var BANDS = { band: "strip", brass: "brass", agento: "agento", landing: "landing" };
  // window.__theme restyles the whole page ("landing": a light page with
  // black heads, after the Figma frame "Homepage restyled
  // · C1 · Landing"); window.__footer restyles the footer alone ("agento").
  if (window.__theme) root.setAttribute("data-design-theme", window.__theme);
  if (window.__footer) root.setAttribute("data-design-footer", window.__footer);
  var bandStyle = BANDS[window.__book] || null;
  if (bandStyle) {
    root.setAttribute("data-design-book", "band");
    root.setAttribute("data-design-band", bandStyle);
  }

  function el(tag, cls, text) {
    var node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text != null) node.textContent = text;
    return node;
  }

  function two(n) { return (n < 10 ? "0" : "") + n; }

  function sq(open) { return el("i", open ? "v-sq o" : "v-sq"); }

  function dots() {
    var d = el("span", "v-dots");
    d.setAttribute("aria-hidden", "true");
    d.appendChild(el("i"));
    d.appendChild(el("i"));
    return d;
  }

  // Filled to FRACTION, a tick, then the remainder. WIDTH fixes its length.
  function bar(fraction, width) {
    var b = el("span", "v-bar" + (fraction >= 1 ? " full" : "") + (width ? " fixed" : ""));
    b.setAttribute("aria-hidden", "true");
    b.style.setProperty("--p", String(Math.max(0, Math.min(1, fraction))));
    if (width) b.style.setProperty("--w", width + "px");
    b.appendChild(el("i", "done"));
    b.appendChild(el("i", "tick"));
    if (fraction < 1) b.appendChild(el("i", "rest"));
    return b;
  }

  function arrow(label) { return el("span", "v-arrow", label); }

  function eyebrow(text, marks) {
    var p = el("p", "v-eyebrow");
    if (marks) p.appendChild(sq());
    p.appendChild(el("span", null, text));
    if (marks) { p.appendChild(sq(true)); p.appendChild(sq()); }
    return p;
  }

  // ---- Article sections

  // The notes in a section's margin are copies of the notes set inline in
  // its text (shown there on narrow pages), in the same order. That is how
  // each margin note finds the subsection it belongs to.
  function sections() {
    var pieces = Array.prototype.slice.call(document.querySelectorAll(".neo-piece"));
    pieces.forEach(function (piece, index) {
      var row = piece.querySelector(".piece-row");
      if (!row || row.classList.contains("v-rows")) return;
      var head = row.querySelector(".piece-head");
      var text = row.querySelector(".piece-text");
      var margin = row.querySelector(".piece-margin");
      if (!head || !text || !margin) return;

      var n = index + 1;
      var name = head.querySelector(".piece-name");
      var notes = Array.prototype.slice.call(margin.querySelectorAll(".margin-note"));

      var h2 = el("header", "v-h2");
      var ghost = el("span", "v-ghost", two(n));
      ghost.setAttribute("aria-hidden", "true");
      h2.appendChild(ghost);
      h2.appendChild(eyebrow("Section " + two(n) + " / " + two(pieces.length), true));
      if (name) h2.appendChild(name);
      var rule = el("div", "v-rule");
      rule.appendChild(bar(n / pieces.length));
      rule.appendChild(dots());
      h2.appendChild(rule);

      // Split the text at each subheading.
      var groups = [[]];
      Array.prototype.slice.call(text.children).forEach(function (child) {
        if (child.matches("h3.subhead")) groups.push([]);
        groups[groups.length - 1].push(child);
      });
      if (!groups[0].length && groups.length > 1) groups.shift();

      var noteIndex = 0;
      var sub = 0;
      var frag = document.createDocumentFragment();
      groups.forEach(function (members, g) {
        var cell = el("div", "piece-cell piece-text");
        var side = el("div", "piece-cell piece-margin" + (g === 0 ? " v-first" : ""));
        if (g === 0) cell.appendChild(h2);
        members.forEach(function (child) {
          if (child.matches("h3.subhead")) {
            sub += 1;
            var h3 = el("div", "v-h3");
            h3.appendChild(el("span", "v-dash"));
            h3.appendChild(eyebrow("Section " + n + "." + sub));
            h3.appendChild(child);
            cell.appendChild(h3);
          } else {
            cell.appendChild(child);
            if (child.matches(".inline-note") && notes[noteIndex]) {
              side.appendChild(notes[noteIndex]);
              noteIndex += 1;
            }
          }
        });
        var hatch = el("span", "hatch-fill");
        hatch.setAttribute("aria-hidden", "true");
        side.appendChild(hatch);
        frag.appendChild(cell);
        frag.appendChild(side);
      });
      // Notes without an inline copy stay with the last group.
      var last = frag.lastChild;
      notes.slice(noteIndex).forEach(function (note) { last.insertBefore(note, last.lastChild); });

      row.textContent = "";
      row.appendChild(frag);
      row.classList.add("v-rows");
      measure(row, h2);
    });
  }

  // The first notes cell starts level with the text under the heading.
  var watcher = window.ResizeObserver ? new ResizeObserver(function (entries) {
    entries.forEach(function (entry) {
      var row = entry.target.closest(".piece-row");
      if (row) row.style.setProperty("--v-h2", entry.target.offsetHeight + "px");
    });
  }) : null;

  function measure(row, h2) {
    row.style.setProperty("--v-h2", h2.offsetHeight + "px");
    if (watcher) watcher.observe(h2);
  }

  // ---- Homepage cards

  function cards() {
    // "MAC  ■  Featured" reads "MAC / Featured" as an eyebrow.
    Array.prototype.forEach.call(document.querySelectorAll(".series-bar > span:last-child"), function (span) {
      if (span.dataset.v) return;
      span.dataset.v = "1";
      span.textContent = span.textContent.replace(/\s*■\s*/g, " / ");
    });

    Array.prototype.forEach.call(document.querySelectorAll(".series-progress"), function (p) {
      var meter = p.querySelector(".meter");
      if (!meter || p.querySelector(".v-dots")) return;
      meter.insertAdjacentElement("afterend", dots());
    });

    Array.prototype.forEach.call(document.querySelectorAll(".series-strip"), function (strip) {
      var span = strip.querySelector("span:last-child");
      if (!span || strip.querySelector(".v-bar")) return;
      var part = /(\d+)\s*\/\s*(\d+)/.exec(span.textContent);
      span.textContent = "Series / " + span.textContent.replace(/\s*■\s*/g, " / ");
      strip.appendChild(bar(part ? Number(part[1]) / Number(part[2]) : 1));
    });

    Array.prototype.forEach.call(document.querySelectorAll(".featured-banner"), function (banner) {
      if (banner.querySelector(".v-rule")) return;
      var count = banner.querySelector(".featured-count");
      var meter = banner.querySelector(".meter");
      var label = banner.querySelector(".featured-label");
      var part = count && /(\d+)/.exec(count.textContent);
      if (part) banner.setAttribute("data-part", two(Number(part[1])));
      if (label) label.textContent = label.textContent.replace(/\s*■\s*/g, " / ");
      if (count) count.textContent = "Part " + count.textContent;
      var rule = el("div", "v-rule");
      if (meter) rule.appendChild(meter);
      rule.appendChild(dots());
      rule.appendChild(arrow("All parts"));
      banner.appendChild(rule);
    });
  }

  // ---- The lead (v31, v32)

  // What the lead says about its article: the series and part when it is in
  // one, otherwise its serial.
  function leadFacts(lead) {
    var facts = { eyebrow: "Latest brief", line: "", label: "", part: 0, total: 0 };
    var serial = lead.querySelector(".byline-spec .spec-row:nth-child(2) dd");
    if (serial) { facts.line = serial.textContent; facts.label = serial.textContent; }

    var tag = lead.querySelector(".series-tag.outline");
    var filled = lead.querySelector(".series-tag.filled");
    var banner = lead.parentElement && lead.parentElement.querySelector(".featured-banner");
    var m;
    if (tag && filled) {
      m = /(\d+)\D+(\d+)/.exec(filled.textContent);
      var side = document.querySelector(".art-head-band > .side-label.right");
      var code = side ? side.textContent.split("■")[1] : "";
      facts.eyebrow = "Series" + (code && code.trim() ? " / " + code.trim() : "");
      facts.line = tag.textContent.replace(/^\[\s*Series:\s*/i, "").replace(/\s*\]$/, "");
    } else if (banner) {
      var count = banner.querySelector(".featured-count");
      var label = banner.querySelector(".featured-label");
      var title = banner.querySelector(".featured-title");
      m = count && /(\d+)\s*\/\s*(\d+)/.exec(count.textContent);
      facts.eyebrow = label ? label.textContent.replace(/featured\s*/i, "").replace(/\s*■\s*/g, " / ") : "Series";
      if (title) facts.line = title.textContent;
    }
    if (m) {
      facts.part = Number(m[1]);
      facts.total = Number(m[2]);
      facts.label = "Part " + facts.part + " of " + facts.total;
    }
    return facts;
  }

  function leads() {
    if (!lead) return;
    Array.prototype.forEach.call(document.querySelectorAll(".neo-lead"), function (panel) {
      var headline = panel.querySelector(".neo-headline");
      if (!headline || panel.querySelector(".v-title")) return;
      var facts = leadFacts(panel);
      var fraction = facts.total ? facts.part / facts.total : 1;

      var title = el("div", "v-title");
      var scale = headline.style.getPropertyValue("--headline-scale");
      if (scale) title.style.setProperty("--headline-scale", scale);
      headline.parentNode.insertBefore(title, headline);
      title.appendChild(headline);

      // v32: a slash, the part number and an asterisk, set after the title.
      var orn = null;
      if (lead === "v32") {
        orn = el("span", "v-orn", "/");
        orn.setAttribute("aria-hidden", "true");
        if (facts.part) orn.appendChild(el("small", null, two(facts.part)));
        orn.appendChild(document.createTextNode(" *"));
      }

      var foot = el("div", "v-title-foot");
      var num = el("span", "v-num", facts.part ? two(facts.part) : "");
      num.setAttribute("aria-hidden", "true");
      var meta = el("div", "v-meta");
      if (lead === "v32") meta.appendChild(arrow("Details"));
      meta.appendChild(el("p", null, (lead === "v31" ? "■ " : "") + facts.eyebrow));
      if (facts.line) meta.appendChild(el("p", null, facts.line));
      if (lead === "v31") meta.appendChild(arrow("Details"));
      if (facts.part) foot.appendChild(num);
      foot.appendChild(meta);
      title.appendChild(foot);

      var rule = el("div", "v-rule");
      if (lead === "v31") {
        rule.appendChild(bar(fraction));
        rule.appendChild(dots());
        rule.appendChild(dots());
        rule.appendChild(arrow(facts.label));
      } else {
        rule.appendChild(bar(0.5, 150));
        rule.appendChild(dots());
        rule.appendChild(el("span", "v-space"));
        rule.appendChild(arrow(facts.label));
        rule.appendChild(dots());
        rule.appendChild(bar(fraction, 300));
      }
      title.insertAdjacentElement("afterend", rule);

      if (orn) {
        ornaments.push({ orn: orn, headline: headline, foot: foot });
        placeOrnament(ornaments[ornaments.length - 1]);
      }
      if (META[design] === "grid") {
        grids.push({ panel: panel, headline: headline });
        sizeGrid(grids[grids.length - 1]);
      }
    });
  }

  // v34 draws its grid across the whole lead, cut to the headline's lines:
  // the cell is one line of the headline, which CSS can't read from here.
  var grids = [];

  function sizeGrid(item) {
    var size = parseFloat(window.getComputedStyle(item.headline).fontSize);
    if (size) item.panel.style.setProperty("--v-cell", (size * 0.84).toFixed(2) + "px");
  }

  function sizeGrids() {
    grids = grids.filter(function (item) { return item.panel.isConnected; });
    grids.forEach(sizeGrid);
  }

  window.addEventListener("resize", sizeGrids);

  // The ornaments follow the title's last word and take no width (see
  // .v-orn), so the title breaks where it does without them. When the last
  // line leaves them no room, they go under the title instead.
  var ornaments = [];

  function placeOrnament(item) {
    if (!item.headline.isConnected) return;
    var target = item.headline.querySelector("a") || item.headline;
    item.orn.classList.remove("in-foot");
    target.appendChild(item.orn);
    var edge = item.headline.getBoundingClientRect().right;
    var start = item.orn.getBoundingClientRect().left;
    if (start + item.orn.scrollWidth > edge) {
      item.orn.classList.add("in-foot");
      item.foot.insertBefore(item.orn, item.foot.firstChild);
    }
  }

  function placeOrnaments() {
    ornaments = ornaments.filter(function (item) { return item.headline.isConnected; });
    ornaments.forEach(placeOrnament);
  }

  window.addEventListener("resize", placeOrnaments);
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(placeOrnaments);
  if (document.fonts && document.fonts.addEventListener) {
    document.fonts.addEventListener("loadingdone", placeOrnaments);
  }

  // ---- The book band

  // The book panel leaves the plate for a band of its own under it. The
  // plate's right-hand gutter label names the book, so it goes too.
  function book() {
    if (!bandStyle) return;
    var plate = document.querySelector(".plate-band");
    var ad = plate && plate.querySelector(".neo-plate > .book-ad");
    if (!ad) return;
    var band = el("div", "band book-band");
    band.appendChild(ad);
    if (bandStyle === "brass") brass(ad);
    if (bandStyle === "agento") agento(ad);
    if (bandStyle === "landing") landing(ad);
    if (window.__figma) {
      // The two Figma designs: a gap band between the plate and the book,
      // and in the Agento band a button for the ebook, at the footer's
      // address, between the other two.
      root.setAttribute("data-design-gap", "book");
      var gap = hidden(el("div", "band gap-band book-gap"));
      plate.insertAdjacentElement("afterend", gap);
      var hard = ad.querySelector(".book-ad-actions .neo-button.secondary");
      var ebookLink = Array.prototype.filter.call(document.querySelectorAll(".footer-book a"), function (a) {
        return /ebook/i.test(a.textContent);
      })[0];
      if (bandStyle === "agento" && hard && ebookLink) {
        var eb = hard.cloneNode(true);
        eb.href = ebookLink.href;
        eb.textContent = "Buy ebook";
        hard.textContent = "Buy hardcover";
        hard.insertAdjacentElement("beforebegin", eb);
      }
    }
    // Short labels: a gutter label is as long as its text, and the band
    // can't be lower than that.
    var left = el("span", "side-label left", "THE BOOK");
    left.setAttribute("aria-hidden", "true");
    band.appendChild(left);
    var right = plate.querySelector(".side-label.right");
    if (right) {
      right.textContent = "FLIGHT MANUAL";
      band.appendChild(right);
    }
    (plate.nextElementSibling && plate.nextElementSibling.classList.contains("book-gap")
      ? plate.nextElementSibling : plate).insertAdjacentElement("afterend", band);
  }

  function hidden(node) { node.setAttribute("aria-hidden", "true"); return node; }

  // After brasshands.com: a ticker across the top, boxed status tags, and a
  // heavy rule to close.
  function brass(ad) {
    var ticker = hidden(el("div", "bb-ticker"));
    var items = ["Lisp & Emacs Essentials: deployed", "Free online: [16] chapters", "Hardcover: in print"];
    for (var i = 0; i < 9; i += 1) {
      ticker.appendChild(el("span", null, items[i % items.length]));
      ticker.appendChild(el("i", null, "/"));
    }
    ad.insertBefore(ticker, ad.firstChild);
    var tags = hidden(el("div", "bb-tags"));
    tags.appendChild(el("span", "bb-slashes", "///////"));
    tags.appendChild(el("span", "bb-tag", "Status: online"));
    tags.appendChild(el("span", "bb-tag on", "Reading"));
    ad.appendChild(tags);
    ad.appendChild(hidden(el("div", "bb-rule")));
  }

  // After Creative Mints' Agento project: a square grid with a hatched
  // block, a ghost numeral, an eyebrow with its squares, and a gauge with
  // dot pairs under the pitch.
  function agento(ad) {
    function pair(cls) {
      var d = hidden(el("span", "ba-dots" + (cls ? " " + cls : "")));
      d.appendChild(el("i"));
      d.appendChild(el("i"));
      return d;
    }
    ad.appendChild(hidden(el("div", "ba-hatch")));
    ad.appendChild(hidden(el("div", "ba-ghost", "16")));
    var squares = hidden(el("div", "ba-squares"));
    squares.appendChild(el("i"));
    squares.appendChild(el("i"));
    ad.appendChild(squares);
    var rule = hidden(el("div", "ba-rule"));
    var gauge = el("span", "ba-gauge");
    gauge.appendChild(el("i", "done"));
    gauge.appendChild(el("i", "tick"));
    gauge.appendChild(el("i", "rest"));
    rule.appendChild(gauge);
    rule.appendChild(pair());
    rule.appendChild(pair("alt"));
    ad.appendChild(rule);
    var corner = hidden(el("div", "ba-corner"));
    corner.appendChild(pair());
    corner.appendChild(pair("alt"));
    ad.appendChild(corner);
  }

  function gauge() {
    var g = hidden(el("span", "vl-gauge"));
    g.appendChild(el("i", "done"));
    g.appendChild(el("i", "tick"));
    g.appendChild(el("i", "rest"));
    return g;
  }

  function pair(alt) {
    var d = hidden(el("span", "vl-dots" + (alt ? " alt" : "")));
    d.appendChild(el("i"));
    d.appendChild(el("i"));
    return d;
  }

  // A card's pulse: what a link in an article does every so often
  // (the link pulse in magazine-fx.js), on a whole card. That pulse is made
  // from a link's shape, which for a card would be a flat block; this one
  // splits the card's own colours. The red, green and blue of the card
  // drift apart (the swell), then at their widest a few bands of it slide
  // sideways and it settles in steps.
  var PULSE_SPLIT = 8, PULSE_TEAR = 24, PULSE_MS = 1500, PULSE_SWELL = 0.65, PULSE_EVERY = 8000;
  var pulseFilter = null;

  function pulseParts() {
    if (pulseFilter) return pulseFilter;
    var ns = "http://www.w3.org/2000/svg";
    var svg = document.createElementNS(ns, "svg");
    svg.setAttribute("aria-hidden", "true");
    svg.setAttribute("width", "0");
    svg.setAttribute("height", "0");
    svg.style.position = "absolute";
    function channel(row, result) {
      var rows = ["0 0 0 0 0", "0 0 0 0 0", "0 0 0 0 0"];
      rows[row] = ["1 0 0 0 0", "0 1 0 0 0", "0 0 1 0 0"][row];
      return '<feColorMatrix in="torn" type="matrix" values="' + rows.join("  ") + '  0 0 0 1 0" result="' + result + '"/>';
    }
    svg.innerHTML =
      '<filter id="vl-card-fx" x="-10%" y="-12%" width="120%" height="124%" color-interpolation-filters="sRGB">' +
        // Noise that only changes down the card, so it moves in bands; most
        // of it is flattened to "stay put" (0.5), the rest pushed apart.
        '<feTurbulence type="fractalNoise" baseFrequency="0.0001 0.018" numOctaves="1" seed="1" result="noise"/>' +
        '<feColorMatrix in="noise" type="matrix" values="2.4 0 0 0 -0.7  0 0 0 0 0.5  0 0 0 0 0  0 0 0 0 1" result="spread"/>' +
        '<feComponentTransfer in="spread" result="map"><feFuncR type="discrete" tableValues="0.5 0 0.5 0.5 0.8 0.5 0.5 0.2 0.5 1 0.5"/></feComponentTransfer>' +
        '<feDisplacementMap in="SourceGraphic" in2="map" scale="0" xChannelSelector="R" yChannelSelector="G" result="torn"/>' +
        channel(0, "r") + channel(1, "g") + channel(2, "b") +
        '<feOffset in="r" dx="0" dy="0" result="ro"/>' +
        '<feOffset in="g" dx="0" dy="0" result="go"/>' +
        '<feOffset in="b" dx="0" dy="0" result="bo"/>' +
        '<feBlend in="ro" in2="go" mode="screen" result="rg"/>' +
        '<feBlend in="rg" in2="bo" mode="screen"/>' +
      "</filter>";
    root.appendChild(svg);
    return (pulseFilter = {
      noise: svg.querySelector("feTurbulence"),
      displace: svg.querySelector("feDisplacementMap"),
      offsets: svg.querySelectorAll("feOffset")
    });
  }

  function cardPulse(card) {
    var due = performance.now() + 2500;
    // The first use of a filter costs the most: it is made, the card gets a
    // layer, and the noise is drawn for the first time. So that happens
    // now, where it can't be seen: the filter is made, and run for a few
    // frames with nothing moved. The card keeps its layer (will-change).
    var warm = pulseParts();
    card.style.willChange = "filter";
    card.style.filter = "url(#vl-card-fx)";
    var frames = 0;
    (function idle() {
      frames += 1;
      if (frames === 2) warm.displace.setAttribute("scale", "0.01");
      if (frames < 4) { requestAnimationFrame(idle); return; }
      warm.displace.setAttribute("scale", "0");
      card.style.filter = "";
    })();
    function play(start) {
      var f = pulseParts();
      var dir = Math.random() < 0.5 ? -1 : 1;
      var frame = 0;
      card.style.filter = "url(#vl-card-fx)";
      (function step(now) {
        var t = (now - start) / PULSE_MS;
        if (t >= 1 || !card.isConnected) {
          f.displace.setAttribute("scale", "0");
          card.style.filter = "";
          due = performance.now() + PULSE_EVERY * (0.6 + Math.random() * 0.8);
          return;
        }
        var split, tear = 0;
        if (t < PULSE_SWELL) {
          var u = t / PULSE_SWELL;
          split = PULSE_SPLIT * u * u;
        } else {
          // Let go in six steps, each with its own bands.
          var k = Math.floor((t - PULSE_SWELL) / (1 - PULSE_SWELL) * 6);
          var left = 1 - k / 6;
          split = PULSE_SPLIT * left;
          tear = PULSE_TEAR * left;
          if (k !== frame) { frame = k; f.noise.setAttribute("seed", String(1 + Math.floor(Math.random() * 90))); }
        }
        f.offsets[0].setAttribute("dx", (dir * split).toFixed(2));
        f.offsets[1].setAttribute("dy", (split * 0.35).toFixed(2));
        f.offsets[2].setAttribute("dx", (-dir * split).toFixed(2));
        f.displace.setAttribute("scale", tear.toFixed(1));
        requestAnimationFrame(step);
      })(start);
    }
    setInterval(function () {
      if (!card.isConnected || card.style.filter) return;
      var r = card.getBoundingClientRect();
      var seen = r.width > 0 && r.bottom > 0 && r.top < window.innerHeight;
      var now = performance.now();
      if (seen && now >= due) play(now);
    }, 500);
  }

  // The Landing band: the pitch as a headline over a gauge, and three ways
  // to read as cards. The ebook's address is the footer's.
  function landing(ad) {
    var pitch = ad.querySelector(".book-ad-pitch");
    var actions = ad.querySelector(".book-ad-actions");
    var read = ad.querySelector(".neo-button.primary");
    var hard = ad.querySelector(".neo-button.secondary");
    var ebook = Array.prototype.filter.call(document.querySelectorAll(".footer-book a"), function (a) {
      return /ebook/i.test(a.textContent);
    })[0];
    var rule = hidden(el("div", "vl-rule"));
    rule.appendChild(gauge());
    rule.appendChild(pair());
    rule.appendChild(pair(true));
    if (pitch) pitch.insertAdjacentElement("afterend", rule);
    var corner = hidden(el("div", "vl-corner"));
    corner.appendChild(pair());
    corner.appendChild(pair(true));
    ad.appendChild(corner);
    // Beside the pitch, under the logo: a ring of dots and a slider.
    var side = hidden(el("div", "vl-side"));
    var mark = el("span", "vl-mark");
    for (var k = 0; k < 6; k += 1) {
      var dot = el("i");
      dot.style.left = (14.5 + 13 * Math.cos(k * Math.PI / 3)).toFixed(1) + "px";
      dot.style.top = (14.5 + 13 * Math.sin(k * Math.PI / 3)).toFixed(1) + "px";
      mark.appendChild(dot);
    }
    side.appendChild(mark);
    var slider = el("span", "vl-slider");
    ["a", "b", "c", "d"].forEach(function (c) { slider.appendChild(el("i", c)); });
    side.appendChild(slider);
    ad.appendChild(side);

    var cards = el("div", "vl-cards");
    [
      { a: read, meta: "Free", title: "Read free online", sub: "All 16 chapters, in your browser." },
      { a: ebook, meta: "PDF / $5", title: "Buy the ebook", sub: "All 16 chapters, to keep and read offline." },
      { a: hard, meta: "Print / $50", title: "Buy the hardcover", sub: "The whole book, on your desk." }
    ].forEach(function (c, i) {
      if (!c.a) return;
      // The hardcover's card is fainter, and the whole of it pulses (see
      // cardPulse).
      var faint = i === 2;
      var card = el("a", "vl-card" + (i === 0 ? " on" : "") + (faint ? " faint" : ""));
      card.href = c.a.href;
      if (faint) cardPulse(card);
      // The corner icon, as drawn in the Figma file: a 3 by 3 grid of dots.
      // The first card has all nine, every other one dimmed; the second has
      // the four edge dots, dimmed; the third the corners and the centre.
      var icon = hidden(el("span", "vl-card-icon"));
      for (var r = 0; r < 3; r += 1) {
        for (var q = 0; q < 3; q += 1) {
          if (i !== 0 && (r + q + i) % 2 !== 0) continue;
          var d = el("i", (r + q) % 2 ? "dim" : null);
          d.style.left = q * 6 + "px";
          d.style.top = r * 6 + "px";
          icon.appendChild(d);
        }
      }
      card.appendChild(icon);
      card.appendChild(el("span", "vl-card-meta", c.meta));
      card.appendChild(el("span", "vl-card-title", c.title));
      card.appendChild(el("span", "vl-card-sub", c.sub));
      card.appendChild(hidden(el("span", "vl-card-arrow", "\u2192")));
      cards.appendChild(card);
    });
    if (actions) actions.replaceWith(cards); else ad.appendChild(cards);
  }

  // The Landing theme's furniture: a gauge and dot pairs in each section
  // head and in the footer's cells.
  function furniture() {
    if (window.__theme !== "landing") return;
    Array.prototype.forEach.call(document.querySelectorAll(".head-band > .section-head"), function (head) {
      if (head.querySelector(".vl-headrule")) return;
      var r = hidden(el("span", "vl-headrule"));
      r.appendChild(gauge());
      r.appendChild(pair());
      r.appendChild(pair(true));
      // It stands where the head's meta line was.
      var meta = head.querySelector(".section-meta");
      if (meta) meta.insertAdjacentElement("afterend", r); else head.appendChild(r);
    });
    // In the gutters beside each band: dot pairs at the top, a gauge at the
    // foot. An article's sections get them too.
    var bands = document.querySelectorAll(
      ".band.plate-band, .band.art-head-band, .band.book-band, .band.head-band, .band.end-band, .band.footer-band");
    Array.prototype.forEach.call(bands, function (band, i) {
      if (band.querySelector(":scope > .vl-m")) return;
      var top = !band.classList.contains("art-head-band");
      if (top) {
        var l = pair(i % 2 === 1); l.className += " vl-m left"; band.appendChild(l);
        var r = pair(i % 2 === 0); r.className += " vl-m right"; band.appendChild(r);
      }
      var g = gauge(); g.className += " vl-m foot"; band.appendChild(g);
    });
    Array.prototype.forEach.call(document.querySelectorAll(".neo-piece"), function (piece, i) {
      if (piece.querySelector(":scope > .vl-p")) return;
      var l = pair(i % 2 === 1); l.className += " vl-p left"; piece.appendChild(l);
      var r = pair(i % 2 === 0); r.className += " vl-p right"; piece.appendChild(r);
      var g = gauge(); g.className += " vl-p foot"; piece.appendChild(g);
    });
    // A gauge over the footer's book buttons.
    var actions = document.querySelector(".footer-book .footer-actions");
    if (actions && !actions.parentNode.querySelector(".vl-footgauge")) {
      var fg = gauge(); fg.className += " vl-footgauge";
      actions.insertAdjacentElement("beforebegin", fg);
    }
    Array.prototype.forEach.call(document.querySelectorAll(".footer-colophon, .footer-block"), function (cell) {
      if (cell.querySelector(".vl-corner")) return;
      var c = hidden(el("span", "vl-corner"));
      c.appendChild(pair());
      c.appendChild(pair(true));
      cell.appendChild(c);
    });
  }

  // Shorter labels for the footer's buttons, so they fit a narrow cell.
  function footerLabels() {
    if (!window.__footer && window.__theme !== "landing") return;
    // The colophon's motto, under the wordmark.
    var colophon = document.querySelector(".footer-colophon");
    if (colophon && !colophon.querySelector(".vl-motto")) {
      var motto = el("span", "vl-motto jp", "\u5FC3\u81D3\u3092\u6367\u3052\u3088\u3046");
      motto.lang = "ja";
      colophon.appendChild(motto);
    }
    var short = { "purchase ebook": "Buy ebook", "purchase hardcover": "Buy hardcover", "read free online": "Read free online" };
    Array.prototype.forEach.call(document.querySelectorAll(".site-footer .neo-button"), function (a) {
      var next = short[a.textContent.trim().toLowerCase()];
      if (next && a.textContent !== next) a.textContent = next;
    });
  }

  function apply() {
    book();
    furniture();
    footerLabels();
    if (restyled) {
      sections();
      cards();
      leads();
    }
    // New paragraphs and new widths: set the justified text again.
    if (window.MagazineJustify) window.MagazineJustify.init();
  }

  window.__applyDesign = apply;
  apply();
  // htmx swaps the body when a magazine link is followed.
  document.addEventListener("htmx:afterSettle", apply);
  document.addEventListener("htmx:historyRestore", apply);
})();
