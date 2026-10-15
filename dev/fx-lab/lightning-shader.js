// Effects test page variant: lightning, drawn by a shader.
//
// As lightning.js (the click, an article link's pulse and the hardcover
// card's pulse, as lightning: white, with a blue glow), but drawn by a
// fragment shader, not stroked on 2D canvases: the bolts have a soft glow,
// taper to their ends, and the flash is brightest where the bolt struck.
// One WebGL canvas over the window does all three (see "lightning" below).
//
// The click is one large bolt across the window, flashing two or three
// times; a link's pulse runs round the edges of its letters. The window
// flashes three times in a click at most: more than three flashes in a second is the
// line for photosensitive viewers.
//
// The live file's page filter, WebGL overlay and card filter are still
// here, unused by the three; the burst (lines of code) is as it is live.
(function () {
  var link = document.createElement("link");
  link.rel = "stylesheet";
  link.href = "/fx-lab/lightning-shader.css";
  document.head.appendChild(link);
})();

// Magazine screen effects.
//
// Hover on a book button, or one of the book band's cards: a quick CRT
// flicker over the whole window (CSS, .fx-flicker in magazine.css).
//
// Click on a book button or card, an article link or a link to another site: the
// page splits into its colour
// channels and tears sideways (an SVG filter on the visible part of .sheet,
// or on the search palette when that is open over it; in Safari, which
// can't run it fast enough, copies of the page instead, and the palette
// goes out of sight as the effect starts),
// while a WebGL overlay adds tear bars, scanlines and noise and then switches
// the screen off like a CRT. The next page settles in with a shorter version
// of the same thing. The shorter version also plays when the fonts finish
// loading after the page has been drawn (see magazine-fonts.js).
//
// Magazine links are boosted by htmx: the page is fetched while the effect
// plays, and swapped in once the screen is off. Other boosted navigations
// (the nameplate, the archive) use a view transition instead. The book
// buttons and links to other sites leave the magazine, so they do a normal
// page load after the effect. This file sets htmx up for all of that.
//
// A link in the text, to an article or to another site: every ten seconds
// or so, a small version of the click's first stage, on the link alone: its
// colours swell apart, then let go in a split and a tear. It marks the link
// as one, and as one that plays the effect, without a hover (a phone has
// none). The hardcover's card in the book band (.book-card.faint) pulses
// the same way, the whole card.
//
// The effect's own elements hang off <html>, not <body>, so htmx never swaps
// them out.
//
// Everything runs only while an effect is playing, but for a twice-a-second
// check for links due a pulse. The effects play whatever the reduced-motion
// setting: they are part of the site's look.
(function () {
  "use strict";

  // Leaving: a hit of colour split, then the picture tears apart, then the
  // CRT switch-off. HIT and BREAK are where each stage ends, as fractions.
  var LEAVE_MS = 650;
  var HIT = 0.28;
  var BREAK = 0.71;
  var ARRIVE_MS = 260;
  var ARRIVE_KEY = "almighty-fx-arrive";
  var SHEET = [229 / 255, 229 / 255, 229 / 255];

  var sheet = null;       // the sheet being torn by the current effect
  var over = null;        // and the search palette, when it is open over it
  var playing = false;
  var run = 0;            // bumps on every effect, so an old frame loop stops
  var htmx = window.htmx;
  var root = document.documentElement;

  // ---------------------------------------------------------------- links

  // The book's buttons, and the cards of the homepage's book band.
  function isBookButton(a) {
    return a.classList.contains("neo-button") || a.classList.contains("book-card");
  }

  // The magazine is the site's root. These are the parts of the site that
  // aren't the magazine: the other modules mounted in src/main.lisp, and the
  // static files. (The magazine's articles are under /article.)
  var OTHER_SECTIONS = /^\/(admin|book|author|static)(\/|$)/;

  function isMagazinePath(path) {
    return !OTHER_SECTIONS.test(path);
  }

  function isArticleLink(a) {
    if (a.origin !== location.origin) return false;
    return /^\/article\/[a-z0-9-]+\/?$/.test(a.pathname);
  }

  // Another site's page. Not mail or phone links, which leave nothing.
  function isOutsideLink(a) {
    return /^https?:$/.test(a.protocol) && a.origin !== location.origin;
  }

  // The links that play the effect when clicked.
  function playsEffect(a) {
    return isBookButton(a) || isArticleLink(a) || isOutsideLink(a);
  }

  function isPlainClick(e, a) {
    return e.button === 0 && !e.metaKey && !e.ctrlKey && !e.shiftKey && !e.altKey &&
           (!a.target || a.target === "_self") && !a.hasAttribute("download");
  }

  // Same page with only a different #fragment: not a navigation.
  function isSamePage(a) {
    return a.origin === location.origin && a.pathname === location.pathname &&
           a.search === location.search;
  }

  // Get ahead of the navigation that follows the effect. Pages on this site
  // are prefetched. Other sites (the hardcover on Amazon) can't be: the
  // browser keeps each site's cache apart, so a copy fetched from here
  // wouldn't be used there. For those, open the connection early instead:
  // DNS, TCP and TLS are done by the time the effect ends.
  var prefetched = {};
  function addHint(rel, href) {
    var link = document.createElement("link");
    link.rel = rel;
    link.href = href;
    document.head.appendChild(link);
  }
  function prefetch(a) {
    var key = a.origin === location.origin ? a.href : a.origin;
    if (prefetched[key]) return;
    prefetched[key] = true;
    if (a.origin === location.origin) {
      addHint("prefetch", a.href);
    } else {
      addHint("dns-prefetch", a.origin);
      addHint("preconnect", a.origin);
    }
  }

  // ---------------------------------------------------------------- hover

  var flicker = null;
  var lastFlicker = 0;

  function hoverFlicker() {
    var now = performance.now();
    if (now - lastFlicker < 600 || playing) return;
    lastFlicker = now;
    if (!flicker) {
      flicker = document.createElement("div");
      flicker.className = "fx-flicker";
      flicker.setAttribute("aria-hidden", "true");
      flicker.addEventListener("animationend", function () {
        flicker.classList.remove("is-on");
      });
      root.appendChild(flicker);
    }
    flicker.classList.remove("is-on");
    void flicker.offsetWidth; // restart the animation
    flicker.classList.add("is-on");
  }

  // ------------------------------------------------- page filter (SVG)

  // Displacement strips: a few columns of random horizontal bands, made
  // once. Swapping between them each frame makes the tears jump. A strip is
  // ROWS high, its bands up to BAND rows, and SHARE of them are torn.
  function makeStrips(count, rows, band, share) {
    var urls = [];
    var canvas = document.createElement("canvas");
    canvas.width = 2;
    canvas.height = rows;
    var ctx = canvas.getContext("2d");
    for (var s = 0; s < count; s++) {
      var y = 0;
      while (y < canvas.height) {
        var h = 1 + Math.floor(Math.random() * band);
        var torn = Math.random() < share;
        var dx = torn ? Math.floor(Math.random() * 255) : 128;
        ctx.fillStyle = "rgb(" + dx + ",128,128)";
        ctx.fillRect(0, y, 2, h);
        y += h;
      }
      urls.push(canvas.toDataURL());
    }
    return urls;
  }

  // Tear maps: the same idea, but violent. Each row of the map either leaves
  // the page alone, throws a slice hundreds of pixels sideways, smears it
  // into a streak (a gradient across the row), or slips a block up or down.
  // Pixels pulled from off screen come out empty, so black shows through.
  function makeTearMaps(count) {
    var urls = [];
    var W = 160, H = 120;
    var canvas = document.createElement("canvas");
    canvas.width = W;
    canvas.height = H;
    var ctx = canvas.getContext("2d");
    for (var m = 0; m < count; m++) {
      var y = 0;
      while (y < H) {
        var h = 1 + Math.floor(Math.random() * 6);
        var kind = Math.random();
        ctx.fillStyle = "rgb(128,128,0)";
        ctx.fillRect(0, y, W, h);
        if (kind < 0.45) {
          // untouched
        } else if (kind < 0.68) {
          var r = Math.random() < 0.5 ? Math.random() * 50 : 205 + Math.random() * 50;
          ctx.fillStyle = "rgb(" + Math.round(r) + ",128,0)";
          ctx.fillRect(0, y, W, h);
        } else if (kind < 0.82) {
          var g = ctx.createLinearGradient(0, 0, W, 0);
          var a = Math.round(Math.random() * 255), b = Math.round(Math.random() * 255);
          g.addColorStop(0, "rgb(" + a + ",128,0)");
          g.addColorStop(1, "rgb(" + b + ",128,0)");
          ctx.fillStyle = g;
          ctx.fillRect(0, y, W, h);
        } else {
          var x = Math.random() * W, w = W * (0.15 + Math.random() * 0.35);
          var dx = Math.round(Math.random() * 255), dy = Math.random() < 0.5 ? 50 : 206;
          ctx.fillStyle = "rgb(" + dx + "," + dy + ",0)";
          ctx.fillRect(x, y, w, h + 2);
        }
        y += h;
      }
      urls.push(canvas.toDataURL());
    }
    return urls;
  }

  // WebKit (Safari, and every browser on iOS) runs SVG filters on the CPU,
  // at full resolution, whenever they change. Measured in Safari at
  // 1440x900 on a Retina screen, the filter below takes the effect from
  // about 55fps to 2-10fps, with stalls of up to 1.7s. There the page
  // doesn't go through the filter at all: the split is three copies of it
  // (see buildSplit) and the tear is more, in bands (see buildTear), all
  // painted once and then only moved.
  //
  // Phones and tablets take the copies too, whatever their browser: the
  // filter runs there, but not smoothly. They are told apart by having
  // touch and no hover, not by the size of the window.
  //
  // The effects test page can ask for either with ?fx-path=filter or
  // ?fx-path=copies, whatever the browser (only on this machine).
  var forcePath = /^(localhost|127\.0\.0\.1)$/.test(location.hostname) &&
    /[?&]fx-path=(filter|copies)\b/.exec(location.search);
  var HANDHELD = window.matchMedia("(hover: none) and (pointer: coarse)").matches;
  var SVG_FILTER = forcePath ? forcePath[1] === "filter"
                             : navigator.vendor !== "Apple Computer, Inc." && !HANDHELD;
  if (!SVG_FILTER) root.classList.add("fx-lite");

  // Before the tear, the bands (below) hold for at least this many frames.
  var LITE_EVERY = 3;
  var liteFrame = -LITE_EVERY;

  function liteDue(frame) {
    if (frame - liteFrame < LITE_EVERY) return false;
    liteFrame = frame;
    return true;
  }

  // The filter is on one element at a time: the sheet, or the search
  // palette when that is open, which covers nearly all of the sheet. A
  // filter on each, both the size of the window, is twice the work and
  // felt slow; the sheet behind the palette only shakes.
  var svg = null, pageFilter = null, strips, tearMaps;

  function ensureFilter() {
    if (svg || !SVG_FILTER) return;
    strips = makeStrips(6, 96, 9, 0.3);
    tearMaps = makeTearMaps(8);
    var ns = "http://www.w3.org/2000/svg";
    svg = document.createElementNS(ns, "svg");
    svg.setAttribute("aria-hidden", "true");
    svg.setAttribute("width", "0");
    svg.setAttribute("height", "0");
    svg.style.position = "absolute";
    svg.innerHTML =
      '<filter id="fx-aberration" filterUnits="userSpaceOnUse" primitiveUnits="userSpaceOnUse" color-interpolation-filters="sRGB">' +
        '<feImage preserveAspectRatio="none" result="map"/>' +
        '<feDisplacementMap in="SourceGraphic" in2="map" scale="0" xChannelSelector="R" yChannelSelector="G" result="torn"/>' +
        '<feColorMatrix in="torn" type="matrix" values="1 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0" result="r"/>' +
        '<feOffset in="r" dx="0" dy="0" result="rs"/>' +
        '<feColorMatrix in="torn" type="matrix" values="0 0 0 0 0  0 1 0 0 0  0 0 0 0 0  0 0 0 1 0" result="g"/>' +
        '<feOffset in="g" dx="0" dy="0" result="gs"/>' +
        '<feColorMatrix in="torn" type="matrix" values="0 0 0 0 0  0 0 0 0 0  0 0 1 0 0  0 0 0 1 0" result="b"/>' +
        '<feOffset in="b" dx="0" dy="0" result="bs"/>' +
        '<feBlend in="rs" in2="gs" mode="screen" result="rg"/>' +
        '<feBlend in="rg" in2="bs" mode="screen"/>' +
      "</filter>";
    root.appendChild(svg);
    var el = svg.querySelector("filter"), offsets = el.querySelectorAll("feOffset");
    pageFilter = { el: el, image: el.querySelector("feImage"), displace: el.querySelector("feDisplacementMap"),
               r: offsets[0], g: offsets[1], b: offsets[2] };
  }

  // Set NAME to VALUE on PART of the filter.
  function setPart(part, name, value) {
    pageFilter[part].setAttribute(name, value);
  }

  // Limit the filter to the part of the page on screen: filtering the
  // whole article would cost a lot more and show nothing extra. The window
  // is placed from where the filtered element is.
  function placeFilter() {
    if (!SVG_FILTER) return;
    var box = (over || sheet).getBoundingClientRect();
    [pageFilter.el, pageFilter.image].forEach(function (part) {
      part.setAttribute("x", -box.left);
      part.setAttribute("y", -box.top);
      part.setAttribute("width", window.innerWidth);
      part.setAttribute("height", window.innerHeight);
    });
  }

  // WebKit's split: three copies of the page on screen, each tinted to one
  // of red, green and blue and added back together (plus-lighter), so
  // moving them apart splits the page as the filter does, colours mixing
  // where they overlap. They're painted once, as they're built, and then
  // only moved, which Safari does at full frame rate. Safari mixes colours
  // in the screen's own colour space, where the sRGB and P3 primaries
  // aren't pure (the copies came out magenta and maroon, and didn't add
  // back up to the page); Rec. 2020's are beyond the screen's gamut, so
  // they come out as its own pure primaries. The tint is inside each copy
  // (see .fx-rgb in magazine.css), and nothing in a copy may get a layer of
  // its own, or the tint mixes with everything under it.
  var RGB_PAD = 80;     // px each copy reaches past the window, for the shake
  var rgbLayer = null, rgbCopies = [];
  var rgbOffsets = [[0, 0], [0, 0], [0, 0]];
  var shake = "";

  function buildSplit() {
    removeSplit();
    sheet.style.transform = "";
    var box = sheet.getBoundingClientRect();
    var bg = getComputedStyle(document.body).backgroundColor;
    // .mag for the page's tokens and base styles.
    rgbLayer = document.createElement("div");
    rgbLayer.className = "mag fx-rgb";
    rgbLayer.setAttribute("aria-hidden", "true");
    ["r", "g", "b"].forEach(function (channel) {
      var holder = document.createElement("div");
      holder.className = "fx-rgb-copy " + channel;
      holder.style.cssText = "inset:" + -RGB_PAD + "px;background-color:" + bg;
      holder.appendChild(copySheet(box.left + RGB_PAD, box.top + RGB_PAD, box.width));
      rgbLayer.appendChild(holder);
      rgbCopies.push(holder);
    });
    root.appendChild(rgbLayer);
  }

  function removeSplit() {
    if (rgbLayer) rgbLayer.remove();
    rgbLayer = null;
    rgbCopies = [];
    rgbOffsets = [[0, 0], [0, 0], [0, 0]];
    shake = "";
  }

  // The copies' offsets, plus the shake.
  function placeCopies() {
    rgbCopies.forEach(function (copy, i) {
      copy.style.transform = "translate3d(" + rgbOffsets[i][0].toFixed(1) + "px," +
        rgbOffsets[i][1].toFixed(1) + "px,0) " + shake;
    });
  }

  // Move the red, green and blue images apart: the filter's offsets, or
  // (WebKit) the copies.
  // Each image also moves along splitSlope, so they part at an angle; RY
  // and BY add a little jitter to that.
  function setSplit(rx, ry, gx, bx, by) {
    ry += rx * splitSlope;
    by += bx * splitSlope;
    var gy = gx * splitSlope;
    if (SVG_FILTER) {
      setPart("r", "dx", rx.toFixed(1));
      setPart("r", "dy", ry.toFixed(1));
      setPart("g", "dx", gx.toFixed(1));
      setPart("g", "dy", gy.toFixed(1));
      setPart("b", "dx", bx.toFixed(1));
      setPart("b", "dy", by.toFixed(1));
      return;
    }
    rgbOffsets = [[rx, ry], [gx, gy], [bx, by]];
  }

  // Shake the page: the sheet and the search palette over it, or (WebKit)
  // the copies over the sheet. In WebKit the palette is out of sight for
  // the effect (.fx-lite .search.fx-torn in magazine.css): a copy of it in
  // every copy of the page made the effect too slow to use, and so did
  // leaving it over them.
  function setShake(x, y, skew) {
    var t = "translate3d(" + x.toFixed(1) + "px," + y.toFixed(1) + "px,0) skewX(" + skew.toFixed(2) + "deg)";
    if (SVG_FILTER) {
      sheet.style.transform = t;
      if (over) over.style.transform = t;
    } else {
      shake = t;
      placeCopies();
    }
  }

  // Peak strengths, reached on the first frame and eased out from there.
  var SPLIT = 30;     // px between the red and blue images
  var TEAR = 150;     // px the torn bands can slide
  var JITTER = 14;    // px the whole page shakes
  var SKEW = 1.5;     // degrees
  var splitDir = 1;   // which way red goes; fixed for one effect
  var splitSlope = 0; // and at what angle: tan of 15-35 degrees, up or down

  function setFilter(amount, frame) {
    var wobble = 0.8 + Math.random() * 0.4;
    var split = amount * SPLIT * wobble;
    if (SVG_FILTER) {
      setPart("image", "href", strips[frame % strips.length]);
      setPart("displace", "scale", (amount * TEAR * (0.6 + Math.random() * 0.8)).toFixed(1));
    }
    if (!SVG_FILTER && liteDue(frame)) setBands(0.35, amount * TEAR, 0);
    setSplit(splitDir * split, amount * (Math.random() * 6 - 3), splitDir * split * 0.12,
             -splitDir * split * 0.85, amount * (Math.random() * 4 - 2));
    setShake(amount * JITTER * (Math.random() * 2 - 1), 0, amount * SKEW * (Math.random() * 2 - 1));
  }

  // The page coming apart. K runs from about 0.55 to 1 over the tear.
  function setTear(k, frame) {
    if (SVG_FILTER) {
      setPart("image", "href", tearMaps[Math.floor(Math.random() * tearMaps.length)]);
      setPart("displace", "scale", Math.round(k * (300 + Math.random() * 500)));
    }
    if (!SVG_FILTER) setBands(0.55, k * (300 + Math.random() * 500), 30 * k);
    var dir = Math.random() < 0.5 ? -1 : 1;
    var split = 8 + Math.random() * 36 * k;
    setSplit(dir * split, Math.random() * 8 - 4, (Math.random() * 2 - 1) * 6 * k,
             -dir * split * 0.9, Math.random() * 8 - 4);
    // now and then the picture loses vertical hold
    setShake((Math.random() * 2 - 1) * 40 * k,
             Math.random() < 0.25 ? (Math.random() * 2 - 1) * 28 * k : 0,
             (Math.random() * 2 - 1) * 3 * k);
  }

  // WebKit's tear: copies of the page, each seen through a band of the
  // window, that slide sideways. A band is wider than the window by PAD on
  // each side, black past the page, so sliding it opens a black gap as the
  // filter's tear does. A band is only ever moved, never redrawn, and one
  // that's out of the tear is moved off screen, not hidden: in Safari,
  // moving bands to new rows cost half the frames, and hiding and showing
  // them (opacity, clip-path) cost 40-60ms a time, where moving them costs
  // next to nothing. Each is a whole copy of the page for Safari to lay
  // out (about 4ms), so they're added a few a frame (growTear): all twelve
  // at once made the first frame 70-100ms.
  var TEAR_BANDS = 12;
  var TEAR_PER_FRAME = 3;
  var TEAR_PAD = 480;   // px; also the furthest a band slides
  var tearLayer = null, tearBands = [], tearBox = null;

  function buildTear() {
    removeTear();
    sheet.style.transform = "";
    tearBox = sheet.getBoundingClientRect();
    // .mag for the page's tokens and base styles; see .fx-tear in magazine.css.
    tearLayer = document.createElement("div");
    tearLayer.className = "mag fx-tear";
    tearLayer.setAttribute("aria-hidden", "true");
    root.appendChild(tearLayer);
  }

  function growTear() {
    if (!tearLayer) return;
    var h = window.innerHeight;
    for (var i = 0; i < TEAR_PER_FRAME && tearBands.length < TEAR_BANDS; i++) {
      var bh = 6 + Math.random() * 60, y = Math.random() * (h - bh);
      var band = document.createElement("div");
      band.className = "fx-tear-band";
      band.style.cssText = "top:" + y.toFixed(0) + "px;height:" + bh.toFixed(0) + "px;" +
        "left:" + -TEAR_PAD + "px;width:calc(100% + " + 2 * TEAR_PAD + "px)";
      parkBand(band);
      band.appendChild(copySheet(tearBox.left + TEAR_PAD, tearBox.top - y, tearBox.width));
      tearLayer.appendChild(band);
      tearBands.push(band);
    }
  }

  // A copy of the sheet, at LEFT, TOP in its holder, drawn as the sheet is.
  function copySheet(left, top, width) {
    var copy = sheet.cloneNode(true);
    copy.removeAttribute("id");
    copy.classList.remove("fx-torn");
    Array.prototype.forEach.call(copy.querySelectorAll("[id]"), function (el) {
      el.removeAttribute("id");
    });
    copy.inert = true;
    copy.style.cssText = "position:absolute;margin:0;left:" + left + "px;top:" + top + "px;width:" + width + "px";
    return copy;
  }

  function moveBand(band, dx, dy) {
    band.style.transform = "translate3d(" + dx.toFixed(0) + "px," + dy.toFixed(0) + "px,0)";
  }

  // Off screen, to the right, by the band's own width and then some.
  function parkBand(band) {
    moveBand(band, window.innerWidth + TEAR_PAD + 8, 0);
  }

  // Tear SHARE of the bands, slid up to REACH px (and now and then SLIP px
  // up or down), and park the rest.
  function setBands(share, reach, slip) {
    if (!tearLayer) return;
    tearBands.forEach(function (band) {
      if (Math.random() >= share) { parkBand(band); return; }
      var dx = (Math.random() < 0.5 ? -1 : 1) * Math.min(TEAR_PAD, reach * (0.25 + 0.75 * Math.random()));
      var dy = Math.random() < 0.2 ? (Math.random() * 2 - 1) * slip : 0;
      moveBand(band, dx, dy);
    });
  }

  function removeTear() {
    if (tearLayer) tearLayer.remove();
    tearLayer = null;
    tearBands = [];
  }

  function clearFilter() {
    removeTear();
    removeSplit();
    if (over) {
      over.classList.remove("fx-torn");
      over.style.transform = "";
      over = null;
    }
    if (!sheet) return;
    sheet.classList.remove("fx-torn");
    sheet.style.transform = "";
    liteFrame = -LITE_EVERY;
  }

  // ---------------------------------------------------- overlay (WebGL)

  var canvas = null, gl = null, uni = {};

  var VERT = "attribute vec2 p; void main(){ gl_Position = vec4(p, 0.0, 1.0); }";

  // mode 0: leaving (glitch, then a CRT switch-off to the sheet colour).
  // mode 1: arriving (starts torn, settles).
  var FRAG = [
    "precision mediump float;",
    "uniform vec2 res; uniform float t; uniform float seed; uniform float mode; uniform vec3 sheet; uniform float tear;",
    "float hash(vec2 p){ return fract(sin(dot(p, vec2(127.1, 311.7)) + seed) * 43758.5453); }",
    // premultiplied 'over': lay colour C at alpha AL on top of D
    "vec4 over(vec4 d, vec3 c, float al){ return vec4(c * al + d.rgb * (1.0 - al), al + d.a * (1.0 - al)); }",
    "void main(){",
    "  vec2 uv = gl_FragCoord.xy / res;",
    "  float y = 1.0 - uv.y;",
    "  float tick = floor(t * 34.0);",
    // how hard it glitches: rises fast, holds, and (leaving) hands over to the switch-off
    "  float amt = mode < 0.5 ? smoothstep(0.0, 0.06, t) * (1.0 - smoothstep(0.70, 0.80, t)) : 1.0 - smoothstep(0.0, 1.0, t);",
    "  float rows = 18.0 + floor(hash(vec2(tick, 1.0)) * 36.0);",
    "  float row = floor(y * rows);",
    "  float r = hash(vec2(row, tick));",
    "  float torn = step(0.8, r);",
    "  float shift = (hash(vec2(row, tick + 7.0)) - 0.5) * 0.25 * amt;",
    "  float x = uv.x + shift;",
    "  float seg = step(0.45, hash(vec2(floor(x * (6.0 + r * 18.0)), row + tick)));",
    "  vec3 fringe = mix(vec3(1.0, 0.1, 0.25), vec3(0.0, 0.85, 1.0), step(0.5, fract(r * 13.0)));",
    "  float a = torn * seg * amt * 0.55;",
    "  vec3 col = fringe * a;",
    // scanlines and grain
    "  float scan = 0.5 + 0.5 * sin(gl_FragCoord.y * 1.6 + t * 90.0);",
    "  float grain = hash(gl_FragCoord.xy * 0.37 + tick);",
    "  float dark = ((1.0 - scan) * 0.14 + step(0.985, grain) * 0.5) * amt;",
    "  col = col * (1.0 - dark);",
    "  a = a + dark * (1.0 - a);",
    // the tear: static in broken blocks, bright seams, a rolling bar, flashes
    "  if (tear > 0.0) {",
    "    vec4 acc = vec4(col, a);",
    "    float bt = floor(t * 50.0);",
    "    vec2 cell = floor(vec2(uv.x * 24.0, y * 40.0));",
    "    float blk = step(1.0 - 0.10 * tear, hash(cell + bt));",
    "    acc = over(acc, vec3(hash(gl_FragCoord.xy + bt * 3.1)), blk * 0.95);",
    "    float seamRow = floor(y * 90.0);",
    "    float seam = step(0.965, hash(vec2(seamRow, bt))) * step(fract(y * 90.0), 0.14);",
    "    vec3 seamCol = mix(vec3(1.0), vec3(1.0, 0.2, 0.3), step(0.5, hash(vec2(seamRow, bt + 2.0))));",
    "    acc = over(acc, seamCol, seam * tear);",
    "    float roll = fract(t * 2.3 + seed);",
    "    float bar = smoothstep(0.0, 0.04, y - roll) * (1.0 - smoothstep(0.04, 0.12, y - roll));",
    "    acc = over(acc, vec3(0.0), bar * 0.5 * tear);",
    "    float flash = step(0.86, hash(vec2(bt, 5.0))) * tear;",
    "    acc = over(acc, vec3(1.0), flash * 0.35);",
    "    col = acc.rgb; a = acc.a;",
    "  }",
    // leaving: the picture collapses to a bright line, then to nothing
    "  if (mode < 0.5) {",
    "    float off = smoothstep(0.71, 1.0, t);",
    "    float band = mix(0.5, 0.0015, off);",
    "    float d = abs(y - 0.5);",
    "    float gone = step(band, d) * step(0.001, off);",
    "    float line = (1.0 - smoothstep(0.0, band * 2.0 + 0.002, d)) * off * (1.0 - smoothstep(0.93, 1.0, t));",
    "    vec3 c = mix(col, sheet, gone);",
    "    c = mix(c, vec3(1.0), line * 0.9);",
    "    float aa = max(max(a, gone), line * 0.9);",
    "    gl_FragColor = vec4(c, aa);",
    "  } else {",
    "    gl_FragColor = vec4(col, a);",
    "  }",
    "}"
  ].join("\n");

  function ensureGL() {
    if (canvas) return !!gl;
    canvas = document.createElement("canvas");
    canvas.className = "fx-canvas";
    canvas.setAttribute("aria-hidden", "true");
    root.appendChild(canvas);
    gl = canvas.getContext("webgl", { premultipliedAlpha: true, alpha: true, antialias: false }) ||
         canvas.getContext("experimental-webgl");
    if (!gl) return false;
    function shader(type, src) {
      var s = gl.createShader(type);
      gl.shaderSource(s, src);
      gl.compileShader(s);
      return s;
    }
    var prog = gl.createProgram();
    gl.attachShader(prog, shader(gl.VERTEX_SHADER, VERT));
    gl.attachShader(prog, shader(gl.FRAGMENT_SHADER, FRAG));
    gl.linkProgram(prog);
    if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) {
      console.warn("magazine-fx: overlay shader failed; the page effect still runs.",
                   gl.getProgramInfoLog(prog));
      gl = null;
      return false;
    }
    gl.useProgram(prog);
    var buf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buf);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 3, -1, -1, 3]), gl.STATIC_DRAW);
    var loc = gl.getAttribLocation(prog, "p");
    gl.enableVertexAttribArray(loc);
    gl.vertexAttribPointer(loc, 2, gl.FLOAT, false, 0, 0);
    ["res", "t", "seed", "mode", "sheet", "tear"].forEach(function (n) {
      uni[n] = gl.getUniformLocation(prog, n);
    });
    return true;
  }

  function sizeCanvas() {
    // Full resolution isn't needed for bands and grain; this keeps it cheap.
    var scale = Math.min(window.devicePixelRatio || 1, 1);
    var w = Math.round(window.innerWidth * scale), h = Math.round(window.innerHeight * scale);
    if (canvas.width !== w || canvas.height !== h) {
      canvas.width = w;
      canvas.height = h;
    }
    gl.viewport(0, 0, w, h);
  }

  function drawOverlay(t, mode, seed, tear) {
    gl.uniform1f(uni.tear, tear || 0);
    gl.uniform2f(uni.res, canvas.width, canvas.height);
    gl.uniform1f(uni.t, t);
    gl.uniform1f(uni.seed, seed);
    gl.uniform1f(uni.mode, mode);
    gl.uniform3f(uni.sheet, SHEET[0], SHEET[1], SHEET[2]);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
  }

  // -------------------------------------------------------- link pulse

  // A link in the text that plays the effect when clicked (an article, or
  // another site) pulses on its own: the click's split and tear, on the
  // link alone. A pulse swells (the colours drift apart,
  // smoothly) and then lets go (the split at its widest and a tear, easing
  // out in steps, as the click's first stage does).
  var LINK_SPLIT = 5;       // px the colours part, at most
  var LINK_TEAR = 5;        // px a band slides, at most
  var LINK_EVERY = 10000;   // ms from one pulse of a link to its next, give or take
  var LINK_MS = 1500;
  var LINK_SWELL = 0.65;    // how much of the pulse is the swell
  var LINK_CHECK = 500;     // ms between looks for links that are due

  var pulses = new WeakMap(); // link -> when its next pulse is due
  var pulsing = [];           // the pulses playing now

  function pulseLinks() {
    return Array.prototype.filter.call(document.querySelectorAll("a.inline-link"), function (a) {
      return playsEffect(a) && !isSamePage(a);
    });
  }

  function onScreen(a) {
    var r = a.getBoundingClientRect();
    return r.width > 0 && r.bottom > 0 && r.top < window.innerHeight;
  }

  // Chrome: a filter on the link, as the page's (ensureFilter), but made
  // from the link's shape, not its colours. The page is opaque, so its
  // channels can be pulled out of its colours; a link is text on nothing.
  // The three copies are its shape in cyan, magenta and yellow, multiplied
  // (where all three overlap, the text is black again); for light text, in
  // the red, green and blue of its colour, screened. One filter for each
  // text colour: links pulsing at once share it, and move as one.
  var linkSvg = null, linkStrips = null, linkFilters = {};

  function linkTint(r, g, b, result) {
    return '<feColorMatrix in="torn" type="matrix" values="' +
      "0 0 0 0 " + r + "  0 0 0 0 " + g + "  0 0 0 0 " + b + '  0 0 0 1 0" result="' + result + '"/>';
  }

  //
  // A burst (below) has a filter to itself, one of a few SLOTs for each
  // colour, so bursts a moment apart don't move as one; and its filter
  // reaches further to the sides, for a wider split.
  function linkFilter(rgb, light, slot) {
    var key = rgb.join("-") + (slot == null ? "" : "-burst-" + slot);
    if (linkFilters[key]) return linkFilters[key];
    var ns = "http://www.w3.org/2000/svg";
    if (!linkSvg) {
      // coarser than the page's strips: one is stretched over a link
      linkStrips = makeStrips(8, 16, 3, 0.35);
      linkSvg = document.createElementNS(ns, "svg");
      linkSvg.setAttribute("aria-hidden", "true");
      linkSvg.setAttribute("width", "0");
      linkSvg.setAttribute("height", "0");
      linkSvg.style.position = "absolute";
      root.appendChild(linkSvg);
    }
    var id = "fx-link-" + key;
    var mode = light ? "screen" : "multiply";
    var holder = document.createElementNS(ns, "g");
    // The filter reaches past the link's box, for what slides out of it.
    holder.innerHTML =
      '<filter id="' + id + '" ' +
        (slot == null ? 'x="-15%" y="-35%" width="130%" height="170%"' : 'x="-50%" y="-35%" width="200%" height="170%"') +
        ' color-interpolation-filters="sRGB">' +
        '<feImage preserveAspectRatio="none" result="map"/>' +
        '<feDisplacementMap in="SourceGraphic" in2="map" scale="0" xChannelSelector="R" yChannelSelector="G" result="torn"/>' +
        (light
          ? linkTint(rgb[0] / 255, 0, 0, "c1") + linkTint(0, rgb[1] / 255, 0, "c2") + linkTint(0, 0, rgb[2] / 255, "c3")
          : linkTint(0, 1, 1, "c1") + linkTint(1, 0, 1, "c2") + linkTint(1, 1, 0, "c3")) +
        '<feOffset in="c1" dx="0" dy="0" result="o1"/>' +
        '<feOffset in="c2" dx="0" dy="0" result="o2"/>' +
        '<feOffset in="c3" dx="0" dy="0" result="o3"/>' +
        '<feBlend in="o1" in2="o2" mode="' + mode + '" result="o12"/>' +
        '<feBlend in="o12" in2="o3" mode="' + mode + '"/>' +
      "</filter>";
    linkSvg.appendChild(holder);
    var image = holder.querySelector("feImage");
    image.setAttribute("href", linkStrips[0]); // Safari drops a filter with no image
    return (linkFilters[key] = {
      id: id,
      image: image,
      displace: holder.querySelector("feDisplacementMap"),
      offsets: holder.querySelectorAll("feOffset")
    });
  }

  // WebKit: the filter is fast enough on something as small as a link, but
  // its colours come out faded: a filter works in sRGB, whose cyan, magenta
  // and yellow fall short of what a wide-gamut screen can show. So, as for
  // the page (buildSplit, buildTear), copies: each line of the link is cut
  // into bands, and a band is a window onto three copies of the link's
  // whole paragraph, laid exactly over it, so their lines break where its
  // do. Everything in a copy is clear but the link, which is in one ink,
  // from Rec. 2020 (see .fx-rgb-copy in magazine.css). Moving a copy splits
  // the colours; moving a band tears the link. The copies mix with each
  // other inside a band, and the band with the page (multiply; for light
  // text, plus-lighter).
  //
  // The bands go inside the paragraph, so every rule that styles it styles
  // them, and are made when a pulse starts and removed when it ends: at
  // rest there is nothing extra in the page. Neither a band nor a copy may
  // get a layer of its own (will-change): in Safari the inks then stop
  // mixing, and the top one, yellow, paints the whole link.
  var LINK_PAD = 48;  // px a band reaches past the paragraph's sides
  var LINK_LIP = 3;   // and past the top and bottom of a line of the link

  function linkInk(r, g, b) {
    return "color(rec2020 " + r.toFixed(3) + " " + g.toFixed(3) + " " + b.toFixed(3) + ")";
  }

  // The nearest box the link's lines are set in.
  function blockOf(a) {
    var el = a.parentElement;
    while (el.parentElement && getComputedStyle(el).display.indexOf("inline") === 0) {
      el = el.parentElement;
    }
    return el;
  }

  function buildLinkBands(a, rgb, light) {
    var box = blockOf(a);
    var at = box.getBoundingClientRect();
    // Where the box is, from what the bands are placed against: the box
    // itself if it's positioned, or else what it is placed against.
    var own = getComputedStyle(box).position !== "static";
    var left = own ? -box.clientLeft : box.offsetLeft;
    var top = own ? -box.clientTop : box.offsetTop;
    var blend = light ? "light" : "ink";
    var inks = light
      ? [linkInk(rgb[0] / 255, 0, 0), linkInk(0, rgb[1] / 255, 0), linkInk(0, 0, rgb[2] / 255)]
      : [linkInk(0, 1, 1), linkInk(1, 0, 1), linkInk(1, 1, 0)];

    // The rows to cut at, in px down from the top of the box.
    var rows = [];
    Array.prototype.forEach.call(a.getClientRects(), function (r) {
      var y = Math.round(r.top - at.top) - LINK_LIP;
      var end = Math.round(r.bottom - at.top) + LINK_LIP;
      while (y < end) {
        var h = 3 + Math.floor(Math.random() * 6);
        if (end - y - h < 3) h = end - y; // no slivers
        rows.push([y, h]);
        y += h;
      }
    });

    a.setAttribute("data-fx-link", "");
    var bands = rows.map(function (row) {
      var band = document.createElement("div");
      band.className = "fx-link-band " + blend;
      band.setAttribute("aria-hidden", "true");
      band.inert = true;
      band.style.left = (left - LINK_PAD) + "px";
      band.style.top = (top + row[0]) + "px";
      band.style.width = (box.offsetWidth + LINK_PAD * 2) + "px";
      band.style.height = row[1] + "px";
      var copies = inks.map(function (colour) {
        var copy = box.cloneNode(true);
        copy.removeAttribute("id");
        Array.prototype.forEach.call(copy.querySelectorAll("[id]"), function (el) {
          el.removeAttribute("id");
        });
        copy.classList.add("fx-link-copy", blend);
        copy.style.setProperty("--fx-link-ink", colour);
        copy.style.left = LINK_PAD + "px";
        copy.style.top = -row[0] + "px";
        copy.style.width = box.offsetWidth + "px";
        copy.style.height = box.offsetHeight + "px";
        band.appendChild(copy);
        return copy;
      });
      return { el: band, copies: copies };
    });
    a.removeAttribute("data-fx-link");
    bands.forEach(function (band) { box.appendChild(band.el); });
    // the copies stand in for it; !important, for a line whose colour an animation sets
    a.style.setProperty("color", "transparent", "important");
    return bands;
  }

  // An element's text colour as [r, g, b], each 0-255. The stylesheet's
  // palette is in OKLCH, and a computed colour comes back in the space it was
  // written in ("oklch(1 0 0)" for white), so it is drawn to a canvas and
  // read there, not picked out of the text.
  var inks = {}, inkPad = null;
  function inkOf(el) {
    var colour = getComputedStyle(el).color;
    if (inks[colour]) return inks[colour];
    if (!inkPad) {
      var canvas = document.createElement("canvas");
      canvas.width = canvas.height = 1;
      inkPad = canvas.getContext("2d", { willReadFrequently: true });
    }
    inkPad.clearRect(0, 0, 1, 1);
    inkPad.fillStyle = "#000";
    inkPad.fillStyle = colour;
    inkPad.fillRect(0, 0, 1, 1);
    var px = inkPad.getImageData(0, 0, 1, 1).data;
    return (inks[colour] = [px[0], px[1], px[2]]);
  }

  function startLink(a, now) {
    var rgb = inkOf(a);
    var light = rgb[0] * 0.3 + rgb[1] * 0.59 + rgb[2] * 0.11 > 128;
    var pulse = {
      a: a,
      start: now,
      frame: 0,
      dir: Math.random() < 0.5 ? -1 : 1, // which way the first copy goes
      filter: SVG_FILTER ? linkFilter(rgb, light) : null,
      bands: SVG_FILTER ? null : buildLinkBands(a, rgb, light)
    };
    if (pulse.filter) a.style.filter = "url(#" + pulse.filter.id + ")";
    pulsing.push(pulse);
    if (pulsing.length === 1) requestAnimationFrame(stepLinks);
  }

  function endLink(pulse) {
    pulse.a.style.filter = "";
    pulse.a.style.removeProperty("color");
    if (!pulse.a.getAttribute("style")) pulse.a.removeAttribute("style");
    if (pulse.bands) pulse.bands.forEach(function (band) { band.el.remove(); });
    pulses.set(pulse.a, performance.now() + LINK_EVERY * (0.6 + Math.random() * 0.8));
  }

  // Before the page is torn, saved, or laid out again.
  function stopLinks() {
    pulsing.forEach(endLink);
    pulsing = [];
    stopZaps(false);
  }

  // Where a pulse's three copies are (each [dx, dy], in px), and how far
  // its bands slide. TEAR is the filter's scale: a band slides up to half
  // of it either way. About a third of the bands slide, as in the strips.
  function setLink(pulse, places, tear) {
    var filter = pulse.filter, bands = pulse.bands;
    if (filter) {
      places.forEach(function (at, i) {
        filter.offsets[i].setAttribute("dx", at[0].toFixed(2));
        filter.offsets[i].setAttribute("dy", at[1].toFixed(2));
      });
      filter.image.setAttribute("href", linkStrips[pulse.frame % linkStrips.length]);
      filter.displace.setAttribute("scale", tear.toFixed(1));
      return;
    }
    bands.forEach(function (band) {
      places.forEach(function (at, i) {
        band.copies[i].style.transform =
          "translate(" + at[0].toFixed(2) + "px," + at[1].toFixed(2) + "px)";
      });
      var dx = tear && Math.random() < 0.35 ? (Math.random() - 0.5) * tear : 0;
      band.el.style.transform = dx ? "translateX(" + dx.toFixed(1) + "px)" : "";
    });
  }

  // A burst: the letting go without the swell, on any element. Its colours
  // start at their widest, with a tear, and ease out in steps. It is what a
  // line of code plays as it is plugged into an evolving block
  // (magazine-evolving.js). OPTIONS are ms, split and tear (px, as LINK_SPLIT
  // and LINK_TEAR) and rgb, the text's colour, when it isn't the element's
  // colour just now, and share: bursts given the same object have one filter
  // between them, and move as one. False where it can't be played: while the
  // page is tearing, or with too many filtered bursts going already.
  var BURST_SLOTS = 8;
  function burst(el, options) {
    if (playing || !el.isConnected) return false;
    var rgb = options.rgb || inkOf(el);
    var light = rgb[0] * 0.3 + rgb[1] * 0.59 + rgb[2] * 0.11 > 128;
    var pulse = {
      a: el,
      start: performance.now(),
      frame: 0,
      dir: Math.random() < 0.5 ? -1 : 1,
      filter: null,
      bands: null,
      burst: { ms: options.ms || 600, split: options.split || LINK_SPLIT, tear: options.tear || LINK_TEAR }
    };
    if (SVG_FILTER) {
      var slot = 0;
      var shared = options.share && pulsing.filter(function (p) { return p.share === options.share; })[0];
      if (shared) slot = shared.slot;
      else while (slot < BURST_SLOTS && pulsing.some(function (p) { return p.slot === slot; })) slot++;
      if (slot === BURST_SLOTS) return false;
      pulse.slot = slot;
      pulse.share = options.share;
      pulse.filter = linkFilter(rgb, light, slot);
      el.style.filter = "url(#" + pulse.filter.id + ")";
    } else {
      pulse.bands = buildLinkBands(el, rgb, light);
    }
    pulsing.push(pulse);
    if (pulsing.length === 1) requestAnimationFrame(stepLinks);
    return true;
  }

  function stepLinks(now) {
    pulsing = pulsing.filter(function (pulse) {
      var burst = pulse.burst;
      var t = (now - pulse.start) / (burst ? burst.ms : LINK_MS);
      // Hovered, the link is white on a black box (.inline-link:hover), and
      // the split would ink in the whole box, or hide the text. Scrolled out
      // of the window, there is nobody to play to.
      if (t >= 1 || !pulse.a.isConnected || !onScreen(pulse.a) ||
          (!burst && pulse.a.matches(":hover"))) {
        endLink(pulse);
        return false;
      }
      var frame = pulse.frame++;
      if (burst) {
        if (frame % 3 === 2) return true;
        var left = Math.pow(1 - t, 2.4);
        var wide = pulse.dir * left * burst.split * (0.8 + Math.random() * 0.4);
        var rise = left * (Math.random() * 1.2 - 0.6);
        setLink(pulse, [[wide, rise], [wide * 0.12, 0], [-wide * 0.85, -rise]],
                left * burst.tear * 2 * (0.6 + Math.random() * 0.8));
        return true;
      }
      if (t < LINK_SWELL) {
        var u = t / LINK_SWELL;
        var swell = pulse.dir * 0.55 * u * u * LINK_SPLIT;
        setLink(pulse, [[swell, 0], [swell * 0.12, 0], [-swell * 0.85, 0]], 0);
        return true;
      }
      // letting go: as setFilter, with every third frame held
      if (frame % 3 === 2) return true;
      var amount = Math.pow(1 - (t - LINK_SWELL) / (1 - LINK_SWELL), 2.4);
      var split = pulse.dir * amount * LINK_SPLIT * (0.8 + Math.random() * 0.4);
      var lift = amount * (Math.random() * 1.2 - 0.6);
      setLink(pulse, [[split, lift], [split * 0.12, 0], [-split * 0.85, -lift]],
              amount * LINK_TEAR * 2 * (0.6 + Math.random() * 0.8));
      return true;
    });
    if (pulsing.length) requestAnimationFrame(stepLinks);
  }

  // Start the pulses that are due, on the links in the window. A link seen
  // for the first time waits a while, so a page's links don't all go off
  // together; one that comes due off screen goes soon after it scrolls in.
  function checkLinks(first) {
    if (playing || document.hidden) return;
    var now = performance.now();
    pulseLinks().forEach(function (a) {
      if (!pulses.has(a)) pulses.set(a, first === true ? 0 : now + Math.random() * LINK_EVERY);
      if (now < pulses.get(a) || a.matches(":hover")) return;
      if (zaps.some(function (z) { return z.el === a; })) return;
      if (onScreen(a)) zapLink(a);
      else pulses.set(a, now + Math.random() * 1500);
    });
  }

  setInterval(checkLinks, LINK_CHECK);
  // The bands are placed for the page as it is when they are made.
  window.addEventListener("resize", stopLinks);

  // -------------------------------------------------------- code pulse

  // The marked lines of a code block in an article (.src-line.is-hit: the
  // lines an :evolve step added, and those in :highlight) play the burst a
  // line plays as it is plugged into an evolving block, in the same
  // measures, but where they stand: nothing slides in. They play it over and
  // over while the block is in the window, a block's lines together, with a
  // rest between. As the site's other effects do, they play whatever the
  // system's Reduce Motion setting says.
  var CODE_SPLIT = 58;      // px the colours part, at most
  var CODE_TEAR = 60;       // px a band slides, at most
  var CODE_MS = 380;
  var CODE_EVERY = 2400;    // ms from the start of one burst to the next

  var codes = new WeakMap(); // block -> when its next burst is due

  function checkCode() {
    if (playing || document.hidden) return;
    var now = performance.now();
    Array.prototype.forEach.call(document.querySelectorAll("figure.src"), function (block) {
      if (now < (codes.get(block) || 0) || !onScreen(block)) return;
      var lines = Array.prototype.filter.call(
        block.querySelectorAll(".src-line.is-hit > code"), onScreen);
      if (!lines.length) return;
      codes.set(block, now + CODE_EVERY);
      var share = {};
      lines.forEach(function (code) {
        burst(code, { ms: CODE_MS, split: CODE_SPLIT, tear: CODE_TEAR, share: share });
      });
    });
  }

  setInterval(checkCode, LINK_CHECK);

  // -------------------------------------------------------- card pulse

  // The hardcover's card in the homepage's book band (.book-card.faint)
  // pulses as a link does, the whole card. A link's pulse is made from its
  // shape, which for a card would be a flat block; this one splits the
  // card's own colours. Its red, green and blue drift apart (the swell);
  // then, at their widest, a few bands of the card slide sideways, and it
  // settles in steps. One SVG filter does it, in every browser.
  var CARD_SPLIT = 8;       // px the colours part, at most
  var CARD_TEAR = 24;       // the displacement's scale at most: bands slide up to half of it
  var CARD_EVERY = 8000;    // ms from one pulse to the next, give or take
  var CARD_FIRST = 2500;    // ms before a card's first
  var CARD_MS = 1500;
  var CARD_SWELL = 0.65;    // how much of the pulse is the swell
  var CARD_STEPS = 6;       // steps it lets go in, each with its own bands

  var cardFilter = null;
  var cards = new WeakMap();  // card -> when its next pulse is due
  var cardPulsing = null;     // the card pulsing now

  function cardParts() {
    if (cardFilter) return cardFilter;
    var svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("aria-hidden", "true");
    svg.setAttribute("width", "0");
    svg.setAttribute("height", "0");
    svg.style.position = "absolute";
    function channel(row, result) {
      var rows = ["0 0 0 0 0", "0 0 0 0 0", "0 0 0 0 0"];
      rows[row] = ["1 0 0 0 0", "0 1 0 0 0", "0 0 1 0 0"][row];
      return '<feColorMatrix in="torn" type="matrix" values="' + rows.join("  ") +
        '  0 0 0 1 0" result="' + result + '"/>';
    }
    // The filter reaches past the card's box, for what slides out of it.
    svg.innerHTML =
      '<filter id="fx-card" x="-10%" y="-12%" width="120%" height="124%" color-interpolation-filters="sRGB">' +
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
    return (cardFilter = {
      noise: svg.querySelector("feTurbulence"),
      displace: svg.querySelector("feDisplacementMap"),
      offsets: svg.querySelectorAll("feOffset")
    });
  }

  // The first use of a filter costs the most: it is made, the card gets a
  // layer, and the noise is drawn for the first time. So that happens when
  // the card is first seen, where it doesn't show: the filter runs for a
  // few frames with nothing moved. The card keeps its layer (will-change).
  function warmCard(card) {
    var f = cardParts();
    var frames = 0;
    card.style.willChange = "filter";
    card.style.filter = "url(#fx-card)";
    cardPulsing = card;
    (function idle() {
      frames += 1;
      if (frames === 2) f.displace.setAttribute("scale", "0.01");
      if (frames < 4) { requestAnimationFrame(idle); return; }
      endCard(card);
    })();
  }

  function endCard(card) {
    cardParts().displace.setAttribute("scale", "0");
    card.style.filter = "";
    if (cardPulsing === card) cardPulsing = null;
  }

  function startCard(card, start) {
    var f = cardParts();
    var dir = Math.random() < 0.5 ? -1 : 1;
    var step = 0;
    card.style.filter = "url(#fx-card)";
    cardPulsing = card;
    (function frame(now) {
      var t = (now - start) / CARD_MS;
      if (t >= 1 || !card.isConnected || cardPulsing !== card) {
        endCard(card);
        cards.set(card, performance.now() + CARD_EVERY * (0.6 + Math.random() * 0.8));
        return;
      }
      var split, tear = 0;
      if (t < CARD_SWELL) {
        var u = t / CARD_SWELL;
        split = CARD_SPLIT * u * u;
      } else {
        var k = Math.floor((t - CARD_SWELL) / (1 - CARD_SWELL) * CARD_STEPS);
        var left = 1 - k / CARD_STEPS;
        split = CARD_SPLIT * left;
        tear = CARD_TEAR * left;
        if (k !== step) {
          step = k;
          f.noise.setAttribute("seed", String(1 + Math.floor(Math.random() * 90)));
        }
      }
      f.offsets[0].setAttribute("dx", (dir * split).toFixed(2));
      f.offsets[1].setAttribute("dy", (split * 0.35).toFixed(2));
      f.offsets[2].setAttribute("dx", (-dir * split).toFixed(2));
      f.displace.setAttribute("scale", tear.toFixed(1));
      requestAnimationFrame(frame);
    })(start);
  }

  function stopCards() {
    stopZaps(true);
  }

  function checkCards(first) {
    if (playing || document.hidden) return;
    var now = performance.now();
    Array.prototype.forEach.call(document.querySelectorAll("a.book-card.faint"), function (card) {
      if (!cards.has(card)) cards.set(card, first === true ? 0 : now + CARD_FIRST);
      if (now < cards.get(card) || !onScreen(card)) return;
      if (zaps.some(function (z) { return z.el === card; })) return;
      zapCard(card);
    });
  }

  setInterval(checkCards, LINK_CHECK);

  // ------------------------------------------------------------ lightning

  // All of it is one fragment shader, on one canvas over the window. A bolt
  // is a line swayed by noise; each pixel takes its distance to the bolts
  // near it, and from that a colour: white in the bolt, a thin blue edge
  // round it, and a blue glow that falls away. The edge is what shows on
  // the light sheet, the glow what shows on the dark.
  //
  // The click fills the canvas, with one bolt across it. A link's or a
  // card's pulse draws only its own box of it (the viewport), so the rest of
  // the window costs nothing. A link's letters reach the shader as a
  // texture: how far each pixel is from the nearest of them.
  var BLUE = "#1f32ff";       // the edge, the glow and the flash
  var NIGHT = "#070a2a";      // the dark the window goes while it storms
  var GLOW = 5;               // how far the glow reaches, in bolt widths
  var GLOW_ALPHA = 0.62;      // and how strong it is, at most
  var BOLT_HOLD = 3;          // frames a bolt keeps its shape, as the tear held its own

  var STORM_VERT =
    "attribute vec2 aPos; void main() { gl_Position = vec4(aPos, 0.0, 1.0); }";

  var STORM_FRAG = [
    "#ifdef GL_FRAGMENT_PRECISION_HIGH",
    "precision highp float;",
    "#else",
    "precision mediump float;",
    "#endif",
    "uniform float uMode;",            // 0: the click. 1: round a box. 2: round letters.
    "uniform vec2 uRes;",              // the canvas, in CSS px
    "uniform float uDpr;",
    "uniform float uSeed;",            // the bolts' shape: changes when they reshape
    "uniform float uAlpha;",
    "uniform float uWidth;",           // a bolt's white, in px
    "uniform float uGlow;",
    "uniform float uGlowAlpha;",
    "uniform vec3 uBlue;",
    "uniform vec3 uNight;",
    // the click
    "uniform vec2 uFrom;",             // the bolt's ends, past the window's sides
    "uniform vec2 uTo;",
    "uniform float uAmp;",             // px it strays
    "uniform float uDark;",
    "uniform float uFlash;",
    "uniform vec3 uFlashCol;",
    "uniform float uOut;",             // the black closing over it all
    // a box
    "uniform vec2 uOrigin;",           // its top left, in the window
    "uniform vec2 uSize;",
    "uniform float uPad;",             // px past the box that are drawn
    "uniform float uSwell;",           // 0 to 1 while sparks gather; -1 after
    "uniform float uLeft;",            // 1 to 0 as the strike dies down
    "uniform float uSway;",            // px a bolt strays from the edge
    "uniform float uForks;",
    "uniform float uCross;",
    // letters
    "uniform sampler2D uField;",       // how far from the letters, 1 being uPad px
    "uniform float uGap;",             // px it keeps from them
    "uniform float uShare;",           // how far along the line it has run
    "uniform vec4 uFork[3];",          // forks: from (xy), and to from there (zw)

    // x: the white of the bolt. y: its blue edge. w: the glow.
    "vec4 lit;",

    "float hash(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }",
    "float vnoise(vec2 p) {",
    "  vec2 i = floor(p), f = fract(p);",
    "  f = f * f * (3.0 - 2.0 * f);",
    "  return mix(mix(hash(i), hash(i + vec2(1.0, 0.0)), f.x),",
    "             mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), f.x), f.y) * 2.0 - 1.0;",
    "}",
    "float fbm(vec2 p) {",
    "  float a = 0.5, v = 0.0;",
    "  for (int i = 0; i < 4; i++) { v += a * vnoise(p); p = p * 2.03 + 17.0; a *= 0.5; }",
    "  return v;",
    "}",

    // A bolt W px wide, D px from here.
    "void lay(float d, float w) {",
    "  if (w < 0.01) return;",
    "  lit.x = max(lit.x, 1.0 - smoothstep(w * 0.8, w * 1.25, d));",
    "  lit.y = max(lit.y, 1.0 - smoothstep(w * 1.5, w * 2.1, d));",
    "  lit.w += exp(-d / (w * uGlow));",
    "}",

    // How far a bolt LEN px long is off its line, U of the way along. PIN
    // is 1 when its far end is held in place, 0 when it is loose. GRAIN is
    // the px of its widest sway.
    "float off(float len, float amp, float seed, float u, float pin, float grain) {",
    "  return (fbm(vec2(u * len / grain, seed)) + 0.16 * fbm(vec2(u * len / grain * 9.0, seed + 31.0))) * amp *",
    "    smoothstep(0.0, 0.1, u) * mix(1.0, smoothstep(1.0, 0.9, u), pin);",
    "}",

    // A bolt from A to B. A loose one thins to its end. SHARE is how much
    // of it is there, from A.
    "void seg(vec2 p, vec2 a, vec2 b, float amp, float w, float seed, float share, float pin, float grain) {",
    "  vec2 d = b - a;",
    "  float len = max(length(d), 1.0);",
    "  vec2 t = d / len;",
    "  vec2 q = p - a;",
    "  float u = dot(q, t) / len;",
    "  float v = dot(q, vec2(-t.y, t.x));",
    "  float far = amp + w * uGlow * 6.0;",
    "  if (abs(v) > far || u * len < -far || (u - 1.0) * len > far) return;",
    "  float uc = clamp(u, 0.0, share);",
    "  float dist = length(vec2((u - uc) * len, v - off(len, amp, seed, uc, pin, grain)));",
    "  lay(dist, w * mix(1.0 - 0.75 * uc, 1.0, pin));",
    "}",

    // One bolt, from side to side.
    "void click(vec2 p) {",
    "  seg(p, uFrom, uTo, uAmp, uWidth, uSeed, 1.0, 1.0, length(uTo - uFrom) / 7.0);",
    "}",

    // The point S px round the box's edge, clockwise from its top left
    // (xy), and the way out of the box there (zw).
    "vec4 rim(float s) {",
    "  vec2 z = uSize;",
    "  float round = 2.0 * (z.x + z.y);",
    "  s = mod(s, round);",
    "  if (s < z.x) return vec4(s, 0.0, 0.0, -1.0);",
    "  if (s < z.x + z.y) return vec4(z.x, s - z.x, 1.0, 0.0);",
    "  if (s < 2.0 * z.x + z.y) return vec4(2.0 * z.x + z.y - s, z.y, 0.0, 1.0);",
    "  return vec4(0.0, round - s, -1.0, 0.0);",
    "}",

    // Noise that comes back to where it started, once round the edge.
    "float ring(float s, float round, float grain, float seed) {",
    "  float turn = s / round * 6.2832;",
    "  return fbm(vec2(cos(turn), sin(turn)) * round / (6.2832 * grain) + seed);",
    "}",

    // Round a box: P is from its top left. Gives how much of the pixel to
    // keep: the drawing fades out before the edge of what is drawn.
    "float box(vec2 p) {",
    "  vec2 z = uSize;",
    "  float round = 2.0 * (z.x + z.y);",
    // the nearest point of the edge, and how far outside it this is
    "  vec2 c = clamp(p, vec2(0.0), z);",
    "  float sd;",
    "  if (c == p) {",
    "    float m = min(min(p.x, z.x - p.x), min(p.y, z.y - p.y));",
    "    if (m == p.y) c.y = 0.0; else if (m == z.y - p.y) c.y = z.y;",
    "    else if (m == p.x) c.x = 0.0; else c.x = z.x;",
    "    sd = -m;",
    "  } else {",
    "    sd = length(p - c);",
    "  }",
    "  float s;",
    "  if (c.y <= 0.0 && c.x < z.x) s = c.x;",
    "  else if (c.x >= z.x && c.y < z.y) s = z.x + c.y;",
    "  else if (c.y >= z.y && c.x > 0.0) s = 2.0 * z.x + z.y - c.x;",
    "  else s = round - c.y;",
    "  float stray = ring(s, round, 26.0, uSeed * 1.7) * uSway * 0.9;",
    "  if (uSwell >= 0.0) {",
    // sparks: stretches of the edge, more of it as they gather
    "    float on = smoothstep(0.0, 0.25, ring(s, round, round / 10.0, uSeed * 5.3 + 40.0) - mix(0.32, -0.12, uSwell));",
    "    lay(abs(sd - stray * (0.4 + 0.6 * uSwell)), uWidth * 0.8 * on);",
    "  } else {",
    "    float wild = 0.6 + uLeft;",
    "    lay(abs(sd - stray * wild), uWidth * (0.6 + 0.6 * uLeft));",
    "    for (int k = 0; k < 8; k++) {",
    "      float f = float(k);",
    "      if (f >= uForks) break;",
    "      float s0 = hash(vec2(f, uSeed)) * round;",
    "      vec4 r = rim(s0);",
    "      vec2 o = r.xy + r.zw * ring(s0, round, 26.0, uSeed * 1.7) * uSway * 0.9 * wild;",
    "      float turn = atan(r.w, r.z) + (hash(vec2(f + 7.0, uSeed)) - 0.5) * 1.6;",
    "      float l = uPad * 0.9 * (0.35 + 0.65 * hash(vec2(f + 11.0, uSeed)));",
    "      seg(p, o, o + vec2(cos(turn), sin(turn)) * l, l * 0.3, uWidth * 0.6, uSeed + 3.0 + f, 1.0, 0.0, 9.0);",
    "    }",
    "    if (uCross > 0.5) {",
    "      float s0 = hash(vec2(31.0, uSeed)) * round;",
    "      vec2 a = rim(s0).xy;",
    "      vec2 b = rim(s0 + round * (0.4 + 0.2 * hash(vec2(37.0, uSeed)))).xy;",
    "      seg(p, a, b, length(b - a) * 0.09, uWidth * 0.8, uSeed + 50.0, 1.0, 1.0, 40.0);",
    "    }",
    "  }",
    "  return 1.0 - smoothstep(uPad * 0.6, uPad, sd);",
    "}",

    // Round letters: P is from the top left of their line's box. The bolt
    // is the line uGap px out from them, swayed; noise picks the stretches
    // of it that are lit, fewer as it dies down.
    "float letters(vec2 p) {",
    "  float d = texture2D(uField, (p + uPad) / (uSize + 2.0 * uPad)).r * uPad;",
    "  float head = (uSize.x + 2.0 * uPad) * uShare - uPad;",
    "  float on = smoothstep(0.0, 0.2, fbm(p / 11.0 + uSeed * 5.3) - mix(0.3, -0.7, uLeft)) * smoothstep(head, head - 5.0, p.x);",
    "  float stray = (fbm(p / 4.0 + uSeed * 1.7) + 0.35 * vnoise(p / 1.3 + uSeed)) * uSway;",
    "  lay(abs(d - uGap - stray), uWidth * (0.7 + 0.5 * uLeft) * on);",
    "  for (int k = 0; k < 3; k++) {",
    "    vec4 f = uFork[k];",
    "    float l = length(f.zw);",
    "    if (l > 0.5) seg(p, f.xy, f.xy + f.zw, l * 0.3, uWidth * 0.8, uSeed + 3.0 + float(k), 1.0, 0.0, 4.0);",
    "  }",
    "  return 1.0 - smoothstep(uPad * 0.55, uPad * 0.95, d);",
    "}",

    "void main() {",
    "  vec2 p = vec2(gl_FragCoord.x, uRes.y * uDpr - gl_FragCoord.y) / uDpr;",
    "  lit = vec4(0.0);",
    "  float keep = 1.0;",
    "  if (uMode < 0.5) click(p); else if (uMode < 1.5) keep = box(p - uOrigin); else keep = letters(p - uOrigin);",
    "  float a = max(lit.y, (1.0 - exp(-lit.w * 1.2)) * uGlowAlpha);",
    "  vec3 col = mix(uBlue, vec3(1.0), lit.x);",
    // premultiplied, as the canvas is
    "  vec4 bolts = vec4(col * a, a) * uAlpha * keep;",
    "  if (uMode > 0.5) { gl_FragColor = bolts; return; }",
    // the dark, the flash (brightest along the bolt), the bolt, the black
    "  float f = uFlash * (0.55 + 0.45 * exp(-abs(p.y - mix(uFrom.y, uTo.y, p.x / uRes.x)) / (uRes.y * 0.3)));",
    "  vec4 o = vec4(uNight * uDark, uDark);",
    "  o = vec4(uFlashCol * f, f) + o * (1.0 - f);",
    "  o = bolts + o * (1.0 - bolts.a);",
    "  gl_FragColor = vec4(0.0, 0.0, 0.0, uOut) + o * (1.0 - uOut);",
    "}"
  ].join("\n");

  var storm = null;           // the canvas, its context and program; { gl: null } without WebGL

  function rgbOf(hex) {
    return [1, 3, 5].map(function (i) { return parseInt(hex.slice(i, i + 2), 16) / 255; });
  }

  // A uniform of the storm's program: one to four numbers, or a list of
  // three.
  function set(name, a, b, c, d) {
    var gl = storm.gl;
    var at = storm.at[name] || (storm.at[name] = gl.getUniformLocation(storm.program, name));
    if (a instanceof Array) gl.uniform3f(at, a[0], a[1], a[2]);
    else if (d !== undefined) gl.uniform4f(at, a, b, c, d);
    else if (c !== undefined) gl.uniform3f(at, a, b, c);
    else if (b !== undefined) gl.uniform2f(at, a, b);
    else gl.uniform1f(at, a);
  }

  // False without WebGL: then nothing here plays.
  function ensureStorm() {
    if (storm) return !!storm.gl;
    var el = document.createElement("canvas");
    el.className = "fx-storm";
    el.setAttribute("aria-hidden", "true");
    var gl = el.getContext("webgl", { alpha: true, premultipliedAlpha: true, antialias: false });
    storm = { el: el, gl: null, at: {}, w: 0, h: 0, dpr: 1 };
    if (!gl) return false;
    function shader(type, src) {
      var s = gl.createShader(type);
      gl.shaderSource(s, src);
      gl.compileShader(s);
      if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) {
        console.warn("fx: " + gl.getShaderInfoLog(s));
        return null;
      }
      return s;
    }
    var vert = shader(gl.VERTEX_SHADER, STORM_VERT), frag = shader(gl.FRAGMENT_SHADER, STORM_FRAG);
    if (!vert || !frag) return false;
    var program = gl.createProgram();
    gl.attachShader(program, vert);
    gl.attachShader(program, frag);
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      console.warn("fx: " + gl.getProgramInfoLog(program));
      return false;
    }
    gl.useProgram(program);
    // one triangle that covers the viewport
    gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 3, -1, -1, 3]), gl.STATIC_DRAW);
    var pos = gl.getAttribLocation(program, "aPos");
    gl.enableVertexAttribArray(pos);
    gl.vertexAttribPointer(pos, 2, gl.FLOAT, false, 0, 0);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    gl.clearColor(0, 0, 0, 0);
    storm.gl = gl;
    storm.program = program;
    set("uBlue", rgbOf(BLUE));
    set("uFlashCol", rgbOf(BLUE));
    set("uNight", rgbOf(NIGHT));
    set("uGlow", GLOW);
    set("uGlowAlpha", GLOW_ALPHA);
    // one byte a pixel, in rows of any length
    gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
    storm.blank = fieldTexture(new Uint8Array([255]), 1, 1);
    root.appendChild(el);
    return true;
  }

  // A texture of one byte a pixel, W by H, its first row the top one.
  function fieldTexture(bytes, w, h) {
    var gl = storm.gl, tex = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE, w, h, 0, gl.LUMINANCE, gl.UNSIGNED_BYTE, bytes);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    return tex;
  }

  // The canvas is the window's size, and on while anything plays.
  function showStorm() {
    // what the canvas covers: the window, less a scrollbar that takes room
    var w = root.clientWidth, h = root.clientHeight;
    var dpr = Math.min(2, window.devicePixelRatio || 1);
    if (storm.w !== w || storm.h !== h || storm.dpr !== dpr) {
      storm.w = w;
      storm.h = h;
      storm.dpr = dpr;
      storm.el.width = Math.ceil(w * dpr);
      storm.el.height = Math.ceil(h * dpr);
      set("uRes", storm.el.width / dpr, storm.el.height / dpr);
      set("uDpr", dpr);
    }
    storm.el.classList.add("is-on");
  }

  function hideStorm() {
    if (!storm || !storm.gl) return;
    storm.gl.clear(storm.gl.COLOR_BUFFER_BIT);
    storm.el.classList.remove("is-on");
  }

  // ------------------------------------------------------------------ zap

  // Lightning on a box, or on letters. Round the hardcover's card: sparks
  // gather on its edge, more and longer (the swell); then a bolt runs the
  // whole way round, forks leave it, and it dies down in steps. Round the
  // letters of a link in the text: it runs along their edges from left to
  // right (the swell), a hair outside them; then it is round all of them,
  // flickers, and dies down in steps, less of it lit at each. The box is
  // found again every frame, so the lightning stays on it as the page
  // scrolls. Nothing of the box itself is touched.
  //
  // ms: how long. swell: how much of that the sparks gather for. pad: px
  // drawn past the box, which is as far as a fork or the glow reaches.
  // width: a bolt's, in px. sway: px a bolt strays from the edge. forks: how
  // many at the strike (8 at most; 3 round letters). cross: also a bolt
  // across the box. letters: round the letters, not the box. gap: px it
  // keeps from them.
  var ZAP_LINK = { lines: true, letters: true, ms: 550, swell: 0.3, pad: 10, width: 0.45, sway: 0.9,
                   gap: 1.5, forks: 3 };
  var ZAP_CARD = { ms: 1700, swell: 0.6, pad: 34, width: 2.2, sway: 10, forks: 7, cross: true };
  var ZAP_STEPS = 6;          // steps it dies down in

  var zaps = [];              // the zaps playing now

  // The letters of a link, for lightning to run round: for each of its
  // lines (one for each of its boxes, or null), how far every pixel of the
  // line's box and PAD px round it is from the nearest letter. They are
  // drawn again on a canvas, each letter where the page has it (so however
  // the line was spaced out), and the distances spread out from them.
  // field: the distances, in the canvas's own px, CW by CH of them, SCALE to
  // a px of the page.
  function letterFields(a, pad) {
    var scale = Math.min(2, window.devicePixelRatio || 1);
    var lines = Array.prototype.map.call(a.getClientRects(), function (r) {
      if (r.width < 1 || r.height < 1) return null;
      var el = document.createElement("canvas");
      el.width = Math.ceil((r.width + pad * 2) * scale);
      el.height = Math.ceil((r.height + pad * 2) * scale);
      var ctx = el.getContext("2d", { willReadFrequently: true });
      // drawn in the window's px
      ctx.setTransform(scale, 0, 0, scale, (pad - r.left) * scale, (pad - r.top) * scale);
      return { r: r, ctx: ctx, cw: el.width, ch: el.height, scale: scale };
    });
    var walker = document.createTreeWalker(a, NodeFilter.SHOW_TEXT);
    var range = document.createRange(), node;
    while ((node = walker.nextNode())) {
      var style = getComputedStyle(node.parentElement);
      var font = style.fontStyle + " " + style.fontWeight + " " + style.fontSize + " " + style.fontFamily;
      var text = node.data;
      for (var i = 0; i < text.length;) {
        var size = text.codePointAt(i) > 0xffff ? 2 : 1;
        var letter = text.substr(i, size);
        range.setStart(node, i);
        range.setEnd(node, i += size);
        if (!letter.trim()) continue;
        var box = range.getClientRects()[0];
        if (!box) continue;
        var x = box.left + box.width / 2, y = box.top + box.height / 2;
        var line = lines.filter(function (l) {
          return l && x >= l.r.left && x <= l.r.right && y >= l.r.top && y <= l.r.bottom;
        })[0];
        if (!line) continue;
        if (style.textTransform === "uppercase") letter = letter.toUpperCase();
        line.ctx.font = font;
        var m = line.ctx.measureText(letter);
        var up = m.fontBoundingBoxAscent || parseFloat(style.fontSize) * 0.8;
        var down = m.fontBoundingBoxDescent || parseFloat(style.fontSize) * 0.2;
        line.ctx.fillText(letter, box.left, box.top + (box.height - up - down) / 2 + up);
      }
    }
    lines.forEach(function (line) {
      if (!line) return;
      var cw = line.cw, ch = line.ch;
      var ink = line.ctx.getImageData(0, 0, cw, ch).data;
      var d = line.field = new Float32Array(cw * ch);
      var i, x, y;
      for (i = 0; i < d.length; i++) d[i] = ink[i * 4 + 3] > 127 ? 0 : 1e4;
      // down the canvas, then back up it: each pixel is as far as its
      // nearest neighbour, and the step to it
      for (y = 0, i = 0; y < ch; y++) {
        for (x = 0; x < cw; x++, i++) {
          if (x > 0) d[i] = Math.min(d[i], d[i - 1] + 1);
          if (y > 0) {
            d[i] = Math.min(d[i], d[i - cw] + 1);
            if (x > 0) d[i] = Math.min(d[i], d[i - cw - 1] + 1.414);
            if (x < cw - 1) d[i] = Math.min(d[i], d[i - cw + 1] + 1.414);
          }
        }
      }
      for (y = ch - 1, i = d.length - 1; y >= 0; y--) {
        for (x = cw - 1; x >= 0; x--, i--) {
          if (x < cw - 1) d[i] = Math.min(d[i], d[i + 1] + 1);
          if (y < ch - 1) {
            d[i] = Math.min(d[i], d[i + cw] + 1);
            if (x < cw - 1) d[i] = Math.min(d[i], d[i + cw + 1] + 1.414);
            if (x > 0) d[i] = Math.min(d[i], d[i + cw - 1] + 1.414);
          }
        }
      }
      line.ctx = null;
    });
    return lines;
  }

  // A link's lines, each with its letters as a texture, and the points of
  // the line round them that a fork can leave from: four numbers each, the
  // place, in px from the top left of the line's box, and the way out.
  function letterParts(a, o) {
    return letterFields(a, o.pad).map(function (line) {
      if (!line) return null;
      var d = line.field, cw = line.cw, ch = line.ch, s = line.scale;
      var bytes = new Uint8Array(d.length), points = [];
      for (var i = 0; i < d.length; i++) bytes[i] = Math.min(255, d[i] / (o.pad * s) * 255);
      for (var y = 1; y < ch - 1; y += 2) {
        for (var x = 1; x < cw - 1; x += 2) {
          i = y * cw + x;
          if (Math.abs(d[i] - o.gap * s) > 0.75) continue;
          var nx = d[i + 1] - d[i - 1], ny = d[i + cw] - d[i - cw], n = Math.hypot(nx, ny) || 1;
          points.push(x / s - o.pad, y / s - o.pad, nx / n, ny / n);
        }
      }
      return { w: line.r.width, h: line.r.height, points: points, tex: fieldTexture(bytes, cw, ch) };
    });
  }

  function zap(el, o, done) {
    if (!ensureStorm()) { done(); return; }
    zaps.push({ el: el, o: o, done: done, start: performance.now(), frame: 0, seed: 0,
                parts: o.letters ? letterParts(el, o) : null });
    if (zaps.length === 1) requestAnimationFrame(stepZaps);
  }

  function endZap(z) {
    if (z.parts) z.parts.forEach(function (part) { if (part) storm.gl.deleteTexture(part.tex); });
    z.done();
  }

  // Before the page is torn, saved, or laid out again. CARDS is whether to
  // stop the cards' or the links'.
  function stopZaps(cards) {
    zaps = zaps.filter(function (z) {
      if (!z.o.lines !== cards) return true;
      endZap(z);
      return false;
    });
    if (!zaps.length && !playing) hideStorm();
  }

  // The canvas is over everything, the sticky header too. A box that
  // something else covers (a link scrolled under the header) is left alone.
  function covered(el, r) {
    var hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
    return !hit || !el.contains(hit);
  }

  function drawZap(z, r, t, n) {
    var gl = storm.gl, o = z.o, dpr = storm.dpr;
    var x = Math.floor((r.left - o.pad) * dpr);
    var y = Math.floor((storm.h - r.bottom - o.pad) * dpr);
    gl.viewport(x, y, Math.ceil((r.width + o.pad * 2) * dpr) + 1, Math.ceil((r.height + o.pad * 2) * dpr) + 1);
    set("uMode", o.letters ? 2 : 1);
    set("uSeed", z.seed + n * 13.7);
    set("uOrigin", r.left, r.top);
    set("uSize", r.width, r.height);
    set("uPad", o.pad);
    set("uWidth", o.width);
    set("uSway", o.sway);
    if (o.letters) {
      // laid out again since (the window's size changed): not these letters
      var part = z.parts[n];
      if (!part || Math.abs(part.w - r.width) > 1 || Math.abs(part.h - r.height) > 1) return;
      var gone = t < o.swell ? 0 : Math.floor((t - o.swell) / (1 - o.swell) * ZAP_STEPS) / ZAP_STEPS;
      var share = Math.min(1, t / o.swell);
      var head = (r.width + o.pad * 2) * share - o.pad;
      var forks = Math.round(o.forks * (1 - gone)), points = part.points;
      for (var k = 0; k < 3; k++) {
        // the same ones until the bolt reshapes
        var pick = Math.sin((z.seed + n) * (k + 1) * 12.9898) * 43758.5453;
        var f = Math.floor((pick - Math.floor(pick)) * (points.length / 4)) * 4;
        if (k >= forks || !points.length || points[f] > head) { set("uFork[" + k + "]", 0, 0, 0, 0); continue; }
        var turn = Math.atan2(points[f + 3], points[f + 2]) + ((pick * 7) % 1) * 0.8;
        var length = o.pad * (0.35 + 0.5 * Math.abs((pick * 13) % 1));
        set("uFork[" + k + "]", points[f], points[f + 1], Math.cos(turn) * length, Math.sin(turn) * length);
      }
      gl.bindTexture(gl.TEXTURE_2D, part.tex);
      set("uGap", o.gap);
      set("uShare", share);
      set("uLeft", 1 - gone);
      set("uAlpha", 1 - gone);
    } else if (t < o.swell) {
      var u = t / o.swell;
      set("uSwell", u);
      set("uAlpha", 0.4 + 0.6 * u);
    } else {
      var step = Math.floor((t - o.swell) / (1 - o.swell) * ZAP_STEPS);
      var left = 1 - step / ZAP_STEPS;
      set("uSwell", -1);
      set("uLeft", left);
      set("uAlpha", left);
      set("uForks", Math.round(o.forks * left));
      set("uCross", o.cross && step < 2 ? 1 : 0);
    }
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    if (o.letters) gl.bindTexture(gl.TEXTURE_2D, storm.blank);
  }

  function stepZaps(now) {
    if (playing) return; // the click has the canvas, and has stopped these
    zaps = zaps.filter(function (z) {
      // Hovered, a link is white on a black box (.inline-link:hover): leave
      // it be. Scrolled out of the window, there is nobody to play to.
      if ((now - z.start) / z.o.ms >= 1 || !z.el.isConnected || !onScreen(z.el) ||
          (z.o.lines && z.el.matches(":hover"))) {
        endZap(z);
        return false;
      }
      return true;
    });
    if (!zaps.length) { hideStorm(); return; }
    showStorm();
    storm.gl.clear(storm.gl.COLOR_BUFFER_BIT);
    zaps.forEach(function (z) {
      if (z.frame++ % BOLT_HOLD === 0) z.seed = Math.random() * 100;
      var t = (now - z.start) / z.o.ms;
      var rects = z.o.lines ? z.el.getClientRects() : [z.el.getBoundingClientRect()];
      Array.prototype.forEach.call(rects, function (r, n) {
        if (r.width < 1 || r.height < 1 || covered(z.el, r)) return;
        drawZap(z, r, t, n);
      });
    });
    requestAnimationFrame(stepZaps);
  }

  function zapLink(a) {
    zap(a, ZAP_LINK, function () {
      pulses.set(a, performance.now() + LINK_EVERY * (0.6 + Math.random() * 0.8));
    });
  }

  function zapCard(card) {
    zap(card, ZAP_CARD, function () {
      cards.set(card, performance.now() + CARD_EVERY * (0.6 + Math.random() * 0.8));
    });
  }

  // ---------------------------------------------------------- playback

  // Leaving, as fractions of LEAVE_MS: the window darkens (to STRIKE); then
  // one large bolt crosses it from side to side, at the height of the
  // click, and flashes two or three times, the window with it. It is the
  // same bolt each time, as lightning strikes again down the path it has
  // made, and between flashes it is gone. The last flash starts at OUT, and
  // the screen goes out behind it. Arriving, the dark lifts off the new
  // page with the bolt fading.
  var STRIKE = 0.16;
  var OUT = 0.76;
  var ON = 0.18;              // how long the bolt is there in a flash, but the last
  var DARK = 0.82;            // how dark the window goes, at most
  var FLASH = 0.6;            // how bright a flash of the window is, at most
  var OUT_FLASH = 0.9;        // and the last one
  var CLICK_WIDTH = 6;        // the bolt's width, in px
  var CLICK_SWAY = 0.14;      // how far it strays, as a share of the window's height
  var CLICK_TILT = 0.12;      // and how far its ends are from level, the same way

  var aim = null;             // where the next effect strikes: [x, y] in the window

  // O: dark, flash, out, alpha (see the shader).
  function drawClick(from, to, seed, o) {
    var gl = storm.gl;
    gl.viewport(0, 0, storm.el.width, storm.el.height);
    gl.clear(gl.COLOR_BUFFER_BIT);
    set("uMode", 0);
    set("uSeed", seed);
    set("uWidth", CLICK_WIDTH);
    set("uFrom", from[0], from[1]);
    set("uTo", to[0], to[1]);
    set("uAmp", storm.h * CLICK_SWAY);
    set("uDark", o.dark);
    set("uFlash", o.flash || 0);
    set("uOut", o.out || 0);
    set("uAlpha", o.alpha);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
  }

  function play(mode, duration, done) {
    stopLinks();
    stopCards();
    playing = true;
    var id = ++run;
    if (!ensureStorm()) { playing = false; if (done) done(); return; }
    showStorm();
    var w = storm.w, h = storm.h;
    // across the window at the height of the click, not too near its edge
    var y = Math.max(h * 0.15, Math.min(h * 0.85, aim ? aim[1] : h * (0.35 + Math.random() * 0.3)));
    aim = null;
    var from = [-20, y + (Math.random() - 0.5) * h * CLICK_TILT];
    var to = [w + 20, y + (Math.random() - 0.5) * h * CLICK_TILT];
    var seed = Math.random() * 100;
    function draw(o) { drawClick(from, to, seed, o); }
    var flashes = Math.random() < 0.5 ? 2 : 3;
    var each = (OUT - STRIKE) / (flashes - 1); // from one flash to the next
    var start = performance.now();
    var flash = 0;          // the window's flash, dying away after each
    var struck = 0;         // flashes so far

    function step(now) {
      if (id !== run) return; // a newer effect has taken over
      var t = Math.min(1, (now - start) / duration);
      if (mode === 1) {
        var left = Math.pow(1 - t, 2);
        draw({ dark: DARK * left, alpha: left });
      } else if (t < STRIKE) {
        draw({ dark: DARK * t / STRIKE, alpha: 0 });
      } else {
        var k = t < OUT ? Math.min(flashes - 2, Math.floor((t - STRIKE) / each)) : flashes - 1;
        if (struck <= k) { struck = k + 1; flash = t < OUT ? FLASH : OUT_FLASH; }
        else flash *= 0.6;
        if (t < OUT) {
          draw({ dark: DARK, flash: flash, alpha: t - STRIKE - k * each < ON ? 1 : 0 });
        } else {
          // out: the last flash, and the black closing over it
          var v = (t - OUT) / (1 - OUT);
          draw({ dark: DARK, flash: flash, alpha: Math.max(0, 1 - v * 1.6), out: t === 1 ? 1 : v * v });
        }
      }
      if (t < 1) {
        requestAnimationFrame(step);
      } else {
        if (mode === 1 || !done) finish();
        if (done) done();
      }
    }
    requestAnimationFrame(step);
  }

  function finish() {
    playing = false;
    hideStorm();
  }

  // A normal page load after the effect: the book buttons, links to other
  // sites, or everything when htmx isn't there.
  function leaveTo(a) {
    var href = a.href, here = a.origin === location.origin;
    play(0, LEAVE_MS, function () {
      if (here) {
        try { sessionStorage.setItem(ARRIVE_KEY, String(Date.now())); } catch (e) {}
      }
      location.href = href;
      // If the navigation is stopped or never happens, don't leave the
      // screen switched off.
      setTimeout(finish, 4000);
    });
  }

  // A boosted article link: htmx fetches the page while this plays. The
  // swap waits for the screen to go off (see htmx:before:swap below).
  var leaving = null;

  function leaveBoosted(a) {
    var href = a.href;
    leaving = { start: performance.now(), href: href };
    leaving.timer = setTimeout(function () {
      // No swap after this long: load the page the ordinary way.
      if (leaving) location.href = leaving.href;
    }, 8000);
    play(0, LEAVE_MS, function () { /* stays switched off until the swap */ });
  }

  // htmx marks the links it has boosted (see htmx:before:init below).
  function isBoosted(a) {
    return !!(a._htmx && a._htmx.boosted);
  }

  // Pages carry their layout in the body's class, but htmx only swaps the
  // body's contents. The sheet names the class its page needs.
  function syncBodyClass() {
    var sheet = document.querySelector(".sheet");
    var cls = (sheet && sheet.dataset.bodyClass) || "mag";
    if (document.body.className !== cls) document.body.className = cls;
  }

  // ------------------------------------------------------------ wiring

  // Capture phase: this runs before htmx's own click handler on the link.
  document.addEventListener("click", function (e) {
    var a = e.target.closest && e.target.closest("a[href]");
    if (!a || e.defaultPrevented || playing) return;
    if (!playsEffect(a)) return;
    if (!isPlainClick(e, a) || isSamePage(a)) return;
    var box = a.getBoundingClientRect();
    aim = e.clientX || e.clientY
      ? [e.clientX, e.clientY]
      : [box.left + box.width / 2, box.top + box.height / 2];
    if (isArticleLink(a) && isBoosted(a)) {
      leaveBoosted(a); // htmx does the fetch and the swap
      return;
    }
    e.preventDefault();
    leaveTo(a);
  }, true);

  if (htmx) {
    // A page that answers with an error isn't swapped in: it is loaded the
    // ordinary way (see fallBack).
    htmx.config.noSwap = [204, 304, "4xx", "5xx"];
    // Fetch a boosted link's page when it is hovered (htmx's preload
    // extension), not only when it is pressed.
    htmx.config.preload = { boostEvent: "mouseover" };

    // Only links to other magazine pages are boosted. The rest (the book,
    // inline links in an article) lead to pages with other styles, and load
    // the ordinary way. htmx asks before it sets up each link, on the first
    // load and after every swap.
    document.addEventListener("htmx:before:init", function (e) {
      var a = e.target;
      if (!(a instanceof HTMLAnchorElement) || a.hasAttribute("hx-get")) return;
      if (a.origin !== location.origin || !isMagazinePath(a.pathname)) e.preventDefault();
    });

    // Hold the new page until the screen has gone off, and skip the view
    // transition: the effect is the transition.
    document.addEventListener("htmx:before:swap", function (e) {
      if (!leaving || e.detail.ctx.target !== document.body) return;
      var wait = Math.max(0, Math.round(LEAVE_MS - (performance.now() - leaving.start)));
      e.detail.tasks.forEach(function (task) {
        if (task.type !== "main") return;
        task.swapSpec.swap = wait + "ms";
        task.swapSpec.transition = false;
      });
    });

    // The new page is in, and scrolled to where its link pointed (which
    // htmx does last): settle in there.
    document.addEventListener("htmx:finally:swap", function (e) {
      if (e.detail.ctx.target !== document.body) return;
      syncBodyClass();
      if (leaving) {
        clearTimeout(leaving.timer);
        leaving = null;
        play(1, ARRIVE_MS);
      }
    });

    function fallBack() {
      if (leaving) location.href = leaving.href;
    }
    document.addEventListener("htmx:response:error", fallBack);
    document.addEventListener("htmx:error", fallBack);

    // Back and forward fetch the page again and swap it in. Whatever was
    // playing stops.
    document.addEventListener("htmx:before:history:restore", function () {
      stopLinks();
      stopCards();
      finish();
    });

    // A swap and a history restore both replace the body's contents, and
    // a swap that fails part-way fires no event: watch the body instead.
    new MutationObserver(syncBodyClass).observe(document.body, { childList: true });
  }

  document.addEventListener("pointerover", function (e) {
    var a = e.target.closest && e.target.closest("a[href]");
    if (!a) return;
    if (isBookButton(a)) {
      if (!a.contains(e.relatedTarget)) hoverFlicker();
      prefetch(a);
    } else if (isOutsideLink(a) || (isArticleLink(a) && !isBoosted(a))) {
      prefetch(a);
    }
  });

  document.addEventListener("focusin", function (e) {
    var a = e.target.closest && e.target.closest("a.neo-button, a.book-card");
    if (!a) return;
    prefetch(a);
    if (e.target.matches(":focus-visible")) hoverFlicker();
  });

  // Touch screens have no hover: warm up on the first touch instead.
  document.addEventListener("touchstart", function (e) {
    var a = e.target.closest && e.target.closest("a[href]");
    if (a && (isBookButton(a) || isOutsideLink(a))) prefetch(a);
  }, { passive: true });

  // Coming back with the back button restores the page as it was left:
  // mid-effect. Put it straight.
  window.addEventListener("pageshow", function (e) {
    if (e.persisted) { leaving = null; stopLinks(); stopCards(); finish(); }
  });

  // Arriving from an effect on the last page: settle in.
  try {
    var stamp = Number(sessionStorage.getItem(ARRIVE_KEY));
    sessionStorage.removeItem(ARRIVE_KEY);
    if (stamp && Date.now() - stamp < 5000) play(1, ARRIVE_MS);
  } catch (e) {}

  // The fonts switching in (magazine-fonts.js): settle in over the switch.
  // An effect already playing covers it on its own.
  window.MagazineFx = {
    arrive: function () { if (!playing) play(1, ARRIVE_MS); },
    // For the test page: an effect without going anywhere. MODE is 0 to
    // leave, 1 to arrive.
    play: function (mode) {
      if (!playing) play(mode, mode ? ARRIVE_MS : LEAVE_MS, mode ? null : finish);
    },
    // The colours of EL split and let go (see burst).
    burst: burst,
    // And every link in the text due its pulse now.
    pulse: function () {
      pulses = new WeakMap();
      checkLinks(true);
    },
    // And the hardcover's card.
    card: function () {
      cards = new WeakMap();
      checkCards(true);
    }
  };

})();
