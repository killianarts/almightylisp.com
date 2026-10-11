// Magazine screen effects.
//
// Hover on a book button, or one of the book band's cards: a quick CRT
// flicker over the whole window (CSS, .fx-flicker in magazine.css).
//
// Click on a book button or card, an article link or a link to another site: the
// page splits into its colour
// channels and tears sideways (an SVG filter on the visible part of .sheet;
// in Safari, which can't run it fast enough, copies of the page instead),
// while a WebGL overlay adds tear bars, scanlines and noise and then switches
// the screen off like a CRT. The next page settles in with a shorter version
// of the same thing. The shorter version also plays when the fonts finish
// loading after the page has been drawn (see magazine-fonts.js).
//
// Magazine links are boosted by htmx: the page is fetched while the effect
// plays, and swapped in once the screen is off. Other boosted navigations
// (the nameplate, the archive) use a view transition instead. The book
// buttons and links to other sites leave the magazine, so they do a normal
// page load after the effect.
//
// A link in the text, to an article or to another site: every ten seconds
// or so, a small version of the click's first stage, on the link alone: its
// colours swell apart, then let go in a split and a tear. It marks the link
// as one, and as one that plays the effect, without a hover (a phone has
// none). The hardcover's card in the book band (.book-card.faint) pulses
// the same way, the whole card.
//
// The effect's own elements hang off <html>, not <body>, so htmx never swaps
// them out or saves them in its history snapshots. (A link's pulse puts
// copies in its paragraph in Safari; they're gone before a page is saved.)
//
// Everything runs only while an effect is playing, but for a twice-a-second
// check for links due a pulse. The effects play whatever the reduced-motion
// setting: they are part of the site's look.
(function () {
  "use strict";

  // The effects test page (dev/fx-lab/, served locally by its serve.py)
  // loads the page with ?fx-lab=NAME to try dev/fx-lab/NAME.js, another
  // version of this file, in its place, after the distortion maps the
  // versions share (distortions.js). Only on this machine.
  var lab = /^(localhost|127\.0\.0\.1)$/.test(location.hostname) &&
    /[?&]fx-lab=([a-z0-9-]+)/.exec(location.search);
  if (lab && !window.MagazineFxLab) {
    window.MagazineFxLab = lab[1];
    ["distortions", lab[1]].forEach(function (name) {
      var script = document.createElement("script");
      script.src = "/fx-lab/" + name + ".js";
      script.async = false; // in order
      document.head.appendChild(script);
    });
    return;
  }

  // Leaving: a hit of colour split, then the picture tears apart, then the
  // CRT switch-off. HIT and BREAK are where each stage ends, as fractions.
  var LEAVE_MS = 650;
  var HIT = 0.28;
  var BREAK = 0.71;
  var ARRIVE_MS = 260;
  var ARRIVE_KEY = "almighty-fx-arrive";
  var SHEET = [229 / 255, 229 / 255, 229 / 255];

  var sheet = null;       // the sheet being torn by the current effect
  var playing = false;
  var run = 0;            // bumps on every effect, so an old frame loop stops
  var htmx = window.htmx;
  var root = document.documentElement;

  // ---------------------------------------------------------------- links

  // The book's buttons, and the cards of the homepage's book band.
  function isBookButton(a) {
    return a.classList.contains("neo-button") || a.classList.contains("book-card");
  }

  function isArticleLink(a) {
    if (a.origin !== location.origin) return false;
    return /^\/magazine\/[a-z0-9-]+\/?$/.test(a.pathname) &&
           !/^\/magazine\/archive\/?$/.test(a.pathname);
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

  var svg = null, filterEl, image, displace, offR, offG, offB, strips, tearMaps;

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
        '<feImage id="fx-strip" preserveAspectRatio="none" result="map"/>' +
        '<feDisplacementMap id="fx-displace" in="SourceGraphic" in2="map" scale="0" xChannelSelector="R" yChannelSelector="G" result="torn"/>' +
        '<feColorMatrix in="torn" type="matrix" values="1 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0" result="r"/>' +
        '<feOffset id="fx-r" in="r" dx="0" dy="0" result="rs"/>' +
        '<feColorMatrix in="torn" type="matrix" values="0 0 0 0 0  0 1 0 0 0  0 0 0 0 0  0 0 0 1 0" result="g"/>' +
        '<feOffset id="fx-g" in="g" dx="0" dy="0" result="gs"/>' +
        '<feColorMatrix in="torn" type="matrix" values="0 0 0 0 0  0 0 0 0 0  0 0 1 0 0  0 0 0 1 0" result="b"/>' +
        '<feOffset id="fx-b" in="b" dx="0" dy="0" result="bs"/>' +
        '<feBlend in="rs" in2="gs" mode="screen" result="rg"/>' +
        '<feBlend in="rg" in2="bs" mode="screen"/>' +
      "</filter>";
    root.appendChild(svg);
    filterEl = svg.querySelector("filter");
    image = svg.querySelector("#fx-strip");
    displace = svg.querySelector("#fx-displace");
    offR = svg.querySelector("#fx-r");
    offG = svg.querySelector("#fx-g");
    offB = svg.querySelector("#fx-b");
  }

  // Limit the filter to the part of the page on screen: filtering the
  // whole article would cost a lot more and show nothing extra.
  function placeFilter() {
    if (!SVG_FILTER) return;
    var box = sheet.getBoundingClientRect();
    var x = -box.left, y = -box.top, w = window.innerWidth, h = window.innerHeight;
    [filterEl, image].forEach(function (el) {
      el.setAttribute("x", x);
      el.setAttribute("y", y);
      el.setAttribute("width", w);
      el.setAttribute("height", h);
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
      offR.setAttribute("dx", rx.toFixed(1));
      offR.setAttribute("dy", ry.toFixed(1));
      offG.setAttribute("dx", gx.toFixed(1));
      offG.setAttribute("dy", gy.toFixed(1));
      offB.setAttribute("dx", bx.toFixed(1));
      offB.setAttribute("dy", by.toFixed(1));
      return;
    }
    rgbOffsets = [[rx, ry], [gx, gy], [bx, by]];
  }

  // Shake the page: the sheet, or (WebKit) the copies over it.
  function setShake(x, y, skew) {
    var t = "translate3d(" + x.toFixed(1) + "px," + y.toFixed(1) + "px,0) skewX(" + skew.toFixed(2) + "deg)";
    if (SVG_FILTER) {
      sheet.style.transform = t;
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
      image.setAttribute("href", strips[frame % strips.length]);
      displace.setAttribute("scale", (amount * TEAR * (0.6 + Math.random() * 0.8)).toFixed(1));
    }
    if (!SVG_FILTER && liteDue(frame)) setBands(0.35, amount * TEAR, 0);
    setSplit(splitDir * split, amount * (Math.random() * 6 - 3), splitDir * split * 0.12,
             -splitDir * split * 0.85, amount * (Math.random() * 4 - 2));
    setShake(amount * JITTER * (Math.random() * 2 - 1), 0, amount * SKEW * (Math.random() * 2 - 1));
  }

  // The page coming apart. K runs from about 0.55 to 1 over the tear.
  function setTear(k, frame) {
    if (SVG_FILTER) {
      image.setAttribute("href", tearMaps[Math.floor(Math.random() * tearMaps.length)]);
      displace.setAttribute("scale", Math.round(k * (300 + Math.random() * 500)));
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

  function startLink(a, now) {
    var rgb = (getComputedStyle(a).color.match(/[\d.]+/g) || [0, 0, 0]).slice(0, 3).map(Number);
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
    var rgb = options.rgb ||
      (getComputedStyle(el).color.match(/[\d.]+/g) || [0, 0, 0]).slice(0, 3).map(Number);
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
      if (pulsing.some(function (pulse) { return pulse.a === a; })) return;
      if (onScreen(a)) startLink(a, now);
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

  // One filter, so one card at a time (the page has one).
  function stopCards() {
    if (cardPulsing) endCard(cardPulsing);
  }

  function checkCards() {
    if (playing || document.hidden || cardPulsing) return;
    var now = performance.now();
    Array.prototype.some.call(document.querySelectorAll("a.book-card.faint"), function (card) {
      if (!cards.has(card)) {
        cards.set(card, now + CARD_FIRST);
        warmCard(card);
        return true;
      }
      if (now < cards.get(card) || !onScreen(card)) return false;
      startCard(card, now);
      return true;
    });
  }

  setInterval(checkCards, LINK_CHECK);

  // ---------------------------------------------------------- playback

  function play(mode, duration, done) {
    stopLinks();
    stopCards();
    playing = true;
    var id = ++run;
    if (sheet && sheet !== document.querySelector(".sheet")) clearFilter();
    sheet = document.querySelector(".sheet");
    // Pages without the sheet layout (the archive) get the overlay only.
    if (sheet) {
      ensureFilter();
      placeFilter();
      if (!SVG_FILTER) {
        buildSplit();
        buildTear();
      }
      sheet.classList.add("fx-torn");
    }
    var hasGL = ensureGL();
    if (!sheet && !hasGL) { playing = false; if (done) done(); return; }
    if (hasGL) {
      sizeCanvas();
      canvas.classList.add("is-on");
    }
    var seed = Math.random() * 100;
    splitDir = Math.random() < 0.5 ? -1 : 1;
    splitSlope = (Math.random() < 0.5 ? -1 : 1) * Math.tan((15 + Math.random() * 20) * Math.PI / 180);
    var start = performance.now();
    var frame = 0;

    function step(now) {
      if (id !== run) return; // a newer effect has taken over
      if (!SVG_FILTER) growTear();
      var t = Math.min(1, (now - start) / duration);
      // the page tears hardest early on, and eases off as the overlay takes over
      // Ease-out: full strength on the first frame, falling fast and then
      // lingering. Leaving holds a little longer so it's still torn when
      // the overlay switches the screen off.
      var amount = mode === 0 ? Math.pow(1 - t, 2.4) : Math.pow(1 - t, 3);
      var tear = 0;
      // stutter: some frames hold still, like a signal dropping out
      if (mode === 1) {
        if (sheet) setFilter(amount, frame);
      } else if (t < HIT) {
        // the hit: full split, easing out (but not all the way) before the tear
        var u = t / HIT;
        if (sheet && frame % 3 !== 2) setFilter(0.3 + 0.7 * Math.pow(1 - u, 2), frame);
      } else {
        // the tear, getting worse, and fading under the switch-off
        tear = t < BREAK
          ? 0.55 + 0.45 * (t - HIT) / (BREAK - HIT)
          : Math.max(0, 1 - (t - BREAK) / 0.12);
        root.classList.add("fx-broken");
        // each broken state holds for two frames, like a signal catching
        if (sheet && frame % 2 === 0) setTear(Math.max(tear, 0.55), frame);
      }
      if (hasGL) drawOverlay(t, mode, seed, tear);
      frame++;
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
    root.classList.remove("fx-broken");
    clearFilter();
    if (canvas) canvas.classList.remove("is-on");
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
  // swap waits for the screen to go off (see htmx:beforeSwap below).
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

  function isBoosted(a) {
    var b = a.closest("[hx-boost]");
    return !!(htmx && b && b.getAttribute("hx-boost") !== "false");
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
    if (isArticleLink(a) && isBoosted(a)) {
      leaveBoosted(a); // htmx does the fetch and the swap
      return;
    }
    e.preventDefault();
    leaveTo(a);
  }, true);

  if (htmx) {
    htmx.config.globalViewTransitions = true;

    // Preload magazine pages when a link is hovered (htmx's preload
    // extension, enabled on the body). htmx sets up each link just after
    // this event, on the first load and after every swap, and the extension
    // reads the preload attribute right after that. Only other magazine
    // pages: not #anchors on this page, the book, or other sites.
    document.addEventListener("htmx:beforeProcessNode", function (e) {
      var a = e.target;
      if (!(a instanceof HTMLAnchorElement) || a.hasAttribute("preload")) return;
      var href = a.getAttribute("href");
      if (!href || href.charAt(0) === "#" || !isBoosted(a)) return;
      if (a.origin !== location.origin || !/^\/magazine(\/|$)/.test(a.pathname)) return;
      a.setAttribute("preload", "mouseover");
    });

    // Boosted links that leave the magazine (the book, inline links in an
    // article) load the ordinary way: their pages have other styles.
    document.addEventListener("htmx:beforeRequest", function (e) {
      if (!e.detail.boosted) return;
      var url = new URL(e.detail.requestConfig.path, location.href);
      if (!/^\/magazine(\/|$)/.test(url.pathname)) {
        e.preventDefault();
        if (!playing) location.href = url.href;
      }
    });

    // Hold the new page until the screen has gone off, and skip the view
    // transition: the effect is the transition.
    document.addEventListener("htmx:beforeSwap", function (e) {
      if (!leaving || !e.detail.boosted) return;
      var wait = Math.max(0, Math.round(LEAVE_MS - (performance.now() - leaving.start)));
      e.detail.swapOverride = "innerHTML swap:" + wait + "ms transition:false";
    });

    document.addEventListener("htmx:afterSettle", function (e) {
      if (e.detail.target !== document.body) return;
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
    document.addEventListener("htmx:responseError", fallBack);
    document.addEventListener("htmx:sendError", fallBack);

    // htmx saves the page it's leaving just before the swap, mid-effect.
    // Save it clean.
    document.addEventListener("htmx:beforeHistorySave", function () {
      stopLinks();
      stopCards();
      var s = sheet;
      if (!s || !s.classList.contains("fx-torn")) return;
      var style = s.getAttribute("style") || "";
      s.classList.remove("fx-torn");
      s.style.transform = "";
      Promise.resolve().then(function () {
        if (playing && s.isConnected) {
          s.classList.add("fx-torn");
          s.setAttribute("style", style);
        }
      });
    });

    document.addEventListener("htmx:historyRestore", function () {
      finish();
    });

    // Swaps, history restores and cache misses all replace the body's
    // contents, and not every path fires the same htmx event: watch the
    // body instead.
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
    }
  };

  // Build the overlay while the browser is idle, so the first click is quick.
  var idle = window.requestIdleCallback || function (fn) { setTimeout(fn, 400); };
  idle(function () { if (!playing) ensureGL(); });
})();
