// Magazine screen effects.
//
// Hover on a book button: a quick CRT flicker over the whole window (CSS,
// .fx-flicker in magazine.css).
//
// Click on a book button or an article link: the page splits into its colour
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
// buttons leave the magazine, so they do a normal page load after the effect.
//
// The effect's own elements hang off <html>, not <body>, so htmx never swaps
// them out or saves them in its history snapshots.
//
// Everything runs only while an effect is playing. The effects play whatever
// the reduced-motion setting: they are part of the site's look.
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
  var playing = false;
  var run = 0;            // bumps on every effect, so an old frame loop stops
  var htmx = window.htmx;
  var root = document.documentElement;

  // ---------------------------------------------------------------- links

  function isBookButton(a) {
    return a.classList.contains("neo-button");
  }

  function isArticleLink(a) {
    if (a.origin !== location.origin) return false;
    return /^\/magazine\/[a-z0-9-]+\/?$/.test(a.pathname) &&
           !/^\/magazine\/archive\/?$/.test(a.pathname);
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
  // once. Swapping between them each frame makes the tears jump.
  function makeStrips(count) {
    var urls = [];
    var canvas = document.createElement("canvas");
    canvas.width = 2;
    canvas.height = 96;
    var ctx = canvas.getContext("2d");
    for (var s = 0; s < count; s++) {
      var y = 0;
      while (y < canvas.height) {
        var h = 1 + Math.floor(Math.random() * 9);
        var torn = Math.random() < 0.3;
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
  var SVG_FILTER = navigator.vendor !== "Apple Computer, Inc.";
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
    strips = makeStrips(6);
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

  // ---------------------------------------------------------- playback

  function play(mode, duration, done) {
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

  // A normal page load after the effect: the book buttons, or everything
  // when htmx isn't there.
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
    if (!(isBookButton(a) || isArticleLink(a))) return;
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
    } else if (isArticleLink(a) && !isBoosted(a)) {
      prefetch(a);
    }
  });

  document.addEventListener("focusin", function (e) {
    var a = e.target.closest && e.target.closest("a.neo-button");
    if (!a) return;
    prefetch(a);
    if (e.target.matches(":focus-visible")) hoverFlicker();
  });

  // Touch screens have no hover: warm up on the first touch instead.
  document.addEventListener("touchstart", function (e) {
    var a = e.target.closest && e.target.closest("a.neo-button");
    if (a) prefetch(a);
  }, { passive: true });

  // Coming back with the back button restores the page as it was left:
  // mid-effect. Put it straight.
  window.addEventListener("pageshow", function (e) {
    if (e.persisted) { leaving = null; finish(); }
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
    arrive: function () { if (!playing) play(1, ARRIVE_MS); }
  };

  // Build the overlay while the browser is idle, so the first click is quick.
  var idle = window.requestIdleCallback || function (fn) { setTimeout(fn, 400); };
  idle(function () { if (!playing) ensureGL(); });
})();
