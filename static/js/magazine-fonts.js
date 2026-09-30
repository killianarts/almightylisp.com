// Magazine fonts.
//
// The page draws in fallback fonts first. The real ones load here, are held
// back until all of them have arrived, and then switch in together: under
// the arrival effect (magazine-fx.js) if the page is already showing, or
// quietly if they came in before it was drawn (from the cache, usually).
//
// FONTS is the list of the magazine's faces. The switch waits for the ones
// marked WAIT; the rest are added straight away and load when the page first
// needs them, as an @font-face rule would. css/magazine-fonts.css repeats
// the list for browsers without JavaScript.
//
// The downloads wait for DOMContentLoaded, when the stylesheets and scripts
// the first paint needs are in: started any earlier, they share the
// connection with those and hold the page up. This runs in <head> all the
// same, to hide the sheet before the first paint. Keep it small: the page
// waits for it.
(function () {
  "use strict";

  var DIR = "/static/assets/fonts/";
  var LATIN = "U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, " +
    "U+0304, U+0308, U+0329, U+2000-206F, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, " +
    "U+FEFF, U+FFFD";
  var LATIN_EXT = "U+0100-02BA, U+02BD-02C5, U+02C7-02CC, U+02CE-02D7, U+02DD-02FF, U+0304, " +
    "U+0308, U+0329, U+1D00-1DBF, U+1E00-1E9F, U+1EF2-1EFF, U+2020, U+20A0-20AB, U+20AD-20C0, " +
    "U+2113, U+2C60-2C7F, U+A720-A7FF";

  var FONTS = [
    { family: "Science Gothic", file: "ScienceGothic-Variable.ttf", wait: true,
      weight: "100 900", stretch: "50% 200%" },
    { family: "Construct Mono", file: "ConstructMono.otf", wait: true },
    { family: "Mohave", file: "Mohave-latin.woff2", wait: true,
      weight: "300 700", unicodeRange: LATIN },
    { family: "Mohave", file: "Mohave-latin-ext.woff2",
      weight: "300 700", unicodeRange: LATIN_EXT },
    // Code blocks, further down an article.
    { family: "Berkeley Mono", file: "BerkeleyMono-Regular.ttf" },
    // Stands in for Avenir Next Condensed where there's no Avenir.
    { family: "Barlow Condensed", file: "BarlowCondensed-SemiBold-latin.woff2",
      weight: "600", unicodeRange: LATIN },
    { family: "Barlow Condensed", file: "BarlowCondensed-SemiBold-latin-ext.woff2",
      weight: "600", unicodeRange: LATIN_EXT }
  ];

  // How long the page may stay hidden while fonts come from the cache. Past
  // this it shows in fallback fonts, and the switch gets the effect.
  var HOLD_MS = 100;

  var FORMATS = { ttf: "truetype", otf: "opentype", woff2: "woff2", woff: "woff" };
  var root = document.documentElement;

  // Too old for the Font Loading API: the ordinary stylesheet.
  if (!document.fonts || typeof FontFace !== "function" || typeof Promise !== "function") {
    var link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = "/static/css/magazine-fonts.css";
    document.head.appendChild(link);
    return;
  }

  var holding = true;
  root.classList.add("fonts-pending");

  function release() {
    holding = false;
    root.classList.remove("fonts-pending");
  }
  setTimeout(release, HOLD_MS);

  function face(font) {
    var ext = font.file.slice(font.file.lastIndexOf(".") + 1);
    return new FontFace(font.family,
      "url(" + DIR + font.file + ") format(\"" + FORMATS[ext] + "\")",
      { weight: font.weight || "400", stretch: font.stretch || "100%",
        unicodeRange: font.unicodeRange || "U+0-10FFFF", display: "swap" });
  }

  var waiting = [];
  FONTS.forEach(function (font) {
    var f = face(font);
    if (font.wait) waiting.push(f);
    else document.fonts.add(f);
  });

  // A face that fails to load is left out; the page keeps its fallback.
  function load() {
    Promise.all(waiting.map(function (f) {
      return f.load().then(function () { return f; }, function () { return null; });
    })).then(function (loaded) {
      loaded.forEach(function (f) { if (f) document.fonts.add(f); });
      if (holding) {
        // Not drawn yet: nothing to cover.
        release();
        return;
      }
      // Same task as the switch: the effect's first frame is the first
      // frame drawn in the new fonts. (magazine-fx.js is deferred, so by
      // DOMContentLoaded it has run.)
      if (window.MagazineFx) window.MagazineFx.arrive();
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", load);
  } else {
    load();
  }
})();
