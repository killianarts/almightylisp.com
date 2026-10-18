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
  // Science Gothic comes in two files cut from ScienceGothic-Variable.ttf, with
  // its CTRS and slnt axes fixed at 0: Latin with the arrows and shapes the
  // pages use, and every other character the font has.
  var SCIENCE_LATIN = LATIN + ", U+2190-2199, U+25A0-25CF";
  var SCIENCE_OTHER = "U+0100-0130, U+0134-0148, U+014A-0151, U+0154-017E, U+0181, " +
    "U+0186, U+018A, U+018F-0190, U+0192, U+0198-0199, U+019D, U+01A0-01A1, U+01AF-01B0, " +
    "U+01B3-01B4, U+01C4-01CC, U+01E6-01E7, U+01EA-01EB, U+01F8-021B, U+022A-022D, " +
    "U+0230-0233, U+0237, U+0253-0254, U+0257, U+0259, U+025B, U+0272, U+02B9-02BA, " +
    "U+02C7, U+02D8-02D9, U+02DB, U+02DD, U+0300-0303, U+0306-0307, U+0309-030C, U+030F, " +
    "U+0311-0312, U+031B, U+0323-0324, U+0326-0328, U+032E, U+0331, U+0335, U+03C0, " +
    "U+0400-045F, U+0462-0463, U+046A-046B, U+0472-0475, U+0490-049D, U+04A0-04A5, " +
    "U+04AA-04AB, U+04AE-04B3, U+04B6-04BB, U+04C0-04C2, U+04CB-04CC, U+04CF-04D9, " +
    "U+04DC-04DF, U+04E2-04E9, U+04EE-04F9, U+051A-051D, U+0E3F, U+1E3E-1E3F, U+1E44-1E45, " +
    "U+1E62-1E63, U+1E80-1E85, U+1E9E, U+1EA0-1EF9, U+2070, U+2074-2079, U+2080-2089, " +
    "U+20A1, U+20A3-20A4, U+20A6-20A7, U+20A9-20AB, U+20AD-20AE, U+20B1-20B2, U+20B4-20B5, " +
    "U+20B8-20BA, U+20BC-20BF, U+2113, U+2116, U+2153-2154, U+215B-215E, U+2219, U+223C, " +
    "U+2248, U+2260, U+2264-2265, U+24B9, U+27E8-27E9";

  var FONTS = [
    { family: "Science Gothic", file: "ScienceGothic-latin.woff2", wait: true,
      weight: "100 900", stretch: "50% 200%", unicodeRange: SCIENCE_LATIN },
    { family: "Construct Mono", file: "ConstructMono.otf", wait: true },
    { family: "Mohave", file: "Mohave-latin.woff2", wait: true,
      weight: "300 700", unicodeRange: LATIN },
    // Article text.
    { family: "Geist", file: "Geist-latin.woff2", wait: true,
      weight: "100 900", unicodeRange: LATIN },
    // Japanese labels: in the running header, so on every page.
    { family: "JF Dot M+Q 10", file: "jf-dot-mplusq10-vert.ttf", wait: true },
    { family: "KKM Analog TV", file: "kkm_analogtv_v2-vert.ttf", wait: true },
    { family: "Science Gothic", file: "ScienceGothic-other.woff2",
      weight: "100 900", stretch: "50% 200%", unicodeRange: SCIENCE_OTHER },
    { family: "Mohave", file: "Mohave-latin-ext.woff2",
      weight: "300 700", unicodeRange: LATIN_EXT },
    { family: "Geist", file: "Geist-latin-ext.woff2",
      weight: "100 900", unicodeRange: LATIN_EXT },
    // On trial for article text: loads only where a rule names it.
    { family: "Geologica", file: "Geologica-latin.woff2",
      weight: "100 900", unicodeRange: LATIN },
    { family: "Geologica", file: "Geologica-latin-ext.woff2",
      weight: "100 900", unicodeRange: LATIN_EXT },
    // The motto, in the footer.
    { family: "JF Dot Kappa 20 0213", file: "jf-dot-kappa20-0213-motto.ttf" },
    // Code blocks, further down an article.
    { family: "Berkeley Mono", file: "BerkeleyMono-Regular.ttf" },
    // =code= in the text.
    { family: "Berkeley Mono", file: "BerkeleyMono-Bold.ttf", weight: "700" },
    // Code in a note.
    { family: "Berkeley Mono UltraCondensed", file: "BerkeleyMono-Bold-UltraCondensed.ttf",
      weight: "700" },
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
      // Justified text was measured in the fallback fonts.
      // (magazine-justify.js is deferred too.)
      if (window.MagazineJustify) window.MagazineJustify.refresh();
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
