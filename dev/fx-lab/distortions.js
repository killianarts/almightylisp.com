// Effects test page: displacement maps for the SVG filter, in several styles.
//
// The filter (see ensureFilter in magazine-fx.js) moves each pixel of the
// page by the colour of a map stretched over the window: red sideways, green
// up and down, 128 for not at all, scaled by the filter's scale. The live
// effect has one style, "classic". These are others, after the monochrome
// glitch textures in https://x.com/nexgridco/status/2105251311943893116.
//
// FxDistortions.get(STYLE) returns { hit, tear, hitScale, tearScale }: maps
// for the hit (and arriving), which change every frame, maps for the tear,
// picked at random, and how much to scale each set's displacement against
// the classic one's. The versions that use them (styles.js, hybrid.js)
// take STYLE from ?fx-style=NAME. The maps are made once a page.
(function () {
  "use strict";

  function canvas(w, h) {
    var c = document.createElement("canvas");
    c.width = w;
    c.height = h;
    return c.getContext("2d");
  }

  function rand(a, b) { return a + Math.random() * (b - a); }
  function pick(n) { return Math.floor(Math.random() * n); }
  function clamp(v) { return Math.max(0, Math.min(255, Math.round(v))); }

  // A map from a function of the map's pixel, (x, y) -> [r, g].
  function field(w, h, f) {
    var ctx = canvas(w, h), img = ctx.createImageData(w, h), d = img.data;
    for (var y = 0; y < h; y++) {
      for (var x = 0; x < w; x++) {
        var v = f(x, y), i = (y * w + x) * 4;
        d[i] = clamp(v[0]);
        d[i + 1] = clamp(v[1]);
        d[i + 2] = 0;
        d[i + 3] = 255;
      }
    }
    ctx.putImageData(img, 0, 0);
    return ctx.canvas.toDataURL();
  }

  function many(count, make, arg) {
    var urls = [];
    for (var i = 0; i < count; i++) urls.push(make(arg, i, count));
    return urls;
  }

  // ------------------------------------------------------------- classic

  // As the live effect: rows 1-9 pixels tall (of 96), 30% pushed sideways.
  function classicHit() {
    var ctx = canvas(2, 96), y = 0;
    while (y < 96) {
      var h = 1 + pick(9);
      var dx = Math.random() < 0.3 ? pick(255) : 128;
      ctx.fillStyle = "rgb(" + dx + ",128,128)";
      ctx.fillRect(0, y, 2, h);
      y += h;
    }
    return ctx.canvas.toDataURL();
  }

  // Rows left alone, thrown sideways, smeared into a streak, or slipped.
  function classicTear() {
    var W = 160, H = 120, ctx = canvas(W, H), y = 0;
    while (y < H) {
      var h = 1 + pick(6), kind = Math.random();
      ctx.fillStyle = "rgb(128,128,0)";
      ctx.fillRect(0, y, W, h);
      if (kind < 0.45) {
        // untouched
      } else if (kind < 0.68) {
        var r = Math.random() < 0.5 ? rand(0, 50) : rand(205, 255);
        ctx.fillStyle = "rgb(" + Math.round(r) + ",128,0)";
        ctx.fillRect(0, y, W, h);
      } else if (kind < 0.82) {
        var g = ctx.createLinearGradient(0, 0, W, 0);
        g.addColorStop(0, "rgb(" + pick(256) + ",128,0)");
        g.addColorStop(1, "rgb(" + pick(256) + ",128,0)");
        ctx.fillStyle = g;
        ctx.fillRect(0, y, W, h);
      } else {
        ctx.fillStyle = "rgb(" + pick(256) + "," + (Math.random() < 0.5 ? 50 : 206) + ",0)";
        ctx.fillRect(rand(0, W), y, W * rand(0.15, 0.5), h + 2);
      }
      y += h;
    }
    return ctx.canvas.toDataURL();
  }

  // -------------------------------------------------------------- blocks

  // Datamosh: rectangles on a coarse grid, each shifted as one piece, some
  // stretched (a gradient across the block drags its pixels), and some
  // slipped up or down. The map is big enough (a quarter of a wide window)
  // that the blocks keep hard edges when it's stretched over the window.
  function blocks(violence) {
    var W = 360, H = 225, CELL = 9, ctx = canvas(W, H);
    ctx.fillStyle = "rgb(128,128,0)";
    ctx.fillRect(0, 0, W, H);
    var n = Math.round(rand(6, 12) * violence);
    for (var i = 0; i < n; i++) {
      var w = CELL * (1 + pick(Math.round(4 + 8 * violence)));
      var h = CELL * (1 + pick(Math.round(2 + 5 * violence)));
      var x = CELL * pick(Math.floor(W / CELL)), y = CELL * pick(Math.floor(H / CELL));
      var kind = Math.random();
      if (kind < 0.5) {
        ctx.fillStyle = "rgb(" + pick(256) + "," + (Math.random() < 0.2 ? pick(256) : 128) + ",0)";
      } else if (kind < 0.8) {
        // smeared sideways: every pixel in the block pulls from its left edge
        var gx = ctx.createLinearGradient(x, 0, x + w, 0);
        gx.addColorStop(0, "rgb(128,128,0)");
        gx.addColorStop(1, "rgb(" + Math.round(128 - w * rand(0.6, 1.2)) + ",128,0)");
        ctx.fillStyle = gx;
      } else {
        // smeared down: the block's top row drawn down it
        var gy = ctx.createLinearGradient(0, y, 0, y + h);
        gy.addColorStop(0, "rgb(128,128,0)");
        gy.addColorStop(1, "rgb(128," + Math.round(128 - h * rand(1, 2)) + ",0)");
        ctx.fillStyle = gy;
      }
      ctx.fillRect(x, y, w, h);
    }
    return ctx.canvas.toDataURL();
  }

  // ------------------------------------------------------------- shatter

  // A burst from a point: the window is cut into wedges around it, and each
  // wedge pulls its pixels in toward the point by its own amount, so it
  // streaks outward. Wedge edges wander with distance, for shards.
  function shatter(violence) {
    var W = 240, H = 150;
    var cx = rand(0.3, 0.7) * W, cy = rand(0.3, 0.7) * H;
    var cuts = [], n = 8 + pick(10);
    for (var i = 0; i < n; i++) cuts.push(Math.random() * Math.PI * 2);
    cuts.sort(function (a, b) { return a - b; });
    var pull = cuts.map(function () {
      return Math.random() < 0.35 ? 0 : rand(0.25, 1) * violence;
    });
    var wobble = rand(0.02, 0.06);
    return field(W, H, function (x, y) {
      var vx = (x - cx) / W, vy = (y - cy) / H;
      var r = Math.sqrt(vx * vx + vy * vy);
      var a = Math.atan2(vy, vx) + Math.PI + Math.sin(r * 40) * wobble;
      var k = 0;
      while (k < n - 1 && a > cuts[k]) k++;
      var m = pull[k];
      return [128 + 255 * m * vx, 128 + 255 * m * vy];
    });
  }

  // --------------------------------------------------------------- smear

  // Tracking: thin rows, most untouched; in some, from a point on, every
  // pixel pulls from that point, so its colour drags across as a streak.
  // A few rows shift whole.
  function smear(share) {
    var W = 320, H = 200, ctx = canvas(W, H);
    ctx.fillStyle = "rgb(128,128,0)";
    ctx.fillRect(0, 0, W, H);
    var y = 0;
    while (y < H) {
      var h = 1 + pick(3);
      var kind = Math.random();
      if (kind < share) {
        var x0 = rand(0, W * 0.8), len = rand(20, 127);
        var g = ctx.createLinearGradient(x0, 0, x0 + len, 0);
        g.addColorStop(0, "rgb(128,128,0)");
        g.addColorStop(1, "rgb(" + Math.round(128 - len) + ",128,0)");
        ctx.fillStyle = g;
        ctx.fillRect(x0, y, len, h);
      } else if (kind < share * 1.3) {
        ctx.fillStyle = "rgb(" + pick(256) + ",128,0)";
        ctx.fillRect(0, y, W, h);
      }
      y += h + pick(4);
    }
    return ctx.canvas.toDataURL();
  }

  // -------------------------------------------------------------- liquid

  // Waves: each row shifts by layered sine waves (a slow one, a faster one,
  // and a fine ripple), and a little up and down. The maps are frames of one
  // flow: played in order, the waves run down the page.
  function liquid(violence, i, count) {
    var H = 256, phase = (i / count) * Math.PI * 2;
    var f1 = rand(2, 4), f2 = rand(9, 16), f3 = rand(40, 70);
    var p1 = rand(0, 6), p2 = rand(0, 6);
    var ctx = canvas(2, H), img = ctx.createImageData(2, H), d = img.data;
    for (var y = 0; y < H; y++) {
      var t = (y / H) * Math.PI * 2;
      var dx = 0.6 * Math.sin(t * f1 + phase + p1) +
               0.3 * Math.sin(t * f2 - phase * 2 + p2) +
               0.1 * Math.sin(t * f3 + phase * 3);
      var dy = 0.15 * Math.sin(t * f2 * 0.5 + phase);
      for (var x = 0; x < 2; x++) {
        var k = (y * 2 + x) * 4;
        d[k] = clamp(128 + 127 * dx * violence);
        d[k + 1] = clamp(128 + 127 * dy * violence);
        d[k + 2] = 0;
        d[k + 3] = 255;
      }
    }
    ctx.putImageData(img, 0, 0);
    return ctx.canvas.toDataURL();
  }

  // ---------------------------------------------------------------- styles

  var STYLES = {
    classic: function () {
      return { hit: many(6, classicHit), tear: many(8, classicTear), hitScale: 1, tearScale: 1 };
    },
    blocks: function () {
      return { hit: many(6, blocks, 0.6), tear: many(8, blocks, 1.4), hitScale: 1.2, tearScale: 0.7 };
    },
    shatter: function () {
      return { hit: many(6, shatter, 0.8), tear: many(8, shatter, 1.3), hitScale: 2.2, tearScale: 1.1 };
    },
    smear: function () {
      return { hit: many(6, smear, 0.22), tear: many(8, smear, 0.45), hitScale: 3, tearScale: 1.5 };
    },
    liquid: function () {
      return { hit: many(12, liquid, 0.5), tear: many(12, liquid, 1), hitScale: 0.8, tearScale: 0.35 };
    }
  };

  window.FxDistortions = {
    styles: Object.keys(STYLES),
    // The style the page asks for (?fx-style=NAME), or classic.
    current: function () {
      var m = /[?&]fx-style=([a-z]+)/.exec(location.search);
      return m && STYLES[m[1]] ? m[1] : "classic";
    },
    get: function (style) {
      return (STYLES[style] || STYLES.classic)();
    }
  };
})();
