// The slide of a plugged-in line, as a CSS linear() easing.
//
// The line comes in from the right and reaches its place at HIT (a share of
// the duration), accelerating. After that it is a spring: it goes past by
// OVERSHOOT (a share of the distance) and swings BOUNCES times, dying away
// at the rate DAMPING. KIND "bounce" has it rebound off its place instead of
// passing through; "none" stops it dead.
//
//   plugEase({ kind, hit, overshoot, bounces, damping }) -> "linear(...)"
(function (root) {
  function displacement(o, t) {
    if (t < o.hit) {
      var u = t / o.hit;
      return 1 - u * u * u;            // far to near, gathering speed
    }
    if (o.kind === "none" || !o.overshoot) return 0;
    var v = (t - o.hit) / (1 - o.hit);
    var swing = Math.sin(v * o.bounces * Math.PI) * Math.exp(-o.damping * v) * (1 - v);
    // Scaled so the first swing peaks at OVERSHOOT.
    var peak = 0.5 / o.bounces;
    var most = Math.sin(peak * o.bounces * Math.PI) * Math.exp(-o.damping * peak) * (1 - peak);
    var size = o.overshoot * swing / most;
    return o.kind === "bounce" ? Math.abs(size) : -size;
  }
  function plugEase(o) {
    var points = [], n = 72;
    for (var i = 0; i <= n; i++) {
      var t = i / n;
      points.push((1 - displacement(o, t)).toFixed(3).replace(/\.?0+$/, "") || "0");
    }
    return "linear(" + points.join(", ") + ")";
  }
  root.plugEase = plugEase;
  if (typeof module !== "undefined") module.exports = plugEase;
})(typeof window !== "undefined" ? window : globalThis);
