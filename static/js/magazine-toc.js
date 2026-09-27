// Article contents strip: shows once the lede has scrolled away, marks the
// section being read, and fills a bar under it as the section goes by.
//
// Pages arrive by htmx swap as well as by normal loads, and each article
// brings this script with it. The scroll listeners are set up once; each new
// page only re-finds its elements.
(function () {
  if (window.MagazineToc) {
    window.MagazineToc.init();
    return;
  }

  // A section counts as current once its top passes this line, just under
  // the strip. When the page ends before the last section can scroll that
  // high, the line moves down by the shortfall, over the last stretch of
  // scrolling only, so the final sections still take their turn.
  var top = 96;
  var ticking = false;
  var dock, head, pieces, cells, sections;

  function init() {
    dock = document.querySelector(".toc-dock");
    head = document.querySelector(".art-head-band");
    pieces = document.querySelector(".piece-list");
    if (!dock || !head || !pieces) {
      dock = null;
      return;
    }
    cells = Array.prototype.slice.call(dock.querySelectorAll(".toc-cell"));
    sections = cells.map(function (cell) {
      return document.getElementById(cell.getAttribute("data-section"));
    });
    update();
  }

  function maxScroll() {
    return document.documentElement.scrollHeight - window.innerHeight;
  }

  function readingLine() {
    var last = sections[sections.length - 1];
    if (!last) return top;
    var left = maxScroll() - window.scrollY;
    var lastTopAtEnd = last.getBoundingClientRect().top - left;
    var shortfall = Math.max(0, lastTopAtEnd - top + 1);
    if (left >= shortfall) return top;
    return top + (shortfall - Math.max(0, left));
  }

  function update() {
    ticking = false;
    if (!dock || !dock.isConnected) return;
    var line = readingLine();
    var atEnd = maxScroll() - window.scrollY <= 1;
    var past = head.getBoundingClientRect().bottom < 0;
    // Hide once the last section has gone up under the strip.
    var before = pieces.getBoundingClientRect().bottom > top;
    dock.classList.toggle("is-shown", past && before);

    var current = 0;
    sections.forEach(function (section, i) {
      if (section && section.getBoundingClientRect().top <= line) current = i;
    });
    cells.forEach(function (cell, i) {
      cell.classList.toggle("is-current", i === current);
      cell.classList.toggle("is-passed", i < current);
    });

    var box = sections[current] && sections[current].getBoundingClientRect();
    if (box) {
      // At the bottom of the page there is nothing left to read.
      var progress = atEnd ? 1 : Math.min(1, Math.max(0, (line - box.top) / box.height));
      cells[current].style.setProperty("--progress", progress.toFixed(3));
    }
  }

  function request() {
    if (!ticking) {
      ticking = true;
      window.requestAnimationFrame(update);
    }
  }

  window.addEventListener("scroll", request, { passive: true });
  window.addEventListener("resize", request);
  // Back and forward through htmx's history restore a page without running
  // its scripts again.
  document.addEventListener("htmx:historyRestore", init);

  window.MagazineToc = { init: init };
  init();
})();
