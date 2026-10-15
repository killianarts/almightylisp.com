// The archive lab (/magazine/archive/lab): filters, months that open and
// close, and jumps to a month, the same for all three designs.
//
// The page marks what the script works on:
//   [data-lab]       a design; data-total is how many articles it lists
//   [data-article]   an article, with data-kind and data-series
//   [data-month]     a month, with data-collapsed while it is closed
//   [data-toggle]    opens and closes the month it is in
//   [data-filter]    "kind" or "series"; data-value is what to keep ("" for all)
//   [data-count]     a month's count; [data-shown] the count of the whole list
//   [data-fill]      hatching that pairs an odd cell out
//   [data-expand]    opens every month
//   [data-sort]      a column's head: sorts or groups the list by that column
//   [data-groups]    where the sorted list's groups go; [data-view] names the sort
//
// Everything hangs off the document, so a page htmx swaps in works without
// setting anything up again.
(function () {
  "use strict";
  if (window.archiveLab) return;
  window.archiveLab = true;

  function all(root, selector) {
    return Array.prototype.slice.call(root.querySelectorAll(selector));
  }

  function plural(n) {
    return n + (n === 1 ? " article" : " articles");
  }

  function setCollapsed(month, collapsed) {
    month.toggleAttribute("data-collapsed", collapsed);
    all(month, "[data-toggle]").forEach(function (toggle) {
      toggle.setAttribute("aria-expanded", collapsed ? "false" : "true");
    });
  }

  function openAll(lab) {
    all(lab, "[data-month]").forEach(function (month) {
      setCollapsed(month, false);
    });
  }

  // Show the articles that match the lab's filters, and bring every count
  // in line with what is showing.
  function apply(lab) {
    var kind = lab.dataset.kind || "";
    var series = lab.dataset.series || "";
    var shown = 0;

    all(lab, "[data-article]").forEach(function (article) {
      var keep = (!kind || article.dataset.kind === kind) &&
                 (!series || article.dataset.series === series);
      article.hidden = !keep;
      if (keep) shown += 1;
    });

    all(lab, "[data-month]").forEach(function (month) {
      var n = all(month, "[data-article]:not([hidden])").length;
      month.hidden = n === 0;
      all(month, "[data-count]").forEach(function (count) {
        count.textContent = plural(n);
      });
      all(month, "[data-fill]").forEach(function (fill) {
        fill.hidden = n % 2 === 0;
      });
    });

    all(lab, "[data-shown]").forEach(function (out) {
      out.textContent = out.hasAttribute("data-bare")
        ? String(shown)
        : shown + " of " + lab.dataset.total;
    });

    all(lab, "[data-filter]").forEach(function (button) {
      var current = button.dataset.filter === "kind" ? kind : series;
      button.setAttribute("aria-pressed", button.dataset.value === current ? "true" : "false");
    });

    // A filter's matches are spread over the closed months too.
    if (kind || series) openAll(lab);
  }

  // Sorting. A column either groups the articles (date by month, series,
  // kind, part) or sorts them into one list (title, serial). Pressing the
  // column that is on turns its order around.
  var MONTHS_OPEN = 2;

  function byText(key) {
    return function (a, b) {
      return a.dataset[key].localeCompare(b.dataset[key]);
    };
  }

  function newestFirst(a, b) {
    return b.dataset.date.localeCompare(a.dataset.date);
  }

  // A group for every value KEY gives, in the order the values first come
  // up. LABEL names a value's group. Articles without a value go in one
  // last group, named NONE, which starts closed: it is the long one.
  function grouped(rows, key, label, none) {
    var groups = [];
    var found = {};
    var rest = { label: none, rows: [], collapsed: true };
    rows.forEach(function (row) {
      var value = key(row);
      if (!value) {
        rest.rows.push(row);
        return;
      }
      if (!found[value]) {
        found[value] = { label: label(row), rows: [], id: value };
        groups.push(found[value]);
      }
      found[value].rows.push(row);
    });
    return { groups: groups, rest: rest.rows.length ? rest : null };
  }

  var SORTS = {
    date: {
      first: "desc",
      view: { desc: "By date, newest first", asc: "By date, oldest first" },
      build: function (rows, dir) {
        rows.sort(newestFirst);
        if (dir === "asc") rows.reverse();
        var out = grouped(rows,
          function (row) { return row.dataset.date.slice(0, 7); },
          function (row) { return row.dataset.monthName; });
        out.groups.forEach(function (group, index) {
          group.month = true;
          group.collapsed = index >= MONTHS_OPEN;
        });
        return out.groups;
      }
    },
    series: {
      first: "asc",
      view: { asc: "By series, A to Z", desc: "By series, Z to A" },
      build: function (rows, dir) {
        rows.sort(function (a, b) {
          return byText("seriesTitle")(a, b) || Number(a.dataset.part) - Number(b.dataset.part) || newestFirst(a, b);
        });
        var out = grouped(rows,
          function (row) { return row.dataset.series; },
          function (row) { return row.dataset.seriesTitle; },
          "Not in a series");
        if (dir === "desc") out.groups.reverse();
        return out.rest ? out.groups.concat(out.rest) : out.groups;
      }
    },
    kind: {
      first: "asc",
      view: { asc: "By kind, A to Z", desc: "By kind, Z to A" },
      build: function (rows, dir) {
        rows.sort(function (a, b) { return byText("kind")(a, b) || newestFirst(a, b); });
        var out = grouped(rows,
          function (row) { return row.dataset.kind; },
          function (row) { return row.dataset.kind; });
        if (dir === "desc") out.groups.reverse();
        return out.groups;
      }
    },
    part: {
      first: "asc",
      view: { asc: "By part, first to last", desc: "By part, last to first" },
      build: function (rows, dir) {
        rows.sort(function (a, b) {
          return Number(a.dataset.part) - Number(b.dataset.part) || byText("seriesTitle")(a, b) || newestFirst(a, b);
        });
        var out = grouped(rows,
          function (row) { return row.dataset.part; },
          function (row) { return "Part " + row.dataset.part; },
          "Not in a series");
        if (dir === "desc") out.groups.reverse();
        return out.rest ? out.groups.concat(out.rest) : out.groups;
      }
    },
    title: {
      first: "asc",
      view: { asc: "By title, A to Z", desc: "By title, Z to A" },
      build: function (rows, dir) {
        rows.sort(byText("title"));
        if (dir === "desc") rows.reverse();
        return [{ label: dir === "asc" ? "Title, A to Z" : "Title, Z to A", rows: rows }];
      }
    },
    serial: {
      first: "asc",
      view: { asc: "By serial, A to Z", desc: "By serial, Z to A" },
      build: function (rows, dir) {
        rows.sort(byText("code"));
        if (dir === "desc") rows.reverse();
        return [{ label: dir === "asc" ? "Serial, A to Z" : "Serial, Z to A", rows: rows }];
      }
    }
  };

  function element(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text) node.textContent = text;
    return node;
  }

  // A group as the page sets a month: a bar that opens and closes it, over
  // its rows. A month's bar carries the month's barcode.
  function groupSection(lab, group) {
    var section = element("section", "lab-month");
    section.setAttribute("data-month", "");
    if (group.month) section.id = group.id;

    var bar = element("button", "lab-month-bar");
    bar.type = "button";
    bar.setAttribute("data-toggle", "");
    bar.appendChild(element("span", "lab-month-name", group.label));
    var hatch = element("span", "lab-hatch");
    hatch.setAttribute("aria-hidden", "true");
    bar.appendChild(hatch);
    var barcode = group.month && lab.barcodes[group.id];
    if (barcode) bar.appendChild(barcode);
    var count = element("span", "lab-month-count");
    count.setAttribute("data-count", "");
    bar.appendChild(count);
    var go = element("span", "lab-month-go", ">>>");
    go.setAttribute("aria-hidden", "true");
    bar.appendChild(go);

    var body = element("div", "lab-rows lab-month-body");
    group.rows.forEach(function (row) { body.appendChild(row); });

    section.appendChild(bar);
    section.appendChild(body);
    setCollapsed(section, !!group.collapsed);
    return section;
  }

  function sortBy(lab, key) {
    var sort = SORTS[key];
    var groups = lab.querySelector("[data-groups]");
    if (!sort || !groups) return;

    // Keep each month's barcode before the first sort takes the months apart.
    if (!lab.barcodes) {
      lab.barcodes = {};
      all(groups, "[data-month]").forEach(function (month) {
        var barcode = month.querySelector(".lab-barcode");
        if (barcode) lab.barcodes[month.id] = barcode;
      });
    }

    var dir = lab.dataset.sort === key
      ? (lab.dataset.dir === "asc" ? "desc" : "asc")
      : sort.first;
    lab.dataset.sort = key;
    lab.dataset.dir = dir;

    var sections = sort.build(all(groups, "[data-article]"), dir).map(function (group) {
      return groupSection(lab, group);
    });
    while (groups.firstChild) groups.removeChild(groups.firstChild);
    sections.forEach(function (section) { groups.appendChild(section); });

    all(lab, "[data-sort]").forEach(function (head) {
      var on = head.dataset.sort === key;
      head.setAttribute("aria-pressed", on ? "true" : "false");
      if (on) head.dataset.dir = dir; else delete head.dataset.dir;
    });
    all(lab, "[data-view]").forEach(function (out) {
      out.textContent = "  \u25a0  " + sort.view[dir];
    });
    apply(lab);
  }

  document.addEventListener("click", function (e) {
    var lab = e.target.closest("[data-lab]");
    if (!lab) return;

    var head = e.target.closest("[data-sort]");
    if (head) {
      if (!lab.dataset.sort) {
        lab.dataset.sort = "date";
        lab.dataset.dir = "desc";
      }
      sortBy(lab, head.dataset.sort);
      return;
    }

    var filter = e.target.closest("[data-filter]");
    if (filter) {
      var key = filter.dataset.filter;
      // Pressing the filter that is on takes it off.
      lab.dataset[key] = lab.dataset[key] === filter.dataset.value ? "" : filter.dataset.value;
      apply(lab);
      return;
    }

    var toggle = e.target.closest("[data-toggle]");
    if (toggle) {
      var month = toggle.closest("[data-month]");
      setCollapsed(month, !month.hasAttribute("data-collapsed"));
      return;
    }

    if (e.target.closest("[data-expand]")) {
      openAll(lab);
      return;
    }

    var link = e.target.closest("a[href]");
    if (!link) return;
    var href = link.getAttribute("href");

    // The sample's articles don't exist.
    if (href === "#") {
      e.preventDefault();
      return;
    }

    // A jump to a month opens it, and marks its cell in the year's rail.
    if (href.charAt(0) === "#") {
      var target = document.getElementById(href.slice(1));
      if (target && target.hasAttribute("data-month")) setCollapsed(target, false);
      if (link.classList.contains("lab-rail-cell")) {
        all(lab, ".lab-rail-cell.on").forEach(function (cell) {
          cell.classList.remove("on");
        });
        link.classList.add("on");
      }
    }
  });
})();
