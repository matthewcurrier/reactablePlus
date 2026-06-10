// reactable-plus-updates.js
// In-place update handlers for the reactablePlus config-driven module.
//
// Instead of re-rendering the entire reactable widget when a cross-cell
// effect fires (mutual exclusion, row-class toggle, displaced display),
// the server sends targeted custom messages that mutate the existing DOM:
//
//   rp_set_displaced  → toggles a cell's data-rp-displaced attribute,
//                       swapping the visible content between the active
//                       widget and the mutual-exclusion display HTML.
//   rp_set_row_class  → manages a row's user-supplied custom CSS class
//                       (e.g. "is-homeschool") without disturbing reactable's
//                       own row classes.
//
// Both handlers are namespace-aware: cell_key and row_key arrive
// pre-namespaced from the server (e.g. "history-school-PK") so multiple
// tables on the same page don't collide.

(function () {
  "use strict";

  if (typeof Shiny === "undefined") return;

  // Tracks the previously-applied custom class per row so we can remove
  // it on the next update. Keyed by namespaced row id.
  var _prevRowClass = {};

  // ── Cell displacement ────────────────────────────────────────────────────
  // Message shape: { cell_key: "history-school-PK", displaced: true|false }

  Shiny.addCustomMessageHandler("rp_set_displaced", function (msg) {
    if (!msg || !msg.cell_key) return;
    var selector =
      '[data-rp-cell="' + cssEscape(msg.cell_key) + '"]';
    var el = document.querySelector(selector);
    if (!el) return;
    el.setAttribute("data-rp-displaced", msg.displaced ? "true" : "false");
  });

  // ── Row custom class ─────────────────────────────────────────────────────
  // Message shape: { row_key: "history-PK", new_class: "is-homeschool" | "" }

  Shiny.addCustomMessageHandler("rp_set_row_class", function (msg) {
    if (!msg || !msg.row_key) return;
    var selector = "tr.rp-row-" + cssEscape(msg.row_key);
    var row = document.querySelector(selector);
    if (!row) return;

    // Remove previously-applied class(es) for this row
    var prev = _prevRowClass[msg.row_key] || "";
    prev
      .split(/\s+/)
      .filter(Boolean)
      .forEach(function (c) {
        row.classList.remove(c);
      });

    // Add the new class(es) and remember them
    var next = (msg.new_class || "").trim();
    _prevRowClass[msg.row_key] = next;
    next
      .split(/\s+/)
      .filter(Boolean)
      .forEach(function (c) {
        row.classList.add(c);
      });
  });

  // ── Gear-toggled column visibility ───────────────────────────────────
  // Message shape: { container_id: "history-table-container",
  //                  toggle_key: "showHomeschool", visible: true|false }
  //
  // Toggles a CSS class (e.g. "hide-col-showHomeschool") on the table
  // container div. A matching generated CSS rule hides all cells and
  // headers with class "gear-col-showHomeschool" when the hide class
  // is present. No table re-render needed.

  Shiny.addCustomMessageHandler("rp_toggle_gear_column", function (msg) {
    if (!msg || !msg.container_id || !msg.toggle_key) return;
    var container = document.getElementById(msg.container_id);
    if (!container) return;
    var cls = "hide-col-" + msg.toggle_key;
    container.classList.toggle(cls, !msg.visible);
  });

  // ── Helpers ──────────────────────────────────────────────────────────────

  // CSS.escape polyfill — needed because cell/row keys can contain
  // characters that have special meaning in selectors (e.g. periods,
  // colons). Falls back to a manual escape on older browsers.
  function cssEscape(str) {
    if (window.CSS && typeof CSS.escape === "function") {
      return CSS.escape(str);
    }
    return String(str).replace(/([!"#$%&'()*+,.\/:;<=>?@[\\\]^`{|}~])/g, "\\$1");
  }
})();
