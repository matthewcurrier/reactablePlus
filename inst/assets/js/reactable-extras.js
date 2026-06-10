// reactable-extras.js
// Cell-level editable inputs for reactable — the *_extra family.
//
// Value-return contract (per-column aggregate):
//   Each *_extra column reports under a single Shiny input identified by
//   the cell's data-input-id. The value is a JS object keyed by the
//   reactable row index (data-row) -> coerced value, which arrives in R
//   as a named list, e.g. input$comment == list(`1` = "...", `2` = "...").
//
// The authoritative values live in an in-memory store (NOT the DOM), so
// they survive reactable's sort / filter / paginate re-renders. On each
// repaint the store WINS: edited values are re-applied to the freshly
// painted cells so they do not visually revert.

(function () {
  "use strict";

  // store[inputId][row] = coerced value
  var store = {};

  // Type registry: a data-rp-type may register read(el)->value and
  // write(el, value) handlers, letting non-input widgets (e.g. popover
  // pickers whose value lives in a JS instance, not a DOM input.value)
  // participate in the store. Built-in primitive types are handled inline.
  var typeRegistry = {};

  function coerce(type, el) {
    var reg = typeRegistry[type];
    if (reg && reg.read) {
      return reg.read(el);
    }
    if (type === "numeric") {
      return el.value === "" ? null : parseFloat(el.value);
    }
    if (type === "checkbox") {
      return !!el.checked;
    }
    if (type === "toggle") {
      return el.getAttribute("data-rp-value") === "true";
    }
    // text, date, dropdown → string
    return el.value;
  }

  function applyToEl(type, el, value) {
    var reg = typeRegistry[type];
    if (reg && reg.write) {
      reg.write(el, value);
      return;
    }
    if (type === "checkbox") {
      el.checked = !!value;
    } else if (type === "toggle") {
      var on = !!value;
      el.setAttribute("data-rp-value", on ? "true" : "false");
      el.setAttribute("aria-pressed", on ? "true" : "false");
      el.textContent = on
        ? el.getAttribute("data-on-label") || "On"
        : el.getAttribute("data-off-label") || "Off";
    } else {
      el.value = value == null ? "" : value;
    }
  }

  function push(inputId) {
    if (typeof Shiny === "undefined" || !Shiny.setInputValue) return;
    Shiny.setInputValue(inputId, store[inputId] || {}, { priority: "event" });
  }

  // Public API for custom widgets (e.g. popover pickers) to plug into the
  // store: register read/write handlers for a data-rp-type, and report a
  // value change so it flows to input[[col]] like any other *_extra cell.
  window.rpExtras = window.rpExtras || {};
  window.rpExtras.registerType = function (type, handlers) {
    typeRegistry[type] = handlers || {};
  };
  window.rpExtras.report = function (el) {
    if (!el) return;
    var target = el.closest ? el.closest(".rp-extra") || el : el;
    recordEdit(target);
  };

  // ── Built-in "picker" type ─────────────────────────────────────────────
  // The popover pickers (school / attendance / homeschool) keep their value
  // in a JS Popover instance reachable via $(innerEl).data("sh-popover"),
  // not a DOM input.value. The config table module wraps each picker cell in
  // an .rp-extra div carrying data-rp-type="picker", so registering read/write
  // here lets a picker participate in the per-column store like any primitive.
  function pickerInner(el) {
    return el.querySelector(
      ".sh-school-picker, .sh-attendance-picker, .sh-homeschool-picker"
    );
  }
  function pickerPopover(inner) {
    if (!inner || typeof $ === "undefined") return null;
    return $(inner).data("sh-popover") || null;
  }
  function parseInitialValue(inner) {
    var raw = inner && inner.getAttribute("data-initial-value");
    if (!raw) return null;
    try {
      return JSON.parse(raw);
    } catch (e) {
      return null;
    }
  }
  window.rpExtras.registerType("picker", {
    // Read the live popover value. Before the picker binding initializes
    // (reconcile can fire first — both run on a render MutationObserver),
    // fall back to the inner element's data-initial-value so the store
    // seeds the authoritative value instead of an empty picker — which
    // would otherwise push a clear back to the server.
    read: function (el) {
      var inner = pickerInner(el);
      if (!inner) return null;
      var pop = pickerPopover(inner);
      if (pop && typeof pop.getValue === "function") {
        return pop.getValue();
      }
      return parseInitialValue(inner);
    },
    // Apply a server/store value to the picker. Silent setValue avoids
    // re-emitting a change (which would echo back into the store). If the
    // popover isn't initialized yet, stash into data-initial-value so the
    // binding's initialize() seeds from it.
    write: function (el, value) {
      var inner = pickerInner(el);
      if (!inner) return;
      var pop = pickerPopover(inner);
      if (pop) {
        pop.setValue(value == null ? null : value, true);
        if (typeof pop.render === "function") pop.render();
      } else {
        inner.setAttribute(
          "data-initial-value",
          value == null ? "" : JSON.stringify(value)
        );
      }
    },
  });

  function readMeta(el) {
    return {
      inputId: el.getAttribute("data-input-id"),
      row: el.getAttribute("data-row"),
      type: el.getAttribute("data-rp-type"),
    };
  }

  // ── Reconcile ──────────────────────────────────────────────────────────
  // For every .rp-extra cell currently under `root`:
  //   - if the store already holds this (inputId, row), the store WINS and
  //     its value is re-applied to the element (edits survive repaint);
  //   - otherwise this is the first sighting, so seed the store from the
  //     rendered value.
  // Then push every touched column input so input$<col> is populated.
  function reconcile(root) {
    if (!root || !root.querySelectorAll) return;
    var els = root.querySelectorAll(".rp-extra");
    var touched = {};
    for (var i = 0; i < els.length; i++) {
      var el = els[i];
      var m = readMeta(el);
      if (!m.inputId || m.row == null) continue;
      if (!store[m.inputId]) store[m.inputId] = {};
      if (Object.prototype.hasOwnProperty.call(store[m.inputId], m.row)) {
        applyToEl(m.type, el, store[m.inputId][m.row]); // store wins
      } else {
        store[m.inputId][m.row] = coerce(m.type, el); // seed
      }
      touched[m.inputId] = true;
    }
    Object.keys(touched).forEach(push);
  }
  window.rpExtrasReconcile = reconcile;

  // ── Edits ──────────────────────────────────────────────────────────────
  // Record the current value of an .rp-extra element into the store and
  // report it to the server. Shared by the input/change listeners and the
  // toggle click handler.
  function recordEdit(el) {
    var m = readMeta(el);
    if (!m.inputId || m.row == null) return;
    if (!store[m.inputId]) store[m.inputId] = {};
    store[m.inputId][m.row] = coerce(m.type, el);
    push(m.inputId);
  }

  // One delegated listener for the whole family. Catches events bubbling
  // from any .rp-extra control regardless of reactable mount/unmount.
  function onEdit(e) {
    var el = e.target;
    if (!el || !el.closest) return;
    el = el.closest(".rp-extra");
    if (el) recordEdit(el);
  }
  document.addEventListener("input", onEdit, true);
  document.addEventListener("change", onEdit, true);

  // Toggle buttons own their state in data-rp-value (no native value/checked),
  // so a click flips the boolean, relabels the button, and records the edit.
  document.addEventListener(
    "click",
    function (e) {
      var el = e.target;
      if (!el || !el.closest) return;
      el = el.closest(".rp-extra[data-rp-type='toggle']");
      if (!el || el.disabled) return;
      applyToEl("toggle", el, el.getAttribute("data-rp-value") !== "true");
      recordEdit(el);
    },
    true
  );

  // Registered-type widgets (e.g. popover pickers) report value changes via
  // a jQuery `change` event ($(el).trigger("change")), which the native
  // addEventListener listeners above do NOT observe — jQuery-triggered events
  // don't reach native handlers. A jQuery-delegated listener does, so route
  // those through the store here. Scoped to types in the registry so native
  // primitive changes (already handled above) are not recorded twice.
  if (typeof $ !== "undefined") {
    $(document).on("change", ".rp-extra", function () {
      if (typeRegistry[this.getAttribute("data-rp-type")]) recordEdit(this);
    });
  }

  // Find the live .rp-extra element for a given (inputId, row), or null.
  // Iterates attributes rather than building a selector so arbitrary
  // namespaced input ids (with hyphens etc.) are matched safely.
  function findEl(inputId, row) {
    var nodes = document.querySelectorAll(".rp-extra");
    var r = String(row);
    for (var i = 0; i < nodes.length; i++) {
      if (
        nodes[i].getAttribute("data-input-id") === inputId &&
        nodes[i].getAttribute("data-row") === r
      ) {
        return nodes[i];
      }
    }
    return null;
  }

  // ── Server → store push ────────────────────────────────────────────────
  // Lets R set authoritative values into the store (and the live DOM) so
  // reconcile KEEPS them rather than fighting a server-side re-render. This
  // is what lets a server-authoritative caller (e.g. the config table
  // module) drive cell values — mutual-exclusion clears, fill-down, reset —
  // without the store overruling them. msg = { inputId, values: {row: v} }.
  var messageHandlersRegistered = false;
  function registerMessageHandlers() {
    if (messageHandlersRegistered) return;
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) return;
    messageHandlersRegistered = true;

    Shiny.addCustomMessageHandler("rp_extras_set", function (msg) {
      if (!msg || !msg.inputId) return;
      var inputId = msg.inputId;
      var values = msg.values || {};
      if (!store[inputId]) store[inputId] = {};
      Object.keys(values).forEach(function (row) {
        store[inputId][row] = values[row];
        var el = findEl(inputId, row);
        if (el) applyToEl(el.getAttribute("data-rp-type"), el, values[row]);
      });
      push(inputId);
    });
  }

  // ── Render hook ────────────────────────────────────────────────────────
  // Reconcile once the table paints, then keep reconciling on every
  // repaint. Unlike shBindPickersOnReady (which disconnects after the
  // first bind), this observer STAYS connected: reactable re-renders cells
  // from its own data on sort / filter / paginate, which would otherwise
  // wipe edits visually — reconcile re-applies them each time.
  window.rpExtrasOnReady = function (el, fallbackMs) {
    if (typeof Shiny === "undefined") return;
    if (!el || !el.nodeType) return;

    // Shiny is up by the time the render hook fires — safe to register the
    // server → store message handler here (idempotent).
    registerMessageHandlers();

    reconcile(el); // in case cells are already present at hook time

    if (typeof MutationObserver !== "undefined") {
      var observer = new MutationObserver(function (mutations) {
        for (var mi = 0; mi < mutations.length; mi++) {
          var added = mutations[mi].addedNodes;
          for (var ni = 0; ni < added.length; ni++) {
            var node = added[ni];
            if (node.nodeType !== 1) continue; // elements only
            if (
              (node.matches && node.matches(".rp-extra")) ||
              (node.querySelector && node.querySelector(".rp-extra"))
            ) {
              reconcile(el);
              break; // one reconcile covers the whole batch
            }
          }
        }
      });
      observer.observe(el, { childList: true, subtree: true });
    }

    // Safety-net for empty / slow first paint where the observer may not
    // have fired yet.
    setTimeout(function () {
      reconcile(el);
    }, fallbackMs || 600);
  };
})();
