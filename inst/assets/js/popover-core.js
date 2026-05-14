// popover-core.js
// Shared popover lifecycle for all picker widgets in schoolhistory.
// Pure vanilla JS — no framework dependency.
//
// Manages:
//   - Dashed-border trigger (empty) → summary (filled) rendering
//   - Popover open / close with header + close button
//   - Outside-click and Esc-to-close
//   - Focus trap: first interactive element on open, restore on close
//   - One-popover-at-a-time rule (global singleton)
//   - Helper factories: pills, radio groups, textareas, select inputs, footers

(function (global) {
  "use strict";

  // ── Global state ──────────────────────────────────────────────────────
  var activePopover = null;

  document.addEventListener("mousedown", function (e) {
    if (
      activePopover &&
      !activePopover._container.contains(e.target) &&
      !(activePopover._popoverEl && activePopover._popoverEl.contains(e.target))
    ) {
      activePopover.close();
    }
  });

  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape" && activePopover) {
      activePopover.close();
    }
  });

  // ── Popover constructor ───────────────────────────────────────────────
  // config = {
  //   triggerLabel       : string            e.g. "+ Mark attendance"
  //   triggerSubLabel    : string | null      e.g. "(details optional)"
  //   ariaLabel          : string | null      accessible name for trigger
  //   popoverTitle       : string
  //   popoverTitleSub    : string | null
  //   popoverWidth       : string            CSS width, e.g. "min(420px, 90vw)"
  //   popoverClass       : string | null      extra class on overlay
  //   initialValue       : any
  //   isFilled           : function(value) → bool
  //   renderFilled       : function(value, containerEl, popoverInstance)
  //   renderPopover      : function(bodyEl, callbacks, popoverInstance)
  //   onOpen             : function(instance)      optional
  //   onClose            : function(instance)      optional
  //   onChange           : function(value, instance) optional
  // }

  function Popover(container, config) {
    this._container = container;
    this._config = config;
    this._isOpen = false;
    this._value = config.initialValue != null ? config.initialValue : null;
    this._triggerEl = null;
    this._popoverEl = null;
    this._lastFocus = null;
    this._uid = "pv-" + Math.random().toString(36).slice(2, 9);

    this._container.classList.add("pv-container");
    this.render();
  }

  // ── Render orchestrator ───────────────────────────────────────────────

  Popover.prototype.render = function () {
    // Preserve open popover across re-renders (don't clobber it)
    var popoverStillOpen = this._isOpen && this._popoverEl;
    var existing = popoverStillOpen ? this._popoverEl : null;

    this._container.innerHTML = "";

    if (this._isFilled()) {
      this._renderFilled();
    } else {
      this._renderEmpty();
    }

    // Re-attach popover if it was open — keep it in body (portal mode)
    // so it's not affected by ancestor transforms or overflow
    if (existing) {
      document.body.appendChild(existing);
      this._positionPopover();
    }
  };

  Popover.prototype._isFilled = function () {
    if (this._config.isFilled) return this._config.isFilled(this._value);
    return this._value != null;
  };

  // ── Empty state ───────────────────────────────────────────────────────

  Popover.prototype._renderEmpty = function () {
    var self = this;
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "pick-trigger";

    var label = document.createTextNode(this._config.triggerLabel || "+ Select");
    btn.appendChild(label);

    if (this._config.triggerSubLabel) {
      var sub = document.createElement("span");
      sub.className = "pick-trigger-sub";
      sub.textContent = " " + this._config.triggerSubLabel;
      btn.appendChild(sub);
    }

    if (this._config.ariaLabel) {
      btn.setAttribute("aria-label", this._config.ariaLabel);
    }

    btn.setAttribute("aria-haspopup", "dialog");
    btn.setAttribute("aria-expanded", "false");

    btn.addEventListener("click", function () {
      self.open();
    });

    this._triggerEl = btn;
    this._container.appendChild(btn);
  };

  // ── Filled state ──────────────────────────────────────────────────────

  Popover.prototype._renderFilled = function () {
    var self = this;
    var tag = this._config.filledElement || "button";
    var summary = document.createElement(tag);
    if (tag === "button") summary.type = "button";
    summary.className = "pv-summary";
    summary.setAttribute("aria-haspopup", "dialog");
    summary.setAttribute("aria-expanded", "false");

    if (this._config.renderFilled) {
      this._config.renderFilled(this._value, summary, this);
    }

    summary.addEventListener("click", function (e) {
      if (e.target.closest(".pv-action")) return;
      self.open();
    });

    this._triggerEl = summary;
    this._container.appendChild(summary);
  };

  // ── Open / close ──────────────────────────────────────────────────────

  Popover.prototype.open = function () {
    if (this._isOpen) return;

    // One-at-a-time rule
    if (activePopover && activePopover !== this) {
      activePopover.close();
    }

    this._lastFocus = document.activeElement;
    this._isOpen = true;
    activePopover = this;

    if (this._triggerEl) {
      this._triggerEl.setAttribute("aria-expanded", "true");
    }

    this._buildPopover();

    if (this._config.onOpen) this._config.onOpen(this);
  };

  Popover.prototype.close = function () {
    if (!this._isOpen) return;

    this._isOpen = false;
    if (activePopover === this) activePopover = null;

    // Remove scroll/resize listener
    if (this._repositionHandler) {
      window.removeEventListener("scroll", this._repositionHandler, true);
      window.removeEventListener("resize", this._repositionHandler);
      this._repositionHandler = null;
    }

    if (this._popoverEl && this._popoverEl.parentNode) {
      this._popoverEl.parentNode.removeChild(this._popoverEl);
    }
    this._popoverEl = null;

    if (this._triggerEl) {
      this._triggerEl.setAttribute("aria-expanded", "false");
    }

    // Re-render summary (value may have changed while popover was open)
    this.render();

    // Restore focus
    if (this._triggerEl) {
      this._triggerEl.focus();
    }

    if (this._config.onClose) this._config.onClose(this);
  };

  // ── Position the popover relative to trigger (portal mode) ────────────

  Popover.prototype._positionPopover = function () {
    if (!this._popoverEl || !this._container) return;

    // If the container is detached from the DOM, just skip repositioning.
    // The popover lives in document.body (portal mode) and survives table
    // re-renders — the new cell is rebuilt in the same position, so the
    // popover stays visually anchored where the user is looking.
    if (!document.body.contains(this._container)) return;

    var rect = this._container.getBoundingClientRect();
    this._popoverEl.style.top = (rect.bottom + 4) + "px";
    this._popoverEl.style.left = rect.left + "px";
  };

  Popover.prototype._buildPopover = function () {
    var self = this;

    var overlay = document.createElement("div");
    overlay.className = "picker-overlay picker-portal";
    if (this._config.popoverClass) overlay.classList.add(this._config.popoverClass);
    if (this._config.popoverWidth) overlay.style.width = this._config.popoverWidth;
    overlay.setAttribute("role", "dialog");
    overlay.setAttribute("aria-modal", "false");
    overlay.setAttribute("aria-label", this._config.popoverTitle || "Picker");

    var picker = document.createElement("div");
    picker.className = "picker";

    // ── Header
    var hd = document.createElement("div");
    hd.className = "picker-hd";

    var title = document.createElement("span");
    title.className = "picker-title";
    title.textContent = this._config.popoverTitle || "";

    if (this._config.popoverTitleSub) {
      var tsub = document.createElement("span");
      tsub.className = "picker-title-sub";
      tsub.textContent = " " + this._config.popoverTitleSub;
      title.appendChild(tsub);
    }

    var closeBtn = document.createElement("button");
    closeBtn.type = "button";
    closeBtn.className = "x-btn";
    closeBtn.innerHTML = "&times;";
    closeBtn.setAttribute("aria-label", "Close");
    closeBtn.addEventListener("click", function () {
      self.close();
    });

    hd.appendChild(title);
    hd.appendChild(closeBtn);
    picker.appendChild(hd);

    // ── Body
    var body = document.createElement("div");
    body.className = "picker-body";

    if (this._config.renderPopover) {
      this._config.renderPopover(
        body,
        {
          getValue: function () {
            return self._value;
          },
          setValue: function (v) {
            self.setValue(v);
          },
          close: function () {
            self.close();
          },
          clear: function () {
            self.setValue(null);
            self.close();
          },
        },
        self
      );
    }

    picker.appendChild(body);
    overlay.appendChild(picker);

    // Portal: append to body, position via getBoundingClientRect
    document.body.appendChild(overlay);
    this._popoverEl = overlay;
    this._positionPopover();

    // Reposition on scroll/resize so it follows the trigger
    this._repositionHandler = function () {
      self._positionPopover();
    };
    window.addEventListener("scroll", this._repositionHandler, true);
    window.addEventListener("resize", this._repositionHandler);

    // Focus first interactive element inside the body (skip close button)
    requestAnimationFrame(function () {
      var first = body.querySelector(
        "input, select, textarea, button"
      );
      if (first) first.focus();
    });
  };

  // ── Value accessors ───────────────────────────────────────────────────

  Popover.prototype.setValue = function (value, silent) {
    this._value = value;
    if (!silent && this._config.onChange) {
      this._config.onChange(value, this);
    }
  };

  Popover.prototype.getValue = function () {
    return this._value;
  };

  Popover.prototype.isOpen = function () {
    return this._isOpen;
  };

  // ── Static helpers: pill, radio group, textarea, select, footer ──────

  var ICONS = {
    school:
      '<svg class="pill-icon" viewBox="0 0 24 24" width="13" height="13" fill="none" ' +
      'stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" ' +
      'aria-hidden="true"><path d="M3 21h18"/><path d="M5 21V10l7-4 7 4v11"/>' +
      '<path d="M10 21v-5h4v5"/><path d="M12 6V3"/></svg>',

    pencil:
      '<svg class="pill-icon" viewBox="0 0 24 24" width="13" height="13" fill="none" ' +
      'stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" ' +
      'aria-hidden="true"><path d="M16.5 3.5l4 4-12 12-5 1 1-5z"/>' +
      '<path d="M14 6l4 4"/><path d="M5.5 15.5l3 3"/></svg>',
  };

  /**
   * createPill(text, className, iconKey)
   *   text      : label string
   *   className : CSS class(es) for color variant, e.g. "p-excellent"
   *   iconKey   : "school" | "pencil" | null
   */
  function createPill(text, className, iconKey) {
    var pill = document.createElement("span");
    pill.className = "attend-pill" + (className ? " " + className : "");

    if (iconKey && ICONS[iconKey]) {
      pill.innerHTML = ICONS[iconKey];
    }

    pill.appendChild(document.createTextNode(text));
    return pill;
  }

  /**
   * createRadioGroup({ label, name, options, value, onChange })
   */
  function createRadioGroup(config) {
    var group = document.createElement("div");
    group.className = "attend-group";
    group.setAttribute("role", "radiogroup");

    var labelId = config.name + "-label-" + Math.random().toString(36).slice(2, 6);

    var labelEl = document.createElement("div");
    labelEl.className = "attend-label";
    labelEl.id = labelId;
    labelEl.textContent = config.label;
    group.setAttribute("aria-labelledby", labelId);
    group.appendChild(labelEl);

    config.options.forEach(function (opt) {
      var row = document.createElement("label");
      row.className = "radio-row";

      var input = document.createElement("input");
      input.type = "radio";
      input.name = config.name;
      input.value = opt;
      if (config.value === opt) input.checked = true;

      input.addEventListener("change", function () {
        if (config.onChange) config.onChange(opt);
      });

      var dot = document.createElement("span");
      dot.className = "radio-dot";

      var text = document.createElement("span");
      text.textContent = opt;

      row.appendChild(input);
      row.appendChild(dot);
      row.appendChild(text);
      group.appendChild(row);
    });

    return group;
  }

  /**
   * createTextarea({ label, placeholder, value, rows, onChange })
   */
  function createTextarea(config) {
    var group = document.createElement("div");
    group.className = "attend-group";

    if (config.label) {
      var labelEl = document.createElement("div");
      labelEl.className = "attend-label";
      labelEl.textContent = config.label;
      group.appendChild(labelEl);
    }

    var ta = document.createElement("textarea");
    ta.className = "cell-textarea";
    ta.rows = config.rows || 3;
    ta.placeholder = config.placeholder || "";
    ta.value = config.value || "";

    ta.addEventListener("input", function () {
      if (config.onChange) config.onChange(ta.value);
    });

    group.appendChild(ta);
    return group;
  }

  /**
   * createSelectInput({ label, options, value, placeholder, onChange })
   *   options : array of strings or { value, label } objects
   */
  function createSelectInput(config) {
    var group = document.createElement("div");
    group.className = "attend-group";

    if (config.label) {
      var labelEl = document.createElement("div");
      labelEl.className = "attend-label";
      labelEl.textContent = config.label;
      group.appendChild(labelEl);
    }

    var sel = document.createElement("select");
    sel.className = "cell-input";

    // Blank option
    var blank = document.createElement("option");
    blank.value = "";
    blank.textContent = config.placeholder || "\u2014 select \u2014";
    sel.appendChild(blank);

    (config.options || []).forEach(function (opt) {
      var o = document.createElement("option");
      if (typeof opt === "string") {
        o.value = opt;
        o.textContent = opt;
      } else {
        o.value = opt.value;
        o.textContent = opt.label;
      }
      sel.appendChild(o);
    });

    sel.value = config.value || "";
    sel.addEventListener("change", function () {
      if (config.onChange) config.onChange(sel.value);
    });

    group.appendChild(sel);
    return { group: group, select: sel };
  }

  /**
   * createTextInput({ label, placeholder, value, onChange })
   */
  function createTextInput(config) {
    var group = document.createElement("div");
    group.className = "attend-group";

    if (config.label) {
      var labelEl = document.createElement("div");
      labelEl.className = "attend-label";
      labelEl.textContent = config.label;
      group.appendChild(labelEl);
    }

    var input = document.createElement("input");
    input.type = "text";
    input.className = "cell-input";
    input.placeholder = config.placeholder || "";
    input.value = config.value || "";

    input.addEventListener("input", function () {
      if (config.onChange) config.onChange(input.value);
    });

    group.appendChild(input);
    return { group: group, input: input };
  }

  /**
   * createFooter({ clearLabel, clearCallback, doneLabel, doneCallback })
   */
  function createFooter(config) {
    var footer = document.createElement("div");
    footer.className = "picker-footer";

    var clearBtn = document.createElement("button");
    clearBtn.type = "button";
    clearBtn.className = "link-btn link-danger";
    clearBtn.textContent = config.clearLabel || "clear";
    clearBtn.addEventListener("click", config.clearCallback);

    var doneBtn = document.createElement("button");
    doneBtn.type = "button";
    doneBtn.className = "btn-done";
    doneBtn.textContent = config.doneLabel || "Done";
    doneBtn.addEventListener("click", config.doneCallback);

    footer.appendChild(clearBtn);
    footer.appendChild(doneBtn);
    return footer;
  }

  // ── Factory ───────────────────────────────────────────────────────────

  function create(container, config) {
    return new Popover(container, config);
  }

  /**
   * closeActive() — close whichever popover is currently open.
   */
  function closeActive() {
    if (activePopover) activePopover.close();
  }

  /**
   * highlightText(text, query)
   * Returns a DocumentFragment with matched tokens wrapped in <mark class="hl">.
   */
  function highlightText(text, query) {
    var wrap = document.createElement("span");
    if (!query) {
      wrap.textContent = text;
      return wrap;
    }
    var tokens = query
      .toLowerCase()
      .split(/\s+/)
      .filter(Boolean);
    if (!tokens.length) {
      wrap.textContent = text;
      return wrap;
    }
    var escaped = tokens.map(function (t) {
      return t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    });
    var re = new RegExp("(" + escaped.join("|") + ")", "ig");
    var parts = text.split(re);
    parts.forEach(function (p) {
      if (re.test(p)) {
        var mark = document.createElement("mark");
        mark.className = "hl";
        mark.textContent = p;
        wrap.appendChild(mark);
      } else {
        wrap.appendChild(document.createTextNode(p));
      }
      re.lastIndex = 0;
    });
    return wrap;
  }

  // ── Public API ────────────────────────────────────────────────────────

  global.PopoverCore = {
    create: create,
    closeActive: closeActive,
    createPill: createPill,
    createRadioGroup: createRadioGroup,
    createTextarea: createTextarea,
    createTextInput: createTextInput,
    createSelectInput: createSelectInput,
    createFooter: createFooter,
    highlightText: highlightText,
    ICONS: ICONS,
  };
})(window);

// ── Deferred binding helper for dynamic content (reactable, etc.) ────
// Shiny.bindAll() doesn't reliably bind picker elements inside
// htmlwidgets like reactable. This helper manually initializes and
// subscribes each picker, replicating what bindAll should do.
//
// Usage (R side):
//   htmlwidgets::onRender(tbl, "function(el) {
//     setTimeout(function() { window.shBindPickers(el); }, 300);
//   }")
(function () {
  "use strict";

  var _selectors = [
    ".sh-school-picker",
    ".sh-attendance-picker",
    ".sh-homeschool-picker",
    ".sh-notes-input",
    ".sh-gear-wrap",
  ];

  function getSelector() {
    return _selectors.join(", ");
  }

  window.shBindPickers = function (scope) {
    if (typeof Shiny === "undefined") return;
    var scopeEl = scope && scope.nodeType ? scope : document;
    var pickers = scopeEl.querySelectorAll(getSelector());
    if (!pickers.length) return;

    var allBindings = Shiny.inputBindings.getBindings();

    pickers.forEach(function (el) {
      var $el = $(el);
      if ($el.data("shinyInputBinding")) return;

      var id = el.id || el.getAttribute("data-input-id");
      if (!id) return;

      // Find the binding whose find() returns this element
      var binding = null;
      for (var i = 0; i < allBindings.length; i++) {
        var b = allBindings[i].binding;
        try {
          var found = b.find($(el.parentNode));
          for (var j = 0; j < found.length; j++) {
            if (found[j] === el) {
              binding = b;
              break;
            }
          }
        } catch (e) {
          /* skip */
        }
        if (binding) break;
      }
      if (!binding) return;

      // Initialize
      try {
        binding.initialize(el);
      } catch (e) {
        console.error("[shBindPickers] initialize failed for #" + id, e);
        return;
      }

      $el.data("shinyInputBinding", binding);
      $el.addClass("shiny-bound-input");

      // Subscribe: forward value changes to Shiny
      binding.subscribe(el, function () {
        var value = binding.getValue(el);
        Shiny.setInputValue(id, value);
      });

      // Send initial value
      Shiny.setInputValue(id, binding.getValue(el));
    });
  };

  /**
   * Register an additional CSS selector for shBindPickers to scan.
   * Call this when creating custom widgets that need reactable integration.
   * @param {string} selector - CSS selector, e.g. ".my-custom-widget"
   */
  window.shBindPickers.addSelector = function (selector) {
    if (_selectors.indexOf(selector) === -1) {
      _selectors.push(selector);
    }
  };
})();
