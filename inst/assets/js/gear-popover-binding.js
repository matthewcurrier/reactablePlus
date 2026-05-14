// gear-popover-binding.js
// Shiny input binding for the gear settings popover.
// Configurable toggles, returns a named object of boolean values.

(function () {
  "use strict";

  var GEAR_SVG =
    '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" ' +
    'stroke-linecap="round" stroke-linejoin="round">' +
    '<circle cx="12" cy="12" r="3"/>' +
    '<path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 2.83l-.06-.06' +
    "a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 1 1-4 0v-.09" +
    "a1.65 1.65 0 0 0-1-1.51 1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 1 1-2.83-2.83" +
    "l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 1 1 0-4h.09" +
    "a1.65 1.65 0 0 0 1.51-1 1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 1 1 2.83-2.83" +
    "l.06.06a1.65 1.65 0 0 0 1.82.33h0a1.65 1.65 0 0 0 1-1.51V3a2 2 0 1 1 4 0v.09" +
    "a1.65 1.65 0 0 0 1 1.51h0a1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 1 1 2.83 2.83" +
    'l-.06.06a1.65 1.65 0 0 0-.33 1.82v0a1.65 1.65 0 0 0 1.51 1H21a2 2 0 1 1 0 4h-.09' +
    'a1.65 1.65 0 0 0-1.51 1z"/>' +
    "</svg>";

  var GearPopoverBinding = new Shiny.InputBinding();

  $.extend(GearPopoverBinding, {
    find: function (scope) {
      return $(scope).find(".sh-gear-wrap");
    },

    initialize: function (el) {
      var togglesRaw = el.getAttribute("data-toggles");
      var toggles = [];
      try {
        toggles = JSON.parse(togglesRaw) || [];
      } catch (e) {
        toggles = [];
      }

      // Current values
      var values = {};
      toggles.forEach(function (t) {
        values[t.key] = t.value;
      });

      var isOpen = false;
      var popoverEl = null;

      // ── Build the gear button
      var btn = document.createElement("button");
      btn.type = "button";
      btn.className = "gear-btn";
      btn.innerHTML = GEAR_SVG;
      btn.setAttribute("aria-label", "Table settings");
      btn.setAttribute("aria-haspopup", "true");
      btn.setAttribute("aria-expanded", "false");
      el.appendChild(btn);

      // ── Open / close
      var repositionHandler = null;

      function positionPopover() {
        if (!popoverEl) return;
        var rect = btn.getBoundingClientRect();
        popoverEl.style.top = (rect.bottom + 6) + "px";
        popoverEl.style.left = (rect.right - popoverEl.offsetWidth) + "px";
      }

      function openPopover() {
        if (isOpen) return;

        // Close any PopoverCore popover
        if (window.PopoverCore && PopoverCore.closeActive) {
          PopoverCore.closeActive();
        }

        isOpen = true;
        btn.classList.add("is-on");
        btn.setAttribute("aria-expanded", "true");
        buildPopover();

        // Reposition on scroll/resize so it follows the gear button
        repositionHandler = function () { positionPopover(); };
        window.addEventListener("scroll", repositionHandler, true);
        window.addEventListener("resize", repositionHandler);
      }

      function closePopover() {
        if (!isOpen) return;
        isOpen = false;
        btn.classList.remove("is-on");
        btn.setAttribute("aria-expanded", "false");
        if (repositionHandler) {
          window.removeEventListener("scroll", repositionHandler, true);
          window.removeEventListener("resize", repositionHandler);
          repositionHandler = null;
        }
        if (popoverEl && popoverEl.parentNode) {
          popoverEl.parentNode.removeChild(popoverEl);
        }
        popoverEl = null;
      }

      btn.addEventListener("click", function () {
        if (isOpen) closePopover();
        else openPopover();
      });

      // Outside click closes — check both the gear wrap AND the portal popover
      document.addEventListener("mousedown", function (e) {
        if (
          isOpen &&
          !el.contains(e.target) &&
          !(popoverEl && popoverEl.contains(e.target))
        ) {
          closePopover();
        }
      });

      // Esc closes
      document.addEventListener("keydown", function (e) {
        if (e.key === "Escape" && isOpen) {
          closePopover();
          btn.focus();
        }
      });

      // ── Build popover content
      function buildPopover() {
        var pop = document.createElement("div");
        pop.className = "gear-popover";
        pop.setAttribute("role", "menu");

        // Section header
        var hd = document.createElement("div");
        hd.className = "gear-popover-hd";
        hd.textContent = "Display";
        pop.appendChild(hd);

        // Toggles
        var options = document.createElement("div");
        options.className = "gear-options";

        toggles.forEach(function (t) {
          var row = document.createElement("div");
          row.className = "gear-toggle";

          var body = document.createElement("div");
          body.className = "gear-toggle-body";

          var title = document.createElement("span");
          title.className = "gear-opt-title";
          title.textContent = t.label;
          body.appendChild(title);

          if (t.desc) {
            var desc = document.createElement("span");
            desc.className = "gear-opt-desc";
            desc.textContent = t.desc;
            body.appendChild(desc);
          }

          var sw = document.createElement("span");
          sw.className = "gear-switch" + (values[t.key] ? " is-on" : "");

          var input = document.createElement("input");
          input.type = "checkbox";
          input.checked = !!values[t.key];

          var track = document.createElement("span");

          sw.appendChild(input);
          sw.appendChild(track);

          // Toggle on click (entire row or switch)
          function doToggle() {
            values[t.key] = !values[t.key];
            input.checked = values[t.key];
            sw.classList.toggle("is-on", values[t.key]);
            $(el).trigger("change");
          }

          sw.addEventListener("click", function (e) {
            e.stopPropagation();
            doToggle();
          });

          row.addEventListener("click", function () {
            doToggle();
          });

          row.appendChild(body);
          row.appendChild(sw);
          options.appendChild(row);
        });

        pop.appendChild(options);

        // Portal mode: append to body so the popover escapes any
        // ancestor overflow:hidden (e.g. bslib card)
        pop.classList.add("gear-popover-portal");
        document.body.appendChild(pop);
        popoverEl = pop;
        positionPopover();
      }

      // Store references
      $(el).data("sh-gear", {
        getValues: function () { return $.extend({}, values); },
        setValues: function (v) {
          values = $.extend(values, v);
        },
      });
    },

    getValue: function (el) {
      var gear = $(el).data("sh-gear");
      return gear ? gear.getValues() : null;
    },

    setValue: function (el, value) {
      var gear = $(el).data("sh-gear");
      if (gear && value) gear.setValues(value);
    },

    subscribe: function (el, callback) {
      $(el).on("change.sh-gear", function () {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".sh-gear");
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
        $(el).trigger("change");
      }
    },
  });

  Shiny.inputBindings.register(
    GearPopoverBinding,
    "schoolhistory.gearPopover"
  );
})();
