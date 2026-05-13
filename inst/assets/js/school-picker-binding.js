// school-picker-binding.js
// Shiny input binding for the school picker.
// Depends on popover-core.js (PopoverCore global).
//
// Server communication protocol:
//   JS → R:  Shiny.setInputValue(ns + "school_search", { query, request_id, source_id })
//   R → JS:  session$sendCustomMessage("sh-search-results", { results, request_id, source_id })
//
// Fill-down action:
//   JS → R:  Shiny.setInputValue(ns + "school_fill_down", { from_grade, school }, { priority: "event" })
//
// Configurable labels (via data attributes):
//   data-trigger-label, data-popover-title, data-search-placeholder,
//   data-empty-hint, data-no-match-hint, data-show-fill-down

(function () {
  "use strict";

  var SEARCH_ICON =
    '<svg class="type-icon" width="14" height="14" viewBox="0 0 14 14" fill="none">' +
    '<circle cx="6" cy="6" r="4.5" stroke="currentColor" stroke-width="1.4"/>' +
    '<path d="M9.5 9.5L13 13" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>' +
    "</svg>";

  // ── Register global search results handler (once) ─────────────────────

  if (!window._shSchoolSearchHandler) {
    window._shSchoolSearchHandler = true;
    Shiny.addCustomMessageHandler("sh-search-results", function (data) {
      var target = document.getElementById(data.source_id);
      if (!target) return;
      var state = $(target).data("sh-search-state");
      if (!state || state.requestId !== data.request_id) return;

      state.results = data.results || [];
      state.loading = false;
      state.highlightIdx = 0;
      renderResults(target);
    });
  }

  // ── Result rendering (called when search results arrive) ──────────────

  function getPopoverEl(el) {
    var popover = $(el).data("sh-popover");
    return popover && popover._popoverEl ? popover._popoverEl : null;
  }

  function renderResults(el) {
    var state = $(el).data("sh-search-state");
    var popoverEl = getPopoverEl(el);
    if (!popoverEl) return;

    var resultsEl = popoverEl.querySelector(".type-results");
    if (!resultsEl) return;

    var shimmer = popoverEl.querySelector(".search-shimmer");
    if (shimmer) shimmer.style.display = state.loading ? "block" : "none";

    resultsEl.innerHTML = "";
    var showNces = el.getAttribute("data-show-nces-id") === "true";
    var emptyHint =
      el.getAttribute("data-empty-hint") ||
      "Type 2+ characters to search 100k+ US schools";
    var noMatchHint =
      el.getAttribute("data-no-match-hint") ||
      "No schools match. Check spelling \u2014 picking is required.";

    if (state.query.length < 2) {
      var hint = document.createElement("div");
      hint.className = "type-empty";
      hint.textContent = emptyHint;
      resultsEl.appendChild(hint);
      return;
    }

    if (state.loading) {
      return;
    }

    if (state.results.length === 0) {
      var noMatch = document.createElement("div");
      noMatch.className = "type-empty";
      noMatch.textContent = noMatchHint;
      resultsEl.appendChild(noMatch);
      return;
    }

    state.results.forEach(function (school, i) {
      var row = document.createElement("button");
      row.type = "button";
      row.className = "result-row" + (i === state.highlightIdx ? " is-active" : "");
      row.setAttribute("data-index", i);

      var main = document.createElement("div");
      main.className = "result-main";
      main.appendChild(PopoverCore.highlightText(school.school_name || school.name || "", state.query));
      row.appendChild(main);

      var sub = document.createElement("div");
      sub.className = "result-sub";

      var typePill = document.createElement("span");
      var schoolType = (school.type || school.school_type || "Public").toLowerCase();
      typePill.className = "type-pill type-" + schoolType;
      typePill.textContent = schoolType === "private" ? "PRIVATE" : "PUBLIC";
      sub.appendChild(typePill);

      sub.appendChild(
        PopoverCore.highlightText(
          (school.city || "") + ", " + (school.state_abbr || school.state || ""),
          state.query
        )
      );

      var dot1 = document.createElement("span");
      dot1.className = "dot";
      dot1.textContent = "\u00b7";
      sub.appendChild(dot1);

      sub.appendChild(
        PopoverCore.highlightText(school.district_name || school.district || "", state.query)
      );

      if (showNces) {
        var dot2 = document.createElement("span");
        dot2.className = "dot";
        dot2.textContent = "\u00b7";
        sub.appendChild(dot2);

        var nces = document.createElement("span");
        nces.className = "ncesid";
        nces.textContent = school.school_id || school.id || "";
        sub.appendChild(nces);
      }

      row.appendChild(sub);

      row.addEventListener("mouseenter", function () {
        state.highlightIdx = i;
        updateHighlight(resultsEl, i);
      });

      row.addEventListener("click", function () {
        selectSchool(el, school);
      });

      resultsEl.appendChild(row);
    });
  }

  function updateHighlight(resultsEl, idx) {
    var rows = resultsEl.querySelectorAll(".result-row");
    rows.forEach(function (r, i) {
      r.classList.toggle("is-active", i === idx);
    });
  }

  function selectSchool(el, school) {
    var popover = $(el).data("sh-popover");
    if (popover) {
      popover.setValue(normalizeSchool(school));
      popover.close();
    }
  }

  // ── Normalize school object (handle both R column names and JS names) ─

  function normalizeSchool(s) {
    return {
      id: s.school_id || s.id || null,
      name: s.school_name || s.name || "",
      district: s.district_name || s.district || "",
      city: s.city || "",
      state: s.state_abbr || s.state || "",
      type: s.type || s.school_type || "Public",
      low_grade: s.low_grade || s.low || null,
      high_grade: s.high_grade || s.high || null,
    };
  }

  // ── Debounced search ──────────────────────────────────────────────────

  function triggerSearch(el) {
    var state = $(el).data("sh-search-state");
    var ns = el.getAttribute("data-ns") || "";

    clearTimeout(state.debounceTimer);
    state.debounceTimer = setTimeout(function () {
      if (state.query.length < 2) {
        state.results = [];
        state.loading = false;
        renderResults(el);
        return;
      }

      state.requestId = Date.now() + "-" + Math.random().toString(36).slice(2, 8);
      state.loading = true;
      renderResults(el);

      Shiny.setInputValue(
        ns + "school_search",
        {
          query: state.query,
          request_id: state.requestId,
          source_id: el.id,
        },
        { priority: "event" }
      );
    }, 200);
  }

  // ── Shiny input binding ───────────────────────────────────────────────

  var SchoolPickerBinding = new Shiny.InputBinding();

  $.extend(SchoolPickerBinding, {
    find: function (scope) {
      return $(scope).find(".sh-school-picker");
    },

    initialize: function (el) {
      var initialRaw = el.getAttribute("data-initial-value");
      var initial = null;
      try {
        var parsed = initialRaw ? JSON.parse(initialRaw) : null;
        if (parsed && (parsed.id || parsed.name)) initial = parsed;
      } catch (e) {
        initial = null;
      }

      var gradeLabel = el.getAttribute("data-grade-label") || "";
      var gradeKey = el.getAttribute("data-grade-key") || "";
      var isHomeschool = el.getAttribute("data-homeschool") === "true";
      var ns = el.getAttribute("data-ns") || "";

      // Read configurable labels
      var triggerLabel =
        el.getAttribute("data-trigger-label") || "+ Pick school";
      var popoverTitle =
        el.getAttribute("data-popover-title") || "Find school";
      var searchPlaceholder =
        el.getAttribute("data-search-placeholder") ||
        "Search by school name, city, district\u2026";
      var showFillDown =
        el.getAttribute("data-show-fill-down") !== "false";

      // Search state (persists while popover is open)
      $(el).data("sh-search-state", {
        query: "",
        requestId: null,
        results: [],
        highlightIdx: 0,
        loading: false,
        debounceTimer: null,
      });

      var popover = PopoverCore.create(el, {
        triggerLabel: triggerLabel,
        ariaLabel: gradeLabel
          ? triggerLabel.replace(/^\+\s*/, "") + " for " + gradeLabel
          : triggerLabel.replace(/^\+\s*/, ""),
        popoverTitle: popoverTitle,
        popoverWidth: "min(560px, 90vw)",
        filledElement: "div",
        initialValue: initial,

        isFilled: function (v) {
          return v !== null && v !== undefined && !!(v.id || v.name);
        },

        // ── Filled card: name, NCES ID, type pill, city/state, district, actions
        renderFilled: function (v, container) {
          var showNces = el.getAttribute("data-show-nces-id") === "true";

          var card = document.createElement("div");
          card.className = "picked-card";

          // Line 1: name + NCES ID
          var main = document.createElement("div");
          main.className = "picked-main";

          var nameEl = document.createElement("strong");
          nameEl.textContent = v.name || "";
          main.appendChild(nameEl);

          if (showNces && v.id) {
            var ncesEl = document.createElement("span");
            ncesEl.className = "ncesid";
            var ncesLabel = document.createElement("span");
            ncesLabel.className = "ncesid-label";
            ncesLabel.textContent = "NCES ID: ";
            ncesEl.appendChild(ncesLabel);
            ncesEl.appendChild(document.createTextNode(v.id));
            main.appendChild(ncesEl);
          }
          card.appendChild(main);

          // Line 2: type pill + city, state · district
          var sub = document.createElement("div");
          sub.className = "picked-sub";

          var typePill = document.createElement("span");
          var schoolType = (v.type || "Public").toLowerCase();
          typePill.className = "type-pill type-" + schoolType;
          typePill.textContent = schoolType === "private" ? "PRIVATE" : "PUBLIC";
          sub.appendChild(typePill);
          sub.appendChild(document.createTextNode((v.city || "") + ", " + (v.state || "")));

          if (v.district) {
            var dot = document.createElement("span");
            dot.className = "dot";
            dot.textContent = " \u00b7 ";
            sub.appendChild(dot);
            sub.appendChild(document.createTextNode(v.district));
          }
          card.appendChild(sub);

          // Line 3: action links
          var actions = document.createElement("div");
          actions.className = "picked-actions";

          var changeBtn = document.createElement("button");
          changeBtn.type = "button";
          changeBtn.className = "link-btn link-change pv-action";
          changeBtn.textContent = "change";
          changeBtn.addEventListener("click", function (e) {
            e.stopPropagation();
            popover.open();
          });
          actions.appendChild(changeBtn);

          if (showFillDown) {
            actions.appendChild(makeDot());

            var fillBtn = document.createElement("button");
            fillBtn.type = "button";
            fillBtn.className = "link-btn pv-action";
            fillBtn.textContent = "fill \u2193";
            fillBtn.title = "Apply this school to all later empty grades";
            fillBtn.addEventListener("click", function (e) {
              e.stopPropagation();
              Shiny.setInputValue(
                ns + "school_fill_down",
                { from_grade: gradeKey, school: v },
                { priority: "event" }
              );
            });
            actions.appendChild(fillBtn);
          }

          actions.appendChild(makeDot());

          var clearBtn = document.createElement("button");
          clearBtn.type = "button";
          clearBtn.className = "link-btn link-danger pv-action";
          clearBtn.textContent = "clear";
          clearBtn.addEventListener("click", function (e) {
            e.stopPropagation();
            popover.setValue(null);
            popover.render();
            $(el).trigger("change");
          });
          actions.appendChild(clearBtn);

          card.appendChild(actions);
          container.appendChild(card);
        },

        // ── Popover: typeahead search
        renderPopover: function (body, callbacks) {
          body.style.padding = "0";
          body.style.maxHeight = "none";
          body.style.overflow = "visible";

          var state = $(el).data("sh-search-state");
          state.query = "";
          state.results = [];
          state.highlightIdx = 0;
          state.loading = false;

          // Search input area
          var inputWrap = document.createElement("div");
          inputWrap.className = "type-input-wrap";
          inputWrap.innerHTML = SEARCH_ICON;

          var input = document.createElement("input");
          input.type = "text";
          input.className = "type-input";
          input.placeholder = searchPlaceholder;
          input.setAttribute("aria-label", popoverTitle);

          var kbdHint = document.createElement("span");
          kbdHint.className = "kbd-hint";
          kbdHint.textContent = "\u2191\u2193 \u21B5";

          input.addEventListener("input", function () {
            state.query = input.value;
            triggerSearch(el);
          });

          input.addEventListener("keydown", function (e) {
            if (e.key === "ArrowDown") {
              e.preventDefault();
              state.highlightIdx = Math.min(
                state.highlightIdx + 1,
                state.results.length - 1
              );
              var pEl = getPopoverEl(el);
              var resultsEl = pEl && pEl.querySelector(".type-results");
              if (resultsEl) updateHighlight(resultsEl, state.highlightIdx);
              scrollToActive(resultsEl);
            } else if (e.key === "ArrowUp") {
              e.preventDefault();
              state.highlightIdx = Math.max(state.highlightIdx - 1, 0);
              var pEl2 = getPopoverEl(el);
              var resultsEl2 = pEl2 && pEl2.querySelector(".type-results");
              if (resultsEl2) updateHighlight(resultsEl2, state.highlightIdx);
              scrollToActive(resultsEl2);
            } else if (e.key === "Enter") {
              e.preventDefault();
              if (state.results[state.highlightIdx]) {
                selectSchool(el, state.results[state.highlightIdx]);
              }
            }
          });

          inputWrap.appendChild(input);
          inputWrap.appendChild(kbdHint);
          body.appendChild(inputWrap);

          // Shimmer (loading indicator)
          var shimmer = document.createElement("div");
          shimmer.className = "search-shimmer";
          shimmer.style.display = "none";
          body.appendChild(shimmer);

          // Results container
          var results = document.createElement("div");
          results.className = "type-results";
          body.appendChild(results);

          // Initial empty state
          renderResults(el);

          // Focus search input
          requestAnimationFrame(function () {
            input.focus();
          });
        },

        onChange: function () {
          $(el).trigger("change");
        },

        onClose: function () {
          var state = $(el).data("sh-search-state");
          if (state) {
            clearTimeout(state.debounceTimer);
            state.query = "";
            state.results = [];
            state.loading = false;
          }
        },
      });

      $(el).data("sh-popover", popover);
    },

    getValue: function (el) {
      var popover = $(el).data("sh-popover");
      if (!popover) return null;
      var v = popover.getValue();
      if (!v || !v.name) return null;
      return v;
    },

    setValue: function (el, value) {
      var popover = $(el).data("sh-popover");
      if (popover) {
        popover.setValue(value || null, true);
        popover.render();
      }
    },

    subscribe: function (el, callback) {
      $(el).on("change.sh-school", function () {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".sh-school");
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
        $(el).trigger("change");
      }
      if (data.hasOwnProperty("showNcesId")) {
        el.setAttribute("data-show-nces-id", data.showNcesId ? "true" : "false");
        var popover = $(el).data("sh-popover");
        if (popover && !popover.isOpen()) popover.render();
      }
      if (data.hasOwnProperty("homeschool")) {
        el.setAttribute("data-homeschool", data.homeschool ? "true" : "false");
      }
    },
  });

  Shiny.inputBindings.register(
    SchoolPickerBinding,
    "schoolhistory.schoolPicker"
  );

  // ── Helpers ───────────────────────────────────────────────────────────

  function makeDot() {
    var d = document.createElement("span");
    d.className = "dot";
    d.textContent = " \u00b7 ";
    return d;
  }

  function scrollToActive(container) {
    if (!container) return;
    var active = container.querySelector(".result-row.is-active");
    if (active) {
      active.scrollIntoView({ block: "nearest" });
    }
  }
})();
