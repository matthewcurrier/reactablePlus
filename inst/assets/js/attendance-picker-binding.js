// attendance-picker-binding.js
// Shiny input binding for the attendance picker.
// Depends on popover-core.js (PopoverCore global).
//
// Sections config (via data-sections JSON):
//   [{ key, label, levels, pill_prefix?, pill_icon?, impact_display? }, ...]
//
// When no data-sections is set, falls back to the original hardcoded
// school-history sections for backward compatibility.

(function () {
  "use strict";

  // ── Default sections (backward compat) ────────────────────────────────

  var DEFAULT_SECTIONS = [
    {
      key: "school",
      label: "School Attendance",
      levels: ["Excellent", "Good", "Satisfactory", "Poor"],
    },
    {
      key: "class_",
      label: "Class Attendance",
      levels: ["Excellent", "Good", "Satisfactory", "Poor"],
      pill_prefix: "Class: ",
      pill_icon: "pencil",
    },
    {
      key: "impacts",
      label: "Does student attendance negatively impact academics?",
      levels: ["Yes", "No"],
      impact_display: {
        Yes: "\u26A0 impacts academics",
        No: "no academic impact",
      },
    },
  ];

  // ── Parse sections from element ───────────────────────────────────────

  function getSections(el) {
    var raw = el.getAttribute("data-sections");
    if (raw) {
      try {
        var parsed = JSON.parse(raw);
        if (Array.isArray(parsed) && parsed.length > 0) return parsed;
      } catch (e) {
        // fall through to defaults
      }
    }
    return DEFAULT_SECTIONS;
  }

  // Map level to CSS class suffix
  function levelClass(level) {
    if (!level) return "";
    return "p-" + level.toLowerCase().replace(/\s+/g, "-");
  }

  // ── Shiny input binding ───────────────────────────────────────────────

  var AttendancePickerBinding = new Shiny.InputBinding();

  $.extend(AttendancePickerBinding, {
    find: function (scope) {
      return $(scope).find(".sh-attendance-picker");
    },

    initialize: function (el) {
      var initialRaw = el.getAttribute("data-initial-value");
      var initial = {};
      try {
        initial = initialRaw ? JSON.parse(initialRaw) : {};
      } catch (e) {
        initial = {};
      }

      var gradeLabel = el.getAttribute("data-grade-label") || "";
      var sections = getSections(el);
      var triggerLabel =
        el.getAttribute("data-trigger-label") || "+ Mark attendance";
      var popoverTitle =
        el.getAttribute("data-popover-title") || "Attendance";
      var showNotes = el.getAttribute("data-show-notes") !== "false";
      var notesPlaceholder =
        el.getAttribute("data-notes-placeholder") ||
        "Absences, tardies, context for this school year\u2026";

      // Build mutable value object from sections
      var currentValue = {};
      sections.forEach(function (sec) {
        currentValue[sec.key] = initial[sec.key] || null;
      });
      if (showNotes) {
        currentValue.notes = initial.notes || "";
      }

      var popover = PopoverCore.create(el, {
        triggerLabel: triggerLabel,
        ariaLabel: gradeLabel
          ? triggerLabel.replace(/^\+\s*/, "") + " for " + gradeLabel
          : triggerLabel.replace(/^\+\s*/, ""),
        popoverTitle: popoverTitle,
        popoverWidth: "min(420px, 90vw)",
        popoverClass: "attend-overlay",
        initialValue: currentValue,

        isFilled: function (v) {
          if (!v) return false;
          for (var i = 0; i < sections.length; i++) {
            if (v[sections[i].key]) return true;
          }
          return false;
        },

        // ── Filled summary: pills + impact sub-lines
        renderFilled: function (v, container) {
          var line = document.createElement("div");
          line.className = "attend-line";
          var hasSub = false;

          sections.forEach(function (sec, idx) {
            var val = v[sec.key];
            if (!val) return;

            // Sections with impact_display render as sub-lines, not pills
            if (sec.impact_display) {
              hasSub = true;
              return;
            }

            var pillText = (sec.pill_prefix || "") + val;
            var pillClass = levelClass(val);
            if (sec.pill_prefix) pillClass = "class " + pillClass;
            var icon = sec.pill_icon || (idx === 0 ? "school" : null);
            line.appendChild(
              PopoverCore.createPill(pillText, pillClass, icon)
            );
          });

          if (line.childNodes.length > 0) {
            container.appendChild(line);
          }

          // Render impact_display sections as sub-lines
          sections.forEach(function (sec) {
            var val = v[sec.key];
            if (!val || !sec.impact_display) return;

            var sub = document.createElement("div");
            sub.className = "attend-sub";
            sub.textContent = sec.impact_display[val] || val;
            container.appendChild(sub);
          });
        },

        // ── Popover body: radio groups + notes + footer
        renderPopover: function (body, callbacks) {
          var val = callbacks.getValue();

          // Build mutable draft from sections
          var draft = {};
          sections.forEach(function (sec) {
            draft[sec.key] = val ? val[sec.key] || null : null;
          });
          if (showNotes) {
            draft.notes = val ? val.notes || "" : "";
          }

          function commit() {
            callbacks.setValue(Object.assign({}, draft));
          }

          // Unique prefix for radio names
          var uid = el.id || Math.random().toString(36).slice(2, 8);

          // Build a radio group for each section
          sections.forEach(function (sec, idx) {
            body.appendChild(
              PopoverCore.createRadioGroup({
                label: sec.label,
                name: sec.key + "-" + uid,
                options: sec.levels,
                value: draft[sec.key],
                onChange: function (v) {
                  draft[sec.key] = v;
                  commit();
                },
              })
            );
          });

          // Notes (optional)
          if (showNotes) {
            body.appendChild(
              PopoverCore.createTextarea({
                label: "Notes",
                placeholder: notesPlaceholder,
                value: draft.notes,
                rows: 3,
                onChange: function (v) {
                  draft.notes = v;
                  commit();
                },
              })
            );
          }

          // Footer: clear + Done
          body.appendChild(
            PopoverCore.createFooter({
              clearLabel: "clear",
              clearCallback: function () {
                callbacks.clear();
              },
              doneLabel: "Done",
              doneCallback: function () {
                callbacks.close();
              },
            })
          );
        },

        onChange: function () {
          $(el).trigger("change");
        },
      });

      // Stash reference for getValue / setValue / receiveMessage
      $(el).data("sh-popover", popover);
    },

    getValue: function (el) {
      var popover = $(el).data("sh-popover");
      if (!popover) return null;
      var v = popover.getValue();
      if (!v) return null;

      // Return null if nothing meaningful is set
      var sections = getSections(el);
      var hasValue = false;
      for (var i = 0; i < sections.length; i++) {
        if (v[sections[i].key]) {
          hasValue = true;
          break;
        }
      }
      if (!hasValue && !v.notes) return null;
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
      $(el).on("change.sh-attendance", function () {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".sh-attendance");
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
        $(el).trigger("change");
      }
    },
  });

  Shiny.inputBindings.register(
    AttendancePickerBinding,
    "schoolhistory.attendancePicker"
  );
})();
