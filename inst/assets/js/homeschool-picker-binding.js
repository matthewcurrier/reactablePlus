// homeschool-picker-binding.js
// Shiny input binding for the homeschool picker.
// Depends on popover-core.js (PopoverCore global).
//
// Value semantics:
//   NULL                → picker is OFF for this row
//   { by, by_other, curriculum, notes }  → picker is ON (all fields optional)
//
// Config (via data-config JSON):
//   providers, provider_label, curriculum_label, curriculum_placeholder,
//   show_curriculum, show_notes, notes_placeholder, trigger_label,
//   trigger_sub_label, popover_title, popover_title_sub,
//   filled_pill_label, clear_label

(function () {
  "use strict";

  // ── Defaults (backward compat) ────────────────────────────────────────

  var DEFAULTS = {
    providers: [
      "Mother", "Father", "Grandmother", "Grandfather",
      "Guardian", "Tutor", "Co-op", "Online program", "Other",
    ],
    provider_label: "Who provided homeschooling?",
    curriculum_label: "Curriculum / program",
    curriculum_placeholder: "e.g. Time4Learning, Sonlight, custom",
    show_curriculum: true,
    show_notes: true,
    notes_placeholder: "Evaluator, co-op participation, any context\u2026",
    trigger_label: "+ Mark homeschool",
    trigger_sub_label: "(details optional)",
    popover_title: "Homeschool details",
    popover_title_sub: "(optional)",
    filled_pill_label: "Homeschool",
    clear_label: "remove homeschool",
  };

  function getConfig(el) {
    var raw = el.getAttribute("data-config");
    if (raw) {
      try {
        var parsed = JSON.parse(raw);
        // Merge with defaults so missing keys fall back
        var config = {};
        for (var k in DEFAULTS) {
          if (DEFAULTS.hasOwnProperty(k)) {
            config[k] =
              parsed[k] !== undefined && parsed[k] !== null
                ? parsed[k]
                : DEFAULTS[k];
          }
        }
        return config;
      } catch (e) {
        // fall through
      }
    }
    return DEFAULTS;
  }

  // ── Detect "Other" option ─────────────────────────────────────────────
  // If the last provider option is "Other", show a free-text follow-up.

  function hasOtherOption(providers) {
    return (
      providers.length > 0 &&
      providers[providers.length - 1].toLowerCase() === "other"
    );
  }

  // ── Shiny input binding ───────────────────────────────────────────────

  var HomeschoolPickerBinding = new Shiny.InputBinding();

  $.extend(HomeschoolPickerBinding, {
    find: function (scope) {
      return $(scope).find(".sh-homeschool-picker");
    },

    initialize: function (el) {
      var initialRaw = el.getAttribute("data-initial-value");
      var initial = null;
      try {
        var parsed = initialRaw ? JSON.parse(initialRaw) : null;
        if (parsed && typeof parsed === "object") initial = parsed;
      } catch (e) {
        initial = null;
      }

      var gradeLabel = el.getAttribute("data-grade-label") || "";
      var gradeKey = el.getAttribute("data-grade-key") || "";
      var ns = el.getAttribute("data-ns") || "";
      var cfg = getConfig(el);

      var popover = PopoverCore.create(el, {
        triggerLabel: cfg.trigger_label,
        triggerSubLabel: cfg.trigger_sub_label,
        ariaLabel: gradeLabel
          ? cfg.trigger_label.replace(/^\+\s*/, "") + " for " + gradeLabel
          : cfg.trigger_label.replace(/^\+\s*/, ""),
        popoverTitle: cfg.popover_title,
        popoverTitleSub: cfg.popover_title_sub,
        popoverWidth: "min(380px, 90vw)",
        popoverClass: "hs-overlay",
        initialValue: initial,

        isFilled: function (v) {
          return v !== null && v !== undefined;
        },

        // ── Filled summary: pills + curriculum sub-line
        renderFilled: function (v, container) {
          var line = document.createElement("div");
          line.className = "attend-line";

          line.appendChild(
            PopoverCore.createPill(cfg.filled_pill_label, "hs-pill")
          );

          var displayBy = v.by;
          if (
            hasOtherOption(cfg.providers) &&
            v.by &&
            v.by.toLowerCase() === "other" &&
            v.by_other
          ) {
            displayBy = v.by_other;
          }
          if (displayBy) {
            line.appendChild(
              PopoverCore.createPill(displayBy, "hs-by-pill")
            );
          }

          container.appendChild(line);

          if (v.curriculum) {
            var currLine = document.createElement("div");
            currLine.className = "attend-sub";
            currLine.textContent = v.curriculum;
            container.appendChild(currLine);
          } else if (!v.by) {
            var configLine = document.createElement("div");
            configLine.className = "attend-sub";
            configLine.style.color = "var(--sh-text-soft)";
            configLine.textContent = "configure\u2026";
            container.appendChild(configLine);
          }
        },

        // ── Popover body: provider, curriculum, notes, footer
        renderPopover: function (body, callbacks) {
          var val = callbacks.getValue();
          var draft = {
            by: val ? val.by || "" : "",
            by_other: val ? val.by_other || "" : "",
            curriculum: val ? val.curriculum || "" : "",
            notes: val ? val.notes || "" : "",
          };

          function commit() {
            callbacks.setValue({
              by: draft.by || null,
              by_other: draft.by_other || null,
              curriculum: draft.curriculum || null,
              notes: draft.notes || null,
            });
          }

          // Provider select
          var providerResult = PopoverCore.createSelectInput({
            label: cfg.provider_label,
            options: cfg.providers,
            value: draft.by,
            onChange: function (v) {
              draft.by = v;
              commit();
              // Show/hide "Other" text input
              if (hasOtherOption(cfg.providers) && v.toLowerCase() === "other") {
                otherWrap.style.display = "";
                otherInput.focus();
              } else if (otherWrap) {
                otherWrap.style.display = "none";
                draft.by_other = "";
                commit();
              }
            },
          });
          body.appendChild(providerResult.group);

          // "Other" text input (conditionally visible, only if providers has "Other")
          var otherWrap = null;
          var otherInput = null;

          if (hasOtherOption(cfg.providers)) {
            var otherResult = PopoverCore.createTextInput({
              placeholder: "Specify\u2026",
              value: draft.by_other,
              onChange: function (v) {
                draft.by_other = v;
                commit();
              },
            });
            otherWrap = otherResult.group;
            otherInput = otherResult.input;
            otherWrap.style.display =
              draft.by && draft.by.toLowerCase() === "other" ? "" : "none";
            otherWrap.style.marginTop = "-6px";
            body.appendChild(otherWrap);
          }

          // Curriculum (optional)
          if (cfg.show_curriculum) {
            var currResult = PopoverCore.createTextInput({
              label: cfg.curriculum_label,
              placeholder: cfg.curriculum_placeholder,
              value: draft.curriculum,
              onChange: function (v) {
                draft.curriculum = v;
                commit();
              },
            });
            body.appendChild(currResult.group);
          }

          // Notes (optional)
          if (cfg.show_notes) {
            body.appendChild(
              PopoverCore.createTextarea({
                label: "Notes",
                placeholder: cfg.notes_placeholder,
                value: draft.notes,
                rows: 3,
                onChange: function (v) {
                  draft.notes = v;
                  commit();
                },
              })
            );
          }

          // Footer: remove + Done
          body.appendChild(
            PopoverCore.createFooter({
              clearLabel: cfg.clear_label,
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

        // On first click (empty state), set value to empty object (= ON)
        onOpen: function (instance) {
          if (instance.getValue() === null) {
            instance.setValue({});
          }
        },

        onChange: function () {
          $(el).trigger("change");
        },
      });

      $(el).data("sh-popover", popover);
    },

    getValue: function (el) {
      var popover = $(el).data("sh-popover");
      if (!popover) return null;
      return popover.getValue();
    },

    setValue: function (el, value) {
      var popover = $(el).data("sh-popover");
      if (popover) {
        popover.setValue(value || null, true);
        popover.render();
      }
    },

    subscribe: function (el, callback) {
      $(el).on("change.sh-homeschool", function () {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".sh-homeschool");
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
        $(el).trigger("change");
      }
    },
  });

  Shiny.inputBindings.register(
    HomeschoolPickerBinding,
    "schoolhistory.homeschoolPicker"
  );
})();
