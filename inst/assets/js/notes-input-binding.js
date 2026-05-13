// notes-input-binding.js
// Shiny input binding for the notes text input.
// Depends on popover-core.css for .cell-input styling.

(function () {
  "use strict";

  var NotesInputBinding = new Shiny.InputBinding();

  $.extend(NotesInputBinding, {
    find: function (scope) {
      return $(scope).find(".sh-notes-input");
    },

    initialize: function (el) {
      var initial = el.getAttribute("data-initial-value") || "";
      var placeholder = el.getAttribute("data-placeholder") || "Optional";

      var input = document.createElement("input");
      input.type = "text";
      input.className = "cell-input";
      input.placeholder = placeholder;
      input.value = initial;

      input.addEventListener("input", function () {
        $(el).trigger("change");
      });

      el.appendChild(input);
      $(el).data("sh-notes-input", input);
    },

    getValue: function (el) {
      var input = $(el).data("sh-notes-input");
      if (!input) return null;
      var val = input.value;
      return val === "" ? null : val;
    },

    setValue: function (el, value) {
      var input = $(el).data("sh-notes-input");
      if (input) {
        input.value = value || "";
      }
    },

    subscribe: function (el, callback) {
      $(el).on("change.sh-notes", function () {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".sh-notes");
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
        $(el).trigger("change");
      }
      if (data.hasOwnProperty("placeholder")) {
        var input = $(el).data("sh-notes-input");
        if (input) input.placeholder = data.placeholder;
      }
    },
  });

  Shiny.inputBindings.register(
    NotesInputBinding,
    "schoolhistory.notesInput"
  );
})();
