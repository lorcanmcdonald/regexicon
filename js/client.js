(function($) {
  $(function() {
    $("body").on("keyup", "input", function(e) {
      const re = $(e.target).val();

      $.ajax("/regex/?n=5", {
        data: re,
        processData: false,
        type: "POST",
        error: function(res) {
          const $results = $(".results");

          $results.text("Error: " + res.responseText);
        },
        success: function(result) {
          const $results = $(".results");

          $results.text("");
          for (const i in result) {
            if (result.hasOwnProperty(i)) {
              const $listItem = $("<li contenteditable>");

              $listItem.text(result[i]);
              $results.append($listItem);
            }
          }
        }
      });
    });
  });
}(jQuery));
