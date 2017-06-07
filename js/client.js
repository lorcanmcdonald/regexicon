import $ from "jquery";

const send = (e) => {
  const re = $(e.target).val();

  if (re) {
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
  }
};

$(function() {
  $("input").val(decodeURIComponent(window.location.search.replace("?q=", "")));
  $("body").on("keyup", "input", send);
  send({ target: $("input") });
});
