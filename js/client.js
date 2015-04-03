(function($){
    $(function(){
        $('body').on("keyup", "input", function(e){
            var re = $(e.target).val();
            console.log("re", re);
            $.ajax("/regex/", {
                data: re,
                processData: false,
                type: "POST",
                error: function(){
                    var $results = $(".results");
                    $results.text("");
                    console.log("arguments", arguments);
                },
                success: function(result){
                    var $results = $(".results"),
                        $node,
                        i;
                    console.log("result", result);
                    console.log("$results", $results);

                    $results.text("");
                    for(i in result){
                        if(result.hasOwnProperty(i)){
                            $node = $("<li>");
                            $node.text(result[i]);
                            $results.append($node);
                        }
                    }
                }
            });
        });
    });

}(jQuery));
