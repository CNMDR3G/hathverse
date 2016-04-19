$(document).ready( function() {
    "use strict";
    var codeblock = $("#code");
    var template = codeblock.html();
    codeblock.html("");
    var editor = CodeMirror($("#code")[0], {
        mode: "haskell",
        value: template,
        lineNumbers: true
    });

    $("#results").hide();

    $("#run").click(function(){
        if($("#run").hasClass("disabled")) return;
        $("#run").addClass("disabled");
        var pid = document.URL.substr(document.URL.lastIndexOf('/')+1);
        pid = parseInt(pid);
        var code = editor.getValue();
        $.ajax({url: "/check",
                type: "POST",
                data: JSON.stringify({"probId": pid, "solCode": code}),
                contentType: "application/json",
                success: function(data){
                    $("#results")
                        .removeClass("alert-info alert-danger alert-success")
                        .show();

                    $("#results").addClass(data.ok 
                                           ? "alert-success"
                                           : "alert-danger");
                    
                    $("#result").text(data.ok
                                      ? "Well done!"
                                      : (data.output || "<empty content>"));
                    $("#run").removeClass("disabled");
                }});
    });
});
