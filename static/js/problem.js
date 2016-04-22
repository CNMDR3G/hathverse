$(document).ready( function() {
    "use strict";
    var codeblock = $("#code");
    var template = codeblock.text();
    codeblock.html("");
    var editor = CodeMirror($("#code")[0], {
        mode: "haskell",
        value: template,
        lineNumbers: true
    });

    console.assert(typeof localStorage !== "undefined",
                   "warning: localStorage not available.");

    var codes = localStorage.codes 
        ? JSON.parse(localStorage.codes) : {};

    var pid = document.URL.substr(document.URL.lastIndexOf('/')+1);
    pid = parseInt(pid);

    // restore saved code for this problem if any
    var thisCode = codes["p"+pid];
    if (typeof thisCode !== "undefined")
        editor.setValue(thisCode);

    $("#run").click(function(){
        if($("#run").hasClass("disabled")) return;
        $("#run").addClass("disabled");

        var code = editor.getValue();

        // save source code on submission
        codes["p" + pid] = code;
        localStorage.codes = JSON.stringify(codes);

        $.ajax({url: "/check",
                type: "POST",
                data: JSON.stringify({"probId": pid, "solCode": code}),
                contentType: "application/json",
                success: function(data){
                    $("#results")
                        .removeClass("alert-danger alert-success")
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
