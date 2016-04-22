$(document).ready( function() {
    "use strict";
    var setup_codemirror = function(elem){
        var codeblock = $(elem);
        var template = codeblock.text();
        codeblock.html("");
        var editor = CodeMirror($(elem)[0], {
            mode: "haskell",
            value: template,
            lineNumbers: true
        });
        return editor;
    }
    var template_editor = setup_codemirror("#template");
    var solution_editor = setup_codemirror("#solution");
    var checkprog_editor = setup_codemirror("#checkprogram");
    var pid = /problems\/(\d+)/.exec(document.URL);
    if(pid != null) pid = pid[1];

    $("#buttons > .btn").click(function(){
        var type = $(this).attr("id"); // "run" or "submit"
        if($("#run").hasClass("disabled")) return;
        $("#run").addClass("disabled");
        var posturl = type == "run" ? "/problems/test" :
              (pid == null ? "/problems/new" : ("/problems/"+pid+"/edit"));
        $.ajax({url: posturl,
                type: "POST",
                data: JSON.stringify(
                    {"problemTitle": $("#titleedit").val(),
                     "problemAuthorId": -1, // fake
                     "problemDescription": $("#descedit").val(),
                     "problemModuleName": $("#modulename").val(),
                     "problemTemplate": template_editor.getValue(),
                     "problemSolution": solution_editor.getValue(),
                     "problemCheckProgram": checkprog_editor.getValue(),
                     "problemIsApproved": false, // for new or fake one for editing
                    }),
                contentType: "application/json",
                success: function(data){
                    $("#results").removeClass("alert-danger alert-success").show();
                    $("#results").addClass(data.ok ? "alert-success" : "alert-danger");
                    $("#result").text(data.ok && type == "run"
                                      ? "Well done!" : (data.output || "<empty content>"));
                    $("#run").removeClass("disabled");
                    if(data.ok)
                        $("#submit").removeClass("disabled");
                    else
                        $("#submit").addClass("disabled");
                    // if created then update pid so next time will be an update
                    if(data.ok && type == "submit" && pid == null)
                        pid = parseInt(/#(\d+)/.exec(data.output)[1]);
                }});});
});
