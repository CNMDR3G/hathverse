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
    var pid = document.URL.substr(document.URL.lastIndexOf('/')+1);
    pid = parseInt(pid);
    if(isNaN(pid)) pid = -1; // new

    var callback = function(type){
        return function(){
            if($("#run").hasClass("disabled")) return;
            $("#run").addClass("disabled");
            $.ajax({url: "/edit",
                    type: "POST",
                    data: JSON.stringify(
                        {"edittype": type,
                         "editpid": pid,
                         "title": $("#titleedit").val(),
                         "desc": $("#descedit").val(),
                         "modulename": $("#modulename").val(),
                         "template": template_editor.getValue(),
                         "solution": solution_editor.getValue(),
                         "checkprog": checkprog_editor.getValue(),
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
                        if(data.ok && type == "submit" && pid == -1)
                            pid = parseInt(/#(\d+)/.exec(data.output)[1]);
                }});}}
    $("#run").click(callback("run"));
    $("#submit").click(callback("submit"));
});
