window.onload = function() {
  var codeblock = $("#code");
  var template = codeblock.html();
  codeblock.html("");
  editor = CodeMirror($("#code")[0], {
    mode: "haskell",
    value: template,
    lineNumbers: true
  });
}

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
            $("#results").removeClass("alert-info alert-danger alert-success");
            if(data.ok){
              $("#results").addClass("alert-success");
              $("#result").html("Well done!");
            } else {
              $("#results").addClass("alert-danger");
              $("#result").html(data.output);
            }
            $("#run").removeClass("disabled");
          }});
});
