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
           $("#result").html(data.result);
           $("#run").removeClass("disabled");
          }});
});
