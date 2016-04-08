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
