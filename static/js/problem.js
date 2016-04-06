window.onload = function() {
  var template = document.getElementById("code").innerHTML;
  document.getElementById("code").innerHTML = "";
  editor = CodeMirror(document.getElementById("code"), {
    mode: "haskell",
    value: template,
    lineNumbers: true
  });
}
