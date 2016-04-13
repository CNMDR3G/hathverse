function callback(type){
  return function(){
    $.ajax({url: "/login",
            type: "POST",
            data: {"username": $("#username").val(),
                   "password": $("#password").val(),
                   "type": type},
            success: function(data){
              if(data.ok){
                window.location.href = "/";
              } else {
                $("#err-msg").html(data.err);
                $("#err-msg").show();
              }
            }});
  }
}

$("#login-btn").click(callback("login"));
$("#signup-btn").click(callback("signup"));
