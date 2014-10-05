//stores the currently selected fold
$(document.body).data("selected", null);
$(document.body).data("focusStack", []);

$(document).on("keyup", function (e) { 
  switch (e.which) {
    case 27:
      var stack = $(document.body).data("focusStack");
      if (stack.length != 0) { 
        var code = stack.pop();
        var container = $("code").children(".node:first");
        container.detach();
        $("code").replaceWith(code);
        $(".snip").replaceWith(container);
      }
      break;
    case 13:
      var fold = $(document.body).data("selected");
      if (fold != null && fold != $(".fold:first")) { 
        var container = fold.parents(".node:first");
        var snip = $("<span class='snip'>snip</span>");
        container.after(snip);
        container.detach();
        var code = $("code");
        code.detach();
        $(document.body).data("focusStack").push(code);
        var newCode = $("<code class='scala hljs'></code>");
        $("pre").append(newCode);
        newCode.append(container);
      }
      break;
  }
});

$(".fold").each(function (i,e) {
  $(this).data("child", $("<span class='child collapsed'>...</span>"));
  //prevent text selection for double clicks
  $(this).on("selectstart", function(e) {
    e.preventDefault();
  });
  $(this).on("dblclick", function () {
    $(this).toggleClass("collapsed");
    var elem = $(this).next(".child");    
    var child = $(this).data("child");
    elem.after(child);
    elem.detach();
    $(this).data("child", elem);
  });
  $(this).on("click", function () {
    if ($(document.body).data("selected") != null) {
      var selected = $(document.body).data("selected");
      selected.toggleClass("selected");
    }
    $(this).toggleClass("selected");
    $(document.body).data("selected", $(this));
  });
});

//collapse all types initially
$(".fold.type").trigger("dblclick");
