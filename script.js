$(document).ready(function () {
    var container = $(".console");
    var controller = container.console({
        promptLabel: "Simplify> ",
        commandValidate: function () { return true; },
        commandHandle: function (line) {
            return simplify_string(line);
        },
        autofocus:true,
        animateScroll:true,
        promptHistory:true
    });
        
            
    // $("#input").keypress(function (e) {
    //     if (e.which === 13) {
    //         var input = $(this).val();
    //         $("#output").html(simplify_string(input));
    //     }
    // });
});
