$(document).ready(function () {
    $("#input").keypress(function (e) {
        if (e.which === 13) {
            var input = $(this).val();
            $("#output").html(simplify_string(input));
        }
    });
});
