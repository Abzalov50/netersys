window.onload = function(){
    var codeElements = document.getElementsByClassName('python_code');
    for(var i = 0; i < codeElements.length; i++) {
	codeElement = codeElements.item(i);
	var code = codeElement.innerText;
	codeElement.innerHTML = "";

	var codeMirror = CodeMirror(
            codeElement,
            {
		value: code,
		mode: "python",
		theme: "default",
		lineNumbers: true,
		readOnly: true
            }
	);
    };

    var resElements = document.getElementsByClassName('python_result');
    for(var i = 0; i < resElements.length; i++) {
	resElement = resElements.item(i);
	var res = resElement.innerText;
	resElement.innerHTML = "";

	var resMirror = CodeMirror(
            resElement,
            {
		value: res,
		mode: "python",
		theme: "blackboard",
		lineNumbers: false,
		readOnly: true
            }
	);
    };
};

$(document).ready(function() {
    "use strict";
    //print("yes")

    //------ Nav item --------//
    $(".navbar-nav .nav-item").click(function(){
	$(".navbar-nav .nav-item").removeClass("active");
	$(this).addClass("active");
    });

    $(document.body).on("click", "tr[data-href]", function () {
	window.location.href = this.dataset.href;
    });

    //------- Mailchimp js --------//  

    /*$(document).ready(function() {
        $('#mc_embed_signup').find('form').ajaxChimp();
    })*/;

});
