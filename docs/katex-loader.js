for (var elem of document.getElementsByClassName("math")) {
    var texText = elem.firstChild;
    if (elem.tagName == "SPAN") {
        katex.render(texText.data, elem, {
            displayMode: elem.classList.contains('display'),
            throwOnError: false,
            macros: macros,
            fleqn: false
        });
    }
}