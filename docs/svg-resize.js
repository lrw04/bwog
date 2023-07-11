for (var elem of document.getElementsByTagName("svg")) {
    elem.setAttribute("width", parseFloat(elem.getAttribute("width")) / 10 + "em");
    elem.setAttribute("height", "");
}