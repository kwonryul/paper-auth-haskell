function ready(callbackFunc) {
    if (document.readyState !== 'loading')
        callbackFunc();
    else
        document.addEventListener('DOMContentLoaded', callbackFunc);
}

function openPopUp(event) {
    const target = event.target;
    if (target.className !== "popUp")
        return;
    event.preventDefault();
    const screenX = event.screenX;
    const screenY = event.screenY;
    window.open(target.href, target.text, `left=${screenX}, top=${screenY}, width=1500, height=1000, status=no, menubar=no, toolbar=no, resizable=no`);
}

ready(function () {
    const el = document.getElementById("content");
    el.addEventListener("click", event => openPopUp(event), false);
});