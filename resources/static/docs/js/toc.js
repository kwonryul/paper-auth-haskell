function ready(callbackFunc) {
    if (document.readyState !== 'loading')
        callbackFunc();
    else
        document.addEventListener('DOMContentLoaded', callbackFunc);
}

ready(function () {
    let ul = document.getElementById('toc-ul');

    let h2s = document.querySelectorAll('h2');
    h2s.forEach(function(h2) {
        let h2Li = document.createElement('li');
        let h2A = document.createElement('a');
        h2A.href = '#' + h2.id;
        h2A.textContent = h2.textContent;
        h2A.style.fontWeight = "300";
        let h2Ul = document.createElement('ul');

        let h3s = h2.nextElementSibling.querySelectorAll('h3');
        h3s.forEach(function(h3) {
            let h3Li = document.createElement('li');
            h3Li.addEventListener('click', function() {
                var clickEvent = new MouseEvent("click", {
                bubbles: true,
                cancelable: true,
                view: window
                });
                h3.dispatchEvent(clickEvent);
            })
            let h3A = document.createElement('a');
            h3A.href = '#' + h3.id;
            h3A.textContent = h3.textContent;
            h3A.style.fontWeight = "300";
            h3Li.appendChild(h3A);
            h2Ul.appendChild(h3Li);
        });
        h2Li.appendChild(h2A);
        h2Li.appendChild(h2Ul);
        ul.appendChild(h2Li);
    });
})