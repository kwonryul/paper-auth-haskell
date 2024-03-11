function ready(callbackFunc) {
    if (document.readyState !== 'loading')
        callbackFunc();
    else
        document.addEventListener('DOMContentLoaded', callbackFunc);
}

function fold(h3) {
    let sib = h3;
    while (sib.nextElementSibling) {
        sib = sib.nextElementSibling;
        sib.style.display = 'none';
        h3.style.color = 'rgba(0, 0, 0, 0.85)';
    }
}

ready(function() {
    const h3s = document.querySelectorAll('h3');
    h3s.forEach(function(h3) {
        h3.style.cursor = 'pointer';
        fold(h3);
        h3.addEventListener('click', function() {
            h3s.forEach(function(newH3) {
                if (newH3 === h3)
                    return;
                fold(newH3);
            });
            let sib = this;
            while (sib.nextElementSibling) {
                sib = sib.nextElementSibling;
                if (sib.style.display === 'none') {
                    sib.style.display = '';
                    this.style.color = '#45c6da';
                }
                else {
                    sib.style.display = 'none';
                    this.style.color = 'rgba(0, 0, 0, 0.85)';
                }
            }
        })
    })
});