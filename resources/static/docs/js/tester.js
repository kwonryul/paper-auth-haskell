let accessToken;

function toggleTester() {
    let tester = document.getElementById('tester');
    if (tester.style.right === '') {
        tester.style.right = '0';
        document.getElementById('hide-button').textContent = 'Hide';
    } else {
        tester.style.right = '';
        document.getElementById('hide-button').textContent = 'Test';
    }
}

function ready(callbackFunc) {
    if (document.readyState !== 'loading')
        callbackFunc();
    else
        document.addEventListener('DOMContentLoaded', callbackFunc);
}

function setTester(h3) {
    let req = document.getElementById('testerrequest');
    let res = document.getElementById('testerresponse');
    while (req.firstChild) {
        req.removeChild(req.firstChild);
    }
    while (res.firstChild) {
        res.removeChild(res.firstChild);
    }
    let h4s = h3.parentElement.querySelectorAll('h4');
    h4s.forEach(function(h4) {
        if (h4.textContent === 'Request') {
            let table = document.createElement('table');
            table.id = 'requestTable';
            req.appendChild(table);

            let methodUrl = h3.textContent.split(' ');
            let method = methodUrl[0];

            let urlNameElement = document.createElement('span');
            urlNameElement.textContent = 'Url';
            let urlInput = document.createElement('input');
            urlInput.value = methodUrl[1];
            urlInput.classList.add('requestInput');
            appendTr(table, urlNameElement, urlInput);

            let headerInputs = [];
            let queryParamInputs = [];
            let bodyInput;

            let strongs = h4.nextElementSibling.querySelectorAll('strong');
            let bodyFlag = false;
            strongs.forEach(function(strong) {
                if (strong.textContent === 'Header') {
                    let headerName = strong.parentElement.parentElement.nextElementSibling.firstChild.textContent;
                    let headerNameElement = document.createElement('span');
                    headerNameElement.textContent = headerName;
                    let headerInput = document.createElement('input');
                    headerInputs.push([headerName, headerInput]);
                    headerInput.classList.add('requestInput');
                    appendTr(table, headerNameElement, headerInput);
                } else if (strong.textContent === 'QueryParam') {
                    let paramName = strong.parentElement.parentElement.nextElementSibling.firstChild.textContent;
                    let paramNameElement = document.createElement('span');
                    paramNameElement.textContent = paramName;
                    let paramInput = document.createElement('input');
                    paramInput.classList.add('requestInput');
                    queryParamInputs.push([paramName, paramInput]);
                    appendTr(table, paramNameElement, paramInput);
                } else if (strong.textContent === 'Body') {
                    bodyFlag = true;
                }
            });

            if (bodyFlag) {
                let bodyNameElement = document.createElement('span');
                bodyNameElement.textContent = 'Body';
                bodyNameElement.id = 'requestBodyName';
                bodyInput = document.createElement('textarea');
                bodyInput.classList.add('requestTextArea');
                bodyInput.addEventListener('input', function() {
                    this.style.height = 'auto';
                    this.style.height = (this.scrollHeight > parseInt(getComputedStyle(this).getPropertyValue('max-height'))) ? 
                        getComputedStyle(this).getPropertyValue('max-height') : 
                        this.scrollHeight + 'px';
                });
                req.appendChild(bodyNameElement);
                req.appendChild(bodyInput);
            }
            let buttonDiv = document.createElement('div');
            let requestButton = document.createElement('button');
            requestButton.textContent = "Send";
            buttonDiv.id = 'requestSendButtonDiv';
            buttonDiv.appendChild(requestButton);
            req.appendChild(buttonDiv);

            requestButton.addEventListener('click', function() {
                while (res.firstChild) {
                    res.removeChild(res.firstChild);
                }
                let queryParams = {};
                queryParamInputs.forEach(function(tup) {
                    queryParams[tup[0]] = tup[1].value;
                });
                let queryString = new URLSearchParams(queryParams).toString();
                let url = urlInput.value + "?" + queryString;
                let fullUrl = window.location.origin + url;
                let headers = new Headers();
                if (accessToken) {
                    headers.append("Authorization", "Bearer " + accessToken);
                }
                headerInputs.forEach(function(tup) {
                    headers.append(tup[0], tup[1].value);
                });
                let body;
                if (bodyInput) {
                    headers.append('Content-Type', 'application/json');
                    body = bodyInput.value;
                }
                fetch(fullUrl, {
                    method,
                    headers,
                    body,
                    credentials: 'include'
                }).then(response => {
                    let status = document.createElement('div');
                    status.textContent = response.status + "\t" + response.statusText;
                    if (!response.ok)
                        status.style.color = '#ba3925';
                    status.style.marginBottom = '1.25rem';
                    res.appendChild(status);

                    let headers = document.createElement('pre');
                    let headersObject = {};
                    response.headers.forEach((value, key) => {
                        headersObject[key] = value;
                    });
                    headers.textContent = JSON.stringify(headersObject, null, 2);
                    if (!response.ok)
                        headers.style.color = '#ba3925';
                    headers.style.fontFamily = '"Open Sans", "DejaVu Sans", sans-serif';
                    headers.style.margin = '0.5625em 0.625em';
                    headers.style.fontSize = '10pt';
                    headers.style.marginBottom = '1.25rem';
                    res.appendChild(headers);

                    let body = document.createElement('pre');
                    response.text().then(x => {
                        let j;
                        try {
                            j = JSON.parse(x);
                            if (j.accessToken)
                                accessToken = j.accessToken;
                            body.textContent = JSON.stringify(j, null, 2);
                        } catch (e) {
                            body.textContent = x;
                        }
                    });
                    if (!response.ok) {
                        body.style.color = '#ba3925';
                    }
                    body.style.fontFamily = '"Open Sans", "DejaVu Sans", sans-serif';
                    body.style.margin = '0.5625em 0.625em';
                    body.style.fontSize = '10pt';
                    body.style.marginBottom = '1.25rem';
                    res.appendChild(body);
                });
            })
        }
    })
}

function appendTr(table, nameElement, input) {
    let tr;
    let td;
    tr = document.createElement('tr');
    td = document.createElement('td');
    td.classList.add('inputTd');
    td.appendChild(nameElement);
    tr.appendChild(td);
    td = document.createElement('td');
    td.appendChild(input);
    tr.appendChild(td);
    table.appendChild(tr);
}

ready(function () {
    let h3s = document.querySelectorAll('h3');
    h3s.forEach(h3 => {
        h3.addEventListener('click', function() {
            setTester(this);
        })
    });
})