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

            let portNameElement = document.createElement('span');
            portNameElement.textContent = 'Port';
            let portInput = document.createElement('input');
            portInput.value = '8080';
            portInput.classList.add('requestInput');
            appendTr(table, portNameElement, portInput);

            let authorizationNameElement = document.createElement('span');
            authorizationNameElement.textContent = 'Authorization';
            let authorizationInput = document.createElement('input');
            authorizationInput.classList.add('requestInput');
            appendTr(table, authorizationNameElement, authorizationInput);

            let refreshTokenNameElement = document.createElement('span');
            refreshTokenNameElement.textContent = 'Paper-Refresh-Token';
            let refreshTokenInput = document.createElement('input');
            refreshTokenInput.classList.add('requestInput');
            appendTr(table, refreshTokenNameElement, refreshTokenInput);

            let headerInputs = [];
            headerInputs.push(['Authorization', authorizationInput]);
            let cookieInputs = [];
            cookieInputs.push(['Paper-Refresh-Token', refreshTokenInput]);
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
                } else if (strong.textContent === 'Cookie') {
                    let cookieName = strong.parentElement.parentElement.nextElementSibling.firstChild.textContent;
                    let cookieNameElement = document.createElement('span');
                    cookieNameElement.textContent = cookieName;
                    let cookieInput = document.createElement('input');
                    cookieInput.classList.add('requestInput');
                    cookieInputs.push([cookieName, cookieInput]);
                    appendTr(table, cookieNameElement, cookieInput);
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
                let fullUrl;
                let host = window.location.hostname;
                let port = portInput.value;
                if (port === '443' || port === '3000')
                    fullUrl = 'https://' + host + ':' + port + url;
                else
                    fullUrl = 'http://' + host + ':' + port + url;
                let headers = new Headers();
                headerInputs.forEach(function(tup) {
                    headers.append(tup[0], tup[1].value);
                });
                cookieInputs.forEach(function(tup) {
                    headers.append('Cookie', tup[0] + "=" + tup[1].value);
                })
                let body;
                if (bodyInput) {
                    headers.append('Content-Type', 'application/json');
                    body = bodyInput.value;
                }
                fetch(fullUrl, {
                    method,
                    headers,
                    body
                }).then(response => {
                    let status = document.createElement('div');
                    status.textContent = response.statusText;
                    if (!response.ok)
                        status.style.color = '#ba3925';
                    res.appendChild(status);

                    let headers = document.createElement('div');
                    headers.textContent = JSON.stringify(response.headers);
                    if (!response.ok)
                        headers.style.color = '#ba3925';
                    res.appendChild(headers);

                    let body = document.createElement('div');
                    response.text().then(x => {
                        body.textContent = x;
                    });
                    if (!response.ok)
                        body.style.color = '#ba3925';
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
            let tester = document.getElementById('tester');
            tester.style.right = '';
            document.getElementById('hide-button').textContent = 'Test';
            setTester(this);
        })
    });
})