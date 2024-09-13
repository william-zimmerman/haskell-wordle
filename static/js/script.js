fetch('http://localhost:8080/answer')
    .then(response => response.text())
    .then(text => document.body.innerHTML = `Today's word is ${text}`)
    .catch(error => console.error("Error!", error))
