var textInput = document.createElement("input");
textInput.type = "text";

var button = document.createElement("button");
button.textContent = "guess";
button.addEventListener("click", buttonFunction);

var feedback = document.createElement("div");

document.body.insertAdjacentElement("beforeend", textInput);
document.body.insertAdjacentElement("beforeend", button);
document.body.insertAdjacentElement("beforeend", feedback);

fetch("http://localhost:8080/answer")
    .then(response => response.json())
    .then(object => feedback.innerHTML = `<p>Today's word is ${object.answer}</p>`)
    .catch(error => console.error("Error!", error));

function buttonFunction() {
    fetch(`http://localhost:8080/guess/${textInput.value}`)
        .then(response => response.json())
        .then(object => feedback.innerHTML = `<p>You guessed ${object.guess}</p>`)
        .catch(error => console.error("Error!", error));
}