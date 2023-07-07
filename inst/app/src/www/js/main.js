let generate_modal_bindings = function(button_id) {
  // Get the button that opens the modal
  var btn = document.getElementById(button_id);
  // When the user clicks on the button, open the modal
  document.getElementById(button_id).onclick = function(event) {
    document.getElementById(event.target.dataset.target).style.display = "block";
  }

  // Get the <span> element that closes the modal
  var span = document.getElementById(btn.dataset.target).querySelector(".close")
  // When the user clicks on <span> (x), close the modal
  span.onclick = function() {
    document.getElementById(event.target.dataset.target).style.display = "none"
  }

  window.onclick = function(event) {
    if (event.target.classList.contains("modal")) {
      event.target.style.display = "none";
    }
  }
}


update_input = function(message) {
  for (var key in message[0]) {
    // check if the property/key is defined in the object itself, not in parent
    Shiny.setInputValue(key, message[0][key], {priority: "event"});
  }
}
Shiny.addCustomMessageHandler("update-input", update_input);
