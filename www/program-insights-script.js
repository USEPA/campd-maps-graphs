
$(function() {
  const boxButtonElements = [...document.getElementsByClassName("box-primary")];
  console.log(boxButtonElements);
  for (let i=0; i < boxButtonElements.length; i++){
    var titleElement = boxButtonElements.getElementByTagName("h3")[0];
    var title = titleElement.textContent || titleElement.innerText;
    const boxButtonTool = boxButtonElements.getElementsByClassName("btn-box-tool")[0];
    boxButtonTool.insertAdjacentText("beforeend", "expand and collapse "+title+" button");
  }
  
  $(document).keyup(function(event) {
      if ($(".preview-button").is(":focus") && (event.key == "Enter")) {
          alert("button was clicked");
      }
  });
  
  $(document).onclick(function() {
    if ($(".preview-button").is(":focus")) {
          alert("button was clicked");
      }
  });
});