
$(function() {
  const minuselements = [...document.getElementsByClassName("fa-minus")];
  const questionelements = [...document.getElementsByClassName("fa-question")];
  const elements = minuselements.concat(questionelements);
  
  for (let i=0; i < elements.length; i++){
    elements[i].setAttribute("role", "img");
  }
  
  const facilitySummaryBoxButton = document.getElementById("facility-summary-box").getElementsByClassName("btn-box-tool")[0];
  facilitySummaryBoxButton.insertAdjacentText("beforeend", "expand and collapse facility summary button");
  
  const complianceSummaryBoxButton = document.getElementById("compliance-summary-box").getElementsByClassName("btn-box-tool")[0];
  complianceSummaryBoxButton.insertAdjacentText("beforeend", "expand and collapse compliance summary button");
  
  const formelements = [...document.getElementsByClassName("form-group")];
  
  for (let i=0; i < formelements.length; i++){
    const id = formelements[i].getElementsByTagName("label")[0].textContent;
    formelements[i].getElementsByTagName("select")[0].setAttribute("label", id);
  }
  
  $(document).keyup(function(event) {
      if ($(".leaflet-marker-icon").is(":focus") && (event.key == "Enter")) {
          event.target.click()
      }
  });
});