
$(function() {
  
  document.getElementById('ss-connect-dialog').setAttribute("role", "dialog");
  
  $(document).on("shiny:disconnected", function(event) {

    // by only finding elements that do not have tabindex="-1" we ensure we don't
    // corrupt the previous state of the element if a modal was already open
    nodes = document.querySelectorAll('body *');
  
    for (var i = 0; i < nodes.length; i++) {
      var node = nodes[i];
  
      node.setAttribute('tabindex', -1);
      node.setAttribute('aria-hidden', 'true');
      node.setAttribute('disabled', '');

      // tabindex=-1 does not prevent the mouse from focusing the node (which
      // would show a focus outline around the element). prevent this by disabling
      // outline styles while the modal is open
      // @see https://www.sitepoint.com/when-do-elements-take-the-focus/
      node.style.outline = 'none';
      
    }
    
    document.getElementById('ss-connect-dialog').setAttribute('aria-hidden', 'false');
    document.getElementById('ss-connect-dialog').removeAttribute('disabled');
    document.getElementById('ss-connect-dialog').removeAttribute('tabindex');
    document.getElementById('ss-reload-link').setAttribute('aria-hidden', 'false');
    document.getElementById('ss-reload-link').removeAttribute('disabled');
    document.getElementById('ss-connect-dialog').focus();
    document.getElementById('ss-reload-link').removeAttribute('tabindex');
    
    }
  )
  
  $(document).on("shiny:busy", function(event) {
    const keyboardfocusableElements = document.querySelectorAll(
      'a[href], button, input, textarea, select, details'
    )
    for (let i=0; i < keyboardfocusableElements.length; i++){
      keyboardfocusableElements[i].disabled = true;
      keyboardfocusableElements[i].blur();
    }
  })
  
  $(document).on("shiny:idle", function(event) {
    const keyboardfocusableElements = document.querySelectorAll(
      'a[href], button, input, textarea, select, details'
    )
    for (let i=0; i < keyboardfocusableElements.length; i++){
      keyboardfocusableElements[i].disabled = false;
    }
  })
  
});

