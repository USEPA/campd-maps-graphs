
$(function() {
  
  document.getElementById('ss-connect-dialog').setAttribute("role", "dialog");
  
  $(document).on("shiny:disconnected", function(event) {

    // by only finding elements that do not have tabindex="-1" we ensure we don't
    // corrupt the previous state of the element if a modal was already open
    nodes = document.querySelectorAll('body *:not(dialog):not([tabindex="-1"])');
  
    for (var i = 0; i < nodes.length; i++) {
      var node = nodes[i];
  
      // save the previous tabindex state so we can restore it on close
      node._prevTabindex = node.getAttribute('tabindex');
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
    document.getElementById('ss-connect-dialog').setAttribute('tabindex', 0);
    document.getElementById('ss-reload-link').setAttribute('aria-hidden', 'false');
    document.getElementById('ss-reload-link').removeAttribute('disabled');
    document.getElementById('ss-connect-dialog').focus();
    document.getElementById('ss-reload-link').setAttribute('tabindex', 0);
    $('#ss-reload-link').keydown(function(e){
      if($('#ss-reload-link').is(":focus") && (e.which || e.keyCode) == 9){
        e.preventDefault();
      }
    });
    }
  )
});

