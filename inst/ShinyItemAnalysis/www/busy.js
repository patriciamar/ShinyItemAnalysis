setInterval(function(){
  if ($('html').hasClass('shiny-busy')) {
    setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
        $('div.busy').show();
      }
    }, 2400);
  } else {
    $('div.busy').hide();
  }
}, 100);
