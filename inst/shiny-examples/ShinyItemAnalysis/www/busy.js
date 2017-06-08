setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show()
      }
    }, 800)
  } else {
    $('div.busy').hide()
  }
}, 100)

$(function() {

    $('#generate').click(function() {
        alert(
          'Generating of reports takes some time. Please, be patient. Please do not change the settings of application before the download is completed. A download button will occure when the report is ready. \n \n (Press "OK" to start report generating.)'
        );
        /*setInterval();*/
    });

});
