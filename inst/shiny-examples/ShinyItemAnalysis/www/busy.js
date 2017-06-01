setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {	  
        $('div.busy').show()
      }
    }, 1000)
  } else {
    $('div.busy').hide()
  }
}, 100)

$(function() {

	$('#report').click(function() {
		alert('Downloading of reports takes some time. Please, be patient. Please do not change the settings of application before the download is completed.');
		/*setInterval();*/
	});
	
});
