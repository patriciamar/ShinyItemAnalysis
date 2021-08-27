$(document).ready(function() {
	
	if ( $(window).width() > 1100 ) {
	$(".navbar-nav > li:nth-child(10)").css({"float": "right", "right" : "100px"});
	$(".navbar-nav > li:nth-child(11)").css({"float": "right"});
	$(".navbar-nav > li:nth-child(12)").css({"float": "right", "right" : "-100px"});
	
	}
  
  $(window).resize(function(){
	if ( $(window).width() > 1100 ) {
		
		$(".navbar-nav > li:nth-child(10)").css({"float": "right", "right" : "100px"});
		$(".navbar-nav > li:nth-child(11)").css({"float": "right"});
		$(".navbar-nav > li:nth-child(12)").css({"float": "right", "right" : "-100px"});
	
	} else {
		
		$(".navbar-nav > li:nth-child(10)").css({"float": "", "right" : ""});
		$(".navbar-nav > li:nth-child(11)").css({"float": ""});
		$(".navbar-nav > li:nth-child(12)").css({"float": "", "right" : ""});
		
	}
  });
});