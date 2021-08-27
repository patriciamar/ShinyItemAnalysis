// function for math mode in tables
$(document).on('shiny:value', function(event) {

  //if((/\b\w*coef_\w*\b/g).test(event.name)){
  if(event.name.includes("coef")){

    if(event.value.match(/(%%+[^%]+%%)/g) !== null) {

      var matches = event.value.match(/(%%+[^%]+%%)/g);
      var newvalue = event.value;

      for(var i=0; i<matches.length; i++){

        var code = '\\' + matches[i].slice(2,-2);
        newvalue = newvalue.replace(matches[i], katex.renderToString(code));

      }

      event.value = newvalue;

    } else {

      event.value;

    }

  }

});
