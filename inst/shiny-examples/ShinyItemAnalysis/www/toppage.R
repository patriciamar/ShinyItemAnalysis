$(document).ready(function () {

  $('#inTabset a[data-toggle=\"tab\"]').bind('click', function (e) {
    $(document).load().scrollTop(0);
  });

});
