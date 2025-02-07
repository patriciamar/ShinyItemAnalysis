insert_ga_tag <- function() {
  # if GA file exists in the parent directory, use it
  # otherwise use the one from the package
  if (file.exists("../google-analytics.js")) {
    return(includeScript("../google-analytics.js"))
  }
  includeScript("google-analytics.js")
}

insert_visitor_counter <- function() {
  # if there is a visitor counter file in the parent directory,
  # read the value (see the server.R for the details), update the content
  # of the visitorCounter span (that reads "loading..." initially),
  # increment the counter and save back
  # otherwise, do nothing (see server.R) and do not inlcude the counter at all
  if (file.exists("../visitor_counter.txt")) {
    HTML(
      r"(
      <script>
        Shiny.addCustomMessageHandler('updateCounter', function(count) {
          document.getElementById('visitorCounter').innerHTML = count;
        });
      </script>
      Hits: <span id="visitorCounter"><em>loading...</em></span>
      )"
    )
  }
}


sm_allow_gui_installation <- function() {
 is_shiny_server <- !is.na(Sys.getenv("SHINY_SERVER_VERSION", unset = NA))
 is_gui_installation_forced <- tolower(Sys.getenv("SIA_MODULES_FORCE_GUI_INSTALLATION", unset = "false")) == "true"

 !is_shiny_server | is_gui_installation_forced
}
