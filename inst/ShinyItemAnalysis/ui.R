# %%%%%%%%%%%%%%%%%%%%%
# GLOBAL LIBRARY #####
# %%%%%%%%%%%%%%%%%%%%%

require(DT)
require(plotly)
require(shinyBS)
require(shinydashboard)
require(shinyjs)

# %%%%%%%%%%%%%%%%%%%%%
# SOURCING ###########
# %%%%%%%%%%%%%%%%%%%%%

source("ui/uiAbout.R", local = T)
source("ui/uiData.R", local = T)
source("ui/uiScores.R", local = T)
source("ui/uiReliability.R", local = T)
source("ui/uiValidity.R", local = T, encoding = "UTF-8")
source("ui/uiTraditionalAnalysis.R", local = T)
source("ui/uiRegression.R", local = T)
source("ui/uiIRT.R", local = T)
source("ui/uiDIF.R", local = T)
source("ui/uiReports.R", local = T)
source("ui/uiReferences.R", local = T, encoding = "UTF-8")
source("ui/uiSetting.R", local = T)

# %%%%%%%%%%%%%%%%%%%%%
# UI #################
# %%%%%%%%%%%%%%%%%%%%%

ui <- tagList(
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "hexbin.png"
    ),
    # CSS
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "busy_indicator.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "margins_and_paddings.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "sliders_colors.css"
    ),

    # social media card
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:site", content = "@PMartinkova"),
    tags$meta(name = "twitter:creator", content = "@PMartinkova"),
    tags$meta(name = "twitter:title", content = "ShinyItemAnalysis"),
    tags$meta(name = "twitter:description", content = "Test and Item Analysis with Shiny"),
    tags$meta(name = "twitter:image", content = "card.png"),

    # math typesetting
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.css",
      integrity = "sha384-RZU/ijkSsFbcmivfdRBQDtwuwVqK7GMOw6IMvKyeWL2K5UAlyp6WonmB8m7Jd0Hn",
      crossorigin = "anonymous"
    ),
    tags$script(
      defer = "defer",
      src = "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.js",
      integrity = "sha384-pK1WpvzWVBQiP0/GjnvRxV4mOb0oxFuyRxJlk6vVw146n3egcN5C925NCP7a7BY8",
      crossorigin = "anonymous"
    ),
    tags$script(
      defer = "defer",
      src = "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/contrib/auto-render.min.js",
      integrity = "sha384-vZTG03m+2yp6N6BNi5iM4rW4oIwk5DfcNdFfxkk9ZWpDriOkXX8voJBFrAO7MpVl",
      crossorigin = "anonymous",
      onload = "renderMathInElement(document.body);"
    ),

    # custom math typesetting for dynamic content
    includeScript("www/katex_dynamic.js"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "box.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "navbar_right.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "tooltip.css"
    ),
    # JS
    tags$script(
      type = "text/javascript",
      src = "busy.js"
    ),
    tags$script(
      type = "text/javascript",
      src = "report_generating_message.js"
    ),
    tags$script(
      type = "text/javascript",
      src = "report_downloading_message.js"
    ),
    tags$script(
      type = "text/javascript",
      src = "collapsible_menu_click.js"
    ),
    tags$script(
      type = "text/javascript",
      src = "toppage.js"
    ),
    tags$script(
      HTML('
      Shiny.addCustomMessageHandler("sessinf",
        function(x) {
          console.log(x);
        }
      );
    ')
    )
  ),
  div(
    class = "busy",
    p("Loading"),
    img(src = "busy_indicator.gif", height = 100, width = 100)
  ),

  shinyjs::useShinyjs(),
  tags$head(includeScript("google-analytics.js")),
  navbarPage(
    title = HTML('<div style="margin-top: -10px;">
                    <div class="header-title"><img src="sia_logo_trans.svg"> ShinyItemAnalysis</div>
                    <div class="header-subtitle"> Test and item analysis</div>
                 </div>'),
    windowTitle = "ShinyItemAnalysis",
    position = "fixed-top",
    selected = "About",
    collapsible = TRUE,
    footer = list(
      HTML('<div style = "clear: both; height: 50px;"></div>
           <div class = "panel-footer", style = "opacity: 1.00; z-index: 1000;">
              <p style = "margin:8px 0 0 0;">
                <div class = "footer-title"> <img src = "sia_logo.svg" style="width: 57px; margin-right: 10px;"> ShinyItemAnalysis </div>
                <div class = "footer-subtitle"> Test and item analysis via Shiny | Version 1.4.1 </div>
                <span style = "float:right">
                  <a href = "http://shinyitemanalysis.org/" id = "tooltipweb" target="_blank"> <img src = "footer_web_icon.png", class = "footer-icons"> </a>
                  <a href = "https://github.com/patriciamar/ShinyItemAnalysis/" id = "tooltipgithub" target="_blank"> <img src = "footer_github_icon.png", class = "footer-icons"> </a>
                  <a href = "https://CRAN.R-project.org/package=ShinyItemAnalysis/" id = "tooltipcran" target="_blank"> <img src = "footer_cran_icon.png", class = "footer-icons"> </a>
                </span>
              </p>
              <script>
                $("#tooltipweb").attr("title", "Web")
                $("#tooltipgithub").attr("title", "GitHub")
                $("#tooltipcran").attr("title", "CRAN")
              </script>
              <br>
              <div class = "footer-copyright">
                &copy; <script>document.write(new Date().getFullYear())</script> ShinyItemAnalysis
              </div>'),
      HTML('<div class = "footer-counter">'),
      textOutput("counter", inline = T),
      HTML("</div></div>")
    ),
    theme = "bootstrap.css",

    # %%%%%%%%%%%%%%%%%%%%%
    # MAIN PANEL #########
    # %%%%%%%%%%%%%%%%%%%%%

    # %%%%%%%%%%%%%%%%%%%%%
    # ABOUT ##############
    # %%%%%%%%%%%%%%%%%%%%%
    uiAbout,

    # %%%%%%%%%%%%%%%%%%%%%
    # DATA ###############
    # %%%%%%%%%%%%%%%%%%%%%
    uiData,

    # %%%%%%%%%%%%%%%%%%%%%
    # SUMMARY ############
    # %%%%%%%%%%%%%%%%%%%%%
    uiSummary,

    # %%%%%%%%%%%%%%%%%%%%%
    # VALIDITY ###########
    # %%%%%%%%%%%%%%%%%%%%%
    uiValidity,

    # %%%%%%%%%%%%%%%%%%%%%
    # RELIABILITY ########
    # %%%%%%%%%%%%%%%%%%%%%
    uiReliability,

    # %%%%%%%%%%%%%%%%%%%%%
    # ITEM ANALYSIS ######
    # %%%%%%%%%%%%%%%%%%%%%
    uiTraditionalAnalysis,

    # %%%%%%%%%%%%%%%%%%%%%
    # REGRESSION #########
    # %%%%%%%%%%%%%%%%%%%%%
    uiRegression,

    # %%%%%%%%%%%%%%%%%%%%%
    # IRT MODELS #########
    # %%%%%%%%%%%%%%%%%%%%%
    uiIRT,

    # %%%%%%%%%%%%%%%%%%%%%
    # DIF/FAIRNESS #######
    # %%%%%%%%%%%%%%%%%%%%%
    uiDIF,

    # %%%%%%%%%%%%%%%%%%%%%
    # REPORTS ############
    # %%%%%%%%%%%%%%%%%%%%%
    uiReports,

    # %%%%%%%%%%%%%%%%%%%%%
    # SETTING #########
    # %%%%%%%%%%%%%%%%%%%%%
    uiSetting,

    # %%%%%%%%%%%%%%%%%%%%%
    # REFERENCES #########
    # %%%%%%%%%%%%%%%%%%%%%
    uiReferences
  )
)
