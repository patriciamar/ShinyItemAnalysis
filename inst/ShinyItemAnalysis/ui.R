# %%%%%%%%%%%%%%%%%%%%%
# GLOBAL LIBRARY #####
# %%%%%%%%%%%%%%%%%%%%%

require(DT)
require(plotly)
require(shinyBS)
require(shinyjs)


# %%%%%%%%%%%%%%%%%%%%%
# SOURCING ###########
# %%%%%%%%%%%%%%%%%%%%%

source("ui/uiAbout.R", local = TRUE, encoding = "UTF-8")
source("ui/uiData.R", local = TRUE)
source("ui/uiScores.R", local = TRUE)
source("ui/uiReliability.R", local = TRUE)
source("ui/uiValidity.R", local = TRUE, encoding = "UTF-8")
source("ui/uiTraditionalAnalysis.R", local = TRUE)
source("ui/uiRegression.R", local = TRUE)
source("ui/uiIRT.R", local = TRUE)
source("ui/uiDIF.R", local = TRUE)
source("ui/uiReports.R", local = TRUE)
source("ui/uiModules.R", local = TRUE)
source("ui/uiReferences.R", local = TRUE, encoding = "UTF-8")
source("ui/uiSetting.R", local = TRUE)

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
    tags$meta(name = "twitter:image", content = "https://cdn.jsdelivr.net/gh/patriciamar/ShinyItemAnalysis/inst/ShinyItemAnalysis/www/card.png"),

    # math typesetting
    includeHTML("www/katex.html"),

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
      href = "tables_overflow.css"
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
      type = "text/javascript",
      src = "console_log.js"
    )
  ),
  div(
    class = "busy",
    p("Loading"),
    img(src = "busy_indicator.gif", height = 100, width = 100)
  ),
  shinyjs::useShinyjs(),
  tags$head(insert_ga_tag()),
  navbarPage(
    title = HTML('<div style="margin-top: -10px;">
                    <div class="header-title"><img src="sia_logo_trans.svg"> ShinyItemAnalysis</div>
                    <div class="header-subtitle"> Test and item analysis</div>
                 </div>'),
    id = "navbar",
    windowTitle = "ShinyItemAnalysis",
    position = "fixed-top",
    selected = "About",
    collapsible = TRUE,
    footer = list(
      HTML('<div style = "clear: both; height: 50px;"></div>
           <div class = "panel-footer", style = "opacity: 1.00; z-index: 1000;">
              <p style = "margin:8px 0 0 0;">
                <div class = "footer-title"> <img src = "sia_logo.svg" style="width: 57px; margin-right: 10px;"> ShinyItemAnalysis </div>
                <div class = "footer-subtitle"> Test and item analysis via Shiny | Version 1.5.5 </div>
                <span style = "float:right">
                  <a href = "https://shinyitemanalysis.org/" id = "tooltipweb" target="_blank"> <img src = "footer_web_icon.png", class = "footer-icons"> </a>
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
      HTML('<div class="footer-counter">'),
      insert_visitor_counter(),
      HTML('</div>'),
      HTML("</div>")
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
    # MODULES ############
    # %%%%%%%%%%%%%%%%%%%%%
    uiModules,

    # %%%%%%%%%%%%%%%%%%%%%
    # SETTING #########
    # %%%%%%%%%%%%%%%%%%%%%
    uiSetting,

    # %%%%%%%%%%%%%%%%%%%%%
    # REFERENCES #########
    # %%%%%%%%%%%%%%%%%%%%%
    uiReferences,

    # %%%%%%%%%%%%%%%%%%%%%
    # REPORTS ############
    # %%%%%%%%%%%%%%%%%%%%%
    uiReports
  )
)
