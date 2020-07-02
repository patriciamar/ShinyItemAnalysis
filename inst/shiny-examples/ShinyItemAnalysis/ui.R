#%%%%%%%%%%%%%%%%%%%%%
# GLOBAL LIBRARY #####
#%%%%%%%%%%%%%%%%%%%%%

require(DT)
require(plotly)
require(shinyBS)
require(shinydashboard)
require(shinyjs)

#%%%%%%%%%%%%%%%%%%%%%
# SOURCING ###########
#%%%%%%%%%%%%%%%%%%%%%

source("ui/uiAbout.R", local = T)
source("ui/uiData.R", local = T)
source("ui/uiSummary.R", local = T)
source("ui/uiReliability.R", local = T)
source("ui/uiValidity.R", local = T, encoding = "UTF-8")
source("ui/uiTraditionalAnalysis.R", local = T)
source("ui/uiRegression.R", local = T)
source("ui/uiIRT.R", local = T)
source("ui/uiDIF.R", local = T)
source("ui/uiReports.R", local = T)
source("ui/uiReferences.R", local = T)
source("ui/uiSetting.R", local = T)

#%%%%%%%%%%%%%%%%%%%%%
# UI #################
#%%%%%%%%%%%%%%%%%%%%%

ui = tagList(
  tags$head(tags$link(rel = "shortcut icon",
                      href = "hexbin.png"),
            # CSS
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "style.css"),
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "margins_and_paddings.css"),
            tags$link(rel = "stylesheet",
                      href = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css",
                      integrity = "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH",
                      crossorigin = "anonymous"),
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "box.css"),
            # JS
            tags$script(type = "text/javascript",
                        src = "busy.js"),
            tags$script(type = "text/javascript",
                        src = "report_generating_message.js"),
            tags$script(type = "text/javascript",
                        src = "report_downloading_message.js"),
            tags$script(type = "text/javascript",
                        src = "collapsible_menu_click.js"),
            tags$script(type = "text/javascript",
                        src = "tabs_icons_right.js"),
            tags$script(src = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js",
                        integrity = "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm",
                        crossorigin = "anonymous"),
            tags$script(type = "text/javascript",
                        src = "math_in_tables.js"),
			      tags$script(type = "text/javascript",
                        src = "toppage.js")
            ),
  div(class = "busy",
      p("Loading"),
      img(src = "busy_indicator.gif", height = 100, width = 100)
  ),

  shinyjs::useShinyjs(),

  tags$head(includeScript("google-analytics.js")),

  navbarPage(title = HTML('<div style = "margin-top: -10px;">
                          <div class = "header-title">
                          <img src = "header_hexbin.png">
                          ShinyItemAnalysis
                          </div>
                          <div class = "header-subtitle">
                          Test and item analysis
                          </div>
                          </div>'),
             windowTitle = 'ShinyItemAnalysis',
             position = 'fixed-top',
             selected = 'About',
             collapsible = TRUE,
             footer = list(
               HTML('<div class = "panel-footer", style = "opacity: 1.00; z-index: 1000;">
                    <p style = "margin:8px 0 0 0;">
                    <div class = "footer-title">
                    <img src = "hexbin.png">
                    ShinyItemAnalysis
                    </div>
                    <div class = "footer-subtitle">
                    Test and item analysis | Version 1.3.3-1
                    </div>
                    <span style = "float:right">
                    <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/" id = "tooltipweb" target="_blank">
                    <img src = "footer_web_icon.png", class = "footer-icons">
                    </a>
                    <a href = "https://github.com/patriciamar/ShinyItemAnalysis/" id = "tooltipgithub" target="_blank">
                    <img src = "footer_github_icon.png", class = "footer-icons">
                    </a>
                    <a href = "https://CRAN.R-project.org/package=ShinyItemAnalysis/" id = "tooltipcran" target="_blank">
                    <img src = "footer_cran_icon.png", class = "footer-icons">
                    </a>
                    </span>
                    </p>
                    <script>
                    $("#tooltipweb").attr("title", "Web");
                    $("#tooltipgithub").attr("title", "GitHub");
                    $("#tooltipcran").attr("title", "CRAN");
                    </script>
                    <br>
                    <div class = "footer-copyright">
                    &copy; <script>document.write(new Date().getFullYear())</script> ShinyItemAnalysis
                    </div>'),
               HTML('<div class = "footer-counter">'),
               textOutput('counter', inline = T),
               HTML('</div></div>')),


             theme = "bootstrap.css",

             #%%%%%%%%%%%%%%%%%%%%%
             # MAIN PANEL #########
             #%%%%%%%%%%%%%%%%%%%%%

             #%%%%%%%%%%%%%%%%%%%%%
             # DATA ###############
             #%%%%%%%%%%%%%%%%%%%%%
             uiData,

             #%%%%%%%%%%%%%%%%%%%%%
             # SUMMARY ############
             #%%%%%%%%%%%%%%%%%%%%%
             uiSummary,

             #%%%%%%%%%%%%%%%%%%%%%
             # RELIABILITY ########
             #%%%%%%%%%%%%%%%%%%%%%
             uiReliability,

             #%%%%%%%%%%%%%%%%%%%%%
             # VALIDITY ###########
             #%%%%%%%%%%%%%%%%%%%%%
             uiValidity,

             #%%%%%%%%%%%%%%%%%%%%%
             # ITEM ANALYSIS ######
             #%%%%%%%%%%%%%%%%%%%%%
             uiTraditionalAnalysis,

             #%%%%%%%%%%%%%%%%%%%%%
             # REGRESSION #########
             #%%%%%%%%%%%%%%%%%%%%%
             uiRegression,

             #%%%%%%%%%%%%%%%%%%%%%
             # IRT MODELS #########
             #%%%%%%%%%%%%%%%%%%%%%
             uiIRT,

             #%%%%%%%%%%%%%%%%%%%%%
             # DIF/FAIRNESS #######
             #%%%%%%%%%%%%%%%%%%%%%
             uiDIF,

             #%%%%%%%%%%%%%%%%%%%%%
             # REPORTS ############
             #%%%%%%%%%%%%%%%%%%%%%
             uiReports,

             #%%%%%%%%%%%%%%%%%%%%%
             # ABOUT ##############
             #%%%%%%%%%%%%%%%%%%%%%
             uiAbout,

             #%%%%%%%%%%%%%%%%%%%%%
             # REFERENCES #########
             #%%%%%%%%%%%%%%%%%%%%%
             uiReferences,

             #%%%%%%%%%%%%%%%%%%%%%
             # SETTING #########
             #%%%%%%%%%%%%%%%%%%%%%
             uiSetting
             ))


