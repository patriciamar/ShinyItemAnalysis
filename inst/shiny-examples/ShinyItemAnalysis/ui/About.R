About <- tabPanel("About",
                  #------------------------------------------------------------------------------------#
                  # Description ####
                  #------------------------------------------------------------------------------------#
                  h3("Description"),
                  p(code("ShinyItemAnalysis"), "provides analysis of educational tests (such as admission
                    tests) and their items including:" ),
                  tags$ul(tags$li("Exploration of total and standard scores on ", strong("Summary"), "page. "),
                          tags$li("Correlation structure and predictive validity analysis on ", strong("Validity"), "page. "),
                          tags$li("Item and distractor analysis on ", strong("Item analysis"), "page. "),
                          tags$li("Item analysis by logistic models on ", strong("Regression"), "page. "),
                          tags$li("Item analysis by item response theory models on ", strong("IRT models"), "page. "),
                          tags$li("Differential item functioning (DIF) and differential distractor functioning (DDF)
                            methods on ", strong("DIF/Fairness"), "page. ")),
                  p("This application is based on the free statistical software",
                    a("R", href = "https://cran.r-project.org/", target = "_blank"),
                    " and its ",
                    a("shiny", href = "http://www.rstudio.com/shiny/", target = "_blank"),
                    "package. "),
                  p("For all graphical outputs a download button is provided. Moreover, on",
                    strong("Reports"), "page HTML or PDF report can be created. Additionaly, all application
                    outputs are complemented by selected R code hence the similar analysis can be run and
                    modified in R."),

                  #------------------------------------------------------------------------------------#
                  # Data ####
                  #------------------------------------------------------------------------------------#
                  h4("Data"),
                  p("For demonstration purposes, by default, 20-item dataset", code("GMAT"),"
                    from R ", code("difNLR")," package is used. Other four datasets are available: ",
                    code("GMAT2"), "and", code("MSAT-B"), "from", code("difNLR"), "package and ",
                    code("Medical 100"),  "and", code("HCI"), "from", code("ShinyItemAnalysis"),
                    "package.  You can change the dataset (and try your own one) on page", strong("Data.")),
                  #------------------------------------------------------------------------------------#
                  # Availability ####
                  #------------------------------------------------------------------------------------#
                  h4("Availability"),
                  p("Application can be downloaded as R package from ",
                    a("CRAN.", href = "https://CRAN.R-project.org/package=ShinyItemAnalysis", target = "_blank"),
                    "It is also available online at ",
                    a("Czech Academy of Sciences ",
                      href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/",
                      target = "_blank"),
                    HTML("<a href = 'https://shiny.cs.cas.cz/ShinyItemAnalysis/' target = '_blank'>
                         <img src = 'flag_CR.png' height = '16' border = '0' align = 'middle'></a>"),
                    "and",
                    a("shinyapps.io",
                      href = "https://cemp.shinyapps.io/ShinyItemAnalysis/",
                      target = "_blank"),
                    HTML("<a href = 'https://cemp.shinyapps.io/ShinyItemAnalysis/' target = '_blank'>
                         <img src = 'flag_USA.png' height = '16' border = '0' align = 'middle'></a>.")),

                  #------------------------------------------------------------------------------------#
                  # Version ####
                  #------------------------------------------------------------------------------------#
                  h4("Version"),
                  p("Current version of ", code("ShinyItemAnalysis"), " available on ",
                    a("CRAN", href = "https://CRAN.R-project.org/package=ShinyItemAnalysis", target = "_blank"), "is 1.2.7.
                    Version available",
                    a("online", href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/", target = "_blank"), "is 1.2.7.
                    The newest development version available on ",
                    a("GitHub", href = "https://github.com/patriciamar/ShinyItemAnalysis", target = "_blank"), "is 1.2.7.",
                    br(),
                    "See also older versions: ",
                    a("0.1.0, ", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV01/", target = "_blank"),
                    a("0.2.0, ", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV02/", target = "_blank"),
                    a("1.0.0, ", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV100/", target = "_blank"),
                    a("1.1.0. ", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV110/", target = "_blank")),

                  #------------------------------------------------------------------------------------#
                  # Authors ####
                  #------------------------------------------------------------------------------------#
                  h4("Authors and contributors"),
                  fluidRow(
                    column(1, align = "center",
                           img(src = "author_patricia.png", width = 75),
                           HTML("<figcaption><a href='http://www.cs.cas.cz/martinkova/' target='_blank'>Patricia<br>Martinkova</a></figcaption>")),
                    column(1, align = "center",
                           img(src = "author_adela.jpg", width = 75),
                           HTML("<figcaption><a href='http://www.cs.cas.cz/drabinova/' target='_blank'>Adela<br>Drabinova</a></figcaption>")),
                    column(1, align = "center",
                           img(src = "author_ondra.png", width = 75),
                           HTML("<figcaption><a href='https://www.linkedin.com/in/ond%C5%99ej-leder-3864b1119' target='_blank'>Ondrej<br>Leder</a></figcaption>")),
                    column(1, align = "center",
                           img(src = "author_jakub.png", width = 75),
                           HTML("<figcaption>Jakub<br>Houdek</figcaption>")),
                    column(1, align = "center",
                           img(src = "author_lubos.jpg", width = 75),
                           HTML("<figcaption>Lubomir<br>Stepanek</figcaption>"))),

                  #------------------------------------------------------------------------------------#
                  # Packages ####
                  #------------------------------------------------------------------------------------#
                  h4("List of packages used"),
                  fluidRow(
                    column(2,
                           code("library(corrplot)"), br(),
                           code("library(CTT)"), br(),
                           code("library(data.table)"), br(),
                           code("library(deltaPlotR)"), br(),
                           code("library(DT)"), br(),
                           code("library(difNLR)"), br(),
                           code("library(difR)"), br()),
                    column(2,
                           code("library(ggplot2)"), br(),
                           code("library(grid)"), br(),
                           code("library(gridExtra)"), br(),
                           code("library(knitr)"), br(),
                           code("library(latticeExtra)"), br(),
                           code("library(ltm)"), br(),
                           code("library(mirt)"), br()),
                    column(2,
                           code("library(moments)"), br(),
                           code("library(msm)"), br(),
                           code("library(nnet)"), br(),
                           code("library(plotly)"), br(),
                           code("library(psych)"), br(),
                           code("library(psychometric)"), br(),
                           code("library(reshape2)"), br()),
                    column(2,
                           code("library(rmarkdown)"), br(),
                           code("library(shiny)"), br(),
                           code("library(shinyBS)"), br(),
                           code("library(shinyjs)"), br(),
                           code("library(stringr)"), br(),
                           code("library(WrightMap)"), br(),
                           code("library(xtable)"), br())),

                  #------------------------------------------------------------------------------------#
                  # References ####
                  #------------------------------------------------------------------------------------#
                  h4("References"),
                  p("To cite package", code("ShinyItemAnalysis"), "in publications please use:"),
                  p("Martinkova P., Drabinova A., Leder O., & Houdek J. (2018).
                    ShinyItemAnalysis: Test and item analysis via shiny.
                    R package version 1.2.6. https://CRAN.R-project.org/package=ShinyItemAnalysis"),
                  p("Martinkova, P., Drabinova, A., & Houdek, J. (2017).
                    ShinyItemAnalysis: Analyza prijimacich a jinych znalostnich ci psychologickych testu
                    [ShinyItemAnalysis: Analyzing admission and other educational and psychological tests].
                    TESTFORUM, 6(9), 16-35. doi:10.5817/TF2017-9-129"),

                  #------------------------------------------------------------------------------------#
                  # Bug reports ####
                  #------------------------------------------------------------------------------------#
                  h4("Feedback"),
                  p("If you discover a problem with this application please contact the project
                    maintainer at martinkova(at)cs.cas.cz or use ",
                    a("GitHub.",
                      href = "https://github.com/patriciamar/ShinyItemAnalysis/issues",
                      target = "_blank"),
                    "We also encourage you to provide your feedback using ",
                    a("google form.",
                      href = "https://goo.gl/forms/5ZVR6mTOFJFwmtT52",
                      target = "_blank")),

                  #------------------------------------------------------------------------------------#
                  # Acknowledgments ####
                  #------------------------------------------------------------------------------------#
                  h4("Acknowledgments"),
                  p(" Project was supported by grant funded by Czech Science Foundation under number ",
                    a("GJ15-15856Y.",
                      href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
                      target = "_blank")),

                  #------------------------------------------------------------------------------------#
                  # Licence ####
                  #------------------------------------------------------------------------------------#
                  h4("License"),
                  p("This program is free software and you can redistribute it and or modify it under
                    the terms of the",
                    a("GNU GPL 3",
                      href = "https://www.gnu.org/licenses/gpl-3.0.en.html",
                      target = "_blank"),
                    "as published by the Free Software Foundation. This program is distributed in the
                    hope that it will be useful, but without any warranty; without even the implied
                    warranty of merchantability of fitness for a particular purpose." ),
                  br(),
                  br())

