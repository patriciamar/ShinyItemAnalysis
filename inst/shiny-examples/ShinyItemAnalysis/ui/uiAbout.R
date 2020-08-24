uiAbout <- tabPanel("",
				  icon = icon("fas fa-home"),
				  value = "About",
				  id = "about",
                  #------------------------------------------------------------------------------------#
                  # Description ####
                  #------------------------------------------------------------------------------------#
                  h3("Welcome"),
                  p("Welcome to ShinyItemAnalysis!"),
                  p("ShinyItemAnalysis is an interactive online application for psychometric analysis of educational
                     and other psychological tests and their items, built
                    on ",
                    a("R", href = "https://cran.r-project.org/", target = "_blank"),
                    "and",
                    a("shiny. ", href = "http://www.rstudio.com/shiny/", target = "_blank"),
                    "You can simply start using the application by choosing toy dataset (or upload your own one)
                    in section ", strong("Data"), " and run analysis including:" ),
                  tags$ul(tags$li("Exploration of total and standard scores in ", strong("Summary"), "section"),
                          tags$li("Analysis of measurement error in ", strong("Reliability"), "section"),
                          tags$li("Correlation structure and criterion validity analysis in ", strong("Validity"), "section"),
                          tags$li("Item and distractor analysis in ", strong("Item analysis"), "section"),
                          tags$li("Item analysis with regression models in ", strong("Regression"), "section"),
                          tags$li("Item analysis by item response theory models in ", strong("IRT models"), "section"),
                          tags$li("Differential item functioning (DIF) and differential distractor functioning (DDF)
                                  methods in ", strong("DIF/Fairness"), "section")),
                  p("All graphical outputs and selected tables can be downloaded via download button. Moreover, you can automatically
                    generate HTML or PDF report in", strong("Reports"), "section. All offered analyses
                    are complemented by selected R code which is ready to be copy-pasted into your R console, hence
                    a similar analysis can be run and modified in R."),
				          p("Visit ",a(strong("www.ShinyItemAnalysis.org"), href = "http://www.shinyitemanalysis.org",
				                       target = "_blank"), "webpage to learn more about ShinyItemAnalysis!"),
                  tags$hr(),
                  #------------------------------------------------------------------------------------#
                  # Availability ####
                  #------------------------------------------------------------------------------------#
                  fluidRow(
                    column(6,
                           h4("Availability"),
                           p("Application can be downloaded as an R package from ",
                             a("CRAN.", href = "https://CRAN.R-project.org/package=ShinyItemAnalysis", target = "_blank"),
                             br(),
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
                           ),

                    #------------------------------------------------------------------------------------#
                    # Version ####
                    #------------------------------------------------------------------------------------#
                    column(6, h4("Versions"),
                           p("Current",
                             a("CRAN", href = "https://CRAN.R-project.org/package=ShinyItemAnalysis", target = "_blank"),
                             "version is 1.3.4.",
                             br(),
                             "Version available",
                             a("online", href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/", target = "_blank"), "is 1.3.4.",
                             br(),
                             "The newest development version available on ",
                             a("GitHub", href = "https://github.com/patriciamar/ShinyItemAnalysis", target = "_blank"), "is 1.3.4.",
                             ))),
                  tags$hr(),

                  #------------------------------------------------------------------------------------#
                  # Feedback ####
                  #------------------------------------------------------------------------------------#
                  h4("Feedback"),
                  p("If you discover a problem with this application please contact the project
                    maintainer at martinkova(at)cs.cas.cz or use ",
                    a("GitHub.",
                      href = "https://github.com/patriciamar/ShinyItemAnalysis/issues",
                      target = "_blank"),
                    "We also encourage you to provide your feedback using ",
                    a("Google form.",
                      href = "https://goo.gl/forms/5ZVR6mTOFJFwmtT52",
                      target = "_blank")),
                  tags$hr(),
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

                  #------------------------------------------------------------------------------------#
                  # References ####
                  #------------------------------------------------------------------------------------#

                  p("To cite ShinyItemAnalysis in publications, please use:"),
                  div(class = "cite-box",
                    "Martinkova P., & Drabinova A. (2018).", br(), "
                    ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.", br(), "
                    The R Journal, 10(2), 503-515. doi:",
                    a("10.32614/RJ-2018-074",
                      href = "https://doi.org/10.32614/RJ-2018-074",
                      target = "_blank")),
				          #p("In Czech written papers you can also use:"),
                  #div(class = "cite-box",
                  #  "Martinkova, P., Drabinova, A., & Houdek, J. (2017).", br(), "
                  #  ShinyItemAnalysis: Analyza prijimacich a jinych znalostnich ci psychologickych testu. [ShinyItemAnalysis: Analyzing admission and other educational and psychological tests. In Czech].", br(), "
                  #  TESTFORUM, 6(9), 16-35. doi:",
				          #  a("10.5817/TF2017-9-129",
				          #    href = "https://doi.org/10.5817/TF2017-9-129",
				          #    target = "_blank")),

                  tags$hr(),

                  #------------------------------------------------------------------------------------#
                  # Acknowledgments ####
                  #------------------------------------------------------------------------------------#
                  h4("Acknowledgments"),
                  p("Project was supported by Czech Science Foundation grant GJ15-15856Y 'Estimation of psychometric
                    measures as part of admission test development' and by Charles University under project PRIMUS/17/HUM/11
                    'Center for Educational Measurement and Psychometrics (CEMP)'."),
                  br(),
                  br()
                  )

