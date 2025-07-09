uiAbout <- tabPanel("",
  icon = icon("house"),
  value = "About",
  id = "about",
  #------------------------------------------------------------------------------------#
  # Description ####
  #------------------------------------------------------------------------------------#
  h3("Welcome"),
  p("Welcome to ShinyItemAnalysis!"),
  p(
    "ShinyItemAnalysis is an interactive online application for the psychometric analysis of educational tests,
    psychological assessments, health-related and other types of multi-item measurements, or ratings from
    multiple raters, built on",
    a("R", href = "https://cran.r-project.org/", target = "_blank"),
    "and",
    a("shiny", href = "http://www.rstudio.com/shiny/", target = "_blank", .noWS = "after"),
    ". You can easily start using the application with the default toy dataset. You may also select from a number of other
    toy datasets or upload your own in the ", strong("Data"), " section. Offered methods include:"
  ),
  tags$ul(
    tags$li("Exploration of total and standard scores in the", strong("Summary"), "section"),
    tags$li("Analysis of measurement error in the", strong("Reliability"), "section"),
    tags$li("Correlation structure and criterion validity analysis in the", strong("Validity"), "section"),
    tags$li("Item and distractor analysis in the", strong("Item analysis"), "section"),
    tags$li("Item analysis with regression models in the", strong("Regression"), "section"),
    tags$li("Item analysis by item response theory models in the", strong("IRT models"), "section"),
    tags$li("Detection of differential item functioning in the", strong("DIF/Fairness"), "section"),
    tags$li("Additional psychometric methods implemented via add-on modules in the", strong("Modules"), "section")
  ),
  p(
  "All graphical outputs and selected tables can be downloaded via the download button. Moreover, you can automatically
   generate a HTML or PDF report in the", strong("Reports"), "section. All offered analyses
   are complemented by selected R codes which are ready to be copied and pasted into your R console, therefore
   a similar analysis can be run and modified in R."),
  p(
    "Visit the", a(strong("ShinyItemAnalysis.org"), href = "https://www.shinyitemanalysis.org",
    target = "_blank"
  ), "webpage to learn more about ShinyItemAnalysis!"),
  tags$hr(),

  #------------------------------------------------------------------------------------#
  # News ####
  #------------------------------------------------------------------------------------#
  h4("News"),
  fluidRow(
    column(
      10,
      tags$ul(
        tags$li(
          "A new book on Computational Aspects of Psychometric Methods: With R ", a("(Martinkova & Hladka, 2023)",
            href = "https://www.taylorfrancis.com/books/mono/10.1201/9781003054313",
            target = "_blank", .noWS = "outside"
          ),
          " is available. Individual chapters of the book describe sections of the ShinyItemAnalysis. "
        ),
        tags$li(
          "A manuscript on algorithms for estimation of item functioning and for the DIF detection is being published in JEBS ",
          a("(Hladka, Martinkova, & Brabec, 2024)",
            href = "https://doi.org/10.3102/10769986241312354",
            target = "_blank", .noWS = "outside"
          ),
          ".  Estimation methods are available in the ",
          strong("DIF/Fairness/Generalized logistics"), "section. "
        ),
        tags$li(
          "A new paper on relationship between the inter-rater reliability and false positive rate has been published in BJMSP ",
          a("(Bartos & Martinkova, 2024)",
            href = "https://doi.org/10.1111/bmsp.12343",
            target = "_blank", .noWS = "outside"
          ),
          ".  To try examples interactively, go to the IRR2FPR Module of the IRR2FPR package, available from the ",
          strong("Reliability"), "section. "
        ),
        tags$li(
          "A new paper on combining item purification and multiple comparison adjustment methods in DIF detection has been published in MBR ",
          a("(Hladka, Martinkova, & Magis, 2024)",
            href = "https://doi.org/10.1080/00273171.2023.2205393",
            target = "_blank", .noWS = "outside"
          ),
          ".  Methods are incorporated in  the ",
          strong("DIF/Fairness"), "section. "
        ),
        tags$li(
          "A new flexible method for assessing inter-rater reliability with heterogeneous variance components models, accounting for contextual variables
      has been published in JEBS ",
          a("(Martinkova, Bartos, & Brabec, 2023)",
            href = "https://doi.org/10.3102/10769986221150517",
            target = "_blank", .noWS = "outside"
          ),
          ".  The paper uses the NIH data available in the ShinyItemAnalysis package. "
        ),
        tags$li(
          "A new paper on machine learning methods for prediction of item difficulty from item wording has been published in Mathematics ",
          a("(Stepanek, Dlouha, & Martinkova, 2023)",
            href = "https://doi.org/10.3390/math11194104",
            target = "_blank", .noWS = "outside"
          ),
          ".  To try examples interactively,
    go to the Item Text Analysis Module of the EduTestTextAnalysis package, available from the ",
          strong("Modules"), "section. "
        ),
        tags$li(
          "A new paper on range-restricted inter-rater reliability has been published in JRSS-A ",
          a("(Erosheva, Martinkova, & Lee, 2021)",
            href = "https://doi.org/10.1111/rssa.12681",
            target = "_blank", .noWS = "outside"
          ),
          ".  To try examples interactively with the", code("AIBS"), "dataset,
    go to the Restricted-range Reliability Module of the SIAmodules package, available from the ",
          strong("Reliability"), "section. "
        ),
        tags$li(
          "A new paper using DIF-C analysis has been published in JCAL ",
          a("(Kolek, Sisler, Martinkova, & Brom, 2021)",
            href = "https://doi.org/10.1111/jcal.12575",
            target = "_blank", .noWS = "outside"
          ),
          ".  To try examples interactively with the", code("AttitudesExpulsion"), "dataset,
    go to the DIF-C Module available from the ",
          strong("DIF"), "section. Also see the follow-up paper ",
          a("(Kolek, Martinkova, Varejkova, Sisler, & Brom, 2023)",
            href = "https://doi.org/10.1111/jcal.12911",
            target = "_blank", .noWS = "outside"
          ),
          ", published in the same journal. "
        ),
        tags$li(
          "New papers on differential item functioning have been published in Learning and Instruction ",
          a("(Martinkova, Hladka, & Potuznikova, 2020)",
            href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
            target = "_blank", .noWS = "outside"
          ),
          " and in The R Journal ",
          a("(Hladka & Martinkova, 2020)",
            href = "https://doi.org/10.32614/RJ-2020-014",
            target = "_blank", .noWS = "outside"
          ),
          ". To try these examples interactively, set the", code("Learning to Learn 9"), "toy dataset in the ", strong("Data"), " section
    by clicking on the menu in the upper left corner and go to the ",
          strong("DIF/Fairness/Generalized logistic"), "section. Or go to the DIF-C module of the SIAmodules package, available in the ",
          strong("DIF/Fairness"), "section.",
          .noWS = "before-end"
        )
      )
    ),
    column(
      2,
      tags$img(
        src = "book_cover.png",
        class = "img-responsive",
        style = "max-width: 180px; margin-left: auto; margin-right: auto;"
      )
    )
  ),
  tags$hr(),

  #------------------------------------------------------------------------------------#
  # Availability ####
  #------------------------------------------------------------------------------------#
  fluidRow(
    column(
      6,
      h4("Availability"),
      p(
        "An application can be downloaded as an R package from",
        a(
          href = "https://CRAN.R-project.org/package=ShinyItemAnalysis",
          "CRAN",
          target = "_blank",
          .noWS = "after"
        ),
        ".",
        br(),
        "It is also available online at the",
        a(
          href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/",
          "Czech Academy of Sciences 🇨🇿",
          target = "_blank"
        ),
        "and",
        a(
          href = "https://cemp.shinyapps.io/ShinyItemAnalysis/",
          "shinyapps.io 🇺🇸",
          target = "_blank",
          .noWS = "after"
        ),
        "."
      )
    ),

    #------------------------------------------------------------------------------------#
    # Version ####
    #------------------------------------------------------------------------------------#
    column(
      6, h4("Versions"),
      p(
        "The current",
        a("CRAN", href = "https://CRAN.R-project.org/package=ShinyItemAnalysis", target = "_blank"),
        "version is 1.5.5.",
        br(),
        "The version available",
        a("online", href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/", target = "_blank"), "is 1.5.5.",
        br(),
        "The newest development version available on ",
        a("GitHub", href = "https://github.com/patriciamar/ShinyItemAnalysis", target = "_blank"), "is 1.5.5.",
      )
    )
  ),
  tags$hr(),

  #------------------------------------------------------------------------------------#
  # Feedback ####
  #------------------------------------------------------------------------------------#
  h4("Feedback"),
  p(
    "If you discover a problem with this application please contact the project
                    maintainer at martinkova(at)cs.cas.cz or use ",
    a("GitHub.",
      href = "https://github.com/patriciamar/ShinyItemAnalysis/issues",
      target = "_blank"
    ),
    "We also encourage you to provide your feedback using ",
    a("Google form.",
      href = "https://goo.gl/forms/5ZVR6mTOFJFwmtT52",
      target = "_blank"
    )
  ),
  tags$hr(),
  #------------------------------------------------------------------------------------#
  # Licence ####
  #------------------------------------------------------------------------------------#
  h4("License"),
  p(
    "This program is free software and you can redistribute it and or modify it under
                    the terms of the",
    a("GNU GPL 3",
      href = "https://www.gnu.org/licenses/gpl-3.0.en.html",
      target = "_blank"
    ),
    "as published by the Free Software Foundation. This program is distributed in the
     hope that it will be useful, but without any warranty; without even the implied
     warranty of merchantability of fitness for a particular purpose."
  ),

  #------------------------------------------------------------------------------------#
  # References ####
  #------------------------------------------------------------------------------------#

  p("To cite ShinyItemAnalysis in publications, please use:"),
  div(
    class = "cite-box",
    "Martinkova, P., & Hladka, A. (2023).", br(),
    em("Computational Aspects of Psychometric Methods: With R. (1st ed.).", .noWS = "outside"), br(),
    "Chapman and Hall/CRC. doi:",
    a("10.1201/9781003054313",
      href = "https://doi.org/10.1201/9781003054313",
      target = "_blank"
    )
  ),
  p("and/or:"),
  div(
    class = "cite-box",
    "Martinkova, P., & Drabinova, A. (2018).", br(), "
                    ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.", br(),
    em("The R Journal, 10", .noWS = "outside"), "(2), 503-515, doi:",
    a("10.32614/RJ-2018-074",
      href = "https://doi.org/10.32614/RJ-2018-074",
      target = "_blank"
    )
  ),
  # p("In Czech written papers you can also use:"),
  # div(class = "cite-box",
  #  "Martinkova, P., Drabinova, A., & Houdek, J. (2017).", br(), "
  #  ShinyItemAnalysis: Analyza prijimacich a jinych znalostnich ci psychologickych testu. [ShinyItemAnalysis: Analyzing admission and other educational and psychological tests. In Czech].", br(), "
  #  TESTFORUM, 6(9), 16-35. doi:",
  #  a("10.5817/TF2017-9-129",
  #    href = "https://doi.org/10.5817/TF2017-9-129",
  #    target = "_blank")),
  tags$hr(),

  #------------------------------------------------------------------------------------#
  # Funding ####
  #------------------------------------------------------------------------------------#
  h4("Funding"),
  p("European Union (CZ. 02.01.01/00/22_008/0004583), Czech Science Foundation (21-03658S, GJ15-15856Y), Charles University (PRIMUS/17/HUM/11).")
)
