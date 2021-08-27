ui_DIF_logistic <- tabPanel(
  "Logistic regression",
  tabsetPanel(
    # ** Summary ####
    tabPanel(
      "Summary",
      h3("Logistic regression"),
      p("The logistic regression method allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by including
        a group-membership variable (uniform DIF) and its interaction with a matching criterion (non-uniform DIF) into a model
        for item \\(i\\) and by testing for significance of their effect."),
      h4("Method specification"),
      p("Here you can change ", strong("type"), " of DIF to be tested and ", strong("parametrization"), "- either based on IRT
        models or classical intercept/slope. You can also select a ", strong("correction method"), " for multiple comparison and/or
        ", strong("item purification. "), "Finally, you may also change the ", strong("Observed score."), " While matching
        on the standardized total score is typical, the upload of other observed scores is possible in the ", strong("Data. "),
        "section. Using a pre-test (standardized) total score as the observed score allows for testing a differential item functioning
        in change (DIF-C) to provide proofs of instructional sensitivity ",
        a("(Martinkova et al., 2020), ",
          href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
          target = "_blank"
        ), "also see", code("Learning To Learn 9"), " toy dataset."),
      fluidRow(
        column(
          3,
          withMathJax(),
          radioButtons(
            inputId = "DIF_logistic_summary_type",
            label = "Type",
            choices = c(
              "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Any DIF" = "both",
              "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Uniform DIF" = "udif",
              "\\(H_{0}\\): Uniform DIF vs. \\(H_{1}\\): Non-uniform DIF" = "nudif"
            ),
            selected = "both"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_logistic_summary_matching",
            label = "Observed score",
            choices = c(
              "Total score" = "score",
              "Standardized total score" = "zscore"
            ),
            selected = "zscore"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_logistic_summary_parametrization",
            label = "Parametrization",
            choices = c(
              "Intercept/slope" = "classic",
              "IRT" = "irt"
            ),
            selected = "irt"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_logistic_summary_correction",
            label = "Correction method",
            choices = c(
              "Benjamini-Hochberg" = "BH",
              "Benjamini-Yekutieli" = "BY",
              "Bonferroni" = "bonferroni",
              "Holm" = "holm",
              "Hochberg" = "hochberg",
              "Hommel" = "hommel",
              "None" = "none"
            ),
            selected = "none"
          ),
          checkboxInput(
            inputId = "DIF_logistic_summary_purification",
            label = "Item purification",
            value = FALSE
          )
        )
      ),
      h4("Equation"),
      p("The probability that respondent ", strong("\\(p\\)"), " with the observed score ",
        uiOutput("DIF_logistic_summary_matching_text", inline = TRUE), " and the group membership variable ",
        strong("\\(G_p\\)"), " answers correctly item \\(i\\) is given by the following equation: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_logistic_summary_equation"))),
      h4("Summary table"),
      p("The summary table contains information about DIF test statistics \\(LR(\\chi^2)\\) based on a likelihood ratio test,
        the corresponding \\(p\\)-values considering selected adjustement, and the significance codes. Moreover, it offers the values of
        Nagelkerke's \\(R^2\\) with DIF effect size classifications. This table also provides estimated parameters for the best
        fitted model for each item, and their standard errors. "),
      uiOutput("DIF_logistic_summary_NA_warning"),
      strong(textOutput("DIF_logistic_summary_dif_items")),
      br(),
      fluidRow(column(12, align = "left", tableOutput("DIF_logistic_summary_coef"))),
      fluidRow(column(12, align = "left", uiOutput("DIF_logistic_summary_table_note"))),
      br(),
      fluidRow(column(2, downloadButton(outputId = "DIF_logistic_summary_table_download", label = "Download table"))),
      br(),
      h4("Purification process"),
      textOutput("DIF_logistic_summary_purification_info"),
      br(),
      tags$head(tags$style("#DIF_logistic_summary_purification_table  {white-space: nowrap;  }")),
      fluidRow(column(12, align = "center", tableOutput("DIF_logistic_summary_purification_table"))),
      conditionalPanel(
        "input.DIF_logistic_summary_purification == 1",
        downloadButton(outputId = "DIF_logistic_summary_purification_table_download", label = "Download table"),
        br(),
        br()
      ),
      h4("Selected R code"),
      code(includeText("sc/dif/log.R"))
    ),
    # ** Items ####
    tabPanel("Items",
      value = "log_it",
      h3("Logistic regression"),
      p("The logistic regression method allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by including
        a group-membership variable (uniform DIF) and its interaction with a matching criterion (non-uniform DIF) into a model
        for item \\(i\\) and by testing for significance of their effect."),
      h4("Method specification"),
      p("Here you can change ", strong("type"), " of DIF to be tested and ", strong("parametrization"), "- either based on IRT
        models or classical intercept/slope. You can also select a ", strong("correction method"), " for multiple comparison and/or
        ", strong("item purification. "), "Finally, you may also change the ", strong("Observed score."), " While matching
        on the standardized total score is typical, the upload of other observed scores is possible in the ", strong("Data "),
        "section. Using a pre-test (standardized) total score as the observed score allows for testing a differential item functioning
        in change (DIF-C) to provide proofs of instructional sensitivity ",
        a("(Martinkova et al., 2020), ",
          href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
          target = "_blank"
        ), "also see", code("Learning To Learn 9"), " toy dataset. For a selected", strong("item"), "you can display a plot of its
        characteristic curves and a table of its estimated parameters with standard errors. "),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_logistic_items_type",
            label = "Type",
            choices = c(
              "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Any DIF" = "both",
              "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Uniform DIF" = "udif",
              "\\(H_{0}\\): Uniform DIF vs. \\(H_{1}\\): Non-uniform DIF" = "nudif"
            ),
            selected = "both"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_logistic_items_matching",
            label = "Observed score",
            choices = c(
              "Total score" = "score",
              "Standardized total score" = "zscore"
            ),
            selected = "zscore"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_logistic_items_parametrization",
            label = "Parametrization",
            choices = c(
              "Intercept/slope" = "classic",
              "IRT" = "irt"
            ),
            selected = "irt"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_logistic_items_correction",
            label = "Correction method",
            choices = c(
              "Benjamini-Hochberg" = "BH",
              "Benjamini-Yekutieli" = "BY",
              "Bonferroni" = "bonferroni",
              "Holm" = "holm",
              "Hochberg" = "hochberg",
              "Hommel" = "hommel",
              "None" = "none"
            ),
            selected = "none"
          ),
          checkboxInput(
            inputId = "DIF_logistic_items_purification",
            label = "Item purification",
            value = FALSE
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "DIF_logistic_items",
            label = "Item",
            min = 1,
            value = 1,
            max = 10,
            step = 1,
            animate = animationOptions(interval = 1600)
          )
        )
      ),
      uiOutput("DIF_logistic_items_NA_warning"),
      h4("Plot with estimated DIF logistic curve"),
      p("Points represent a proportion of the correct answer (empirical probabilities) with respect to the observed score. Their size is determined
        by the count of respondents who achieved a given level of the observed score and who selected given option with
        respect to the group membership."),
      plotlyOutput("DIF_logistic_items_plot"),
      downloadButton("DIF_logistic_items_plot_download", label = "Download figure"),
      h4("Equation"),
      p("The probability that respondent ", strong("\\(p\\)"), " with the observed score ",
        uiOutput("DIF_logistic_items_matching_text", inline = TRUE), " and the group membership variable ",
        strong("\\(G_p\\)"), " answers correctly item \\(i\\) is given by the following equation: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_logistic_items_equation"))),
      h4("Table of parameters"),
      p("This table summarizes estimated item parameters and their standard errors. "),
      fluidRow(column(12, align = "center", tableOutput("DIF_logistic_items_coef"))),
      br(),
      h4("Selected R code"),
      code(includeText("sc/dif/log_it.R"))
    )
  )
)
