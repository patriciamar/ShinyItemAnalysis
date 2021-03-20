ui_DIF_cumulative <- tabPanel(
  "Cumulative logit",
  tabsetPanel(
    # ** Summary ####
    tabPanel(
      "Summary",
      h3("Cumulative logit model for DIF detection"),
      p("Cumulative logit regression allows for detection of uniform and non-uniform DIF among ordinal data by
        adding a group-membership variable (uniform DIF) and its interaction with observed score
        (non-uniform DIF) into a model for item \\(i\\) and by testing for their significance."),
      h4("Method specification"),
      p("Here you can change the ", strong("type"), " of DIF to be tested, the ", strong("Observed score", .noWS = "outside"),
        ", and the ", strong("parametrization"), "- either the IRT or the classical intercept/slope. You can also
        select a ", strong("correction method"), " for a multiple comparison and/or ", strong("item purification. ")),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_cumulative_summary_type",
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
            inputId = "DIF_cumulative_summary_matching",
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
            inputId = "DIF_cumulative_summary_parametrization",
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
            inputId = "DIF_cumulative_summary_correction",
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
            inputId = "DIF_cumulative_summary_purification",
            label = "Item purification",
            value = FALSE
          )
        )
      ),
      h4("Equation"),
      p("The probability that respondent ", strong("\\(p\\)"), " with the observed score (e.g., standardized total
        score) ", strong("\\(Z_p\\)"), " and the group membership variable ", strong("\\(G_p\\)"), " obtained at least ",
        strong("\\(k\\)"), " points in item ", strong("\\(i\\)"), " is given by the following equation: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_cumulative_summary_equation_cumulative"))),
      p("The probability that respondent ", strong("\\(p\\)"), " with the observed score (e.g., standardized total
        score) ", strong("\\(Z_p\\)"), " and group membership ", strong("\\(G_p\\)"), " obtained exactly ", strong("\\(k\\)"),
        " points in item ", strong("\\(i\\)"), " is then given as the difference between the probabilities of obtaining at least",
        strong("\\(k\\)"), " and ", strong("\\(k + 1\\)"), "points: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_cumulative_summary_equation_category"))),
      h4("Summary table"),
      p("This summary table contains information about \\(\\chi^2\\)-statistics of the likelihood ratio test, corresponding
        \\(p\\)-values considering selected correction method, and significance codes. The table also provides estimated parameters
        for the best fitted model for each item. "),
      uiOutput("DIF_cumulative_summary_NA_warning"),
      strong(textOutput("DIF_cumulative_summary_dif_items")),
      br(),
      tags$head(tags$style("#DIF_cumulative_summary_coef  {white-space: nowrap;}")),
      fluidRow(column(12, align = "left", tableOutput("DIF_cumulative_summary_coef"))),
      fluidRow(column(12, align = "left", uiOutput("DIF_cumulative_summary_table_note"))),
      br(),
      fluidRow(column(2, downloadButton(outputId = "DIF_cumulative_summary_table_download", label = "Download table"))),
      br(),
      h4("Purification process"),
      textOutput("DIF_cumulative_summary_purification_info"),
      br(),
      tags$head(tags$style("#DIF_cumulative_summary_purification_table {white-space: nowrap;}")),
      fluidRow(column(12, align = "center", tableOutput("DIF_cumulative_summary_purification_table"))),
      conditionalPanel(
        "input.DIF_cumulative_summary_purification == 1",
        downloadButton(outputId = "DIF_cumulative_summary_purification_table_download", label = "Download table"),
        br(),
        br()
      ),
      h4("Selected R code"),
      code(includeText("sc/dif/cum.R"))
    ),
    # ** Items ####
    tabPanel("Items",
      value = "cumulative_it",
      h3("Cumulative logit model for DIF detection"),
      p("Cumulative logit regression allows for detection of uniform and non-uniform DIF among ordinal data by
        adding a group-membership variable (uniform DIF) and its interaction with observed score
        (non-uniform DIF) into a model for item \\(i\\) and by testing for their significance."),
      h4("Method specification"),
      p("Here you can change the ", strong("type"), " of DIF to be tested, the ", strong("Observed score", .noWS = "outside"),
        ", and the ", strong("parametrization"), "- either the IRT or classical intercept/slope. You can also
        select a ", strong("correction method"), " for a multiple comparison and/or ", strong("item purification. ")),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_cumulative_items_type",
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
            inputId = "DIF_cumulative_items_matching",
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
            inputId = "DIF_cumulative_items_parametrization",
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
            inputId = "DIF_cumulative_items_correction",
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
            inputId = "DIF_cumulative_items_purification",
            label = "Item purification",
            value = FALSE
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "DIF_cumulative_items",
            label = "Item",
            min = 1,
            value = 1,
            max = 10,
            step = 1,
            animate = animationOptions(interval = 1600)
          )
        )
      ),
      uiOutput("DIF_cumulative_items_NA_warning"),
      h4("Plot with estimated DIF curves"),
      p("Points represent a proportion of the obtained score with respect to the observed score. Their size is determined
        by the count of respondents who achieved a given level of the observed score and who selected given option with
        respect to the group membership."),
      splitLayout(
        cellWidths = c("50%", "50%"),
        plotlyOutput("DIF_cumulative_items_plot_cumulative"),
        plotlyOutput("DIF_cumulative_items_plot_category")
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton("DIF_cumulative_items_plot_cumulative_download", label = "Download figure"),
        downloadButton("DIF_cumulative_items_plot_category_download", label = "Download figure")
      ),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("DIF_cumulative_items_equation_cumulative"))),
      fluidRow(column(12, align = "center", uiOutput("DIF_cumulative_items_equation_category"))),
      h4("Table of parameters"),
      p("This table summarizes estimated item parameters together with the standard errors. "),
      fluidRow(column(12, align = "center", tableOutput("DIF_cumulative_items_coef"))),
      br(),
      h4("Selected R code"),
      code(includeText("sc/dif/cum_it.R"))
    )
  )
)
