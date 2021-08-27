ui_DIF_adjacent <- tabPanel(
  "Adjacent category logit",
  tabsetPanel(
    # ** Summary ####
    tabPanel(
      "Summary",
      h3("Adjacent category logit model for DIF detection"),
      p("An adjacent category logit regression allows for detection of uniform and non-uniform DIF among ordinal
        data by adding a group-membership variable (uniform DIF) and its interaction with observed score
        (non-uniform DIF) into a model for item \\(i\\) and by testing for their significance."),
      h4("Method specification"),
      p("Here you can change the ", strong("type"), " of DIF to be tested, the ", strong("Observed score", .noWS = "outside"),
        ", and", strong("parametrization"), "- either based on IRT models or classical intercept/slope. You can also
        select the ", strong("correction method"), " for multiple comparison and/or ", strong("item purification. ")),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_adjacent_summary_type",
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
            inputId = "DIF_adjacent_summary_matching",
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
            inputId = "DIF_adjacent_summary_parametrization",
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
            inputId = "DIF_adjacent_summary_correction",
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
            inputId = "DIF_adjacent_summary_purification",
            label = "Item purification",
            value = FALSE
          )
        )
      ),
      h4("Equation"),
      p("The probability that respondent ", strong("\\(p\\)"), " with the observed score (e.g., standardized total
        score) ", strong("\\(Z_p\\)"), " and the group membership variable ", strong("\\(G_p\\)"), " obtained ",
        strong("\\(k\\)"), " points in item ", strong("\\(i\\)"), " is given by the following equation: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_adjacent_summary_equation"))),
      h4("Summary table"),
      p("Summary table contains information about \\(\\chi^2\\)-statistics of the likelihood ratio test, corresponding
        \\(p\\)-values considering selected correction method, and significance codes. Table also provides estimated parameters
        for the best fitted model for each item. "),
      uiOutput("DIF_adjacent_summary_NA_warning"),
      strong(textOutput("DIF_adjacent_summary_dif_items")),
      br(),
      tags$head(tags$style("#DIF_adjacent_summary_coef  {white-space: nowrap;}")),
      fluidRow(column(12, align = "left", tableOutput("DIF_adjacent_summary_coef"))),
      fluidRow(column(12, align = "left", uiOutput("DIF_adjacent_summary_table_note"))),
      br(),
      fluidRow(column(2, downloadButton(outputId = "DIF_adjacent_summary_table_download", label = "Download table"))),
      br(),
      h4("Purification process"),
      textOutput("DIF_adjacent_summary_purification_info"),
      br(),
      tags$head(tags$style("#DIF_adjacent_summary_purification_table  {white-space: nowrap;}")),
      fluidRow(column(12, align = "center", tableOutput("DIF_adjacent_summary_purification_table"))),
      conditionalPanel(
        "input.DIF_adjacent_summary_purification == 1",
        downloadButton(outputId = "DIF_adjacent_summary_purification_table_download", label = "Download table"), br(), br()
      ),
      h4("Selected R code"),
      code(includeText("sc/dif/adj.R"))
    ),
    # ** Items ####
    tabPanel("Items",
      value = "acl_it",
      h3("Adjacent category logit model for DIF detection"),
      p("An adjacent category logit regression allows for detection of uniform and non-uniform DIF among ordinal
        data by adding a group-membership variable (uniform DIF) and its interaction with observed score
        (non-uniform DIF) into a model for item \\(i\\) and by testing for their significance."),
      h4("Method specification"),
      p("Here you can change ", strong("type"), " of DIF to be tested, ", strong("Observed score", .noWS = "outside"),
        ", and", strong("parametrization"), "- either based on IRT models or classical intercept/slope. You can also
        select ", strong("correction method"), " for multiple comparison and/or ", strong("item purification. ")),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_adjacent_items_type",
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
            inputId = "DIF_adjacent_items_matching",
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
            inputId = "DIF_adjacent_items_parametrization",
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
            inputId = "DIF_adjacent_items_correction",
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
            inputId = "DIF_adjacent_items_purification",
            label = "Item purification",
            value = FALSE
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "DIF_adjacent_items",
            label = "Item",
            min = 1,
            value = 1,
            max = 10,
            step = 1,
            animate = animationOptions(interval = 1600)
          )
        )
      ),
      uiOutput("DIF_adjacent_items_NA_warning"),
      h4("Plot with estimated DIF curves"),
      p("Points represent proportion of obtained score with respect to the observed score. Their size is determined
        by count of respondents who achieved given level of the observed score and who selected given option with
        respect to the group membership."),
      plotlyOutput("DIF_adjacent_items_plot"),
      downloadButton("DIF_adjacent_items_plot_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("DIF_adjacent_items_equation"))),
      h4("Table of parameters"),
      p("Table summarizes estimated item parameters together with standard errors. "),
      fluidRow(column(12, align = "center", tableOutput("DIF_adjacent_items_coef"))),
      br(),
      h4("Selected R code"),
      code(includeText("sc/dif/adj_it.R"))
    )
  )
)
