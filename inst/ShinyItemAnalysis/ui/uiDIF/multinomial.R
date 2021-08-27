ui_DIF_multinomial <- tabPanel(
  "Multinomial",
  tabsetPanel(
    # ** Summary ####
    tabPanel(
      "Summary",
      h3("Multinomial model for DDF detection"),
      p("Differential distractor functioning (DDF) occurs when respondents from different groups but with the same ability
        have a different probability of selecting item responses in a multiple-choice item. DDF is examined here by multinomial
        log-linear regression model. "),
      h4("Method specification"),
      p(
        "Here you can change the ", strong("type"), " of DDF to be tested, the ", strong("Observed score", .noWS = "outside"),
        ", and the", strong("parametrization"), "- either IRT or intercept/slope. You can also
        select the ", strong("correction method"), " for a multiple comparison and/or ", strong("item purification. ")
      ),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_multinomial_summary_type",
            label = "Type",
            choices = c(
              "\\(H_{0}\\): No DDF vs. \\(H_{1}\\): Any DDF" = "both",
              "\\(H_{0}\\): No DDF vs. \\(H_{1}\\): Uniform DDF" = "udif",
              "\\(H_{0}\\): Uniform DDF vs. \\(H_{1}\\): Non-uniform DDF" = "nudif"
            ),
            selected = "both"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_multinomial_summary_matching",
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
            inputId = "DIF_multinomial_summary_parametrization",
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
            inputId = "DIF_multinomial_summary_correction",
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
            inputId = "DIF_multinomial_summary_purification",
            label = "Item purification",
            value = FALSE
          )
        )
      ),
      h4("Equation"),
      p("For ", strong("\\(K_i\\)"), " possible item responses, the probability of the correct answer ",
        strong("\\(K_i\\)"), " for respondent ", strong("\\(p\\)"), " with a DIF matching variable
            (e.g., standardized total score) ", strong("\\(Z_p\\)"), " and a group membership ", strong("\\(G_p\\)"),
        " in item ", strong("\\(i\\)"), "is given by the following equation: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_multinomial_summary_equation_correct"))),
      p("The probability of choosing distractor ", strong("\\(k\\)"), " is then given by: "),
      fluidRow(column(12, align = "center", uiOutput("DIF_multinomial_summary_equation_distractor"))),
      h4("Summary table"),
      p("This summary table contains information about \\(\\chi^2\\)-statistics of the likelihood ratio test, corresponding
        \\(p\\)-values considering selected correction method, and significance codes. "),
      uiOutput("DIF_multinomial_summary_NA_warning"),
      strong(textOutput("DIF_multinomial_summary_dif_items")),
      br(),
      tags$head(tags$style("#DIF_multinomial_summary_coef  {white-space: nowrap;}")),
      fluidRow(column(12, align = "left", tableOutput("DIF_multinomial_summary_coef"))),
      fluidRow(column(12, align = "left", uiOutput("DIF_multinomial_summary_table_note"))),
      br(),
      fluidRow(column(2, downloadButton(outputId = "DIF_multinomial_summary_table_download", label = "Download table"))),
      br(),
      h4("Estimates of item parameters"),
      p("Table provides estimated parameters for the fitted model for each item and distractor (incorrect option). "),
      tags$head(tags$style("#DIF_multinomial_summary_coef_parameters  {white-space: nowrap;}")),
      fluidRow(column(12, align = "left", tableOutput("DIF_multinomial_summary_coef_parameters"))),
      # fluidRow(column(12, align = "left", uiOutput("note_multinomial_2"))),
      br(),
      fluidRow(column(2, downloadButton(outputId = "DIF_multinomial_summary_parameters_download", label = "Download table"))),
      br(),
      h4("Purification process"),
      textOutput("DIF_multinomial_summary_purification_info"),
      br(),
      tags$head(tags$style("#DIF_multinomial_summary_purification_table  {white-space: nowrap;}")),
      fluidRow(column(12, align = "center", tableOutput("DIF_multinomial_summary_purification_table"))),
      conditionalPanel(
        "input.DIF_multinomial_summary_purification == 1",
        downloadButton(outputId = "DIF_multinomial_summary_purification_table_download", label = "Download table"),
        br(),
        br()
      ),
      h4("Selected R code"),
      code(includeText("sc/dif/mult.R"))
    ),
    # ** Items ####
    tabPanel("Items",
      value = "mrm_it",
      h3("Multinomial model for DDF detection"),
      p("Differential distractor functioning (DDF) occurs when respondents from different groups but with the same ability
        have a different probability of selecting item responses in a multiple-choice item. DDF is examined here by multinomial
        log-linear regression model. "),
      h4("Method specification"),
      p("Here you can change the ", strong("type"), " of DDF to be tested, the ", strong("Observed score", .noWS = "outside"),
        ", and the", strong("parametrization"), "- either IRT or intercept/slope. You can also
        select the ", strong("correction method"), " for a multiple comparison and/or ", strong("item purification. ")),
      fluidRow(
        column(
          3,
          radioButtons(
            inputId = "DIF_multinomial_items_type",
            label = "Type",
            choices = c(
              "\\(H_{0}\\): No DDF vs. \\(H_{1}\\): Any DDF" = "both",
              "\\(H_{0}\\): No DDF vs. \\(H_{1}\\): Uniform DDF" = "udif",
              "\\(H_{0}\\): Uniform DDF vs. \\(H_{1}\\): Non-uniform DDF" = "nudif"
            ),
            selected = "both"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_multinomial_items_matching",
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
            inputId = "DIF_multinomial_items_parametrization",
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
            inputId = "DIF_multinomial_items_correction",
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
            inputId = "DIF_multinomial_items_purification",
            label = "Item purification",
            value = FALSE
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "DIF_multinomial_items",
            label = "Item",
            min = 1,
            value = 1,
            max = 10,
            step = 1,
            animate = animationOptions(interval = 1600)
          )
        )
      ),
      uiOutput("DIF_multinomial_items_NA_warning"),
      h4("Plot with estimated DDF curves"),
      p("Points represent a proportion of the response selection with respect to the observed score. Their size is determined
        by the count of respondents from a given group who achieved a given level of the observed score and who selected a given response option."),
      plotlyOutput("DIF_multinomial_items_plot"),
      downloadButton("DIF_multinomial_items_plot_download", label = "Download figure"),
      downloadButton("DIF_multinomial_items_plot_download_all", label = "Download all figures"),
      h4("Equation"),
      fluidRow(column(12, uiOutput("DIF_multinomial_items_equation"))),
      h4("Table of parameters"),
      p("Table summarizes estimated item parameters together with standard errors. "),
      fluidRow(column(12, align = "center", tableOutput("DIF_multinomial_items_coef"))),
      br(),
      h4("Selected R code"),
      code(includeText("sc/dif/mult_it.R"))
    )
  )
)
