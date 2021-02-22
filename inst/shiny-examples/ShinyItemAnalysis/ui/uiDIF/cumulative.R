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
      div(code(HTML('library(difNLR)<br><br>#&nbsp;Loading&nbsp;data<br>data(dataMedicalgraded,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br>Data&nbsp;<-&nbsp;dataMedicalgraded[,&nbsp;1:100]<br>group&nbsp;<-&nbsp;dataMedicalgraded[,&nbsp;101]<br><br>#&nbsp;DIF&nbsp;with&nbsp;cumulative&nbsp;logit&nbsp;regression&nbsp;model<br>(fit&nbsp;<-&nbsp;difORD(Data&nbsp;=&nbsp;Data,&nbsp;group&nbsp;=&nbsp;group,&nbsp;focal.name&nbsp;=&nbsp;1,&nbsp;model&nbsp;=&nbsp;\"cumulative\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;type&nbsp;=&nbsp;\"both\",&nbsp;match&nbsp;=&nbsp;\"zscore\",&nbsp;p.adjust.method&nbsp;=&nbsp;\"none\",&nbsp;purify&nbsp;=&nbsp;FALSE,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;parametrization&nbsp;=&nbsp;\"classic\"))'))),
      br()
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
      div(code(HTML('library(difNLR)<br><br>#&nbsp;Loading&nbsp;data<br>data(dataMedicalgraded,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br>Data&nbsp;<-&nbsp;dataMedicalgraded[,&nbsp;1:100]<br>group&nbsp;<-&nbsp;dataMedicalgraded[,&nbsp;101]<br><br>#&nbsp;DIF&nbsp;with&nbsp;cumulative&nbsp;logit&nbsp;regression&nbsp;model<br>(fit&nbsp;<-&nbsp;difORD(Data&nbsp;=&nbsp;Data,&nbsp;group&nbsp;=&nbsp;group,&nbsp;focal.name&nbsp;=&nbsp;1,&nbsp;model&nbsp;=&nbsp;\"cumulative\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;type&nbsp;=&nbsp;\"both\",&nbsp;match&nbsp;=&nbsp;\"zscore\",&nbsp;p.adjust.method&nbsp;=&nbsp;\"none\",&nbsp;purify&nbsp;=&nbsp;FALSE,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;parametrization&nbsp;=&nbsp;\"classic\"))<br><br>#&nbsp;Plot&nbsp;of&nbsp;characteristic&nbsp;curves&nbsp;for&nbsp;item&nbsp;X2003,&nbsp;cumulative&nbsp;probabilities<br>plot(fit,&nbsp;item&nbsp;=&nbsp;\"X2003\",&nbsp;plot.type&nbsp;=&nbsp;\"cumulative\")<br><br>#&nbsp;Plot&nbsp;of&nbsp;characteristic&nbsp;curves&nbsp;for&nbsp;item&nbsp;X2003,&nbsp;category&nbsp;probabilities<br>plot(fit,&nbsp;item&nbsp;=&nbsp;\"X2003\",&nbsp;plot.type&nbsp;=&nbsp;\"category\")<br><br>#&nbsp;Estimated&nbsp;coefficients&nbsp;for&nbsp;all&nbsp;items&nbsp;with&nbsp;standard&nbsp;errors<br>coef(fit,&nbsp;SE&nbsp;=&nbsp;TRUE)'))),
      br()
    )
  )
)
