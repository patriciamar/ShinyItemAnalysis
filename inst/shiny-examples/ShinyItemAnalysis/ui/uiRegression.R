uiRegression <-
  navbarMenu(
    "Regression",
    "Dichotomous models",
    # * LOGISTIC ####
    tabPanel(
      "Logistic",
      h3("Logistic regression on total scores"),
      withMathJax(),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Logistic regression"),
        "can model dependency of pthe robability of correctly answering item \\(i\\) by respondent \\(p\\) on their
                        total score \\(X_p\\) by an S-shaped logistic curve. Parameter", strong("\\(\\beta_{i0}\\)"), " describes
                        horizontal position of the fitted curve and parameter ", strong("\\(\\beta_{i1}\\)"), " describes its slope."
      ),
      br(),
      h4("Plot with estimated logistic curve"),
      p("Points represent proportion of correct answers with respect to the total score. Their size is determined by the count of respondents
                        who achieved a given level of the total score."),
      sliderInput(
        inputId = "regression_logistic_item_slider", label = "Item",
        min = 1, value = 1, max = 10,
        step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_logistic_na_alert"),
      plotlyOutput("regression_logistic_plot"),
      downloadButton("regression_logistic_plot_download", label = "Download figure"),
      h4("Equation"),
      withMathJax(),
      ("$$\\mathrm{P}(Y_{pi} = 1|X_p) = \\mathrm{E}(Y_{pi}|X_p) = \\frac{e^{\\left(\\beta_{i0} + \\beta_{i1} X_p\\right)}}{1 + e^{\\left(\\beta_{i0} + \\beta_{i1} X_p\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_logistic_coef"))),
      htmlOutput("regression_logistic_interpretation"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/regr/log.R"))
    ),
    # * LOGISTIC Z ####
    tabPanel(
      "Logistic Z",
      h3("Logistic regression on standardized total scores"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.",
        strong("Logistic regression"), "can model dependency of the probability of correctly answering item \\(i\\) by respondent
                        \\(p\\) on their standardized total score \\(Z_p\\) (Z-score) by an S-shaped logistic curve. Parameter",
        strong("\\(\\beta_{i0}\\)"), " describes horizontal position of the fitted curve and parameter ",
        strong("\\(\\beta_{i1}\\)"), " describes its slope."
      ),
      br(),
      h4("Plot with estimated logistic curve"),
      p("Points represent proportion of correct answers with respect to the standardized total score. Their size is determined by
                        the count of respondents who achieved a given level of the standardized total score."),
      sliderInput("regression_logistic_Z_item_slider", "Item",
        min = 1, value = 1, max = 10,
        step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_logistic_Z_na_alert"),
      plotlyOutput("regression_logistic_Z_plot"),
      downloadButton("regression_logistic_Z_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = \\frac{e^{\\left(\\beta_{i0} + \\beta_{i1} Z_p\\right)}}{1 + e^{\\left(\\beta_{i0} + \\beta_{i1} Z_p\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_logistic_Z_coef"))),
      htmlOutput("regression_logistic_Z_interpretation"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/regr/log_z.R"))
    ),
    # * LOGISTIC IRT Z ####
    tabPanel(
      "Logistic IRT Z",
      h3("Logistic regression on standardized total scores with IRT parameterization"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Logistic regression"),
        "can model dependency of the probability of correctly answering item \\(i\\) by respondent \\(p\\) on
                        their standardized total score \\(Z_p\\) (Z-score) by an S-shaped logistic curve. Note change in
                        parametrization - the IRT parametrization used here corresponds to the parametrization used in IRT
                        models. Parameter", strong("\\(b_{i}\\)"), " describes horizontal position of the fitted curve
                        (difficulty) and parameter ", strong("\\(a_{i}\\)"), " describes its slope at the inflection point
                        (discrimination). "
      ),
      br(),
      h4("Plot with estimated logistic curve"),
      p("Points represent proportion of correct answers with respect to the standardized total score. Their size is determined by
                        the count of respondents who achieved a given level of the standardized total score."),
      sliderInput(
        inputId = "regression_logistic_IRT_item_slider", label = "Item",
        min = 1, value = 1, max = 10,
        step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_logistic_IRT_na_alert"),
      plotlyOutput("regression_logistic_IRT_plot"),
      downloadButton("regression_logistic_IRT_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = \\frac{e^{a_i\\left(Z_p - b_i\\right)}}{1 + e^{a_i\\left(Z_p - b_i\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_logistic_IRT_coef"))),
      htmlOutput("regression_logistic_IRT_interpretation"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/regr/log_irt_z.R"))
    ),
    # * NONLINEAR 3P IRT Z ####
    tabPanel(
      "Nonlinear 3P IRT Z",
      h3("Nonlinear three parameter regression on standardized total scores with IRT parameterization"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Nonlinear regression"), "can model
                        dependency of the probability of correctly answering item \\(i\\) by respondent \\(p\\) on their standardized total
                        score \\(Z_p\\) (Z-score) by an S-shaped logistic curve. The IRT parametrization used here corresponds to the
                        parametrization used in IRT models. Parameter", strong("\\(b_{i}\\)"), " describes horizontal position of the
                        fitted curve (difficulty) and parameter ", strong("\\(a_{i}\\)"), " describes its slope at the inflection point
                        (discrimination). This model allows for nonzero lower left asymptote ", strong("\\(c_i\\)"), " (pseudo-guessing
                        parameter). "
      ),
      br(),
      h4("Plot with estimated nonlinear curve"),
      p("Points represent proportion of correct answers with respect to the standardized total score. Their size is determined by the count of
                        respondents who achieved a given level of the standardized total score."),
      sliderInput(
        inputId = "regression_3pl_item_slider", label = "Item",
        min = 1, value = 1, max = 10, step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_3pl_na_alert"),
      plotlyOutput("regression_3pl_plot"),
      downloadButton("regression_3pl_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = c_i + \\left(1 - c_i\\right) \\cdot \\frac{e^{a_i\\left(Z_p - b_i\\right)}}{1 + e^{a_i\\left(Z_p - b_i\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_3pl_coef"))),
      htmlOutput("regression_3pl_interpretation"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/regr/nlr_3.R"))
    ),
    # * NONLINEAR 4P IRT Z ####
    tabPanel(
      "Nonlinear 4P IRT Z",
      h3("Nonlinear four parameter regression on standardized total scores with IRT parameterization"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Nonlinear regression"),
        "can model dependency of the probability of correctly answering item \\(i\\) by respondent \\(p\\) on their
                        standardized total score \\(Z_p\\) (Z-score) by an S-shaped logistic curve. The IRT parametrization used here
                        corresponds to the parametrization used in IRT models. Parameter", strong("\\(b_{i}\\)"), " describes
                        horizontal position of the fitted curve (difficulty), parameter ", strong("\\(a_{i}\\)"), " describes its
                        slope at the inflection point (discrimination), pseudo-guessing parameter ", strong("\\(c_i\\)"), "describes its
                        lower asymptote and inattention parameter ", strong("\\(d_i\\)"), "describes its upper asymptote."
      ),
      br(),
      h4("Plot with estimated nonlinear curve"),
      p("Points represent proportion of correct answers with respect to the standardized total score. Their size is determined by the count
                        of respondents who achieved a given level of the standardized total score."),
      sliderInput(
        inputId = "regression_4pl_item_slider", label = "Item",
        min = 1, value = 1, max = 10, step = 1, animate = animationOptions(interval = 1200)
      ),
      uiOutput("regression_4pl_na_alert"),
      plotlyOutput("regression_4pl_plot"),
      downloadButton("regression_4pl_plot_download", label = "Download figure"),
      h4("Equation"),
      ("$$\\mathrm{P}(Y_{pi} = 1|Z_p) = \\mathrm{E}(Y_{pi}|Z_p) = c_i + \\left(d_i - c_i\\right) \\cdot \\frac{e^{a_i\\left(Z_p - b_i\\right)}}{1 + e^{a_i\\left(Z_p - b_i\\right)}}$$"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_4pl_coef"))),
      htmlOutput("regression_4pl_interpretation"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/regr/nlr_4.R"))
    ),
    # * MODELS COMPARISON ####
    tabPanel(
      "Model comparison",
      h3("Logistic regression model selection"),
      p("Here you can compare a classic 2PL logistic regression model to non-linear 3PL and 4PL models item by item using some information criteria: "),
      tags$ul(
        tags$li(strong("AIC"), "is the Akaike information criterion (Akaike, 1974), "),
        tags$li(strong("BIC"), "is the Bayesian information criterion (Schwarz, 1978)")
      ),
      # p('Another approach to nested models can be likelihood ratio chi-squared test.
      #   Significance level is set to 0.05. As tests are performed item by item, it is
      #   possible to use multiple comparison correction method. '),
      # selectInput("correction_method_regrmodels", "Correction method",
      #             choices = c("Benjamini-Hochberg" = "BH",
      #                         "Benjamini-Yekutieli" = "BY",
      #                         "Bonferroni" = "bonferroni",
      #                         "Holm" = "holm",
      #                         "Hochberg" = "hochberg",
      #                         "Hommel" = "hommel",
      #                         "None" = "none"),
      #             selected = "none"),
      h4("Table of comparison statistics"),
      # p('Rows ', strong('BEST'), 'indicate which model has the lowest value of criterion, or is the largest
      #   significant model by likelihood ratio test.'),
      p("Rows ", strong("BEST"), "indicate which model has the lowest value of given information criterion."),
      DT::dataTableOutput("regression_comparison_table"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/regr/compar.R"))
    ),
    "----",
    "Polytomous models",
    # * CUMULATIVE LOGIT ####
    tabPanel("Cumulative logit",
      value = "regr_cum_logit",
      h3("Cumulative logit regression"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Cumulative logit regression"),
        " can model cumulative probabilities, i.e., probabilities to obtain an item score higher than or equal to 1, 2, 3, etc. "
      ),
      p("A cumulative logit model can be fitted on selected ", strong("Observed score"), "- standardized total scores or total
                        scores, using IRT or classical (intercept/slope) ", strong("parametrization. ")),
      br(),
      fluidRow(
        column(2, selectInput(
          inputId = "regression_cumulative_matching",
          label = "Observed score",
          choices = c(
            "Total score" = "total",
            "Standardized score" = "zscore"
          ),
          selected = "zscore"
        )),
        column(2, selectInput(
          inputId = "regression_cumulative_parametrization",
          label = "Parametrization",
          choices = c(
            "Intercept/slope" = "classic",
            "IRT" = "irt"
          ),
          selected = "irt"
        )),
        column(2, sliderInput(
          inputId = "regression_cumulative_item_slider",
          label = "Item",
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          animate = animationOptions(interval = 1200)
        ))
      ),
      uiOutput("regression_cumulative_na_alert"),
      h4("Plot of cumulative probabilities"),
      p("Lines determine the cumulative probabilities \\(\\mathrm{P}(Y_{pi} \\geq k)\\). Circles represent a proportion of answers having
                        at least \\(k\\) points with respect to the matching criterion, i.e., the empirical cumulative probabilities.
                        The size of the points is determined by the count of respondents who achieved a given level of the matching
                        criterion."),
      plotlyOutput("regression_cumulative_plot_cumulative"),
      downloadButton("regression_cumulative_plot_cumulative_download", label = "Download figure"),
      h4("Plot of category probabilities"),
      p("Lines determine the category probabilities \\(\\mathrm{P}(Y_{pi} = k)\\). Circles represent a proportion of answers having \\(k\\)
                        points with respect to the matching criterion, i.e., the empirical category probabilities. The size of the points
                        is determined by the count of respondents who achieved a given level of the matching criterion."),
      plotlyOutput("regression_cumulative_plot_category"),
      downloadButton("regression_cumulative_plot_category_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("regression_cumulative_equation"))),
      uiOutput("regression_cumulative_interpretation"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_cumulative_coef"))),
      h4("Selected R code"),
      code(includeText("sc/regr/cum.R"))
    ),
    # * ADJACENT CATEGORY LOGIT ####
    tabPanel("Adjacent category logit",
      value = "regr_adjacent",
      h3("Adjacent category logit regression"),
      p("Models for ordinal responses need not use cumulative probabilities. An ", strong("adjacent categories model"), "assumes linear form
                        of logarithm of the ratio of probabilities of two successive scores (e.g., 1 vs. 2, 2 vs. 3, etc.), i.e., of the
                        adjacent category logits."),
      p("An adjacent category logit model can be fitted on selected ", strong("Observed score"), "- standardized total scores or total
                        scores, using IRT or classical (intercept/slope) ", strong("parametrization. ")),
      br(),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "regression_adjacent_matching",
            choices = c(
              "Total score" = "total",
              "Standardized score" = "zscore"
            ),
            selected = "zscore",
            label = "Observed score"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "regression_adjacent_parametrization",
            choices = c(
              "Intercept/slope" = "classic",
              "IRT" = "irt"
            ),
            selected = "irt",
            label = "Parametrization"
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "regression_adjacent_item_slider",
            min = 1,
            max = 10,
            step = 1,
            value = 1,
            label = "Item",
            animate = animationOptions(interval = 1200)
          )
        )
      ),
      uiOutput("regression_adjacent_na_alert"),
      h4("Plot with category probabilities"),
      p("Lines determine the category probabilities \\(\\mathrm{P}(Y_{pi} = k)\\). Circles represent the proportion of answers with \\(k\\)
                        points with respect to the total score, i.e., the empirical category probabilities. The size of the circles is determined by
                        the count of respondents who achieved a given level of the total score. "),
      plotlyOutput("regression_adjacent_plot"),
      downloadButton("regression_adjacent_plot_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("regression_adjacent_equation"))),
      uiOutput("regression_adjacent_interpretation"),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_adjacent_coef"))),
      h4("Selected R code"),
      code(includeText("sc/regr/adj.R"))
    ),
    # * MULTINOMIAL ####
    tabPanel("Multinomial",
      value = "regr_multinom",
      h3("Multinomial regression on standardized total scores"),
      p(
        "Various regression models may be fitted to describe item properties in more detail.", strong("Multinomial regression"), "allows
                        for simultaneous modelling of the probability of choosing given distractors on selected ", strong("Observed score"),
                        "- standardized total scores or total scores, using IRT or classical (intercept/slope) ", strong("parametrization. ")
      ),
      br(),
      h4("Plot with estimated curves of multinomial regression"),
      p("Points represent the proportion of a selected option with respect to the matching criterion. Their size is determined by the count of
                        respondents who achieved a given level of the matching criterion and who selected a given option."),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "regression_multinomial_matching",
            choices = c(
              "Total score" = "total",
              "Standardized score" = "zscore"
            ),
            selected = "zscore",
            label = "Observed score"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "regression_multinomial_parametrization",
            choices = c(
              "Intercept/slope" = "classic",
              "IRT" = "irt"
            ),
            selected = "irt",
            label = "Parametrization"
          )
        ),
        column(
          2,
          sliderInput(
            inputId = "regression_multinomial_item_slider",
            min = 1,
            max = 10,
            step = 1,
            value = 1,
            label = "Item",
            animate = animationOptions(interval = 1200)
          )
        )
      ),
      uiOutput("regression_multinomial_na_alert"),
      plotlyOutput("regression_multinomial_plot"),
      downloadButton("regression_multinomial_plot_download", label = "Download figure"),
      h4("Equation"),
      fluidRow(column(12, align = "center", uiOutput("regression_multinomial_equation"))),
      h4("Table of parameters"),
      fluidRow(column(12, align = "center", tableOutput("regression_multinomial_coef"))),
      htmlOutput("regression_multinomial_interpretation"),
      h4("Selected R code"),
      code(includeText("sc/regr/mult.R"))
    )
  )
