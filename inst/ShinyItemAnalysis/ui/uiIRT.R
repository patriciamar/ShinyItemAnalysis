source("ui/uiIRT/uiDIRT.R")

uiIRT <- navbarMenu(
  "IRT models",
  "Dichotomous models",
  # * DICHOTOMOUS MODELS ####
  tabPanel(
    "Dichotomous models",
    value = "irt_dichotomous",
    tabsetPanel(
      #  ** SUMMARY ####
      tabPanel("Summary",
        value = "IRT_binary",
        h3("Dichotomous model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which
                 respondent ability \\(\\theta_p\\) is assumed to be latent and is estimated together
                 with item paramters. "),
        # ** Equations ####
        h4("Equations"),
        p("Item characteristic function \\(\\pi_{pi} = \\mathrm{P}\\left(Y_{pi} = 1\\vert
                 \\theta_{p}\\right)\\) describes the probability of a correct answer for given item
                 \\(i\\). Item information  function \\(\\mathrm{I}_i(\\theta_p)\\) describes how well
                 the item discriminates from two nearby ability levels, i.e., how much information it
                 provides for the given ability. The test information  function \\(\\mathrm{T}(\\theta_p)\\)
                 sums up all item informations and thus describes the information of the whole test.
                 The inverse of the test information is the standard error (SE) of measurement. "),
        p("The equation and estimated item parameters can be displayed using the IRT or
                 intercept/slope ", strong("parametrization.")),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "IRT_binary_summary_model",
              label = "Model",
              choices = c(
                "Rasch" = "Rasch",
                "1PL" = "1PL",
                "2PL" = "2PL",
                "3PL" = "3PL",
                "4PL" = "4PL"
              )
            )
          ),
          column(
            2,
            selectInput(
              inputId = "IRT_binary_summary_parametrization",
              label = "Parametrization",
              choices = c(
                "IRT" = "irt",
                "Intercept/slope" = "classical"
              )
            )
          )
        ),
        uiOutput("IRT_binary_summary_model_description"),
        uiOutput("IRT_binary_summary_icc_equation", inline = TRUE),
        uiOutput("IRT_binary_summary_iic_equation", inline = TRUE),
        "$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m \\pi_{pi} (1 - \\pi_{pi})$$",
        uiOutput("IRT_binary_summary_equation_interpretation"),
        uiOutput("IRT_binary_summary_model_converged"),
        # ** Plots ####
        h4("Item characteristic curves"),
        plotlyOutput("IRT_binary_summary_icc"),
        downloadButton(
          outputId = "IRT_binary_summary_icc_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("IRT_binary_summary_iic"),
        downloadButton(
          outputId = "IRT_binary_summary_iic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Test information curve and SE"),
        plotlyOutput("IRT_binary_summary_tic"),
        downloadButton(
          outputId = "IRT_binary_summary_tic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # ** Estimated parameters ####
        h4("Table of estimated parameters"),
        p(
          "Estimates of item parameters can be displayed using the IRT or intercept/slope ",
          strong("parametrization,"), "which can be selected at the top of this tab. Parameter estimates
                 are completed by SX2 item fit statistics (Orlando & Thissen, 2000). SX2 statistics are computed
                 only when no missing data are present."
        ),
        tableOutput("IRT_binary_summary_coef"),
        downloadButton(
          outputId = "IRT_binary_summary_coef_download",
          label = "Download table"
        ),
        br(),
        br(),
        # ** Ability estimates ####
        h4("Ability estimates"),
        p("This table shows the response and factor scores for only six respondents. If you want to see the
                 scores for all respondents, click on", strong("Download abilities"), "button."),
        tableOutput("IRT_binary_summary_ability"),
        downloadButton(
          outputId = "IRT_binary_summary_ability_download",
          label = "Download abilities"
        ),
        br(),
        br(),
        textOutput("IRT_binary_summary_ability_correlation_text"),
        plotlyOutput("IRT_binary_summary_ability_plot"),
        downloadButton(
          outputId = "IRT_binary_summary_ability_plot_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # ** Wright map ####
        conditionalPanel(
          condition = "input.IRT_binary_summary_model == 'Rasch' || input.IRT_binary_summary_model == '1PL'",
          h4("Wright map"),
          p("The Wright map (Wilson, 2005; Wright & Stone, 1979), also called an item-person map, is a graphical
                   tool used to display person ability estimates and item parameters on one scale. The person side (left)
                   represents a histogram of estimated abilities of the respondents. The item side (right) displays
                   estimates of the difficulty parameters of individual items. "),
          plotlyOutput("IRT_binary_summary_wrightmap"),
          downloadButton(
            outputId = "IRT_binary_summary_wrightmap_download",
            label = "Download figure"
          ),
          br(),
          br()
        ),
        # ** Selected R code ####
        h4("Selected R code"),
        conditionalPanel(
          "input.IRT_binary_summary_model == 'Rasch'",
          code(includeText("sc/irt/rasch.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_summary_model == '1PL'",
          code(includeText("sc/irt/1pl.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_summary_model == '2PL'",
          code(includeText("sc/irt/2pl.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_summary_model == '3PL'",
          code(includeText("sc/irt/3pl.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_summary_model == '4PL'",
          code(includeText("sc/irt/4pl.R"))
        )
      ),
      # ** ITEMS ####
      tabPanel("Items",
        value = "irt_dichotomous_it",
        h3("Dichotomous model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which
                 respondent ability \\(\\theta_p\\) is assumed to be latent and is estimated together
                 with item paramters. "),
        # ** Equations ####
        h4("Equations"),
        p("Item characteristic function \\(\\pi_{pi} = \\mathrm{P}\\left(Y_{pi} = 1\\vert
                 \\theta_{p}\\right)\\) describes the probability of a correct answer for given item
                 \\(i\\). Item information  function \\(\\mathrm{I}_i(\\theta_p)\\) describes how well
                 the item discriminates from two nearby ability levels, i.e., how much information it
                 provides for the given ability. "),
        p("The equation and estimated item parameters can be displayed using the IRT or
                 intercept/slope ", strong("parametrization.")),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "IRT_binary_items_model",
              label = "Model",
              choices = c(
                "Rasch" = "Rasch",
                "1PL" = "1PL",
                "2PL" = "2PL",
                "3PL" = "3PL",
                "4PL" = "4PL"
              )
            )
          ),
          column(
            2,
            selectInput(
              inputId = "IRT_binary_items_parametrization",
              label = "Parametrization",
              choices = c(
                "IRT" = "irt",
                "Intercept/slope" = "classical"
              )
            )
          ),
          column(
            2,
            sliderInput(
              inputId = "IRT_binary_items", label = "Item",
              min = 1, value = 1, max = 20,
              step = 1, animate = TRUE
            )
          )
        ),
        uiOutput("IRT_binary_items_model_description"),
        uiOutput("IRT_binary_items_icc_equation", inline = TRUE),
        uiOutput("IRT_binary_items_iic_equation", inline = TRUE),
        uiOutput("IRT_binary_items_equation_interpretation"),
        uiOutput("IRT_binary_items_model_converged"),
        # ** Plots ####
        h4("Item characteristic curves"),
        plotlyOutput("IRT_binary_items_icc"),
        downloadButton(
          outputId = "IRT_binary_items_icc_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("IRT_binary_items_iic"),
        downloadButton(
          outputId = "IRT_binary_items_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # ** Estimated parameters ####
        h4("Table of estimated parameters"),
        p(
          "Estimates of item parameters can be displayed using the IRT or intercept/slope ",
          strong("parametrization,"), "which can be selected at the top of this tab. Parameter estimates
                 are completed by SX2 item fit statistics (Orlando & Thissen, 2000). SX2 statistics are computed
                 only when no missing data are present."
        ),
        fluidRow(column(12, align = "center", tableOutput("IRT_binary_items_coef"))),

        # ** Selected R code ####
        h4("Selected R code"),
        conditionalPanel(
          "input.IRT_binary_items_model == 'Rasch'",
          code(includeText("sc/irt/rasch_item.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_items_model == '1PL'",
          code(includeText("sc/irt/1pl_item.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_items_model == '2PL'",
          code(includeText("sc/irt/2pl_item.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_items_model == '3PL'",
          code(includeText("sc/irt/3pl_item.R"))
        ),
        conditionalPanel(
          "input.IRT_binary_items_model == '4PL'",
          code(includeText("sc/irt/4pl_item.R"))
        )

      )
    )
  ),
  # * MODEL COMPARISON ####
  tabPanel("Model comparison",
    value = "irt_mod_comp",
    h3("IRT model selection"),
    withMathJax(),
    p("Item Response Theory (IRT) models are mixed-effect regression models in which
      respondent ability \\(\\theta_p\\) is assumed to be latent and is estimated together
      with item paramters. Model parameters  are estimated using a marginal maximum likelihood
      method, in 1PL, 2PL, 3PL, and 4PL IRT models,  ability \\(\\theta_p\\) is assumed
      to follow standard normal distribution."),
    p("IRT models can be compared by several information criteria: "),
    tags$ul(
      tags$li(strong("AIC"), "is the Akaike information criterion (Akaike, 1974), "),
      tags$li(strong("AICc"), "is AIC with a correction for finite sample size, "),
      tags$li(strong("BIC"), "is the Bayesian information criterion (Schwarz, 1978)."),
      tags$li(strong("SABIC"), "is the sample-sized adjusted BIC criterion, ")
    ),
    h4("Table of comparison statistics"),
    p("Row ", strong("BEST"), "indicates which model has the lowest value of given information criterion."),
    tableOutput("IRT_binary_comparison"),
    tags$style(type = "text/css", "#IRT_binary_comparison tr:last-child {font-weight:bold;}"),
    uiOutput("IRT_binary_comparison_model_converged"),
    br(),
    h4("Selected R code"),
    code(includeText("sc/irt/comp.R"))
  ),
  "----",
  "Polytomous models",
  # * BOCK'S NOMINAL MODEL ####
  tabPanel("Bock's nominal model",
    value = "bock",
    tabsetPanel(
      # ** SUMMARY ####
      tabPanel("Summary",
        value = "bock_mod",
        h3("Bock's nominal IRT model"),
        p("The Nominal Response Model (NRM) was introduced by Bock (1972) as a way to model responses to items with two or more nominal
                                        categories. This model is suitable for multiple-choice items with no particular ordering of
                                        distractors. It is also a generalization of some models for ordinal data, e.g., Generalized Partial
                                        Credit Model (GPCM) or its restricted versions Partial Credit Model (PCM) and Rating Scale Model
                                        (RSM)."),
        # ** Equations ####
        h4("Equations"),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "IRT_bock_summary_parametrization",
              label = "Parametrization",
              choices = c(
                # "IRT" = "irt",
                "Intercept/slope" = "classical"
              )
            )
          )
        ),
        withMathJax(
          "For ", strong("\\(K_i\\)"), " possible test choices, the probability of selecting distractor ", strong("\\(k\\)"),
          " by person ", strong("\\(p\\)"), " with latent trait", strong("\\(\\theta_p\\)"), " in item ", strong("\\(i\\)"),
          "is given by the following equation: "
        ),
        br(),
        uiOutput("IRT_bock_summary_icc_equation", inline = TRUE),
        br(),
        # ** Plots ####
        h4("Item characteristic curves"),
        p("Item characteristic curves may be displayed for each item in the Items subtab. "),
        # plotlyOutput("IRT_bock_summary_icc"),
        # downloadButton(
        #   outputId = "IRT_bock_summary_icc_download",
        #   label = "Download figure"
        # ),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("IRT_bock_summary_iic"),
        downloadButton(
          outputId = "IRT_bock_summary_iic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Test information curve and SE"),
        plotlyOutput("IRT_bock_summary_tic"),
        downloadButton(
          outputId = "IRT_bock_summary_tic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # ** Estimated parameters ####
        h4("Table of parameters"),
        tableOutput("IRT_bock_summary_coef"),
        # ** Ability estimates ####
        h4("Ability estimates"),
        p("This table shows the response score of only six respondents. If you want to see scores for all respondents, click on the", strong("Download abilities"), " button. "),
        tableOutput("IRT_bock_summary_ability"),
        downloadButton(
          outputId = "IRT_bock_summary_ability_download",
          label = "Download abilities"
        ),
        br(),
        br(),
        textOutput("IRT_bock_summary_ability_correlation_text"),
        plotlyOutput("IRT_bock_summary_ability_plot"),
        downloadButton(
          outputId = "IRT_bock_summary_ability_plot_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Selected R code"),
        code(includeText("sc/irt/bock.R"))
      ),
      # ** ITEMS ####
      tabPanel("Items",
        value = "bock_it",
        h3("Bock's nominal IRT model"),
        p("The Nominal Response Model (NRM) was introduced by Bock (1972) as a way to model responses to items with two or more nominal
                                        categories. This model is suitable for multiple-choice items with no particular ordering of
                                        distractors. It is also generalization of some models for ordinal data, e.g., Generalized Partial
                                        Credit Model (GPCM) or its restricted versions Partial Credit Model (PCM) and Rating Scale Model
                                        (RSM)."),
        # ** Equations ####
        h4("Equations"),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "IRT_bock_items_parametrization",
              label = "Parametrization",
              choices = c(
                # "IRT" = "irt",
                "Intercept/slope" = "classical"
              )
            )
          )
        ),
        withMathJax(
          "For ", strong("\\(K_i\\)"), " possible test choices the probability of the distractor ", strong("\\(k\\)"), " for person ",
          strong("\\(p\\)"), " with latent trait", strong("\\(\\theta_p\\)"), " in item ", strong("\\(i\\)"),
          "is given by the following equation: "
        ),
        br(),
        uiOutput("IRT_bock_items_icc_equation", inline = TRUE),
        br(),
        # ** Plots ####
        h4("Item characteristic curves"),
        sliderInput(
          inputId = "IRT_bock_items",
          label = "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("IRT_bock_items_icc"),
        downloadButton(
          outputId = "IRT_bock_items_icc_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("IRT_bock_items_iic"),
        downloadButton(
          outputId = "IRT_bock_items_iic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # ** Estimated parameters ####
        h4("Table of parameters"),
        fluidRow(column(12, align = "center", tableOutput("IRT_bock_items_coef")))
      )
    )
  ),
  "----",
  "Training",
  # * TRAINING  ####
  # ** Dichotomous models ####
  uiDIRT,
  # ** Polytomous models ####
  tabPanel(
    "Polytomous models",
    tabsetPanel(
      # *** Intro ####
      tabPanel("Intro",
        value = "polytom_intro",
        h3("Polytomous models"),
        p("Polytomous models are used when a partial score is possible, or when items are graded
                                        on the Likert scale (e.g. from Totally disagree to Totally agree); some polytomous
                                        models can also be used when analyzing multiple-choice items.  In this section you
                                        can explore item response functions for some polytomous models."),
        br(),
        p("Two main classes of polytomous IRT models are considered:"),
        p(strong("Difference models"), "are defined by setting the mathematical form to cumulative
                                        probabilities, while category probabilities are calculated by their difference.
                                        These models are sometimes called", strong("cumulative logit models"), "as they
                                        set a linear form to cumulative logits."),
        p("As an example, the ", strong("Graded Response Model"), "(GRM; Samejima, 1970) uses a 2PL
                                        IRT model to describe cumulative probabilities (probabilities to obtain a score higher
                                        than 1, 2, 3, etc.). Category probabilities are then described as the differences between two
                                        subsequent cumulative probabilities. "), br(),
        p("For the", strong("divide-by-total models,"), "response category probabilities are defined
                                        as the ratio between category-related functions and their sum. "),
        p(
          "In the", strong("Generalized Partial Credit Model"), "(GPCM; Muraki, 1992), probability
                                        of the successful transition from one category score to the next category score is
                                        modelled by the 2PL IRT model, while the ", strong("Partial Credit Model"), "(PCM; Masters, 1982)
                                        uses the 1PL IRT model to describe this probability. In an even more restricted version, the",
          strong("Rating Scale Model"), "(RSM; Andrich, 1978) assumes exactly the same K response
                                        categories for each item and threshold parameters which can be split into a response-threshold
                                        parameter and an item-specific location parameter. These models are sometimes called
                                        ", strong("adjacent-category logit models"), "because they set linear form to adjacent logits."
        ),
        p(
          "To model distractor properties in multiple-choice items, the", strong("Nominal Response Model"),
          "(NRM; Bock, 1972) can be used. NRM is an IRT analogy of a multinomial regression model. This
                                        model is also a generalization of GPCM/PCM/RSM ordinal models. NRM is sometimes called
                                        a ", strong("baseline-category logit model"), "because it sets linear form to log of the odds of selecting a given category
                                        to the baseline category. The baseline can be chosen arbitrarily, although normally the correct
                                        answer is the first answer chosen."
        )
      ),
      # *** Graded response model ####
      tabPanel("Graded response model",
        value = "polytom_grm",
        h3("Graded response model"),
        p("Graded response model (GRM; Samejima, 1970) uses the 2PL IRT model to describe cumulative probabilities
          (probabilities to obtain a score higher than 1, 2, 3, etc.). Category probabilities are then described
          as the differences between two subsequent cumulative probabilities. "),
        p("It belongs to a class of difference models, which are defined by setting mathematical form to cumulative
          probabilities, while category probabilities are calculated as their difference. These models are sometimes
          called cumulative logit models, because they set linear form to cumulative logits."),
        h4("Parameters"),
        p("Select the number of responses, inflection points of cumulative probabilities \\(b_k\\), and the common
          discrimination parameter \\(a\\). Cumulative probability \\(P(Y \\geq 0 \\vert \\theta)\\) is always equal
          to 1 and it is not displayed, the corresponding category probability \\(P(Y = 0 \\vert \\theta)\\) is
          displayed with a black color."),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          numericInput(
            inputId = "irt_training_grm_numresp",
            label = "Highest score",
            value = 4,
            min = 2,
            max = 6
          )
        ),
        br(),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          sliderInput(
            inputId = "irt_training_grm_a",
            label = "\\(a\\) - discrimination",
            value = 1,
            min = 0,
            max = 4,
            step = 0.1
          )
        ),
        br(),
        uiOutput("irt_training_grm_sliders"),
        br(),
        h4("Equations"),
        ("$$\\pi_k* = \\mathrm{P}\\left(Y \\geq k \\vert \\theta\\right) = \\frac{e^{a\\left(\\theta - b_k\\right) }}{1 + e^{a\\left(\\theta - b_k\\right) }} $$"),
        ("$$\\pi_k =\\mathrm{P}\\left(Y = k \\vert \\theta\\right) = \\pi_k* - \\pi_{k + 1}* $$"),
        ("$$\\mathrm{E}\\left(Y \\vert \\theta\\right) = \\sum_{k = 0}^K k \\pi_k$$"),
        h4("Plots"),
        splitLayout(
          cellWidths = c("33%", "33%", "33%"),
          plotlyOutput("irt_training_grm_plot_cumulative"),
          plotlyOutput("irt_training_grm_plot_category"),
          plotlyOutput("irt_training_grm_plot_expected")
        ),
        splitLayout(
          cellWidths = c("33%", "33%", "33%"),
          downloadButton("DB_irt_training_grm_plot_cumulative", label = "Download figure"),
          downloadButton("DB_irt_training_grm_plot_category", label = "Download figure"),
          downloadButton("DB_irt_training_grm_plot_expected", label = "Download figure")
        ),
        br(),
        #------------------------------------------------------------------------------------#
        # **** Exercise ####
        #------------------------------------------------------------------------------------#
        h4("Exercise "),
        p("Consider an item following a graded response model rated \\(0-1-2-3\\), with discrimination \\(a = 1\\) and
										 difficulties \\(b_{1} = \u2212 0.5\\), \\(b_{2} = 1\\) and \\(b_{3} = 1.5\\)."),
        tags$ul(
          tags$li(
            "Calculate the probabilities of obtaining \\(k\\) and more points for a specific level of ability \\(\\theta\\)",
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 0 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_1a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1e_answer"), ""
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 1 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_2a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 2 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_3a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 3 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_4a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4e_answer")
            ), ""
          ),

          tags$li(
            "Calculate the probabilities of obtaining  exactly \\(k\\) points for a specific level of ability \\(\\theta\\)",
            bsButton(
              inputId = "irt_training_grm_1_help",
              label = "", icon = icon("question"),
              style = "info", size = "extra-small"
            ),
            bsPopover(
              id = "irt_training_grm_1_help",
              title = "Help",
              content = "What should be the sum in the columns?",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 0\\) "),
              numericInput(
                inputId = "irt_training_grm_2_1a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 1\\) "),
              numericInput(
                inputId = "irt_training_grm_2_2a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 2\\) "),
              numericInput(
                inputId = "irt_training_grm_2_3a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 3\\) "),
              numericInput(
                inputId = "irt_training_grm_2_4a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4e_answer")
            ), ""
          ),
          tags$li(
            "What is the expected item score for the specific level of ability \\(\\theta\\)?",
            bsButton(
              inputId = "irt_training_grm_2_help",
              label = "", icon = icon("question"),
              style = "info", size = "extra-small"
            ),
            bsPopover(
              id = "irt_training_grm_2_help",
              title = "Help",
              content = "Look at the third figure.",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -2\\)"),
              numericInput(
                inputId = "irt_training_grm_3_1a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_1a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -1\\)"),
              numericInput(
                inputId = "irt_training_grm_3_2a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_2a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 0\\)"),
              numericInput(
                inputId = "irt_training_grm_3_3a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_3a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 1\\)"),
              numericInput(
                inputId = "irt_training_grm_3_4a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_4a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 2\\)"),
              numericInput(
                inputId = "irt_training_grm_3_5a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_5a_answer")
            ),
            div(
              style = "display: inline-block; float: right; width: 200px;",
              htmlOutput("irt_training_grm_answer"),
              actionButton(
                inputId = "irt_training_grm_1_submit",
                label = "Submit answers", width = "80%"
              )
            ), ""
          )
        ),
        br(),
        h4("Selected R code"),
        code(includeText("sc/irt/train_poly_grm.R"))
      ),
      # *** Generalized partial credit model ####
      tabPanel("Generalized partial credit model",
        value = "polytom_gpcm",
        h3("Generalized partial credit model"),
        p("In the Generalized Partial Credit Model (GPCM; Muraki, 1992), the probability of successful transition
          from one category score to the next category score is modelled by the 2PL IRT model. The response category
          probabilities are then ratios between category-related functions (cumulative sums of exponentials) and
          their sum."),
        p("Two simpler models can be derived from GPCM by restricting some parameters: The Partial Credit Model
          (PCM; Masters, 1982) uses the 1PL IRT model to describe this probability, thus parameters \\(a = 1\\).
          An even more restricted version, the Rating Scale Model (RSM; Andrich, 1978) assumes exactly the same
          \\(K\\) response categories for each item and threshold parameters which can be split into a response-threshold
          parameter \\(\\lambda_k\\) and an item-specific location parameter \\(b_i\\). These models are
          sometimes called adjacent category logit models, as they set linear form to adjacent category logits. "),
        h4("Parameters"),
        p("Select the number of responses and their threshold parameters \\(b_k\\)  and common discrimination parameter
          \\(a\\). With \\(a = 1\\) you get the PCM. Numerator of \\(\\pi_0 = P(Y = 0 \\vert \\theta)\\) is set to 1 and
          \\(\\pi_0\\) is displayed with a black color."),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          numericInput(
            inputId = "irt_training_gpcm_numresp",
            label = "Highest score",
            value = 4,
            min = 2,
            max = 6
          )
        ),
        br(),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          sliderInput(
            inputId = "irt_training_gpcm_a",
            label = "\\(a\\) - discrimination",
            value = 1,
            min = 0,
            max = 4,
            step = 0.1
          )
        ),
        br(),
        uiOutput("irt_training_gpcm_sliders"),
        br(),
        h4("Equations"),
        ("$$\\pi_k =\\mathrm{P}\\left(Y = k \\vert \\theta\\right) = \\frac{\\exp\\sum_{t = 0}^k a(\\theta - b_t)}{\\sum_{r = 0}^K\\exp\\sum_{t = 0}^r a(\\theta - b_t)} $$"),
        ("$$\\mathrm{E}\\left(Y \\vert \\theta\\right) = \\sum_{k = 0}^K k \\pi_k$$"),
        h4("Plots"),
        splitLayout(
          cellWidths = c("50%", "50%"),
          plotlyOutput("irt_training_gpcm_plot"),
          plotlyOutput("irt_training_gpcm_plot_expected")
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("DB_irt_training_gpcm_plot", label = "Download figure"),
          downloadButton("DB_irt_training_gpcm_plot_expected", label = "Download figure")
        ),
        br(),
        #------------------------------------------------------------------------------------#
        # ***** Exercise ####
        #------------------------------------------------------------------------------------#
        h4("Exercise"),
        p("Consider an item  following  the generalized  partial  credit  model  rated  \\(0-1-2\\),
          with  a discrimination \\(a = 1\\)  and threshold parameters \\(b_{1} = \u2212 1\\) and \\(b_{2} = 1\\)."),
        tags$ul(
          tags$li(
            "For what ability levels do the category probability curves cross?",
            splitLayout(
              cellWidths = c("90%", "5%", "5%"),
              checkboxGroupInput(
                inputId = "irt_training_gpcm_1",
                label = NULL,
                choices = c(
                  "\\(\\theta = -4\\)" = 1,
                  "\\(\\theta = -3\\)" = 2,
                  "\\(\\theta = -2\\)" = 3,
                  "\\(\\theta = -1\\)" = 4,
                  "\\(\\theta = 0\\)" = 5,
                  "\\(\\theta = 1\\)" = 6,
                  "\\(\\theta = 2\\)" = 7,
                  "\\(\\theta = 3\\)" = 8,
                  "\\(\\theta = 4\\)" = 9
                ),
                inline = TRUE
              ),
              uiOutput("irt_training_gpcm_1_answer")
            )
          ),
          tags$li(
            "What is the expected item score for these ability levels?",
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_2_1",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_2_1_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 0\\)"),
              numericInput(
                inputId = "irt_training_gpcm_2_2",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_2_2_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_2_3",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_2_3_answer")
            ), ""
          ),
          tags$li(
            "Change the discrimination to \\(a = 2\\).  Do the category probability curves cross at the same ability
             levels?",
            splitLayout(
              cellWidths = c("21%", "4%", "75%"),
              radioButtons(
                inputId = "irt_training_gpcm_3",
                label = NULL,
                choices = c("Yes", "No"),
                inline = TRUE
              ),
              uiOutput("irt_training_gpcm_3_answer")
            ), ""
          ),
          tags$li(
            "What is the new expected item score for these ability levels? ",
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_4_1",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_4_1_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 0\\)"),
              numericInput(
                inputId = "irt_training_gpcm_4_2",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_4_2_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_4_3",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_4_3_answer")
            ),
            div(
              style = "display: inline-block; float: right; width: 200px;",
              htmlOutput("irt_training_gpcm_answer"),
              actionButton(
                inputId = "irt_training_gpcm_1_submit",
                label = "Submit answers", width = "80%"
              )
            ), ""
          )
        ),
        h4("Selected R code"),
        code(includeText("sc/irt/train_poly_gpcm.R"))
      ),
      # *** Nominal response model ####
      tabPanel("Nominal response model",
        value = "polytom_nrm",
        h3("Nominal response model"),
        p("In the Nominal Response Model (NRM; Bock, 1972), the probability of selecting a given category over the
          baseline category is modelled by the 2PL IRT model. This model is sometimes called the baseline-category logit
          model, because it sets linear form to the log odds of selecting a given category to the baseline category.
          The baseline can be chosen arbitrarily, although normally the correct answer is the first answer chosen.
          The NRM model is a generalization of the GPCM model by setting item-specific and category-specific intercept
          and slope parameters."),
        h4("Parameters"),
        p("Select the number of distractors, their threshold parameters \\(b_k\\), and discrimination parameters
          \\(a_k\\). Parameters of \\(\\pi_0 = P(Y = 0 \\vert \\theta)\\) are set to zeros and \\(\\pi_0\\) is displayed
          with a black color."),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          numericInput(
            inputId = "irt_training_nrm_numresp",
            label = "Number of distractors",
            value = 4,
            min = 2,
            max = 6
          )
        ),
        br(),
        uiOutput("irt_training_nrm_sliders"),
        br(),
        h4("Equations"),
        ("$$\\pi_k =\\mathrm{P}\\left(Y = k \\vert \\theta\\right) = \\frac{\\exp\\left(a_k(\\theta - b_k)\\right)}{\\sum_{r = 0}^K\\exp\\left(a_r(\\theta - b_r)\\right)} $$"),
        h4("Plots"),
        plotlyOutput("irt_training_nrm_plot"),
        downloadButton("DB_irt_training_nrm_plot", label = "Download figure"),
        h4("Selected R code"),
        code(includeText("sc/irt/train_poly_nrm.R"))
      )
    )
  )
)
