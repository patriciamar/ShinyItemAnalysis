
# source subtabs ----------------------------------------------------------

# source dichotomous IRT UI
source("ui/uiIRT/uiDIRT.R", local = T, encoding = "UTF-8")

# source polytomous IRT UI (only NRM for the moment)
source("ui/uiIRT/uiPolyIRT.R", local = T, encoding = "UTF-8")

# polytomous methods training
source("ui/uiIRT/uiPolyTraining.R", local = T, encoding = "UTF-8")


# IRT user interface ------------------------------------------------------

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
    p("Item Response Theory (IRT) models are mixed-effect regression models in which
      respondent ability \\(\\theta_p\\) is assumed to be latent and is estimated together
      with item paramters. Model parameters  are estimated using a marginal maximum likelihood
      method, in 1PL, 2PL, 3PL, and 4PL IRT models,  ability \\(\\theta_p\\) is assumed
      to follow standard normal distribution."),
    p("IRT models can be compared by several information criteria: "),
    tags$ul(
      tags$li(strong("AIC"), "is the Akaike information criterion (Akaike, 1974), "),
      tags$li(strong("BIC"), "is the Bayesian information criterion (Schwarz, 1978),"),
      tags$li(strong("logLik"), "is the logarithm of likelihood. Likelihood ratio test is suitable only for comparison of 1PL and 2PL models.")
      # tags$li(strong("SABIC"), "is the sample-sized adjusted BIC criterion, "),
      # tags$li(strong("HQ"), "is Hannan-Quinn criterion.")
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
  uiPolyIRT, # UI sourced in the beginning of this .R file
  "----",
  "Training",
  # * TRAINING  ####
  # ** Dichotomous models ####
  uiDIRT,
  # ** Polytomous models ####
  uiPolyTraining
)
