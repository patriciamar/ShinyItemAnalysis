uiPolyTraining <- tabPanel(
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
      p("Select the number of responses by specifying the highest category,
           specify the category locations (inflection points of cumulative probabilities) \\(b_k\\), and the common
          discrimination parameter (slopes at inflection points) \\(a\\). Cumulative probability \\(P(Y \\geq 0 \\vert \\theta)\\) is always equal
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
										 difficulties \\(b_{1} = -0.5\\), \\(b_{2} = 1\\) and \\(b_{3} = 1.5\\)."),
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
          from one category to the next category is modelled by the 2PL IRT model. The response category
          probabilities are then ratios between category-related functions (cumulative sums of exponentials) and
          their sum."),
      p("Two simpler models can be derived from GPCM by restricting some parameters: The Partial Credit Model
          (PCM; Masters, 1982) uses the 1PL IRT model to describe this probability,
          thus the slope parameter is fixed to \\(a = 1\\).
          An even more restricted version, the Rating Scale Model (RSM; Andrich, 1978) assumes exactly the same
          \\(K\\) response categories for each item and threshold parameters which can be split into a response-threshold
          parameter \\(\\lambda_k\\) and an item-specific location parameter \\(b_i\\)."),
      p("These models are
          sometimes called adjacent category logit models, as they set linear form to adjacent category logits. "),
      h4("Parameters"),
      p("Select the number of responses by specifying the highest category score,
          specify the threshold parameters \\(b_k\\)  and the common discrimination parameter
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
      p(
        "In the Nominal Response Model (NRM; Bock, 1972), the probability of",
        "selecting a given category over the  baseline category is modelled by",
        "the 2PL IRT model. This model is sometimes called the",
        "baseline-category logit  model, because it sets linear form to the log",
        "odds of selecting a given category to the baseline category.  The",
        "baseline is often chosen arbitrarily (as in the case of ", code("mirt"),
        "package), but we may benefit from  constraining the model in the way",
        "that the correct response category is set as a baseline.",
        "Here we present 6 parametrizations:",
        tags$ol(
          tags$li(
            strong("BLIRT (Baseline-category Logit IRT)"),
            "that utilizes IRT (slope/threshold) parametrization and fixes",
            "the correct response's parameters to zero"
          ),
          tags$li(
            strong("BLIS (Baseline-category Logit Intercept-Slope)"),
            "which is a mere intercept/slope reparametrization of BLIRT"
          ),
          tags$li(
            strong("Thissen et al."),
            "that - rather arbitrarily - fixes slopes of the",
            "first and last categories to zero and \\(K-1\\),",
            "respectively (where \\(K\\) is the number of categories);",
            "to generalize for multidimensional models, Thissen et al. \"factor out\" so-called overall slope \\(a^*\\)"
          ),
          tags$li(strong("Thissen et al. IRT"), "which is a mere IRT reparametrization of Thissen's model (first threshold parameter is here constrained to zero)"),
          tags$li(strong("Bock's"), "original model constrained in the way that both slope and intercept parameters have a sum of zero"),
          tags$li(strong("Bock IRT"), "which is IRT reparametrization of Bock's model")
        )
      ), br(),
      h4("Parameters"),
      p(
        "Select the number of distractors, their threshold parameters \\(b_k\\), and discrimination parameters
          \\(a_k\\) (in BLIRT parametrization). The last parameter (for correct response, displayed in ",
        span(style = "color: gray", "grey", .noWS = "after"),
        ") is fixed to zero and all parameters for distractors are smaller that zero."
      ),
      div(
        style = "display: inline-block; vertical-align: middle; width: 18%;",
        numericInput(
          inputId = "irt_training_nrm_numresp",
          label = "Number of distractors",
          value = 3,
          min = 1,
          max = 7
        )
      ),
      br(),
      uiOutput("irt_training_nrm_sliders"),
      h4("Parametrizations"),
      p(
        "In the following tables, BLIRT parameters you have set above are",
        "presented in all parametrizations described in the introductory paragraphs.",
        "Note that \\(a^*\\) parameter is defined only for Thissen's parametrizations.",
        "Note further that \\(b_k\\) parameters of BLIRT represent intercepts of correct, \"",
        span(style = "color: gray", "grey", .noWS = c("before", "after")),
        "\" category with the distractors (as denoted in the plot below by vertical dashed lines)."
      ),
      fluidRow(
        style = "display: flex; flex-wrap: wrap;",
        div(style = "margin-right: 20px", tableOutput("irt_training_nrm_irt_parameters")),
        div(tableOutput("irt_training_nrm_int_slope_parameters"))
      ),
      h4("Plot"),
      plotlyOutput("irt_training_nrm_cat_probs_plotly"),
      # downloadButton("DB_irt_training_nrm_plot", label = "Download figure"),
      h4("Selected R code"),
      code(includeText("sc/irt/train_poly_nrm.R"))
    )
  )
)
