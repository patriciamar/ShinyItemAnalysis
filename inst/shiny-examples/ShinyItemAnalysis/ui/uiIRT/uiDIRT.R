#------------------------------------------------------------------------------------#
# DICHOTOMOUS MODELS ####
#------------------------------------------------------------------------------------#
uiDIRT <- tabPanel(
  "Dichotomous models",
  value = "irt_dichotomous_train",

  #------------------------------------------------------------------------------------#
  # Dichotomous models ####
  #------------------------------------------------------------------------------------#
  h3("Dichotomous models"),
  p("Dichotomous models are used for modelling items producing a simple binary response
                   (i.e., true/false). The most complex unidimensional dichotomous IRT model described here
                   is the 4PL IRT model. The Rasch model (Rasch, 1960) assumes discrimination fixed to \\(a = 1\\),
                   guessing fixed to \\(c = 0\\), and innatention to \\(d = 1\\). Additionally, other restricted
                   models (1PL, 2PL, and 3PL models)  can be obtained by fixing appropriate parameters in
                   the 4PL model."),
  p("In this section, you can explore the behavior of two item characteristic curves
                   \\(\\mathrm{P}\\left(Y = 1|\\theta\\right)\\) and their item information functions
                   \\(\\mathrm{I}\\left(\\theta\\right)\\) in the 4PL IRT model. "),

  #------------------------------------------------------------------------------------#
  # Parameters ####
  #------------------------------------------------------------------------------------#
  h4("Parameters"),
  p(
    "Select parameters ", strong("\\(a\\)"), "(discrimination), ", strong("\\(b\\)"),
    "(difficulty), ", strong("\\(c\\)"), "(guessing), and ", strong("\\(d\\)"), "(inattention).
                   By constraining \\(a = 1\\), \\(c = 0\\), \\(d = 1\\) you get the Rasch model. With option
                   \\(c = 0\\) and \\(d = 1\\) you get the 2PL model, and with option \\(d = 1\\) the 3PL model."
  ),
  fluidRow(
    style = "margin-left: 0px; margin-right: 0px;",
    splitLayout(
      cellWidths = c("20%", "5%", "20%", "5%", "20%", "5%", "20%", "5%"),
      tags$div(
        class = "js-irs-red",
        sliderInput("ccIRTSlider_a1", "\\(a\\) - discrimination",
          min = -4, max = 4, value = 1, step = 0.1
        )
      ),
      "",
      tags$div(
        class = "js-irs-red",
        sliderInput("ccIRTSlider_b1", "\\(b\\) - difficulty",
          min = -4, max = 4, value = 0, step = 0.1
        )
      ),
      "",
      tags$div(
        class = "js-irs-red",
        sliderInput("ccIRTSlider_c1", "\\(c\\) - guessing",
          min = 0, max = 1, value = 0, step = 0.1
        )
      ),
      "",
      tags$div(
        class = "js-irs-red",
        sliderInput("ccIRTSlider_d1", "\\(d\\) - inattention",
          min = 0, max = 1, value = 1, step = 0.1
        )
      ),
      ""
    )
  ),
  fluidRow(
    style = "margin-left: 0px; margin-right: 0px;",
    splitLayout(
      cellWidths = c("20%", "5%", "20%", "5%", "20%", "5%", "20%", "5%"),
      tags$div(
        class = "js-irs-blue",
        sliderInput("ccIRTSlider_a2", "\\(a\\) - discrimination",
          min = -4, max = 4, value = 2, step = 0.1
        )
      ),
      "",
      tags$div(
        class = "js-irs-blue",
        sliderInput("ccIRTSlider_b2", "\\(b\\) - difficulty",
          min = -4, max = 4, value = 0.5, step = 0.1
        )
      ),
      "",
      tags$div(
        class = "js-irs-blue",
        sliderInput("ccIRTSlider_c2", "\\(c\\) - guessing",
          min = 0, max = 1, value = 0, step = 0.1
        )
      ),
      "",
      tags$div(
        class = "js-irs-blue",
        sliderInput("ccIRTSlider_d2", "\\(d\\) - inattention",
          min = 0, max = 1, value = 1, step = 0.1
        )
      ),
      ""
    )
  ),
  p("You may also select the value of latent ability \\(\\theta\\) to obtain the interpretation of the item
                   characteristic curves for this ability. "),
  fluidRow(
    style = "margin-left: 0px; margin-right: 0px;",
    splitLayout(
      cellWidths = c("20%", "5%", "75%"),
      tags$div(
        class = "js-irs-gray",
        sliderInput("ccIRTSlider_theta", "\\(\\theta\\) - latent ability",
          min = -4, max = 4, value = 0, step = 0.1
        )
      ),
      ""
    )
  ),

  #------------------------------------------------------------------------------------#
  # Equations ####
  #------------------------------------------------------------------------------------#
  h4("Equations"),
  ("$$\\mathrm{P}\\left(Y = 1 \\vert\\theta\\right) = \\pi(\\theta) = c + \\left(d - c\\right) \\cdot \\frac{e^{a\\left(\\theta-b\\right) }}{1+e^{a\\left(\\theta-b\\right) }} $$"),
  ("$$\\mathrm{I}\\left(\\theta\\right) = \\frac{(\\pi(\\theta)')^2}{\\pi(\\theta)(1 - \\pi(\\theta))} = \\frac{a^2 \\cdot \\left(\\pi(\\theta) - c\\right)^2 \\cdot \\left(d - \\pi(\\theta)\\right)^2}{\\pi(\\theta) \\cdot \\left(1 - \\pi(\\theta)\\right) \\left(d - c\\right)^2} $$"),

  #------------------------------------------------------------------------------------#
  # Interpretation ####
  #------------------------------------------------------------------------------------#
  uiOutput("ccIRT_interpretation"),
  br(),
  p("Note that for 1PL and 2PL models, the item information is the highest at \\(\\theta = b\\). This is not necessarily the case for 3PL and 4PL models."),
  #------------------------------------------------------------------------------------#
  # Plots ####
  #------------------------------------------------------------------------------------#
  splitLayout(
    cellWidths = c("50%", "50%"),
    plotlyOutput("ccIRT_plot"),
    plotlyOutput("iicIRT_plot")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("DB_ccIRT", label = "Download figure"),
    downloadButton("DB_iicIRT", label = "Download figure")
  ),
  br(),

  #------------------------------------------------------------------------------------#
  # Exercise 1 ####
  #------------------------------------------------------------------------------------#
  h4("Exercise 1"),
  p(
    "Consider the following 2PL items with parameters", br(),
    strong("Item 1:"), "\\(a = 2.5, b = -0.5\\)", br(),
    strong("Item 2:"), "\\(a = 1.5, b = 0\\)", br(),
    "For these items fill in the following exercises with an accuracy of up to 0.05,
                   then click on the ", strong("Submit answers"), "button.
                   If you need a hint, click on the blue button with a question mark."
  ),
  tags$ul(
    tags$li(
      "Sketch the item characteristic and information curves.",
      bsButton(
        inputId = "irt_training_dich1_1_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich1_1_help", title = "Help",
        content = "Set item parameters using the red and the blue slider above.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      htmlOutput("irt_training_dich1_1_answer", inline = T)
    ),
    tags$li(
      "Calculate the probability of a correct answer for latent abilities
                           \\(\\theta  = -2, -1, 0, 1, 2\\).",
      bsButton(
        inputId = "irt_training_dich1_2_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich1_2_help",
        title = "Help",
        content = "Set &theta; to the desired value using the gray slider above.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
        strong("Item 1: "),
        numericInput(
          inputId = "irt_training_dich1_1_2a",
          label = "\\(\\theta = -2\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2a_1_answer"),
        numericInput(
          inputId = "irt_training_dich1_1_2b",
          label = "\\(\\theta = -1\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2b_1_answer"),
        numericInput(
          inputId = "irt_training_dich1_1_2c",
          label = "\\(\\theta = 0\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2c_1_answer"),
        numericInput(
          inputId = "irt_training_dich1_1_2d",
          label = "\\(\\theta = 1\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2d_1_answer"),
        numericInput(
          inputId = "irt_training_dich1_1_2e",
          label = "\\(\\theta = 2\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2e_1_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
        strong("Item 2: "),
        numericInput(
          inputId = "irt_training_dich1_2_2a",
          label = "\\(\\theta = -2\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2a_2_answer"),
        numericInput(
          inputId = "irt_training_dich1_2_2b",
          label = "\\(\\theta = -1\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2b_2_answer"),
        numericInput(
          inputId = "irt_training_dich1_2_2c",
          label = "\\(\\theta = 0\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2c_2_answer"),
        numericInput(
          inputId = "irt_training_dich1_2_2d",
          label = "\\(\\theta = 1\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2d_2_answer"),
        numericInput(
          inputId = "irt_training_dich1_2_2e",
          label = "\\(\\theta = 2\\)", value = 0
        ),
        htmlOutput("irt_training_dich1_2e_2_answer"), ""
      )
    ),
    tags$li(
      "For what level of ability \\(\\theta\\) are the probabilities equal?",
      bsButton(
        inputId = "irt_training_dich1_3_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich1_3_help",
        title = "Help",
        content = "You can find this value in the left figure. Alternatively, you need to find &theta; satisfying P<sub>1</sub>(&theta;) = P<sub>2</sub>(&theta;), that is a<sub>1</sub>(&theta; - b<sub>1</sub>) = a<sub>2</sub>(&theta; - b<sub>2</sub>).",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "4%", "89%"),
        numericInput(
          inputId = "irt_training_dich1_3",
          label = "\\(\\theta\\) = ?", value = 0
        ),
        uiOutput("irt_training_dich1_3_answer"), ""
      )
    ),
    tags$li(
      "Which item provides more information for weak (\\(\\theta = -2\\)), average (\\(\\theta = 0\\))
                           and strong (\\(\\theta = 2\\)) students?",
      bsButton(
        inputId = "irt_training_dich1_4_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich1_4_help",
        title = "Help",
        content = "Look at the figure on the right side. Which curve does have a larger value for the desired level of ability &theta;?",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "14%", "4%", "75%"),
        HTML("\\(\\theta = -2\\)"),
        radioButtons(
          inputId = "irt_training_dich1_4a",
          label = NULL,
          choices = list("Item 1" = 1, "Item 2" = 2),
          inline = T
        ),
        uiOutput("irt_training_dich1_4a_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "14%", "4%", "75%"),
        HTML("\\(\\theta = 0\\)"),
        radioButtons(
          inputId = "irt_training_dich1_4b",
          label = NULL,
          choices = list("Item 1" = 1, "Item 2" = 2),
          inline = T
        ),
        uiOutput("irt_training_dich1_4b_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "14%", "4%", "75%"),
        HTML("\\(\\theta = 2\\)"),
        radioButtons(
          inputId = "irt_training_dich1_4c",
          label = NULL,
          choices = list("Item 1" = 1, "Item 2" = 2),
          inline = T
        ),
        uiOutput("irt_training_dich1_4c_answer"),
        div(
          style = "display: inline-block; float: right; width: 200px;",
          htmlOutput("irt_training_dich1_answer"),
          actionButton(
            inputId = "irt_training_dich1_submit",
            label = "Submit answers", width = "80%"
          )
        )
      )
    )
  ),
  br(),

  #------------------------------------------------------------------------------------#
  # Exercise 2 ####
  #------------------------------------------------------------------------------------#
  h4("Exercise 2"),
  p(
    "Now consider 2 items with the following parameters", br(),
    strong("Item 1:"), "\\(a = 1.5, b = 0, c = 0, d = 1\\)", br(),
    strong("Item 2:"), "\\(a = 1.5, b = 0, c = 0.2, d = 1\\)", br(),
    "For these items fill in the following exercises with an accuracy of up to 0.05,
                   then click on the ", strong("Submit answers"), "button."
  ),
  tags$ul(
    tags$li(
      "What is the lower asymptote for items?",
      bsButton(
        inputId = "irt_training_dich2_1_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich2_1_help", title = "Help",
        content = "Lower asymptote is determined by parameter c.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 1:"),
        numericInput("irt_training_dich2_1a", label = NULL, value = 0),
        uiOutput("irt_training_dich2_1a_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 2:"),
        numericInput("irt_training_dich2_1b", label = NULL, value = 0),
        uiOutput("irt_training_dich2_1b_answer"), ""
      )
    ),
    tags$li(
      "What is the probability of a correct answer for latent ability \\(\\theta = b\\)?",
      bsButton(
        inputId = "irt_training_dich2_2_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich2_2_help", title = "Help",
        content = "You can find this value at the left figure. Alternatively, you can calculate the value using the formula for P(&theta;), using the fact that exp(0) = 1.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 1:"),
        numericInput("irt_training_dich2_2a", label = NULL, value = 0),
        uiOutput("irt_training_dich2_2a_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 2:"),
        numericInput("irt_training_dich2_2b", label = NULL, value = 0),
        uiOutput("irt_training_dich2_2b_answer"), ""
      )
    ),
    tags$li(
      "Which item is more informative?",
      bsButton(
        inputId = "irt_training_dich2_3_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich2_3_help", title = "Help",
        content = "Compare the curves at the right figure. Alternatively, you can compare formulas for information function.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("21%", "4%", "75%"),
        radioButtons("irt_training_dich2_3",
          label = NULL,
          choices = list(
            "Depends on the level the of \\(\\theta\\)" = 12,
            "Item 1 for all levels of \\(\\theta\\)" = 1,
            "Item 2 for all levels of \\(\\theta\\)" = 2
          )
        ),
        uiOutput("irt_training_dich2_3_answer"),
        div(
          style = "display: inline-block; float: right; width: 200px;",
          htmlOutput("irt_training_dich2_answer"),
          actionButton(
            inputId = "irt_training_dich2_submit",
            label = "Submit answers", width = "80%"
          )
        )
      )
    )
  ),
  br(),

  #------------------------------------------------------------------------------------#
  # Exercise 3 ####
  #------------------------------------------------------------------------------------#
  h4("Exercise 3"),
  p(
    "Now consider 2 items with the following parameters", br(),
    strong("Item 1:"), "\\(a = 1.5, b = 0, c = 0, d = 0.9\\)", br(),
    strong("Item 2:"), "\\(a = 1.5, b = 0, c = 0, d = 1\\)", br(),
    "For these items fill in the following exercises with an accuracy of up to 0.05, then click on the ", strong("Submit answers"), "button."
  ),
  tags$ul(
    tags$li(
      "What is the upper asymptote for items?",
      bsButton(
        inputId = "irt_training_dich3_1_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich3_1_help", title = "Help",
        content = "The upper asymptote is determined by parameter d.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 1:"),
        numericInput("irt_training_dich3_1a", label = NULL, value = 0),
        uiOutput("irt_training_dich3_1a_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 2:"),
        numericInput("irt_training_dich3_1b", label = NULL, value = 0),
        uiOutput("irt_training_dich3_1b_answer"), ""
      )
    ),
    tags$li(
      "What is the probability of a correct answer for latent ability \\(\\theta = b\\)?",
      bsButton(
        inputId = "irt_training_dich3_2_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich3_2_help", title = "Help",
        content = "You can find this value at the left figure. Alternatively, you can calculate the value by formula for P(&theta;), where you can use the fact that exp(0) = 1.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 1:"),
        numericInput("irt_training_dich3_2a", label = NULL, value = 0),
        uiOutput("irt_training_dich3_2a_answer"), ""
      ),
      splitLayout(
        cellWidths = c("7%", "7%", "4%", "82%"),
        strong("Item 2:"),
        numericInput("irt_training_dich3_2b", label = NULL, value = 0),
        uiOutput("irt_training_dich3_2b_answer"), ""
      )
    ),
    tags$li(
      "Which item is more informative?",
      bsButton(
        inputId = "irt_training_dich3_3_help",
        label = "", icon = icon("question"),
        style = "info", size = "extra-small"
      ),
      bsPopover(
        id = "irt_training_dich3_3_help", title = "Help",
        content = "Compare the curves at the right figure. Alternatively, you can compare formulas for information function.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      splitLayout(
        cellWidths = c("21%", "4%", "75%"),
        radioButtons("irt_training_dich3_3",
          label = NULL,
          choices = list(
            "Depends on the level of \\(\\theta\\)" = 12,
            "Item 1 for all levels of \\(\\theta\\)" = 1,
            "Item 2 for all levels of \\(\\theta\\)" = 2
          )
        ),
        uiOutput("irt_training_dich3_3_answer"),
        div(
          style = "display: inline-block; float: right; width: 200px;",
          htmlOutput("irt_training_dich3_answer"),
          actionButton(inputId = "irt_training_dich3_submit", label = "Submit answers", width = "80%")
        )
      )
    )
  ),
  br(),
  br(),

  #------------------------------------------------------------------------------------#
  # Selected R code ####
  #------------------------------------------------------------------------------------#
  h4("Selected R code"),
  code(includeText("sc/irt/train_dich.R"))
)
