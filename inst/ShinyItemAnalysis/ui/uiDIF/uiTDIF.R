#------------------------------------------------------------------------------------#
# TRAINING ####
#------------------------------------------------------------------------------------#
uiTDIF <- tabPanel(
  "Exercises",
#  tabsetPanel(
#    tabPanel(
#      "Basics", #General concepts? Group-specific model?

      h3("DIF training"),
      p("In this section, you can explore the group-specific model for testing differential item functioning among two groups -
                                reference and focal."),
      #------------------------------------------------------------------------------------#
      # Parameters ####
      #------------------------------------------------------------------------------------#
      h4("Parameters"),
      p(
        "Select parameters ", strong("\\(a\\)"), "(discrimination) and ", strong("\\(b\\)"),
        "(difficulty) for an item given by 2PL IRT model for ",
        HTML('<font color="blue"><strong>reference</strong></font>'), " and ",
        HTML('<font color="#e6b800"><strong>focal</strong></font>'), " group. When the item parameters for
                                the reference and the focal group differ, this phenomenon is termed differential item functioning. "
      ),
      fluidRow(
        splitLayout(
          cellWidths = "25%",
          cellArgs = list(style = "padding: 15px;"),
          tags$div(
            class = "js-irs-blue",
            sliderInput("DIF_training_parameter_aR", "\\(a_R\\) - discrimination",
              min = 0, max = 4, value = 1, step = 0.1
            )
          ),
          tags$div(
            class = "js-irs-blue",
            sliderInput("DIF_training_parameter_bR", "\\(b_R\\) - difficulty",
              min = -4, max = 4, value = 0, step = 0.1
            )
          ),
          tags$div(
            class = "js-irs-yellow",
            sliderInput("DIF_training_parameter_aF", "\\(a_F\\) - discrimination",
              min = 0, max = 4, value = 1, step = 0.1
            )
          ),
          tags$div(
            class = "js-irs-yellow",
            sliderInput("DIF_training_parameter_bF", "\\(b_F\\) - difficulty",
              min = -4, max = 4, value = 0, step = 0.1
            )
          )
        )
      ),
      p("You may also select the value of latent ability \\(\\theta\\) to obtain the interpretation of the item
                                characteristic curves for this ability. "),
      fluidRow(
        splitLayout(
          cellWidths = "25%",
          cellArgs = list(style = "padding: 15px;"),
          tags$div(
            class = "js-irs-gray",
            sliderInput("DIF_training_parameter_theta", "\\(\\theta\\) - latent ability",
              min = -4, max = 4, value = 0, step = 0.1
            )
          )
        )
      ),
      #------------------------------------------------------------------------------------#
      # Interpretation ####
      #------------------------------------------------------------------------------------#
      uiOutput("DIF_training_interpretation"),
      br(),
      #------------------------------------------------------------------------------------#
      # Plots ####
      #------------------------------------------------------------------------------------#
      plotlyOutput("DIF_training_plot"),
      downloadButton("DB_DIF_training_plot", label = "Download figure"),
      br(),
      br(),
      #------------------------------------------------------------------------------------#
      # Exercise 1 ####
      #------------------------------------------------------------------------------------#
      h4("Exercise 1"),
      p("Consider item following 2PL model with the following parameters"),
      p(strong("Reference group:"), "\\(a_R = 1, b_R = 0\\)"),
      p(strong("Focal group:    "), "\\(a_F = 1, b_F = 1\\)"),
      p("For this item, fill in the following exercises with an accuracy of up to 0.05.
                                Then click on ", strong("Submit answers"), "button.
                                If you need a hint, click on blue button with question mark."),
      tags$ul(
        tags$li(
          "Sketch item characteristic curves for both groups.",
          bsButton(
            inputId = "DIF_training_1_1_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_1_1_help", title = "Help",
            content = "Set item parameters using blue and yellow sliders above.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          htmlOutput("DIF_training_1_1_answer", inline = T)
        ),
        tags$li(
          "What type of DIF is displayed?",
          bsButton(
            inputId = "DIF_training_1_2_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_1_2_help", title = "Help",
            content = "Uniform means difference in difficulty only; non-uniform means difference also in discrimination.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          htmlOutput("DIF_training_1_2_answer", inline = T),
          radioButtons(
            inputId = "DIF_training_1_2",
            label = NULL,
            choices = list(
              "None" = "none",
              "Uniform" = "uniform",
              "Non-uniform" = "nonuniform"
            )
          )
        ),
        tags$li(
          "What are the probabilities of correct answer for latent abilities
                                        \\(\\theta  = -2, 0, 2\\) for reference and focal group?",
          bsButton(
            inputId = "DIF_training_1_3_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_1_3_help",
            title = "Help",
            content = "Set &theta; to desired value using gray slider above.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          splitLayout(
            cellWidths = c("9%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "58%"),
            strong("Reference: "),
            numericInput(
              inputId = "DIF_training_1_3_1R",
              label = "\\(\\theta = -2\\)", value = 0
            ),
            htmlOutput("DIF_training_1_3_1R_answer"),
            numericInput(
              inputId = "DIF_training_1_3_2R",
              label = "\\(\\theta = 0\\)", value = 0
            ),
            htmlOutput("DIF_training_1_3_2R_answer"),
            numericInput(
              inputId = "DIF_training_1_3_3R",
              label = "\\(\\theta = 2\\)", value = 0
            ),
            htmlOutput("DIF_training_1_3_3R_answer")
          ),
          splitLayout(
            cellWidths = c("9%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "58%"),
            strong("Focal: "),
            numericInput(
              inputId = "DIF_training_1_3_1F",
              label = "\\(\\theta = -2\\)", value = 0
            ),
            htmlOutput("DIF_training_1_3_1F_answer"),
            numericInput(
              inputId = "DIF_training_1_3_2F",
              label = "\\(\\theta = 0\\)", value = 0
            ),
            htmlOutput("DIF_training_1_3_2F_answer"),
            numericInput(
              inputId = "DIF_training_1_3_3F",
              label = "\\(\\theta = 2\\)", value = 0
            ),
            htmlOutput("DIF_training_1_3_3F_answer")
          )
        ),
        tags$li(
          "Which group is favored?",
          bsButton(
            inputId = "DIF_training_1_4_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_1_4_help", title = "Help",
            content = "Which group has higher probability to answer correctly for all levels of ability?",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          htmlOutput("DIF_training_1_4_answer", inline = T),
          radioButtons(
            inputId = "DIF_training_1_4",
            label = NULL,
            choices = list(
              "None" = "none",
              "It depends on ability level" = "depends",
              "Reference" = "reference",
              "Focal" = "focal"
            )
          )
        )
      ),
      div(
        style = "display: inline-block; float: right; width: 200px;",
        htmlOutput("DIF_training_1_answer"),
        actionButton(
          inputId = "DIF_training_1_submit",
          class = "btn btn-large btn-primary",
          label = "Submit answers", width = "70%"
        )
      ),
      br(),
      #------------------------------------------------------------------------------------#
      # Exercise 2 ####
      #------------------------------------------------------------------------------------#
      h4("Exercise 2"),
      p("Consider item  following 2PL  model with the following parameters"),
      p(strong("Reference group:"), "\\(a_R = 0.8, b_R = -0.5\\)"),
      p(strong("Focal group:    "), "\\(a_F = 1.5, b_F = 1\\)"),
      p("For this item fill in the following exercises with an accuracy of up to 0.05.
                                Then click on ", strong("Submit answers"), "button.
                                If you need a hint, click on blue button with question mark."),
      tags$ul(
        tags$li(
          "Sketch item characteristic curves for both groups.",
          bsButton(
            inputId = "DIF_training_2_1_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_2_1_help", title = "Help",
            content = "Set item parameters using blue and yellow sliders above.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          htmlOutput("DIF_training_2_1_answer", inline = T)
        ),
        tags$li(
          "What type of DIF is displayed?",
          bsButton(
            inputId = "DIF_training_2_2_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_2_2_help", title = "Help",
            content = "Uniform means difference in difficulty only; non-uniform means difference also in discrimination.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          htmlOutput("DIF_training_2_2_answer", inline = T),
          radioButtons(
            inputId = "DIF_training_2_2",
            label = NULL,
            choices = list(
              "None" = "none",
              "Uniform" = "uniform",
              "Non-uniform" = "nonuniform"
            )
          )
        ),
        tags$li(
          "What are the probabilities of correct answer for latent abilities
                                        \\(\\theta  = -1, 0, 1\\) for reference and focal group?",
          bsButton(
            inputId = "DIF_training_2_3_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_2_3_help",
            title = "Help",
            content = "Set &theta; to desired value using gray slider above.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          splitLayout(
            cellWidths = c("9%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "58%"),
            strong("Reference: "),
            numericInput(
              inputId = "DIF_training_2_3_1R",
              label = "\\(\\theta = -1\\)", value = 0
            ),
            htmlOutput("DIF_training_2_3_1R_answer"),
            numericInput(
              inputId = "DIF_training_2_3_2R",
              label = "\\(\\theta = 0\\)", value = 0
            ),
            htmlOutput("DIF_training_2_3_2R_answer"),
            numericInput(
              inputId = "DIF_training_2_3_3R",
              label = "\\(\\theta = 1\\)", value = 0
            ),
            htmlOutput("DIF_training_2_3_3R_answer")
          ),
          splitLayout(
            cellWidths = c("9%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "58%"),
            strong("Focal: "),
            numericInput(
              inputId = "DIF_training_2_3_1F",
              label = "\\(\\theta = -1\\)", value = 0
            ),
            htmlOutput("DIF_training_2_3_1F_answer"),
            numericInput(
              inputId = "DIF_training_2_3_2F",
              label = "\\(\\theta = 0\\)", value = 0
            ),
            htmlOutput("DIF_training_2_3_2F_answer"),
            numericInput(
              inputId = "DIF_training_2_3_3F",
              label = "\\(\\theta = 1\\)", value = 0
            ),
            htmlOutput("DIF_training_2_3_3F_answer")
          )
        ),
        tags$li(
          "Which group is favored?",
          bsButton(
            inputId = "DIF_training_2_4_help",
            label = "", icon = icon("question"),
            style = "info", size = "extra-small"
          ),
          bsPopover(
            id = "DIF_training_2_4_help", title = "Help",
            content = "Which group has higher probability to answer correctly for all levels of ability?",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          ),
          htmlOutput("DIF_training_2_4_answer", inline = T),
          radioButtons(
            inputId = "DIF_training_2_4",
            label = NULL,
            choices = list(
              "None" = "none",
              "It depends on ability level" = "depends",
              "Reference" = "reference",
              "Focal" = "focal"
            )
          )
        )
      ),
      div(
        style = "display: inline-block; float: right; width: 200px;",
        htmlOutput("DIF_training_2_answer"),
        actionButton(
          inputId = "DIF_training_2_submit",
          class = "btn btn-large btn-primary",
          label = "Submit answers", width = "70%"
        )
      )

      #------------------------------------------------------------------------------------#
      # Selected R code ####
      #------------------------------------------------------------------------------------#
      # h4("Selected R code"),
      # div(code(HTML(""))),
    # ),
    # tabPanel(
    #   "Score-based methods",
    #   h3("Score-based methods")
    # ),
    # tabPanel(
    #   "IRT-based methods",
    #   h3("IRT-based methods")
    # ),
    # tabPanel(
    #   "Other topics",
    #   h3("Item purification"),
    #   h3("Corrections for multiple comparisons")
#    )
#  )
)
