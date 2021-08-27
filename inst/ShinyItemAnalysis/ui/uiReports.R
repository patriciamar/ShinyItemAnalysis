uiReports <-
  tabPanel(
    "Reports",
    h3("Download report"),
    # * GENERAL SETTINGS ####
    h4("Settings of report"),
    p(
      code("ShinyItemAnalysis"), " offers an option to download a report in HTML or PDF format. PDF report
             creation requires the latest version of", a("MiKTeX",
        href = "https://miktex.org/howto/install-miktex",
        target = "_blank"
      ),
      "(or other TeX distribution). If you don't have the latest installation, please, use the HTML report."
    ),
    p(
      "There is also an option to use customized settings. When checking the", strong("Customize settings,"),
      "local settings will be offered and used for each selected section of the report. Otherwise, the settings
             will be taken from sections made in the individual sections of the application.
             You may also include your name into the report, and change the name of the analyzed dataset. "
    ),
    fluidRow(
      column(2, radioButtons(
        inputId = "report_format",
        label = "Format of report",
        choices = c("HTML" = "html", "PDF" = "pdf")
      )),
      column(2, checkboxInput(
        inputId = "customizeCheck",
        label = "Customize settings",
        value = FALSE
      )),
      column(2, textInput(
        inputId = "reportAuthor",
        label = "Author"
      )),
      column(2, textInput(
        inputId = "reportDataName",
        label = "Dataset"
      ))
    ),
    h4("Content of report"),
    p("Reports by default contain a summary of total scores, table of standard scores, item analysis,
             distractor plots for each item and multinomial regression plots for each item. Other analyses
             can be selected below. "),
    tags$hr(),
    # * VALIDITY ####
    fluidRow(
      column(
        9,
        p(strong("Validity")),
        checkboxInput(
          inputId = "corr_report",
          label = "Correlation structure",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.customizeCheck",
          conditionalPanel(
            condition = "input.corr_report",
            fluidPage(
              column(
                3,
                numericInput(
                  inputId = "corr_plot_clust_report",
                  label = "Number of clusters",
                  value = 1,
                  min = 1,
                  max = 1
                )
              ),
              column(
                3,
                selectInput(
                  inputId = "corr_plot_clustmethod_report",
                  label = "Clustering method",
                  choices = list(
                    "None" = "none",
                    "Ward's" = "ward.D",
                    "Ward's n. 2" = "ward.D2",
                    "Single" = "single",
                    "Complete" = "complete",
                    "Average" = "average",
                    "McQuitty" = "mcquitty",
                    "Median" = "median",
                    "Centroid" = "centroid"
                  )
                )
              ),
              column(
                3,
                selectInput(
                  inputId = "corr_plot_type_of_corr_report",
                  label = "Choose correlation",
                  choices = c(
                    "Polychoric" = "polychoric",
                    "Pearson" = "pearson",
                    "Spearman" = "spearman"
                  ),
                  selected = "Polychoric"
                )
              )
            )
          )
        ),
        checkboxInput(
          inputId = "predict_report",
          label = "Predictive validity",
          value = FALSE
        )
      )
    ),
    tags$hr(),
    # * ITEM ANALYSIS ####
    conditionalPanel(
      condition = "input.customizeCheck",
      fluidRow(column(
        9,
        p(strong("Difficulty/discrimination plot")),
        column(
          3,
          selectInput(
            inputId = "report_itemanalysis_DDplot_difficulty",
            label = "Difficulty type:",
            choices = c(
              "Average scaled score" = "AVGSS",
              "Average item score" = "AVGS"
            ),
            selected = "AVGSS"
          )
        ),
        column(
          3,
          selectInput(
            inputId = "report_itemanalysis_DDplot_discrimination",
            label = "Discrimination type:",
            choices = c(
              "ULI" = "ULI",
              "RIT" = "RIT",
              "RIR" = "RIR",
              "none" = "none"
            ),
            selected = "ULI"
          )
        ),
        conditionalPanel(
          condition = "input.report_itemanalysis_DDplot_discrimination == 'ULI'",
          column(
            3,
            sliderInput(
              inputId = "report_itemanalysis_DDplot_groups_slider",
              label = "Number of groups:",
              min = 2,
              max = 5,
              value = 3
            )
          ),
          column(
            3,
            sliderInput(
              inputId = "report_itemanalysis_DDplot_range_slider",
              label = "Which two groups to compare:",
              min = 1,
              max = 3,
              step = 1,
              value = c(1, 3)
            )
          )
        )
      )),
      tags$hr()
    ),
    conditionalPanel(
      condition = "input.customizeCheck",
      fluidRow(column(
        9,
        p(strong("Distractors plots")),
        column(
          3,
          radioButtons(
            inputId = "report_distractor_type",
            label = "Type",
            choices = list("Combinations", "Distractors")
          )
        ),
        column(
          3,
          sliderInput(
            inputId = "report_distractor_group_slider",
            label = "Number of groups:",
            min = 1,
            max = 5,
            value = 3
          )
        ),
        column(
          3,
          uiOutput("report_distractor_groups_alert")
        )
      )),
      # uiOutput("report_distractor_groups_alert"),
      tags$hr()
    ),
    # * IRT ####
    fluidRow(
      column(
        2,
        radioButtons(
          inputId = "report_IRT_binary_model",
          label = "IRT model selection",
          choices = c(
            "None" = "none",
            "Rasch" = "Rasch",
            "1PL" = "1PL",
            "2PL" = "2PL",
            "3PL" = "3PL",
            "4PL" = "4PL"
          ),
          selected = "1PL"
        )
      )
    ),
    tags$hr(),
    # * DIF ####
    fluidRow(
      column(
        2,
        p(strong("DIF method selection")),
        checkboxInput(
          inputId = "histCheck",
          label = "None - histograms by group",
          value = FALSE
        ),
        checkboxInput(
          inputId = "deltaplotCheck",
          label = "Delta plot",
          value = FALSE
        ),
        checkboxInput(
          inputId = "MHCheck",
          label = "Mantel-Haenszel test",
          value = FALSE
        ),
        checkboxInput(
          inputId = "logregCheck",
          label = "Logistic regression",
          value = FALSE
        ),
        checkboxInput(
          inputId = "multiCheck",
          label = "Multinomial regression",
          value = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.customizeCheck",
        conditionalPanel(
          condition = "input.deltaplotCheck",
          column(
            2,
            p(strong("Delta plot settings")),
            radioButtons(
              inputId = "type_threshold_report",
              label = "Threshold",
              choices = list("Fixed", "Normal")
            ),
            checkboxInput(
              inputId = "puri_DP_report",
              label = "Item purification",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.puri_DP_report",
              selectInput(
                inputId = "puri_DP_type_report",
                label = "Purification method",
                choices = c(
                  "IPP1" = "IPP1",
                  "IPP2" = "IPP2",
                  "IPP3" = "IPP3"
                ),
                selected = "IPP1"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.MHCheck",
          column(
            2,
            p(strong("Mantel-Haenszel test settings")),
            # selectInput(inputId = "matching_MH_report",
            #             label = "DIF matching variable",
            #             choices = c("Total score" = "total_score",
            #                         "Standardized total score" = "zscore"
            #             ),
            #             selected = "score",
            #             width = "85%"),
            selectInput(
              inputId = "correction_method_MH_report",
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
              selected = "none",
              width = "85%"
            ),
            checkboxInput(
              inputId = "puri_MH_report",
              label = "Item purification",
              value = FALSE
            )
          )
        ),
        conditionalPanel(
          condition = "input.logregCheck",
          column(
            3, p(strong("Logistic regression settings")),
            withMathJax(),
            radioButtons(
              inputId = "type_print_DIF_logistic_report",
              label = "Type",
              choices = c(
                "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Any DIF" = "both",
                "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Uniform DIF" = "udif",
                "\\(H_{0}\\): Uniform DIF vs. \\(H_{1}\\): Non-uniform DIF" = "nudif"
              ),
              selected = "both"
            ),
            # selectInput(inputId = "matching_logistic_report",
            #             label = "DIF matching variable",
            #             choices = c("Total score" = "total_score",
            #                         "Standardized total score" = "zscore"
            #             ),
            #             selected = "score",
            #             width = "85%"),
            selectInput(
              inputId = "correction_method_log_report",
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
              selected = "none",
              width = "57%"
            ),
            checkboxInput(
              inputId = "puri_LR_report",
              label = "Item purification",
              value = FALSE
            )
          )
        ),
        conditionalPanel(
          condition = "input.multiCheck",
          column(
            3, p(strong("Multinomial regression settings")),
            withMathJax(),
            radioButtons(
              inputId = "type_DDF_report",
              label = "Type",
              choices = c(
                "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Any DIF" = "both",
                "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Uniform DIF" = "udif",
                "\\(H_{0}\\): Uniform DIF vs. \\(H_{1}\\): Non-uniform DIF" = "nudif"
              ),
              selected = "both"
            ),
            # selectInput(inputId = "matching_DDF_report",
            #                    label = "DIF matching variable",
            #                    choices = c("Total score" = "score",
            #                                "Standardized total score" = "zscore"
            #                    ),
            #                    selected = "score",
            #                    width = "57%"),
            selectInput(
              inputId = "correction_method_DDF_report",
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
              selected = "none",
              width = "57%"
            ),
            checkboxInput(
              inputId = "puri_DDF_report",
              label = "Item purification",
              value = FALSE
            )
          )
        )
      )
    ),
    tags$hr(),
    # * SESSION INFO ####
    radioButtons(
      inputId = "include_session",
      label = "Include session info",
      choices = c("Yes" = "yes", "No" = "no"),
      selected = "no",
      inline = TRUE
    ),
    tags$hr(),
    # * DOWNLOAD ####
    p(
      strong("Recommendation: "), "Report generation can be faster and more reliable when you first check
             sections of intended contents. For example, if you wish to include a ", strong("3PL IRT"),
      " model, you can first visit the ", strong("Dichotomous models"), " subsection of the ",
      strong("IRT models"), "section and fit the ", strong("3PL IRT"), " model."
    ),
    uiOutput("report_na_alert"),
    br(),
    div(style = "display:inline-block", actionButton(
      inputId = "generate",
      label = "Generate report",
      class = "btn btn-primary",
      icon = icon("bar-chart-o")
    )),
    tags$style(HTML("#download_report_button { margin-left: 25px }")),
    div(style = "display:inline-block", uiOutput("download_report_button"))
  )
