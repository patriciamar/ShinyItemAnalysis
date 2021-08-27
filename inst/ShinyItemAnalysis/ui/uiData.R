uiData <- tabPanel(
  "Data",
  tabsetPanel(
    #------------------------------------------------------------------------------------#
    # DATA ####
    #------------------------------------------------------------------------------------#
    tabPanel("Data",
      value = "data_data",
      h3("Data"),
      #------------------------------------------------------------------------------------#
      # * Data ####
      #------------------------------------------------------------------------------------#
      p(
        "For demonstration purposes, the 20-item dataset", code("GMAT"),
        " is used. While on this page, you may select one of several other toy datasets or you may upload your own
        dataset (see below). To return to the demonstration dataset, click on the ", strong("Unload data"), " button."
      ),
      tags$hr(),
      #------------------------------------------------------------------------------------#
      # * Training datasets ####
      #------------------------------------------------------------------------------------#
      h4("Training datasets"),
      br(),
      fluidRow(
        column(
          3,
          selectInput(
            inputId = "data_toydata",
            label = "Select dataset",
            choices = c(
              "GMAT" = "GMAT_difNLR",
              # "GMAT2" = "GMAT2_difNLR",
              "HCI" = "HCI_ShinyItemAnalysis",
              # "AIBS Grant Peer Review Scoring" = "AIBS_ShinyItemAnalysis",
              "Learning To Learn 9" = "LearningToLearn_ShinyItemAnalysis_9",
              "Learning To Learn 6" = "LearningToLearn_ShinyItemAnalysis_6",
              "MSAT-B" = "MSATB_difNLR",
              "Medical 100" = "dataMedical_ShinyItemAnalysis",
              "Medical 100 Graded" = "dataMedicalgraded_ShinyItemAnalysis",
              "Science" = "Science_mirt"
            ),
            selected = "GMAT_difNLR"
          ),
        ),
        column(
          9,
          uiOutput("data_description")
        )
      ),
      tags$hr(),

      #------------------------------------------------------------------------------------#
      # * Upload your own datasets ####
      #------------------------------------------------------------------------------------#
      h4("Upload your own datasets"),
      p(
        "Here you can upload your own dataset. Select all necessary files and use the ", strong("Upload data"),
        " button on bottom of this page."
      ),
      fluidRow(
        box(
          width = 3,
          fileInput(
            inputId = "data_csvdata_main",
            label = "Choose data (CSV file)",
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              "text/plain",
              ".csv",
              ".tsv"
            )
          )
        ),
        column(
          9,
          p(
            "The main ", strong("data"), " file should contain the responses of individual respondents (rows)
            to given items (columns). Data need to be either binary, nominal (e.g. in ABCD format), or ordinal
            (e.g. in Likert scale). The header may contain item names, however, no row names should be included.
            In all data sets, the ", strong("header"), "should be either included or excluded. Columns of dataset
            are by default renamed to the Item and number of a particular column. If you want to keep your own
            names, check the box ", strong("Keep item names"), "below. Missing values in scored dataset are by
            default evaluated as 0. If you want to keep them as missing, check the box", strong("Keep missing values"),
            "below."
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          column(
            2,
            radioButtons(
              inputId = "data_csvdata_data_type",
              label = list(
                "Type of data",
                bsButton(
                  inputId = "data_csvdata_data_type_info",
                  label = "",
                  icon = icon("info"),
                  style = "info",
                  size = "extra-small"
                ),
                bsPopover(
                  id = "data_csvdata_data_type_info",
                  title = "Info",
                  content = "Binary data are of 0-1 form, where 0 is incorrect answer and 1 is correct one. Nominal data
                            may take, e.g., ABCD form. Ordinal data are, e.g., those on the Likert scale 1-2-3-4-5. ",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")
                )
              ),
              choices = c(
                "Binary" = "binary",
                "Nominal" = "nominal",
                "Ordinal" = "ordinal"
              ),
              selected = "nominal"
            )
          ),
          column(
            2,
            radioButtons(
              inputId = "data_csvdata_sep",
              label = "Separator",
              choices = c(
                Comma = ",",
                Semicolon = ";",
                Tab = "\t"
              ),
              selected = ","
            )
          ),
          column(2,
            radioButtons(
              inputId = "data_csvdata_quote",
              label = "Quote",
              choices = c(
                "None" = "",
                "Double Quote" = '"',
                "Single Quote" = "'"
              )
            ),
            selected = '"'
          ),
          column(
            3,
            strong("Data specification"),
            checkboxInput(
              inputId = "data_csvdata_header",
              label = list(
                "Header",
                bsButton(
                  inputId = "data_csvdata_header_info",
                  label = "",
                  icon = icon("info"),
                  style = "info",
                  size = "extra-small"
                ),
                bsPopover(
                  id = "data_csvdata_header_info",
                  title = "Info",
                  content = "Header including item names should be included/excluded in all datasets.",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")
                )
              ),
              value = TRUE
            ),
            checkboxInput(
              inputId = "data_csvdata_keep_itemnames",
              label = list(
                "Keep item names",
                bsButton(
                  inputId = "data_csvdata_keep_itemnames_info",
                  label = "",
                  icon = icon("info"),
                  style = "info",
                  size = "extra-small"
                ),
                bsPopover(
                  id = "data_csvdata_keep_itemnames_info",
                  title = "Info",
                  content = "Should item names be preserved?",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")
                )
              ),
              value = TRUE
            )
          ),
          column(
            3,
            strong("Missing values"),
            checkboxInput(
              inputId = "data_csvdata_keep_missing",
              label = list(
                "Keep missing values",
                bsButton(
                  inputId = "data_csvdata_keep_missing_info",
                  label = "",
                  icon = icon("info"),
                  style = "info",
                  size = "extra-small"
                ),
                bsPopover(
                  id = "data_csvdata_keep_missing_info",
                  title = "Info",
                  content = "Should missing values be preserved?",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")
                )
              ),
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.data_csvdata_keep_missing",
              div(
                id = "inline-left",
                textInput(
                  inputId = "data_csvdata_missing_coding",
                  label = list(
                    bsButton(
                      inputId = "data_csvdata_missing_coding_info",
                      label = "",
                      icon = icon("info"),
                      style = "info",
                      size = "extra-small"
                    ),
                    bsPopover(
                      id = "data_csvdata_missing_coding_info",
                      title = "Info",
                      content = "Enter encoding of missing values. Values should be seperated with comma, e.g., 9, 99, XXX. ",
                      placement = "right",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  ),
                  placeholder = "Missing values"
                )
              ),
              div(
                id = "inline-left",
                disabled(textInput(
                  inputId = "data_csvdata_notadministred_coding",
                  label = list(
                    bsButton(
                      inputId = "data_csvdata_notadministred_coding_info",
                      label = "",
                      icon = icon("info"),
                      style = "info",
                      size = "extra-small"
                    ),
                    bsPopover(
                      id = "data_csvdata_notadministred_coding_info",
                      title = "Info",
                      content = "Enter encoding of not administred values. Values should be seperated with comma, e.g., 9, 99, NA.",
                      placement = "right",
                      trigger = "hover",
                      options = list(container = "body")
                    )
                  ),
                  placeholder = "Not administred values"
                ))
              )
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.data_csvdata_data_type == 'ordinal'",
        fluidRow(
          box(
            width = 3,
            fileInput(
              inputId = "data_csvdata_cutscore_ordinal",
              label = "Choose cut-score (CSV file)",
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                "text/plain",
                ".csv",
                ".tsv"
              )
            ),
            textInput(
              inputId = "data_csvdata_cutscore_ordinal_global",
              label = "Dataset cut-score"
            )
          ),
          column(
            9,
            # conditionalPanel(
            #   condition = "input.data_csvdata_data_type == 'nominal'",
            #   p("For nominal data, it is necessary to upload ", strong("key"), "of correct answers.")
            # ),
            # conditionalPanel(
            # condition = "input.data_csvdata_data_type == 'ordinal'",
            p("For ordinal data, you are advised to include vector containing", strong("cut-score"), "which is used for
                binarization of uploaded data, i.e., values greater or equal to provided cut-score are set to 1, otherwise
                to 0. You can either upload dataset of item-specific values, or you can provide one value for whole dataset."),
            p(strong("Note: "), "In case that cut-score is not provided, vector of maximal values is used. ")
            # )
          )
        )
      ),
      conditionalPanel(
        condition = "input.data_csvdata_data_type == 'nominal'",
        fluidRow(
          box(
            width = 3,
            fileInput(
              inputId = "data_csvdata_key_nominal",
              label = "Choose key (CSV file)",
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                "text/plain",
                ".csv",
                ".tsv"
              )
            )
          ),
          column(
            9,
            # conditionalPanel(
            # condition = "input.data_csvdata_data_type == 'nominal'",
            p("For nominal data, it is necessary to upload ", strong("key"), "of correct answers.")
            # ),
            # conditionalPanel(
            #   condition = "input.data_csvdata_data_type == 'ordinal'",
            #   p("For ordinal data, you are advised to include vector containing", strong("cut-score"), "which is used for binarization of uploaded data, i.e.,
            #                                values greater or equal to provided cut-score are set to 1, otherwise to 0. You can either upload dataset of item-specific values, or you can
            #                                provide one value for whole dataset."),
            #   p(strong("Note: "), "In case that cut-score is not provided, vector of maximal values is used. ")
            # )
          )
        )
      ),
      conditionalPanel(
        condition = "input.data_csvdata_data_type == 'ordinal'",
        fluidRow(
          box(
            width = 6,
            fluidRow(
              column(
                6,
                fileInput(
                  inputId = "data_csvdata_minimal",
                  label = "Choose minimal values",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values",
                    "text/tab-separated-values",
                    "text/plain",
                    ".csv",
                    ".tsv"
                  )
                ),
                textInput(
                  inputId = "data_csvdata_minimal_global",
                  label = "Dataset minimal value"
                )
              ),
              column(
                6,
                fileInput(
                  inputId = "data_csvdata_maximal",
                  label = "Choose maximal values",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values",
                    "text/tab-separated-values",
                    "text/plain",
                    ".csv",
                    ".tsv"
                  )
                ),
                textInput(
                  inputId = "data_csvdata_maximal_global",
                  label = "Dataset maximal value"
                )
              )
            )
          ),
          column(
            6,
            p("For ordinal data, it is optional to upload ", strong("minimal and maximal"), "values of answers. You can
              either upload datasets of item-specific values, or you can provide one value for whole dataset."),
            p(strong("Note: "), "If no minimal or maximal values are provided, these values are set automatically based
              on observed values.")
          )
        )
      ),
      fluidRow(
        box(
          width = 3,
          fileInput(
            inputId = "data_csvdata_group",
            label = "Choose group (optional)",
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              "text/plain",
              ".csv",
              ".tsv"
            )
          )
        ),
        column(
          9,
          p(strong("Group"), " is a variable for DIF and DDF analyses. It should be a binary vector, where 0 represents the
          reference group and 1 represents the focal group. Its length needs to be the same as the number of individual
          respondents in the main dataset. Missing values are not supported for the group variable and such cases/rows of
          the data should be removed."),
          p(strong("Note: "), "If no group variable is provided, the DIF and DDF analyses in the ", strong("DIF/Fairness"), "
          section are not available. ")
        )
      ),
      fluidRow(
        box(
          width = 3,
          fileInput(
            inputId = "data_csvdata_criterion",
            label = "Choose criterion (optional)",
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              "text/plain",
              ".csv",
              ".tsv"
            )
          )
        ),
        column(
          9,
          p(
            strong("Criterion"), " is either a discrete or continuous variable (e.g., future study success or future
            GPA in the case of admission tests) which should be predicted by the measurement. Its length needs to be the
            same as the number of individual respondents in the main dataset. "
          ),
          p(strong("Note: "), "If no criterion variable is provided, it won't be possible to run a validity analysis in
            the ", strong("Predictive validity"), " section on ", strong("Validity"), " page.")
        )
      ),
      fluidRow(
        box(
          width = 3,
          fileInput(
            inputId = "data_csvdata_DIFmatching",
            label = "Choose observed score (optional)",
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              "text/plain",
              ".csv",
              ".tsv"
            )
          )
        ),
        column(
          9,
          p(
            strong("Observed score"), " is a variable describing observed ability or trait of respondents. If supplied,
            it is offered in the ", strong("Regression"), " and in the ", strong("DIF/Fairness"), " sections for analyses
            with respect to this external variable. Its length needs to be the same as the number of individual respondents
            in the main dataset. "
          ),
          p(strong("Note: "), "If no observed score is provided, the total scores or standardized total scores are used
            instead. ")
        )
      ),
      fluidRow(
        column(
          10,
          div(
            style = "vertical-align: top; float: right;",
            uiOutput("data_unload_button")
          )
        ),
        column(
          2,
          div(
            style = "vertical-align: top; float: right;",
            actionButton(
              inputId = "data_upload",
              label = "Upload data",
              class = "btn btn-large btn-primary",
              icon = icon("upload"),
              width = "150px"
            )
          )
        )
      ),
      div(
        style = "vertical-align: top; float: left;",
        htmlOutput("data_check_text")
      ),
      div(
        style = "vertical-align: top; float: left;",
        htmlOutput("data_check_binary_all01_text")
      ),
      div(
        style = "vertical-align: top; float: right;",
        uiOutput("data_remove_binary_all01_button")
      ),
      div(
        style = "vertical-align: top; float: right;",
        htmlOutput("data_check_binary_all01_confirmation")
      ),
      br(),
      div(
        style = "vertical-align: top; float: left;",
        htmlOutput("data_check_group_withNA_text")
      ),
      div(
        style = "vertical-align: top; float: right;",
        uiOutput("data_remove_group_withNA_button")
      ),
      div(
        style = "vertical-align: top; float: right;",
        htmlOutput("data_check_group_withNA_confirmation")
      )
    ),
    #------------------------------------------------------------------------------------#
    # BASIC SUMMARY ####
    #------------------------------------------------------------------------------------#
    tabPanel(
      "Basic summary",
      #------------------------------------------------------------------------------------#
      # * Data exploration ####
      #------------------------------------------------------------------------------------#
      h3("Basic summary"),
      h4("Main dataset"),
      textOutput("data_rawdata_dim"),
      verbatimTextOutput("data_rawdata_summary"),
      h4("Scored test"),
      verbatimTextOutput("data_binary_summary"),
      h4("Group"),
      verbatimTextOutput("data_group_summary"),
      h4("Criterion variable"),
      verbatimTextOutput("data_criterion_summary"),
      h4("Observed score"),
      verbatimTextOutput("data_DIFmatching_summary")
    ),
    #------------------------------------------------------------------------------------#
    # DATA EXPLORATION ####
    #------------------------------------------------------------------------------------#
    tabPanel(
      "Data exploration",

      #------------------------------------------------------------------------------------#
      # * Data exploration ####
      #------------------------------------------------------------------------------------#
      h3("Data exploration"),
      p("Here you can explore uploaded dataset. The rendering of tables can take some time."),
      br(),
      #------------------------------------------------------------------------------------#
      # * Main dataset ####
      #------------------------------------------------------------------------------------#
      h4("Main dataset"),
      DT::dataTableOutput("data_exploration_main"),
      br(),
      #------------------------------------------------------------------------------------#
      # * Key ####
      #------------------------------------------------------------------------------------#
      h4("Key (correct answers) / cut-score"),
      DT::dataTableOutput("data_exploration_key"),
      br(),
      #------------------------------------------------------------------------------------#
      # * Scored / binarized data ####
      #------------------------------------------------------------------------------------#
      h4("Scored / binarized data"),
      DT::dataTableOutput("data_exploration_binary"),
      br(),
      #------------------------------------------------------------------------------------#
      # * Other variables ####
      #------------------------------------------------------------------------------------#
      h4("Other variables"),
      DT::dataTableOutput("data_exploration_variables")
    )
  )
)
