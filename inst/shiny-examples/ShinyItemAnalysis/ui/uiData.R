uiData <- tabPanel("Data",
                 tabsetPanel(
                   #------------------------------------------------------------------------------------#
                   # DATA ####
                   #------------------------------------------------------------------------------------#
                   tabPanel("Data",
                            h3("Data"),
                            #------------------------------------------------------------------------------------#
                            # * Data ####
                            #------------------------------------------------------------------------------------#
                            p("For demonstration purposes, 20-item dataset", code("GMAT"), "from", code("difNLR"),"
                              R package is used. On this page, you may select one of five datasets offered by",
                              code("difNLR"), "and", code("ShinyItemAnalysis"), "packages or you may upload your own
                              dataset (see below). To return to demonstration dataset, click on", strong("Unload data"), " button."),
                            tags$hr(),
                            #------------------------------------------------------------------------------------#
                            # * Training datasets ####
                            #------------------------------------------------------------------------------------#
                            h4("Training datasets"),
                            br(),
                            fluidRow(
                              column(3,
                                     selectInput(inputId = "dataSelect",
                                                 label = "Select dataset",
                                                 choices = c("GMAT" = "GMAT_difNLR",
                                                             "GMAT2" = "GMAT2_difNLR",
                                                             "MSAT-B" = "MSATB_difNLR",
                                                             "Medical 100" = "dataMedical_ShinyItemAnalysis",
                                                             "Medical 100 Graded" = "dataMedicalgraded_ShinyItemAnalysis",
                                                             "HCI" = "HCI_ShinyItemAnalysis",
                                                             "Science" = "Science_mirt"),
                                                 selected = "GMAT_difNLR")),
                              column(9,
                                     uiOutput("data_description"))),
                            tags$hr(),

                            #------------------------------------------------------------------------------------#
                            # * Upload your own datasets ####
                            #------------------------------------------------------------------------------------#
                            h4("Upload your own datasets"),
                            p("Here you can upload your own dataset. Select all necessary files and use ", strong("Upload data"), " button on bottom of this page."),
                            fluidRow(
                              box(width = 3,
                                  fileInput(inputId = "data",
                                            label = "Choose data (CSV file)",
                                            accept = c("text/csv",
                                                       "text/comma-separated-values",
                                                       "text/tab-separated-values",
                                                       "text/plain",
                                                       ".csv",
                                                       ".tsv"))),
                            column(9,
                                     p("Main ", strong("data"), " file should contain responses of individual respondents (rows)
                              to given items (columns). Data need to be either binary, nominal (e.g. in ABCD format), or ordinal (e.g. in Likert scale).
                              Header may contain item names, no row names should be included. In all data sets", strong("header"), "should
                              be either included or excluded. Columns of dataset are by default renamed to Item and number of particular column.
                              If you want to keep your own names, check box ", strong("Keep item names"), "below. Missing values in scored
                              dataset are by default evaluated as 0. If you want to keep them as missing, check box" , strong("Keep missing values"),
                                       "below."))),
                            fluidRow(
                              box(width = 12,
                                  column(2,
                                         radioButtons(inputId = "data_type",
                                                      label = list("Type of data",
                                                                   bsButton(inputId = "data_type_info",
                                                                            label = "",
                                                                            icon = icon("info"),
                                                                            style = "info",
                                                                            size = "extra-small"),
                                                                   bsPopover(id = "data_type_info",
                                                                             title = "Info",
                                                                             content = "Binary data are of 0-1 form, where 0 is incorrect answer and 1 is correct one. Nominal data may take e.g. ABCD form. Ordinal data are e.g. those on the Likert scale 1-2-3-4-5.",
                                                                             placement = "right",
                                                                             trigger = "hover",
                                                                             options = list(container = "body"))),
                                                      choices = c("Binary" = "binary",
                                                                  "Nominal" = "nominal",
                                                                  "Ordinal" = "ordinal"),
                                                      selected = "nominal")
                                  ),
                                  column(2,
                                         radioButtons(inputId = "sep",
                                                      label = "Separator",
                                                      choices = c(Comma = ",",
                                                                  Semicolon = ";",
                                                                  Tab = "\t"),
                                                      selected = ",")),
                                  column(2,
                                         radioButtons(inputId = "quote",
                                                      label = "Quote",
                                                      choices = c("None" = "",
                                                                  "Double Quote" = '"',
                                                                  "Single Quote" = "'")),
                                         selected = '"'),
                                  column(3,
                                         strong("Data specification"),
                                         checkboxInput(inputId = "header",
                                                       label = list("Header",
                                                                    bsButton(inputId = "header_info",
                                                                             label = "",
                                                                             icon = icon("info"),
                                                                             style = "info",
                                                                             size = "extra-small"),
                                                                    bsPopover(id = "header_info",
                                                                              title = "Info",
                                                                              content = "Header including item names should be included/excluded in all datasets.",
                                                                              placement = "right",
                                                                              trigger = "hover",
                                                                              options = list(container = "body"))),
                                                       value = TRUE),
                                         checkboxInput(inputId = "itemnam",
                                                       label = list("Keep item names",
                                                                    bsButton(inputId = "itemnam_info",
                                                                             label = "",
                                                                             icon = icon("info"),
                                                                             style = "info",
                                                                             size = "extra-small"),
                                                                    bsPopover(id = "itemnam_info",
                                                                              title = "Info",
                                                                              content = "Should item names be preserved?",
                                                                              placement = "right",
                                                                              trigger = "hover",
                                                                              options = list(container = "body"))),
                                                       value = FALSE)),
                                  column(3,
                                         strong("Missing values"),
                                         checkboxInput(inputId = "missval",
                                                       label = list("Keep missing values",
                                                                    bsButton(inputId = "missval_info",
                                                                             label = "",
                                                                             icon = icon("info"),
                                                                             style = "info",
                                                                             size = "extra-small"),
                                                                    bsPopover(id = "missval_info",
                                                                              title = "Info",
                                                                              content = "Should missing values be preserved?",
                                                                              placement = "right",
                                                                              trigger = "hover",
                                                                              options = list(container = "body"))),
                                                       value = FALSE),
                                         conditionalPanel(
                                           condition = "input.missval",
                                           div(id = "inline-left",
                                               textInput(inputId = "data_missingcoding",
                                                         label = list( bsButton(inputId = "data_missingcoding_info",
                                                                                label = "",
                                                                                icon = icon("info"),
                                                                                style = "info",
                                                                                size = "extra-small"),
                                                                       bsPopover(id = "data_missingcoding_info",
                                                                                 title = "Info",
                                                                                 content = "Enter encoding of missing values. Values should be seperated with comma, e.g. 9, 99, XXX. ",
                                                                                 placement = "right",
                                                                                 trigger = "hover",
                                                                                 options = list(container = "body"))),
                                                         placeholder = "Missing values")),
                                           div(id = "inline-left",
                                               disabled(textInput(inputId = "data_NAcoding",
                                                         label = list(bsButton(inputId = "data_NAcoding_info",
                                                                               label = "",
                                                                               icon = icon("info"),
                                                                               style = "info",
                                                                               size = "extra-small"),
                                                                      bsPopover(id = "data_NAcoding_info",
                                                                                title = "Info",
                                                                                content = "Enter encoding of not administred values. Values should be seperated with comma, e.g. 9, 99, NA.",
                                                                                placement = "right",
                                                                                trigger = "hover",
                                                                                options = list(container = "body"))),
                                                         placeholder = "Not administred values")
                                           )))))),

                            conditionalPanel(
                              condition = "input.data_type != 'binary'",
                              fluidRow(
                                box(width = 3,
                                    uiOutput("data_key_file_input"),
                                    conditionalPanel(
                                      condition = "input.data_type == 'ordinal'",
                                      textInput(inputId = "globalCut",
                                                label = "Dataset cut-score"))
                                    ),
                                column(9,
                                       conditionalPanel(
                                         condition = "input.data_type == 'nominal'",
                                         p("For nominal data, it is necessary to upload ", strong("key"), "of correct answers.")),
                                       conditionalPanel(
                                         condition = "input.data_type == 'ordinal'",
                                         p("For ordinal data, you are advised to include vector containing", strong("cut-score"), "which is used for binarization of uploaded data, i.e.,
                                           values greater or equal to provided cut-score are set to 1, otherwise to 0. You can either upload dataset of item-specific values, or you can
                                           provide one value for whole dataset."),
                                         p(strong("Note: "), "In case that cut-score is not provided, vector of maximal values is used. "))
                                )
                              )),
                            conditionalPanel(
                              condition = "input.data_type == 'ordinal'",
                              fluidRow(
                                box(width = 6,
                                    fluidRow(column(6,
                                                    fileInput(inputId = "minOrdinal",
                                                              label = "Choose minimal values",
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values",
                                                                         "text/tab-separated-values",
                                                                         "text/plain",
                                                                         ".csv",
                                                                         ".tsv")),
                                                    textInput(inputId = "globalMin",
                                                              label = "Dataset minimal value")),
                                             column(6,
                                                    fileInput(inputId = "maxOrdinal",
                                                              label = "Choose maximal values",
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values",
                                                                         "text/tab-separated-values",
                                                                         "text/plain",
                                                                         ".csv",
                                                                         ".tsv")),
                                                    textInput(inputId = "globalMax",
                                                              label = "Dataset maximal value")))),
                                column(6,
                                       p("For ordinal data, it is optional to upload ", strong("minimal and maximal"), "values of answers.
                                         You can either upload datasets of item-specific values, or you can provide one value for whole dataset."),
                                       p(strong("Note: "), "If no minimal or maximal values are provided, these
                                        values are set automatically based on observed values.")
                                )
                              )
                            ),
                            fluidRow(
                              box(width = 3,
                                  fileInput(inputId = "groups",
                                            label = "Choose group (optional)",
                                            accept = c("text/csv",
                                                       "text/comma-separated-values",
                                                       "text/tab-separated-values",
                                                       "text/plain",
                                                       ".csv",
                                                       ".tsv"))),
                              column(9,
                                     p(strong("Group"), " is binary vector, where 0 represents reference group
                              and 1 represents focal group. Its length needs to be the same as number of individual
                              respondents in the main dataset. If the group is not provided then it won't be possible to run
                              DIF and DDF detection procedures in ", strong("DIF/Fairness"), " section. Missing values
                              are not supported for group membership vector and such cases/rows of the data should be removed."))
                            ),
                            fluidRow(
                              box(width = 3,
                                     fileInput(inputId = "criterion_variable",
                                               label = "Choose criterion variable (optional)",
                                               accept = c("text/csv",
                                                          "text/comma-separated-values",
                                                          "text/tab-separated-values",
                                                          "text/plain",
                                                          ".csv",
                                                          ".tsv"))),
                              column(9,
                                     p(strong("Criterion variable"), " is either discrete or continuous vector (e.g. future study
                              success or future GPA in case of admission tests) which should be predicted by the measurement.
                              Its length needs to be the same as number of individual respondents in the main dataset.
                              If the criterion variable is not provided then it wont be possible to run validity analysis in ",
                                       strong("Predictive validity"), " section on ", strong("Validity"), " page."))
                            ),
							fluidRow(
								column(10,
									div(style = "vertical-align: top; float: right;",
									uiOutput('removeBut_output'))),
								column(2,
									div(style = "vertical-align: top; float: right;",
									actionButton(inputId = "submitButton",
												label = "Upload data",
												class = "btn btn-large btn-primary",
												icon = icon("upload"),
												width = "150px")))),
                            div(style = "vertical-align: top; float: left;",
                                htmlOutput("checkDataText")),
                            br(),
                            br(),
                            br(),
                            br(),
                            div(style = "vertical-align: top; float: left;",
                                htmlOutput("checkDataColumns01Text")),
                            div(style = "vertical-align: top; float: right;",
                                uiOutput("renderdeleteButtonColumns01")),
                            div(style = "vertical-align: top; float: right;",
                                htmlOutput("removedItemsText")),
                            br(),
                            div(style = "vertical-align: top; float: left;",
                                htmlOutput("checkGroupText")),
                            div(style = "vertical-align: top; float: right;",
                                uiOutput("renderdeleteButtonGroup")),
                            div(style = "vertical-align: top; float: right;",
                                htmlOutput("removedGroupText")),
                            br(),
                            br()),
                   #------------------------------------------------------------------------------------#
                   # BASIC SUMMARY ####
                   #------------------------------------------------------------------------------------#
                   tabPanel("Basic summary",
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
                            br(),
                            br()
                   ),
                   #------------------------------------------------------------------------------------#
                   # DATA EXPLORATION ####
                   #------------------------------------------------------------------------------------#
                   tabPanel("Data exploration",

                            #------------------------------------------------------------------------------------#
                            # * Data exploration ####
                            #------------------------------------------------------------------------------------#
                            h3("Data exploration"),
                            p("Here you can explore uploaded dataset. Rendering of tables can take some time."),
                            br(),
                            #------------------------------------------------------------------------------------#
                            # * Main dataset ####
                            #------------------------------------------------------------------------------------#
                            h4("Main dataset"),
                            DT::dataTableOutput('headdata'),
                            br(),
                            #------------------------------------------------------------------------------------#
                            # * Key ####
                            #------------------------------------------------------------------------------------#
                            h4("Key (correct answers)"),
                            DT::dataTableOutput('key'),
                            br(),
                            #------------------------------------------------------------------------------------#
                            # * Scored test ####
                            #------------------------------------------------------------------------------------#
                            h4("Scored test"),
                            DT::dataTableOutput('sc01'),
                            br(),
                            #------------------------------------------------------------------------------------#
                            # * Group vector ####
                            #------------------------------------------------------------------------------------#
                            h4("Group vector"),
                            DT::dataTableOutput('group'),
                            br(),
                            #------------------------------------------------------------------------------------#
                            # * Criterion variable vector ####
                            #------------------------------------------------------------------------------------#
                            h4("Criterion variable vector"),
                            DT::dataTableOutput('critvar'),

                            br(),
                            br())

                 ))
