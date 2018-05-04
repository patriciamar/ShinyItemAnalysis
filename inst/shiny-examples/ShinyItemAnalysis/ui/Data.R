Data <- tabPanel("Data",
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
                              dataset (see below). To return to demonstration dataset, refresh this page in your
                              browser", strong("(F5)"), "."),

                            #------------------------------------------------------------------------------------#
                            # * Training datasets ####
                            #------------------------------------------------------------------------------------#
                            h4("Training datasets"),
                            p("Used dataset ", code("GMAT"),
                              a("(Martinkova, et al., 2017)",
                                href = "http://www.lifescied.org/content/16/2/rm2.full.pdf+html?with-ds=yes",
                                target = "_blank"),
                              "is generated dataset based on parameters of real Graduate Management Admission Test
                              (GMAT) (Kingston et al., 1985). However, first two items were simulated to function
                              differently in uniform and non-uniform way respectively. The dataset represents responses
                              of 2,000 subjects (1,000 males, 1,000 females) to multiple-choice test of 20 items.
                              The distribution of total scores is the same for both groups. See ",
                              a("Martinkova, et al. (2017) ",
                                href = "http://www.lifescied.org/content/16/2/rm2.full.pdf+html?with-ds=yes",
                                target = "_blank"),
                              "for further discussion. ", code("GMAT"), "containts simulated continuous criterion variable. "),
                            p(code("GMAT2"), " (Drabinova & Martinkova, 2017) is also simulated dataset based
                              on parameters of GMAT (Kingston et al., 1985) from ", code("difNLR"), "R package .
                              Again, first two items were generated to function differently in uniform and non-uniform
                              way respectively. The dataset represents responses of 1,000 subjects (500 males, 500 females)
                              to multiple-choice test of 20 items. "),
                            p(code("MSAT-B"), " (Drabinova & Martinkova, 2017) is a subset of real Medical School Admission
                              Test in Biology in Czech Republic. The dataset represents responses of 1,407 subjects (484 males,
                              923 females) to multiple-choice test of 20 items. First item was previously detected as
                              functioning differently. For more details of item selection see Drabinova and Martinkova (2017).
                              Dataset can be found in ", code("difNLR"), " R package."),
                            p(code("Medical 100"), " is a real dataset of admission test to medical school
                              from ", code("ShinyItemAnalysis"), " R package. The data set represents responses of
                              2,392 subjects (750 males, 1,633 females and 9 subjects without gender specification)
                              to multiple-choice test of 100 items. ", code("Medical 100"), "contains criterion variable -
                              indicator whether student studies standardly or not. "),
                            p(code("HCI"), " (McFarland et al., 2017) is a real dataset of Homeostasis Concept Inventory
                              from ", code("ShinyItemAnalysis"), " R package. The dataset represents responses of
                              651 subjects (405 males, 246 females) to multiple-choice test of 20 items. ", code("HCI"), "contains
                              criterion variable -  indicator whether student plans to major in the life sciences. "),
                            br(),
                            selectInput(inputId = "dataSelect",
                                        label = "Select dataset",
                                        choices = c("GMAT" = "GMAT_difNLR",
                                                    "GMAT2" = "GMAT2_difNLR",
                                                    "MSAT-B" = "MSATB_difNLR",
                                                    "Medical 100" = "dataMedical_ShinyItemAnalysis",
                                                    "HCI" = "HCI_ShinyItemAnalysis"),
                                        selected = "GMAT_difNLR"),
                            tags$hr(),

                            #------------------------------------------------------------------------------------#
                            # * Upload your own datasets ####
                            #------------------------------------------------------------------------------------#
                            h4("Upload your own datasets"),
                            p("Main ", strong("data"), " file should contain responses of individual respondents (rows)
                              to given items (columns). Header may contain item names, no row names should be included.
                              If responses are in unscored ABCD format, the ", strong("key"), " provides correct response
                              for each item. If responses are scored 0-1, key is vector of 1s."),
                            p(strong("Group"), " is 0-1 vector, where 0 represents reference group
                              and 1 represents focal group. Its length need to be the same as number of individual
                              respondents in main dataset. If the group is not provided then it wont be possible to run
                              DIF and DDF detection procedures on ", strong("DIF/Fairness"), " page. "),
                            p(strong("Criterion variable"), " is either discrete or continuous vector (e.g. future study
                              success or future GPA in case of admission tests) which should be predicted by the measurement.
                              Again, its length needs to be the same as number of individual respondents in the main dataset.
                              If the criterion variable is not provided then it wont be possible to run validity analysis in ",
                              strong("Predictive validity"), " section on ", strong("Validity"), " page."),
                            p("In all data sets", strong("header"), "should be either included or excluded. Columns of dataset
                              are by default renamed to Item and number of particular column. If you want to keep your own names,
                              check box ", strong("Keep items names"), "below. Missing values in scored dataset are by default
                              evaluated as 0. If you want to keep them as missing, check box" , strong("Keep missing values"),
                              "below."),

                            fluidRow(
                              column(3, offset = 0,
                                     fileInput(inputId = "data",
                                               label = "Choose data (csv file)",
                                               accept = c("text/csv",
                                                          "text/comma-separated-values",
                                                          "text/tab-separated-values",
                                                          "text/plain",
                                                          ".csv",
                                                          ".tsv"))),
                              column(3,
                                     fileInput(inputId = "key",
                                               label = "Choose key (csv file)",
                                               accept = c("text/csv",
                                                          "text/comma-separated-values",
                                                          "text/tab-separated-values",
                                                          "text/plain",
                                                          ".csv",
                                                          ".tsv"))),
                              column(3,
                                     fileInput(inputId = "groups",
                                               label = "Choose groups for DIF (optional)",
                                               accept = c("text/csv",
                                                          "text/comma-separated-values",
                                                          "text/tab-separated-values",
                                                          "text/plain",
                                                          ".csv",
                                                          ".tsv"))),
                              column(3,
                                     fileInput(inputId = "criterion_variable",
                                               label = "Choose criterion variable (optional)",
                                               accept = c("text/csv",
                                                          "text/comma-separated-values",
                                                          "text/tab-separated-values",
                                                          "text/plain",
                                                          ".csv",
                                                          ".tsv")))),
                            div(style = "display: inline-block; vertical-align: top; horizontal-align: center;",
                                actionButton(inputId = "submitButton", label = "Submit Data"),
                                htmlOutput("checkDataText"),
                                htmlOutput("checkDataColumns01Text")),
                            tags$hr(),

                            #------------------------------------------------------------------------------------#
                            # * Data specification ####
                            #------------------------------------------------------------------------------------#
                            h4("Data specification"),
                            fluidRow(
                              column(3, offset = 0,
                                     checkboxInput(inputId = "header",
                                                   label = "Header",
                                                   value = TRUE),
                                     checkboxInput(inputId = "itemnam",
                                                   label = "Keep items names",
                                                   value = FALSE),
                                     checkboxInput(inputId = "missval",
                                                   label = "Keep missing values",
                                                   value = FALSE)),
                              column(3, offset = 1,
                                     radioButtons(inputId = "sep",
                                                  label = "Separator",
                                                  choices = c(Comma = ",",
                                                              Semicolon = ";",
                                                              Tab = "\t"),
                                                  selected = ",")),
                              column(3, offset = 0,
                                     radioButtons(inputId = "quote",
                                                  label = "Quote",
                                                  choices = c("None" = "",
                                                              "Double Quote" = '"',
                                                              "Single Quote" = "'"),
                                                  selected = '"'))),
                            br(),
                            br()),
                   #------------------------------------------------------------------------------------#
                   # DATA EXPLORATION ####
                   #------------------------------------------------------------------------------------#
                   tabPanel("Data exploration",

                            #------------------------------------------------------------------------------------#
                            # * Data exploration ####
                            #------------------------------------------------------------------------------------#
                            h3("Data exploration"),
                            p("Here you can explore uploaded dataset. Rendering of tables can take some time."),

                            #------------------------------------------------------------------------------------#
                            # * Main dataset ####
                            #------------------------------------------------------------------------------------#
                            h4("Main dataset"),
                            DT::dataTableOutput('headdata'),

                            #------------------------------------------------------------------------------------#
                            # * Key ####
                            #------------------------------------------------------------------------------------#
                            h4("Key (correct answers)"),
                            DT::dataTableOutput('key'),

                            #------------------------------------------------------------------------------------#
                            # * Scored test ####
                            #------------------------------------------------------------------------------------#
                            h4("Scored test"),
                            DT::dataTableOutput('sc01'),

                            #------------------------------------------------------------------------------------#
                            # * Group vector ####
                            #------------------------------------------------------------------------------------#
                            h4("Group vector"),
                            DT::dataTableOutput('group'),

                            #------------------------------------------------------------------------------------#
                            # * Criterion variable vector ####
                            #------------------------------------------------------------------------------------#
                            h4("Criterion variable vector"),
                            DT::dataTableOutput('critvar'),

                            br(),
                            br()))
                 )
