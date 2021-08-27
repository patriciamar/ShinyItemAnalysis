
uiTraditionalAnalysis <- navbarMenu(
  "Item analysis",

  # * TRADITIONAL ITEM ANALYSIS ####
  tabPanel(
    "Traditional item analysis",
    h3("Traditional item analysis"),
    p("Traditional item analysis uses proportions and correlations to estimate item properties."),
    h4("Item difficulty/discrimination plot"),
    p(
      "Displayed is difficulty (red) and discrimination (blue) for all items. Items are ordered by difficulty. ", br(),
      strong("Difficulty"), " of the item is by default estimated as its average scaled score, i.e. average item score
                                               divided by its range. Below you can change the estimate of difficulty to the average score of the item. For binary
                                               items both estimates are equivalent and can be interpreted as the percentage of respondents who answered the item correctly. ",
      br(),
      strong("Discrimination"), " is by default estimated as the coRrelation between Item and Total score (RIT index). Other
                                               options for the discrimination index include coRrelation between Item and total score based on Rest of the items (RIR index).
                                               Discrimination can also be estimated as the difference in (scaled) item score
                                               in the upper and lower third of the respondents (Upper-Lower Index, ULI). ULI can be further customized by changing the number of
                                               groups and by changing which groups should be compared (see also Martinkova, Stepanek et al., 2017). By a rule of thumb, all items with a discrimination
                                               lower than 0.2 (threshold in the plot), should be checked for content. Lower discrimination is excpectable
                                               in the case of very easy or very difficult items, or in ULI based on more homogeneous groups
                                               (such as 4th and last fifth). A threshold may be adjusted for these cases or may be set to 0."
    ),
    br(),
    fluidRow(
      column(2, selectInput(
        inputId = "itemanalysis_DDplot_difficulty",
        label = "Difficulty type:",
        choices = c(
          "Average scaled score" = "AVGSS",
          "Average item score" = "AVGS"
        ),
        selected = "AVGSS"
      )),
      column(2, selectInput(
        inputId = "itemanalysis_DDplot_discrimination",
        label = "Discrimination type:",
        choices = c(
          "ULI" = "ULI",
          "RIT" = "RIT",
          "RIR" = "RIR",
          "none" = "none"
        ),
        selected = "RIT"
      )),
      conditionalPanel(
        condition = "input.itemanalysis_DDplot_discrimination=='ULI'",
        column(2, sliderInput(
          inputId = "itemanalysis_DDplot_groups_slider",
          label = "Number of groups:",
          min = 2,
          max = 5,
          value = 3
        )),
        column(2, sliderInput(
          inputId = "itemanalysis_DDplot_range_slider",
          label = "Groups to compare:",
          min = 1,
          max = 3,
          step = 1,
          value = c(1, 3)
        ))
      ),
      column(
        2,
        div(
          style = "horizontal-align:left",
          checkboxInput(
            inputId = "itemanalysis_DDplot_threshold",
            label = "Show threshold",
            value = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.itemanalysis_DDplot_threshold",
          fluidRow(
            div(style = "display: inline-block; vertical-align:center; padding-left:10pt", HTML("<b>Threshold:</b>")),
            div(
              style = "display: inline-block; vertical-align:center; width: 45%;",
              numericInput(
                inputId = "itemanalysis_DDplot_threshold_value",
                label = NULL,
                value = .2,
                min = 0,
                max = 1,
                step = .1
              )
            )
          )
        )
      )
    ),
    htmlOutput("itemanalysis_DDplot_text"),
    plotlyOutput("itemanalysis_DDplot"),
    downloadButton("itemanalysis_DDplot_download", label = "Download figure"),
    br(), br(),
    h4("Traditional item analysis table"),
    withMathJax(),
    uiOutput("itemanalysis_table_text"),
    br(),
    tags$head(tags$style("#itemanalysis_table_coef {white-space: nowrap;}")),
    tableOutput("itemanalysis_table_coef"),
    uiOutput("itemanalysis_cronbach_note"), br(),
    # download item analysis table button
    downloadButton(
      outputId = "itemanalysis_table_download",
      label = "Download table"
    ),
    br(), br(),
    h4("Selected R code"),
    code(includeText("sc/tia/tia.R"))
  ),

  # * DISTRACTORS ####
  tabPanel(
    "Item response curves",
    h3("Empirical item response curves"),
    p("Empirical item response curves describe how test takers from different ability groups select available responses. In case of multiple-choice items
       these curves can show how the distractors (wrong answers) were able to function effectively by drawing the test takers away from the correct answer."),
    h4("Empirical item response curves / Distractors plot"),
    htmlOutput("distractor_text"),
    p(
      "With the option ", strong("Combinations"), "all item selection patterns are plotted (e.g., AB, ACD, BC). With the option",
      strong("Distractors"), "answers are split among the remaining incorect answers (e.g., A, B, C, D)."
    ),
    fluidPage(
      div(
        class = "input-slider",
        sliderInput(
          inputId = "distractor_group_slider",
          label = "Number of groups:",
          min = 1,
          max = 5,
          value = 3
        )
      ),
      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
      div(
        class = "input-radio",
        radioButtons(
          inputId = "distractor_type",
          label = "Type",
          choices = list("Combinations", "Distractors")
        )
      ),
      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
      div(
        class = "input-slider",
        sliderInput(
          inputId = "distractor_item_slider",
          label = "Item",
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          animate = animationOptions(interval = 1200)
        )
      )
    ),
    uiOutput("distractor_groups_alert"),
    br(),
    plotlyOutput("distractor_plot"),
    downloadButton("distractor_plot_download", label = "Download figure"),
    br(),
    h4("Table with counts"),
    fluidRow(column(12, align = "center", tableOutput("distractor_table_counts"))),
    h4("Table with proportions"),
    fluidRow(column(12, align = "center", tableOutput("distractor_table_proportions"))),
    br(),
    h4("Barplot of item response patterns"),
    plotlyOutput("distractor_barplot_item_response_patterns"),
    downloadButton("distractor_barplot_item_response_patterns_download", label = "Download figure"),
    h4("Histogram of total scores"),
    plotlyOutput("distractor_histogram"),
    downloadButton("distractor_histogram_download", label = "Download figure"),
    br(),
    h4("Table of total scores by groups"),
    fluidRow(column(12, align = "center", tableOutput("distractor_table_total_score_by_group"))),
    br(),
    br(),
    h4("Selected R code"),
    code(includeText("sc/tia/distr.R"))
  ),

  # ** Items ####
  tabPanel("Item criterion validity",
    value = "crit_val_items",
    h3("Item criterion validity"),
    p("This section requires a criterion variable (e.g. future study success or future GPA in case
                                   of admission tests) which should correlate with the measurement. A criterion variable
                                   can be uploaded in the ", strong("Data"), "section. Here you can explore how the the criterion correlates with individual items. "), br(),

    h4("Item difficulty / criterion validity plot"),
    p('The following plot intelligibly depicts the criterion validity of every individual item (blue) together with its difficulty (red).
                                   Items are ordered by difficulty. You can choose from two indices of criterion validity - item-criterion correlation and the so-called "item validity index".
                                   The former refers to a simple Pearson product-moment correlation (or, in the case of a binary dataset, point-biserial correlation),
                                   the later also takes into account the item varinace (see Allen & Yen, 1979, for details).
                                   Further item analysis can be performed in an Item Analysis tab.'),
    fluidRow(
      column(
        2,
        selectInput(
          inputId = "DCplot_difficulty",
          label = "Difficulty type:",
          choices = c(
            "Average scaled score" = "AVGSS",
            "Average item score" = "AVGS"
          ),
          selected = "AVGSS"
        )
      ),
      column(
        2,
        selectInput(
          inputId = "DCplot_validity",
          label = "Validity type:",
          choices = c(
            "item-criterion corr." = "simple",
            "item validity index" = "index"
          ),
          selected = "simple"
        )
      ),
      column(
        2, br(),
        div(
          style = "horizontal-align:left",
          checkboxInput(
            inputId = "DCplotThr_cb",
            label = "Show threshold",
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.DCplotThr_cb",
          fluidRow(
            div(style = "display: inline-block; vertical-align:center; padding-left:10pt", HTML("<b>Threshold:</b>")),
            div(
              style = "display: inline-block; vertical-align:center; width: 45%;",
              numericInput(
                inputId = "DCplotThr",
                label = NULL,
                value = .2,
                min = 0,
                max = 1,
                step = .1
              )
            )
          )
        )
      )
    ),
    plotlyOutput("DCplot"),

    # download item analysis table button
    downloadButton("DB_DCplot", label = "Download figure"),
    br(), br(),

    h4("Distractor plot"),
    p("In a distractor analysis based on a criterion variable, we are interested in how test takers
                                   select the correct answer and the distractors (wrong answers) with respect to a group based
                                   on criterion variable."),
    htmlOutput("validity_distractor_text"),
    p("With option ", strong("Combinations"), "all item selection patterns are plotted (e.g. AB, ACD, BC). With
                                   option", strong("Distractors"), "answers are split into the various distractors (e.g. A, B, C, D)."),
    fluidPage(
      div(
        class = "input-slider",
        sliderInput(
          inputId = "validity_group",
          label = "Number of groups:",
          min = 1,
          max = 5,
          value = 3
        )
      ),
      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
      div(
        class = "input-radio",
        radioButtons(
          inputId = "type_validity_combinations_distractor",
          label = "Type",
          choices = list("Combinations", "Distractors")
        )
      ),
      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
      div(
        class = "input-slider",
        sliderInput(
          inputId = "validitydistractorSlider",
          label = "Item",
          min = 1,
          value = 1,
          max = 10,
          step = 1,
          animate = animationOptions(interval = 1200)
        )
      )
    ),
    uiOutput("validity_groups_alert"),
    plotlyOutput("validity_distractor_plot"),
    downloadButton(outputId = "DB_validity_distractor_plot", label = "Download figure"),
    h4("Correlation of criterion variable and scored item"),
    p("A test for association between the total score and criterion variable is based on Pearson product-moment correlation coefficient", em("r."), "The null hypothesis is that correlation is 0. "),
    uiOutput("validity_table_item"), br(),
    htmlOutput("validity_table_item_interpretation"), br(),
    h4("Selected R code"),
    code(includeText("sc/tia/crit_val_it.R"))
  )
)
