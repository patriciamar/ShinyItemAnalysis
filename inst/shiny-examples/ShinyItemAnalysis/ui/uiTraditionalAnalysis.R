
uiTraditionalAnalysis <- navbarMenu(
  "Item analysis",

  # * TRADITIONAL ITEM ANALYSIS ####
  tabPanel(
    "Traditional item analysis",
    h3("Traditional item analysis"),
    p("Traditional item analysis uses proportions of correct answers or correlations to estimate item properties."),
    h4("Item difficulty/discrimination plot"),
    p(
      "Displayed is difficulty (red) and discrimination (blue) for all items. Items are ordered by difficulty. ", br(),
      strong("Difficulty"), " of the item is by default estimated as its average scaled score, i.e. average item score
                                               divided by its range. Below you can change the estimate of difficulty to average score of the item. For binary
                                               items both estimates are equivalent and can be interpreted as percent of respondents who answered the item correctly. ",
      br(),
      strong("Discrimination"), " is by default estimated as difference in (scaled) item score
                                               in upper and lower third of respondents (Upper-Lower Index, ULI). ULI can be customized by changing number of
                                               groups and by changing which groups should be compared (see also Martinkova, Stepanek et al., 2017). Other
                                               options for discrimination index include coRrelation between Item and Total score (RIT index) and coRrelation
                                               between Item and total score based on Rest of the items (RIR index). By rule of thumb, all items with discrimination
                                               lower than 0.2 (threshold in the plot), should be checked for content. Lower discrimination is excpectable
                                               in case of very easy or very difficult items, or in ULI based on more homogeneous groups
                                               (such as 4th and last fifth). Threshold may be adjusted for these cases or may be set to 0."
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
        selected = "ULI"
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
    br(), br(), HTML("<div class='pb' style='page-break-after:always'></div>"),
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
    div(code(HTML("library(difNLR)&nbsp;<br>library(psych)<br>library(ShinyItemAnalysis)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;difficulty&nbsp;and&nbsp;discrimination&nbsp;plot&nbsp;<br>DDplot(data,&nbsp;discrim&nbsp;=&nbsp;'ULI',&nbsp;k&nbsp;=&nbsp;3,&nbsp;l&nbsp;=&nbsp;1,&nbsp;u&nbsp;=&nbsp;3)&nbsp;<br><br>#&nbsp;Cronbach&nbsp;alpha&nbsp;<br>psych::alpha(data)&nbsp;<br><br>#&nbsp;traditional&nbsp;item&nbsp;analysis&nbsp;table&nbsp;<br>ItemAnalysis(data)"))),
    br()
  ),

  # * DISTRACTORS ####
  tabPanel(
    "Distractors",
    h3("Distractor analysis"),
    p("In distractor analysis, we are interested in how test takers select the correct answer and how the distractors
                      (wrong answers) were able to function effectively by drawing the test takers away from the correct answer."),
    h4("Distractors plot"),
    htmlOutput("distractor_text"),
    p("With option ", strong("Combinations"), "all item selection patterns are plotted (e.g., AB, ACD, BC). With option",
                      strong("Distractors"), "answers are splitted into distractors (e.g., A, B, C, D)."),
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
    div(code(HTML("library(difNLR)<br>library(ShinyItemAnalysis)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMATtest)&nbsp;<br>data&nbsp;<-&nbsp;GMATtest[,&nbsp;1:20]&nbsp;<br>data(GMATkey)&nbsp;<br>key&nbsp;<-&nbsp;unlist(GMATkey)&nbsp;<br><br>#&nbsp;combinations&nbsp;-&nbsp;plot&nbsp;for&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>plotDistractorAnalysis(data,&nbsp;key,&nbsp;num.group&nbsp;=&nbsp;3,&nbsp;item&nbsp;=&nbsp;1,&nbsp;multiple.answers&nbsp;=&nbsp;TRUE)&nbsp;<br><br>#&nbsp;distractors&nbsp;-&nbsp;plot&nbsp;for&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>plotDistractorAnalysis(data,&nbsp;key,&nbsp;num.group&nbsp;=&nbsp;3,&nbsp;item&nbsp;=&nbsp;1,&nbsp;multiple.answers&nbsp;=&nbsp;FALSE)&nbsp;<br><br>#&nbsp;table&nbsp;with&nbsp;counts&nbsp;and&nbsp;margins&nbsp;-&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>DA&nbsp;<-&nbsp;DistractorAnalysis(data,&nbsp;key,&nbsp;num.groups&nbsp;=&nbsp;3)[[1]]&nbsp;<br>dcast(as.data.frame(DA),&nbsp;response&nbsp;~&nbsp;score.level,&nbsp;sum,&nbsp;margins&nbsp;=&nbsp;TRUE,&nbsp;value.var&nbsp;=&nbsp;\"Freq\")&nbsp;<br><br>#&nbsp;table&nbsp;with&nbsp;proportions&nbsp;-&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>DistractorAnalysis(data,&nbsp;key,&nbsp;num.groups&nbsp;=&nbsp;3,&nbsp;p.table&nbsp;=&nbsp;TRUE)[[1]]"))),
    br()
  )
)
