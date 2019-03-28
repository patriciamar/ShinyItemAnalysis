
uiTraditionalAnalysis <- navbarMenu('Item analysis',

# * TRADITIONAL ITEM ANALYSIS ####
tabPanel("Traditional item analysis",
         h3("Traditional item analysis"),
         p('Traditional item analysis uses proportions of correct answers or correlations to estimate item properties.'),
         h4("Item difficulty/discrimination plot"),
         p("Displayed is difficulty (red) and discrimination (blue)
           for all items. Items are ordered by difficulty. ", br(),
           strong("Difficulty"),' of items is estimated as percent of respondents who
           answered correctly to that item.', br(),
           strong("Discrimination"),' is by default described by difference of percent correct
           in upper and lower third of respondents (Upper-Lower Index, ULI). By rule of
           thumb it should not be lower than 0.2 (borderline in the plot), except for
           very easy or very difficult items. Discrimination can be customized (see also Martinkova, Stepanek, et al.
           (2017)) by changing number of groups and by changing which groups should be compared: '),
         fluidPage(selectInput("DDplotDiscriminationSelect", "Discrimination type:",
                               c("ULI" = "ULI",
                                 "RIT" = "RIT",
                                 "RIR" = "RIR",
                                 "none" = "none"),
                               selected = "ULI"),
                   conditionalPanel(condition = "input.DDplotDiscriminationSelect=='ULI'",
                                    div(class = "input-slider",
                                        sliderInput(inputId = 'DDplotNumGroupsSlider',
                                                    label = 'Number of groups:',
                                                    min = 1,
                                                    max = 5,
                                                    value = 3)),
                                    div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                    div(class = "input-slider",
                                        sliderInput(inputId = "DDplotRangeSlider",
                                                    label = "Which two groups to compare:",
                                                    min = 1,
                                                    max = 3,
                                                    step = 1,
                                                    value = c(1, 3)))
                                    )
                   ),
         htmlOutput("DDplot_text"),
         br(),
         plotOutput('DDplot'),
         downloadButton("DB_DDplot", label = "Download figure"),
         h4("Cronbach's alpha"),
         p("Chronbach's alpha is an estimate of the reliability of a psychometric test. It is a function
            of the number of items in a test, the average covariance between item-pairs, and the variance
            of the total score (Cronbach, 1951)."),
         tableOutput('cronbachalpha_table'),
         h4("Traditional item analysis table"),
         htmlOutput("itemanalysis_table_text"),
         tableOutput('itemanalysis_table'),
         br(),
         #download item analysis table button
         downloadButton(outputId = "download_itemanal_table",
                        label    = "Download table"),
         br(),
         h4("Selected R code"),
         div(code(HTML("library(difNLR)&nbsp;<br>library(psych)<br>library(ShinyItemAnalysis)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;difficulty&nbsp;and&nbsp;discrimination&nbsp;plot&nbsp;<br>DDplot(data,&nbsp;discrim&nbsp;=&nbsp;'ULI',&nbsp;k&nbsp;=&nbsp;3,&nbsp;l&nbsp;=&nbsp;1,&nbsp;u&nbsp;=&nbsp;3)&nbsp;<br><br>#&nbsp;Cronbach&nbsp;alpha&nbsp;<br>psych::alpha(data)&nbsp;<br><br>#&nbsp;traditional&nbsp;item&nbsp;analysis&nbsp;table&nbsp;<br>ItemAnalysis(data)"))),
         br()
         ),

# * DISTRACTORS ####
tabPanel("Distractors",
         h3("Distractor analysis"),
         p('In distractor analysis, we are interested in how test takers select
         the correct answer and how the distractors (wrong answers) were able
         to function effectively by drawing the test takers away from the correct answer.'),
         h4("Distractors plot"),
         htmlOutput("distractor_text"),
         p('With option ', strong('Combinations'), 'all item selection patterns are plotted (e.g. AB, ACD, BC). With
           option', strong('Distractors'), 'answers are splitted into distractors (e.g. A, B, C, D).'),
         fluidPage(div(class = "input-slider",
                       sliderInput(inputId = 'gr',
                                   label = 'Number of groups:',
                                   min = 1,
                                   max   = 5,
                                   value = 3)),
                   div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                   div(class = "input-radio",
                       radioButtons(inputId = 'type_combinations_distractor',
                                    label = 'Type',
                                    choices = list("Combinations", "Distractors"))),
                   div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                   div(class = "input-slider",
                       sliderInput(inputId = "distractorSlider",
                                   label = "Item",
                                   min = 1,
                                   max = 10,
                                   value = 1,
                                   step = 1,
                                   animate = animationOptions(interval = 1200)))),
         plotOutput('distractor_plot'),
         downloadButton("DB_distractor_plot", label = "Download figure"),
         br(),
         h4("Table with counts"),
         fluidRow(column(12, align = "center", tableOutput('distractor_table_counts'))),
         h4("Table with proportions"),
         fluidRow(column(12, align = "center", tableOutput('distractor_table_proportions'))),
         br(),
         h4('Barplot of item response patterns'),
         plotOutput("distractor_barplot_item_response_patterns"),
         downloadButton( "DB_distractor_barplot_item_response_patterns", label = "Download figure"),
         h4('Histogram of total scores'),
         plotOutput('distractor_histogram'),
         downloadButton("DB_distractor_histogram", label = "Download figure"),
         br(),
         h4('Table of total scores by groups'),
         fluidRow(column(12, align = "center", tableOutput('distractor_table_total_score_by_group'))),
         br(),
         br(),
         h4("Selected R code"),
         div(code(HTML("library(difNLR)<br>library(ShinyItemAnalysis)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMATtest)&nbsp;<br>data&nbsp;<-&nbsp;GMATtest[,&nbsp;1:20]&nbsp;<br>data(GMATkey)&nbsp;<br>key&nbsp;<-&nbsp;GMATkey&nbsp;<br><br>#&nbsp;combinations&nbsp;-&nbsp;plot&nbsp;for&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>plotDistractorAnalysis(data,&nbsp;key,&nbsp;num.group&nbsp;=&nbsp;3,&nbsp;item&nbsp;=&nbsp;1,&nbsp;multiple.answers&nbsp;=&nbsp;TRUE)&nbsp;<br><br>#&nbsp;distractors&nbsp;-&nbsp;plot&nbsp;for&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>plotDistractorAnalysis(data,&nbsp;key,&nbsp;num.group&nbsp;=&nbsp;3,&nbsp;item&nbsp;=&nbsp;1,&nbsp;multiple.answers&nbsp;=&nbsp;FALSE)&nbsp;<br><br>#&nbsp;table&nbsp;with&nbsp;counts&nbsp;and&nbsp;margins&nbsp;-&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>DA&nbsp;<-&nbsp;DistractorAnalysis(data,&nbsp;key,&nbsp;num.groups&nbsp;=&nbsp;3)[[1]]&nbsp;<br>dcast(as.data.frame(DA),&nbsp;response&nbsp;~&nbsp;score.level,&nbsp;sum,&nbsp;margins&nbsp;=&nbsp;TRUE,&nbsp;value.var&nbsp;=&nbsp;\"Freq\")&nbsp;<br><br>#&nbsp;table&nbsp;with&nbsp;proportions&nbsp;-&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>DistractorAnalysis(data,&nbsp;key,&nbsp;num.groups&nbsp;=&nbsp;3,&nbsp;p.table&nbsp;=&nbsp;TRUE)[[1]]"))),
         br()
         )
)
