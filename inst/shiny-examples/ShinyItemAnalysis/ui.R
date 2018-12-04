#%%%%%%%%%%%%%%%%%%%%%
# GLOBAL LIBRARY #####
#%%%%%%%%%%%%%%%%%%%%%

require(DT)
require(plotly)
require(shinyBS)
require(shinydashboard)
require(shinyjs)

#%%%%%%%%%%%%%%%%%%%%%
# SOURCING ###########
#%%%%%%%%%%%%%%%%%%%%%

source("ui/About.R", local = T)
source("ui/Data.R", local = T)
source("ui/IRT.R", local = T)

#%%%%%%%%%%%%%%%%%%%%%
# UI #################
#%%%%%%%%%%%%%%%%%%%%%

ui = tagList(
  tags$head(tags$link(rel = "shortcut icon",
                      href = "hexbin.png"),
            # CSS
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "style.css"),
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "margins_and_paddings.css"),
            tags$style(type = "text/css",
                       ".panel-footer {
                       position: fixed;
                       right: 0;
                       bottom: 0;
                       left: 0;
                       }"),
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "box.css"),
            tags$style(type = "text/css",
                       "#inline-left {
                       display: table;
                       width: 100%;
                       }
                       #inline-left label{
                       display: table-cell;
                       text-align: center;
                       vertical-align: middle;
                       padding-right: 5px;
                       }
                       #inline-left .form-group {
                       display: table-row;
                       width: 80%;
                       }"),
            # JS
            tags$script(type = "text/javascript",
                        src = "busy.js"),
            tags$script(type = "text/javascript",
                        src = "report_generating_message.js"),
            tags$script(type = "text/javascript",
                        src = "report_downloading_message.js"),
            tags$script(type = "text/javascript",
                        src = "collapsible_menu_click.js")
            ),
  div(class = "busy",
      p("Loading"),
      img(src = "busy_indicator.gif", height = 100, width = 100)
  ),

  shinyjs::useShinyjs(),

  tags$head(includeScript("google-analytics.js")),

  navbarPage(title = HTML('<div style = "margin-top: -10px;">
                          <div class = "header-title">
                          <img src = "header_hexbin.png">
                          ShinyItemAnalysis
                          </div>
                          <div class = "header-subtitle">
                          Test and item analysis
                          </div>
                          </div>'),
             windowTitle = 'ShinyItemAnalysis',
             position = 'fixed-top',
             selected = 'About',
             collapsible = TRUE,
             footer = list(
               HTML('<div class = "panel-footer", style = "opacity: 1.00; z-index: 1000;">
                    <p style = "margin:8px 0 0 0;">
                    <div class = "footer-title">
                    <img src = "hexbin.png">
                    ShinyItemAnalysis
                    </div>
                    <div class = "footer-subtitle">
                    Test and item analysis | Version 1.2.8-6
                    </div>
                    <span style = "float:right">
                    <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/" id = "tooltipweb" target="_blank">
                    <img src = "footer_web_icon.png", class = "footer-icons">
                    </a>
                    <a href = "https://github.com/patriciamar/ShinyItemAnalysis/" id = "tooltipgithub" target="_blank">
                    <img src = "footer_github_icon.png", class = "footer-icons">
                    </a>
                    <a href = "https://CRAN.R-project.org/package=ShinyItemAnalysis/" id = "tooltipcran" target="_blank">
                    <img src = "footer_cran_icon.png", class = "footer-icons">
                    </a>
                    </span>
                    </p>
                    <script>
                    $("#tooltipweb").attr("title", "Web");
                    $("#tooltipgithub").attr("title", "GitHub");
                    $("#tooltipcran").attr("title", "CRAN");
                    </script>
                    <br>
                    <div class = "footer-copyright">
                    &copy; 2018  ShinyItemAnalysis
                    </div>'),
               HTML('<div class = "footer-counter">'),
               textOutput('counter', inline = T),
               HTML('</div></div>')),


             theme = "bootstrap.css",
             #%%%%%%%%%%%%%%%%%%%%%
             # MAIN PANEL #########
             #%%%%%%%%%%%%%%%%%%%%%

             #%%%%%%%%%%%%%%%%%%%%%
             # ABOUT ##############
             #%%%%%%%%%%%%%%%%%%%%%
             About,
             #%%%%%%%%%%%%%%%%%%%%%
             # DATA ###############
             #%%%%%%%%%%%%%%%%%%%%%
             Data,
             #%%%%%%%%%%%%%%%%%%%%%
             # SUMMARY ############
             #%%%%%%%%%%%%%%%%%%%%%
             navbarMenu("Summary",
                        # * TOTAL SCORES ####
                        tabPanel("Total scores",
                                 h3("Analysis of total scores"),
                                 p("Total score, also known as raw score or sum score, is a total number of correct
                                   answers."),
                                 h4("Summary table"),
                                 p("Table below summarizes basic characteristics of total scores including
                                   minimum and maximum, mean, median, standard deviation, skewness and kurtosis.
                                   The kurtosis here is estimated by sample kurtosis \\(\\frac{m_4}{s_4}\\),
                                   where \\(m_4\\) is the fourth central moment and \\(s^2\\) is sample variance.
                                   The skewness is estimated by sample skewness \\(\\frac{m_3}{s^3}\\), where
                                   \\(m_3\\) is the third central moment. The kurtosis for normally distributed
                                   scores is near the value of 3 and the skewness is near the value of 0. "),
                                 tableOutput('totalscores_table'),
                                 h4("Histogram of total score"),
                                 fluidPage(div(class = "input-slider",
                                               sliderInput(inputId = "slider_totalscores_histogram",
                                                           label = "Cut-score",
                                                           min = 0,
                                                           max = 10,
                                                           value = 1,
                                                           step = 1))),
                                 p('For selected cut-score, blue part of histogram shows respondents with total score
                                   above the cut-score, grey column shows respondents with total score equal
                                   to the cut-score and red part of histogram shows respondents below the cut-score.'),
                                 plotOutput('totalscores_histogram'),
                                 downloadButton(outputId = "DB_totalscores_histogram", label = "Download figure"),
                                 br(),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)<br>library(ggplot2)<br>library(moments)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br><br>#&nbsp;total&nbsp;score&nbsp;calculation<br>score&nbsp;<-&nbsp;apply(data,&nbsp;1,&nbsp;sum)<br><br>#&nbsp;summary&nbsp;of&nbsp;total&nbsp;score&nbsp;<br>c(min(score),&nbsp;max(score),&nbsp;mean(score),&nbsp;median(score),&nbsp;sd(score),&nbsp;skewness(score),&nbsp;kurtosis(score))<br><br>#&nbsp;colors&nbsp;by&nbsp;cut-score<br>cut&nbsp;<-&nbsp;median(score)&nbsp;#&nbsp;cut-score&nbsp;<br>color&nbsp;<-&nbsp;c(rep(\"red\",&nbsp;cut&nbsp;-&nbsp;min(score)),&nbsp;\"gray\",&nbsp;rep(\"blue\",&nbsp;max(score)&nbsp;-&nbsp;cut))<br>df&nbsp;<-&nbsp;data.frame(score)<br><br>#&nbsp;histogram<br>ggplot(df,&nbsp;aes(score))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_histogram(binwidth&nbsp;=&nbsp;1,&nbsp;fill&nbsp;=&nbsp;color,&nbsp;col&nbsp;=&nbsp;\"black\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Number&nbsp;of&nbsp;respondents\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),
                        # * STANDARD SCORES ####
                        tabPanel("Standard scores",
                                 h3('Standard scores'),
                                 p(strong('Total score'), 'also known as raw score is a total number of correct
                                   answers. It can be used to compare individual score to a norm group, e.g. if the mean
                                   is 12, then individual score can be compared to see if it is below or above this average. ', br(),
                                   strong('Percentile'), 'indicates the value below which a percentage of observations
                                   falls, e.g. a individual score at the 80th percentile means that the individual score
                                   is the same or higher than the scores of 80% of all respondents. ', br(),
                                   strong('Success rate'), 'is the percentage of success, e.g. if the maximum points of test
                                   is equal to 20 and individual score is 12 then success rate is 12/20 = 0.6, i.e. 60%.', br(),
                                   strong('Z-score'), 'or also standardized score is a linear transformation of total
                                   score with a mean of 0 and with variance of 1. If X is total score, M its mean and SD its
                                   standard deviation then Z-score = (X - M) / SD. ', br(),
                                   strong('T-score'), 'is transformed Z-score with a mean of 50 and standard deviation
                                   of 10. If Z is Z-score then T-score = (Z * 10) + 50. '),
                                 h4("Table by score"),
                                 tableOutput('scores_tables'),
                                 br(),
                                 #download button for downloading table
                                 downloadButton( outputId = 'download_standard_scores',
                                                 label = 'Download table'),
                                 br(),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;scores&nbsp;calculations<br>score&nbsp;<-&nbsp;apply(data,&nbsp;1,&nbsp;sum)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Total&nbsp;score&nbsp;<br>tosc&nbsp;<-&nbsp;sort(unique(score))&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Levels&nbsp;of&nbsp;total&nbsp;score&nbsp;<br>perc&nbsp;<-&nbsp;cumsum(prop.table(table(score)))&nbsp;#&nbsp;Percentiles&nbsp;<br>sura&nbsp;<-&nbsp;100&nbsp;*&nbsp;(tosc&nbsp;/&nbsp;max(score))&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Success&nbsp;rate&nbsp;<br>zsco&nbsp;<-&nbsp;sort(unique(scale(score)))&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Z-score&nbsp;<br>tsco&nbsp;<-&nbsp;50&nbsp;+&nbsp;10&nbsp;*&nbsp;zsco&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;T-score"))),
                                 br()
                                 )
                                 ),
             #%%%%%%%%%%%%%%%%%%%%%
             # RELIABILITY ########
             #%%%%%%%%%%%%%%%%%%%%%
             navbarMenu("Reliability",
                        "Description",
                        # * RELIABILITY ####
                        tabPanel("Reliability",
                                 h3("Reliability"),
                                 p("We are typically interested in unobserved true score \\(T\\), but have available only the
                                   observed score \\(X\\) which is contaminated by some measurement error \\(e\\), such that
                                   \\(X = T + e\\) and error term is uncorrelated with the true score."),
                                 h4("Equation"),
                                 p("Reliability is defined as squared correlation of the true and observed score"),
                                 withMathJax(),
                                 ('$$\\text{rel}(X) = \\text{cor}(T, X)^2$$'),
                                 p("Equivalently, reliability can be re-expressed as the ratio of the true score variance
                                   to total observed variance"),
                                 withMathJax(),
                                 ('$$\\text{rel}(X) = \\frac{\\sigma^2_T}{\\sigma^2_X}$$'),
                                 br()
                                 ),
                        "----",
                        "Used methods",
                        # * SPEARMAN-BROWN FORMULA ####
                        tabPanel("Spearman-Brown formula",
                                 h3("Spearman-Brown formula"),
                                 h4("Equation"),
                                 p("For test with \\(I\\) items total score is calculated as \\(X = X_1 + ... + X_I\\).
                                   Let \\(\\text{rel}(X)\\) be a reliability of the test. For test consisting of
                                   \\(I^*\\) items (equally precise, measuring the same construct),  that is for test which is
                                   \\(m = \\frac{I^*}{I}\\) times longer/shorter, the reliability would be"),
                                 withMathJax(),
                                 ('$$\\text{rel}(X^*) = \\frac{m\\cdot \\text{rel}(X)}{1 + (m - 1)\\cdot\\text{rel}(X)}.$$'),
                                 p("Spearman-Brown formula can be used to determine reliability of test with similar items but of
                                   different number of items. It can also be used to determine necessary number of items to achieve
                                   desired reliability."),
                                 p("In calculations below", strong("reliability of original data"), "is by
                                   default set to value of Cronbach's \\(\\alpha\\). ", strong("Number of items in original data"), "is
                                   by default set to number of items of dataset currently in use. "),
                                 fluidRow(column(3,
                                                 numericInput(inputId = "reliability_SBformula_reliability_original",
                                                              label = "Reliability of original data",
                                                              max = 0,
                                                              min = 1,
                                                              value = 0.7)),
                                          column(3,
                                                 numericInput(inputId = "reliability_SBformula_items_original",
                                                              label = "Number of items in original data",
                                                              min = 1,
                                                              step = 1,
                                                              value = 20))),
                                 br(),
                                 h4("Estimate of reliability with different number of items"),
                                 p("Here you can calculate estimate of reliability of a test consisting of different number of
                                   items (equally precise, measuring the same construct). "),
                                 fluidRow(column(3,
                                                 numericInput(inputId = "reliability_SBformula_items_new",
                                                              label = "Number of items in new data",
                                                              min = 1,
                                                              step = 1,
                                                              value = 30))),
                                 uiOutput("reliability_SBformula_reliability_text"),
                                 br(),
                                 h4("Necessary number of items for required level of reliability"),
                                 p("Here you can calculate necessary number of items (equally precise, measuring the same construct)
                                   to gain required level of reliability. "),
                                 fluidRow(column(3,
                                                 numericInput(inputId = "reliability_SBformula_reliability_new",
                                                              label = "Reliability of new data",
                                                              max = 0,
                                                              min = 1,
                                                              value = 0.8))),
                                 uiOutput("reliability_SBformula_items_text"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(psychometrics)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;reliability&nbsp;of&nbsp;original&nbsp;data<br>rel.original&nbsp;<-&nbsp;psychometric::alpha(data)<br>#&nbsp;number&nbsp;of&nbsp;items&nbsp;in&nbsp;original&nbsp;data<br>items.original&nbsp;<-&nbsp;ncol(data)<br><br><br>#&nbsp;number&nbsp;of&nbsp;items&nbsp;in&nbsp;new&nbsp;data<br>items.new&nbsp;<-&nbsp;30<br>#&nbsp;ratio&nbsp;of&nbsp;tests&nbsp;lengths<br>m&nbsp;<-&nbsp;items.new/items.original<br>#&nbsp;determining&nbsp;reliability<br>psychometric::SBrel(Nlength&nbsp;=&nbsp;m,&nbsp;rxx&nbsp;=&nbsp;rel.original)<br><br><br>#&nbsp;desired&nbsp;reliability<br>rel.new&nbsp;<-&nbsp;0.8<br>#&nbsp;determining&nbsp;test&nbsp;length<br>(m.new&nbsp;<-&nbsp;psychometric::SBlength(rxxp&nbsp;=&nbsp;rel.new,&nbsp;rxx&nbsp;=&nbsp;rel.original))<br>#&nbsp;number&nbsp;of&nbsp;required&nbsp;items<br>m.new*items.original"))),
                                 br(),
                                 br()
                                 ),
                        # * SPLIT-HALF METHOD ####
                        tabPanel("Split-half method",
                                 h3("Split-half method"),
                                 p("Split-half method uses correlation between two subscores for estimation of reliability.
                                   The underlying assumption is that the two halves of the test (or even all items on the test) are
                                   equally precise and measure the same underlying construct. Spearman-Brown formula is then used to
                                   correct the estimate for the number of items."),
                                 h4("Equation"),
                                 p("For test with \\(I\\) items total score is calculated as \\(X = X_1 + ... + X_I\\).
                                   Let \\(X^*_1\\) and \\(X^*_2\\) be total scores calculated from items only in the first
                                   and second subsets. Then estimate of reliability is given by Spearman-Brown formula (Spearman, 1910; Brown, 1910)
                                   with \\(m = 2\\)."),
                                 withMathJax(),
                                 ('$$\\text{rel}(X) = \\frac{m\\cdot \\text{cor}(X^*_1, X^*_2)}{1 + (m - 1)\\cdot\\text{cor}(X^*_1, X^*_2)} =
                                  \\frac{2\\cdot \\text{cor}(X^*_1, X^*_2)}{1 + \\text{cor}(X^*_1, X^*_2)}$$'),
                                 p("Below you can choose from different split-half approaches. ",
                                   strong("First-last"), "method uses correlation between the first half of items and the second
                                   half of items.", strong("Even-odd"), "includes even items into the first subset and odd items
                                   into the second one. ", strong("Random"), "method performs random split of items, thus the
                                   resulting estimate may be different for each call. ", strong("Revelle's \\(\\beta\\)"), "is
                                   actually the worst split-half (Revelle, 1979). Estimate is here calculated as the lowest split-half
                                   reliability of by default 10,000 random splits. Finally, ", strong("Average"), "considers by default
                                   10,000 split halves and averages the resulting estimates. Number of split halves can be changed below.
                                   In case of odd number of items, first subset contains one more item than second one."),
                                 uiOutput("reliability_splithalf_allpossible_text"),
                                 br(),
                                 fluidRow(column(3,
                                                 withMathJax(),
                                                 selectInput(inputId = "reliability_splithalf_method",
                                                             label = "Split half method",
                                                             choices = c("First-last" = "firstlast",
                                                                         "Even-odd" = "evenodd",
                                                                         "Random" = "random",
                                                                         "Revelle's beta" = "worst",
                                                                         "Average" = "average"),
                                                             selected = "First_last")),
                                          column(4,
                                                 numericInput(inputId = "reliability_splithalf_number",
                                                              label = textOutput("reliability_splithalf_number_label"),
                                                              value = 10000,
                                                              min = 1,
                                                              step = 1))),
                                 conditionalPanel(
                                   condition = "input.reliability_splithalf_method != 'average'",
                                   uiOutput("reliability_splithalf_text"),
                                   br()),
                                 h4("Reliability estimate with confidence interval"),
                                 p("Estimate of reliability for ", strong("First-last"), ", ", strong("Even-odd"), ", ", strong("Random"), "and",
                                   strong("Revelle's \\(\\beta\\)"), "is calculated using Spearman-Brown formula. Confidence interval is based on
                                   confidence interval of correlation using delta method. Estimate of reliability for ", strong("Average"),
                                   "method is mean value of sampled reliabilities and confidence interval is confidence interval of this mean. "),
                                 uiOutput("reliability_splithalf_table"),
                                 br(),
                                 h4("Histogram of reliability estimates"),
                                 p("Histogram is based on selected number of split halves estimates (10,000 by default).
                                   The current estimate is highlighted by red colour."),
                                 plotOutput("reliability_splithalf_histogram"),
                                 downloadButton("DB_reliability_splithalf_histogram"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(psych)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br><br>#&nbsp;First-last&nbsp;splitting<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;1:10]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;11:20]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;Even-odd&nbsp;splitting<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;seq(1,&nbsp;20,&nbsp;2)]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;seq(2,&nbsp;20,&nbsp;2)]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;Random&nbsp;splitting<br>samp&nbsp;<-&nbsp;sample(1:20,&nbsp;10)<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;samp]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;setdiff(1:20,&nbsp;samp)]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;Minimum&nbsp;of&nbsp;10,000&nbsp;split-halves&nbsp;(Revelle's&nbsp;beta)<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE)<br>items1&nbsp;<-&nbsp;which(split$minAB[,&nbsp;'A']&nbsp;==&nbsp;1)<br>items2&nbsp;<-&nbsp;which(split$minAB[,&nbsp;'B']&nbsp;==&nbsp;1)<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;items1]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;items2]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;calculation&nbsp;of&nbsp;CI<br>z.r&nbsp;<-&nbsp;0.5*log((1&nbsp;+&nbsp;cor.x)/(1&nbsp;-&nbsp;cor.x))<br>n&nbsp;<-&nbsp;length(ts1)<br>z.low&nbsp;<-&nbsp;z.r&nbsp;-&nbsp;1.96&nbsp;*&nbsp;sqrt(1/(n&nbsp;-&nbsp;3))<br>z.upp&nbsp;<-&nbsp;z.r&nbsp;+&nbsp;1.96&nbsp;*&nbsp;sqrt(1/(n&nbsp;-&nbsp;3))<br><br>cor.low&nbsp;<-&nbsp;(exp(2*z.low)&nbsp;-&nbsp;1)/(exp(2*z.low)&nbsp;+&nbsp;1)<br>cor.upp&nbsp;<-&nbsp;(exp(2*z.upp)&nbsp;-&nbsp;1)/(exp(2*z.upp)&nbsp;+&nbsp;1)<br><br>rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x)<br>rel.low&nbsp;<-&nbsp;2*cor.low/(1&nbsp;+&nbsp;cor.low)<br>rel.upp&nbsp;<-&nbsp;2*cor.upp/(1&nbsp;+&nbsp;cor.upp)<br><br><br>#&nbsp;Average&nbsp;10,000&nbsp;split-halves<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE)<br>(rel.x&nbsp;<-&nbsp;mean(split$raw))<br><br>#&nbsp;Average&nbsp;all&nbsp;split-halves<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE,&nbsp;brute&nbsp;=&nbsp;TRUE)<br>(rel.x&nbsp;<-&nbsp;mean(split$raw))<br><br>#&nbsp;calculation&nbsp;of&nbsp;CI<br>n&nbsp;<-&nbsp;length(split$raw)<br>rel.low&nbsp;<-&nbsp;rel.x&nbsp;-&nbsp;1.96&nbsp;*&nbsp;sd(split$raw)/sqrt(n)<br>rel.upp&nbsp;<-&nbsp;rel.x&nbsp;+&nbsp;1.96&nbsp;*&nbsp;sd(split$raw)/sqrt(n)"))),
                                 br()
                                 ),
                        # * CRONBACH'S ALPHA ####
                        tabPanel("Cronbach's \\(\\alpha\\)",
                                 h3("Cronbach's \\(\\alpha\\)"),
                                 p("Cronbach's \\(\\alpha\\) is an estimate of internal consistency of a psychometric test.
                                   It is a function of the number of items in a test, the average covariance
                                   between item-pairs, and the variance of the total score (Cronbach, 1951)."),
                                 h4("Equation"),
                                 p("For test with \\(I\\) items where \\(X = X_1 + ... + X_I\\) is a total score,
                                   \\(\\sigma^2_X\\) its variance and \\(\\sigma^2_{X_i}\\) variances of items,
                                   Cronbach's \\(\\alpha\\) is given by following equation"),
                                 withMathJax(),
                                 ('$$\\alpha = \\frac{I}{I-1}\\left(1 - \\frac{\\sum_{i = 1}^I \\sigma^2_{X_i}}{\\sigma^2_X}\\right)$$'),
                                 h4("Estimate with confidence interval"),
                                 p("Confidence interval is based on F distribution as proposed by Feldt et al. (1987)."),
                                 tableOutput("reliability_cronbachalpha_table"),
                                 # h3("McDonald's \\(\\omega\\)"),
                                 # p(),
                                 # br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(psychometric)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;Cronbach's&nbsp;alpha&nbsp;with&nbsp;confidence&nbsp;interval<br>a&nbsp;<-&nbsp;psychometric::alpha(data)<br>psychometric::alpha.CI(a,&nbsp;N&nbsp;=&nbsp;nrow(data),&nbsp;k&nbsp;=&nbsp;ncol(data),&nbsp;level&nbsp;=&nbsp;0.95)"))),
                                 br()
                                 )
                                 ),
             #%%%%%%%%%%%%%%%%%%%%%
             # VALIDITY ###########
             #%%%%%%%%%%%%%%%%%%%%%
             navbarMenu("Validity",
                        # * CORRELATION STRUCTURE ####
                        tabPanel("Correlation structure",
                                 h3("Correlation structure"),
                                 h4("Correlation heat map"),
                                 p("Correlation heat map displays selected type of",
                                   HTML("<b>correlations</b>"), "between items. The size and shade of circles indicate how much the
                                   items are correlated (larger and darker circle mean larger correlations).
                                   The color of circles indicates in which way the items are correlated - blue
                                   color mean possitive correlation and red color mean negative correlation.
                                   Correlation heat map can be reordered using hierarchical",
                                   HTML("<b>clustering method</b>"), "selected below. With", HTML("<b>number  of clusters</b>"), "larger than 1, the rectangles representing
                                   clusters are drawn. The values of correlation heatmap may be displayed and also downloaded."),
                                 fluidRow(div(style = "display: inline-block; vertical-align: top; width: 5%;"),
                                          column(3, selectInput(inputId = "type_of_corr",
                                                               label    = "Choose correlation",
                                                               choices  = c("Pearson"    = "pearson",
                                                                            "Spearman"   = "spearman",
                                                                            "Polychoric" = "polychoric"),
                                                               selected = 'polychoric',
                                                               width    =  '100%')),
                                          column(3, div(class = "input-box",
                                                       selectInput(inputId = 'corr_plot_clustmethod',
                                                                   label = 'Clustering method',
                                                                   choices = list("None" = "none",
                                                                                  "Ward's"  = "ward.D",
                                                                                  "Ward's n. 2" = "ward.D2",
                                                                                  "Single" = "single",
                                                                                  "Complete" = "complete",
                                                                                  "Average" = "average",
                                                                                  "McQuitty" = "mcquitty",
                                                                                  "Median" = "median",
                                                                                  "Centroid" = "centroid"),
                                                                   selected = "none"))),
                                          column(3, div(class = "input-box",
                                                       numericInput(inputId = 'corr_plot_clust',
                                                                    label = 'Number of clusters',
                                                                    value = 1,
                                                                    min = 1,
                                                                    max = 1))),
                                          column(3, actionButton(inputId = 'show_corr',
                                                                 label = 'Display correlation values',
                                                                 class = 'btn btn-primary'))),
                                 conditionalPanel(condition = "input.type_of_corr == 'pearson'",
                                                   p(HTML('<b>Pearson correlation coefficient</b>'), 'describes linear correlation between
                                                     two random variables X and Y. It is given by formula'),
                                                   withMathJax(),
                                                   ('$$\\rho = \\frac{cov(X,Y)}{\\sqrt{var(X)}\\sqrt{var(Y)}}.$$'),
                                                   p('Sample Pearson corelation coefficient may be calculated as'),
                                                   withMathJax(),
                                                   ('$$ r = \\frac{\\sum_{i = 1}^{n}(x_{i} - \\bar{x})(y_{i} - \\bar{y})}{\\sqrt{\\sum_{i = 1}^{n}(x_{i} - \\bar{x})^2}\\sqrt{\\sum_{i = 1}^{n}(y_{i} - \\bar{y})^2}}$$'),
                                                   p('Pearson correlation coefficient has a value between -1 and +1. Sample correlation of -1 and +1 correspond to all data points lying exactly on a line
                                                    (decreasing in case of negative linear correlation -1 and increasing for +1). If coefficient is
                                                     equal to 0 it implies no linear correlation between the variables.')),
                                 conditionalPanel(condition = "input.type_of_corr == 'polychoric'",
                                                   p(HTML("<b>Polychoric/tetrachoric correlation</b>"), "between two ordinal/binary variables is calculated from their contingency table,
                                                     under the assumption that the ordinal variables dissect continuous latent variables that are bivariate normal.")),
                                 conditionalPanel(condition = "input.type_of_corr == 'spearman'",
                                                   p(HTML("<b>Spearman's rank correlation coefficient</b>"), 'describes strength and direction of monotonic relationship between random variables X
                                                     and Y, i.e. dependence between the rankings of two variables. It is given by formula'),
                                                   withMathJax(),
                                                   ('$$\\rho = \\frac{cov(rg_{X},rg_{Y})}{\\sqrt{var(rg_{X})}\\sqrt{var(rg_{Y})}},$$'),
                                                   p('where rgX and rgY are transformed random variables X and Y into ranks, i.e Spearman correlation coefficient is the Pearson correlation coefficient between the ranked variables.'),
                                                   p('Sample Spearman correlation is calculated by converting X and Y to ranks (average ranks are used in case of ties) and by applying Pearson correlation formula. If both X and Y have',HTML('<i>n</i>'),'unique ranks, i.e. there are no ties, then sample correlation coefficient is given by formula'),
                                                   withMathJax(),
                                                   ('$$ r = 1 - \\frac{6\\sum_{i = 1}^{n}d_i^{2}}{n(n-1)}$$'),
                                                   p('where d = rgX - rgY is the difference between two ranks and ',HTML('<i>n</i>'), 'is size of X and Y.
                                                     Spearman rank correlation coefficient has value between -1 and 1, where 1  means perfect increasing relationship
                                                     between variables and -1 means decreasing relationship between the two variables.
                                                     In case of no repeated values, Spearman correlation of +1 or -1 means all data points lying exactly on some monotone line.
                                                     If coefficient is equal to 0, it means, there is no tendency for Y to either increase or decrease with X increasing.')),
                                 p(HTML("<b>Clustering methods.</b>"),
                                   "Ward's method aims at finding compact clusters based on minimizing the within-cluster
                                   sum of squares.
                                   Ward's n. 2 method uses squared disimilarities.
                                   Single method connects clusters with the nearest neighbours, i.e. the distance between
                                   two clusters is calculated as the minimum of distances of observations in one cluster and
                                   observations in the other clusters.
                                   Complete linkage with farthest neighbours on the other hand uses maximum of distances.
                                   Average linkage method uses the distance based on weighted average of the individual distances.
                                   McQuitty method uses unweighted average.
                                   Median linkage calculates the distance as the median of distances between an observation
                                   in one cluster and observation in the other cluster.
                                   Centroid method uses distance between centroids of clusters. "),
                                 br(),
                                 plotOutput('corr_plot'),
                                 br(),
                                 downloadButton(outputId = "DB_corr_plot", label = "Download figure"),
                                 # download correlation matrix button
                                 tags$style(HTML('#corr_matrix { margin: 10px }')),
                                 downloadButton(outputId = "corr_matrix",  label = "Download matrix"),
                                 br(),
                                 conditionalPanel(condition = "input.corr_plot_clustmethod != 'none'",
                                                  h4("Dendrogram"),
                                                  plotOutput('dendrogram_plot'),
                                                  downloadButton(outputId = "DB_dendrogram", label = "Download figure")),
                                 h4("Selected R code"),
                                 div(code(HTML("library(corrplot)&nbsp;<br>library(ggdendro)<br>library(difNLR)&nbsp;<br>library(psych)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;calculation&nbsp;of&nbsp;correlation<br>###&nbsp;Pearson<br>corP&nbsp;<-&nbsp;cor(data,&nbsp;method&nbsp;=&nbsp;\"pearson\")<br>###&nbsp;Spearman<br>corP&nbsp;<-&nbsp;cor(data,&nbsp;method&nbsp;=&nbsp;\"spearman\")<br>###&nbsp;Polychoric<br>corP&nbsp;<-&nbsp;polychoric(data)&nbsp;<br>corP$rho&nbsp;<br><br>#&nbsp;correlation&nbsp;heat&nbsp;map&nbsp;<br>corrplot(corP$rho)&nbsp;#&nbsp;correlation&nbsp;plot&nbsp;<br>corrplot(corP$rho,&nbsp;order&nbsp;=&nbsp;\"hclust\",&nbsp;hclust.method&nbsp;=&nbsp;\"ward.D\",&nbsp;addrect&nbsp;=&nbsp;3)&nbsp;#&nbsp;correlation&nbsp;plot&nbsp;with&nbsp;3&nbsp;clusters&nbsp;using&nbsp;Ward&nbsp;method<br><br>#&nbsp;dendrogram<br>hc&nbsp;<-&nbsp;hclust(as.dist(1&nbsp;-&nbsp;corP$rho),&nbsp;method&nbsp;=&nbsp;\"ward.D\")&nbsp;#&nbsp;hierarchical&nbsp;clustering&nbsp;<br>ggdendrogram(hc)&nbsp;#&nbsp;dendrogram"))),
                                 br()
                                 ),
                        # * FACTOR ANALYSIS ####
                        tabPanel("Factor analysis",
                                 h3("Factor analysis"),
                                 h4("Scree plot"),
                                 p('A scree plot displays the eigenvalues associated with an component or a factor in descending order
                                   versus the number of the component or factor. Location of a bend (an elbow) suggests a suitable number of clusters.'),
                                 plotOutput('scree_plot'),
                                 downloadButton(outputId = "DB_scree_plot", label = "Download figure"),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(psych)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;scree&nbsp;plot&nbsp;<br>ev&nbsp;<-&nbsp;eigen(corP$rho)$values&nbsp;#&nbsp;eigen&nbsp;values<br>df&nbsp;<-&nbsp;data.frame(comp&nbsp;=&nbsp;1:length(ev),&nbsp;ev)<br><br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;comp,&nbsp;y&nbsp;=&nbsp;ev))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_point()&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Eigen&nbsp;value\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Component&nbsp;number\")&nbsp;+<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),
                        # * PREDICTIVE VALIDITY ####
                        tabPanel('Criterion validity',
                                 tabsetPanel(
                                   # ** Summary ####
                                   tabPanel('Summary',
                                            h3('Criterion validity'),
                                            p('This section requires criterion variable (e.g. future study success or future GPA in case
                                              of admission tests) which should correlate with the measurement. Criterion variable
                                              can be uploaded in ', strong('Data'), 'section.'),
                                            h4('Descriptive plots of criterion variable on total score'),
                                            p('Total scores are plotted according to criterion variable. Boxplot or scatterplot is displayed
                                              depending on the type of criterion variable - whether it is discrete or continuous. Scatterplot is
                                              provided with red linear regression line. '),
                                            plotOutput('validity_plot'),
                                            downloadButton(outputId = "DB_validity_plot", label = "Download figure"),
                                            h4('Correlation of criterion variable and total score'),
                                            p('Test for association between total score and criterion variable is based on Spearman`s \\(\\rho\\).
                                              This rank-based measure has been recommended if bivariate normal distribution is not guaranteed.
                                              The null hypothesis is that correlation is 0. '),
                                            tableOutput('validity_table'),
                                            htmlOutput('validity_table_interpretation'),
                                            h4("Selected R code"),
                                            div(code(HTML("library(ShinyItemAnalysis)&nbsp;<br>library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data01&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>score&nbsp;<-&nbsp;apply(data01,&nbsp;1,&nbsp;sum)&nbsp;<br>#&nbsp;criterion&nbsp;variable<br>criterion&nbsp;<-&nbsp;GMAT[,&nbsp;\"criterion\"]&nbsp;<br>#&nbsp;number&nbsp;of&nbsp;respondents&nbsp;in&nbsp;each&nbsp;criterion&nbsp;level<br>size&nbsp;<-&nbsp;as.factor(criterion)<br>levels(size)&nbsp;<-&nbsp;table(as.factor(criterion))<br>size&nbsp;<-&nbsp;as.numeric(paste(size))<br>df&nbsp;<-&nbsp;data.frame(score,&nbsp;criterion,&nbsp;size)<br><br>#&nbsp;descriptive&nbsp;plots&nbsp;<br>###&nbsp;boxplot,&nbsp;for&nbsp;discrete&nbsp;criterion<br>ggplot(df,&nbsp;aes(y&nbsp;=&nbsp;score,&nbsp;x&nbsp;=&nbsp;as.factor(criterion),&nbsp;fill&nbsp;=&nbsp;as.factor(criterion)))&nbsp;+<br>&nbsp;&nbsp;geom_boxplot()&nbsp;+<br>&nbsp;&nbsp;geom_jitter(shape&nbsp;=&nbsp;16,&nbsp;position&nbsp;=&nbsp;position_jitter(0.2))&nbsp;+<br>&nbsp;&nbsp;scale_fill_brewer(palette&nbsp;=&nbsp;\"Blues\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Criterion&nbsp;group\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;coord_flip()&nbsp;+<br>&nbsp;&nbsp;theme_app()<br><br>###&nbsp;scatterplot,&nbsp;for&nbsp;continuous&nbsp;criterion<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;score,&nbsp;y&nbsp;=&nbsp;criterion))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_point()&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Criterion&nbsp;variable\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_smooth(method&nbsp;=&nbsp;lm,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;se&nbsp;=&nbsp;FALSE,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"red\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()<br><br>#&nbsp;correlation&nbsp;<br>cor.test(criterion,&nbsp;score,&nbsp;method&nbsp;=&nbsp;\"spearman\",&nbsp;exact&nbsp;=&nbsp;FALSE)"))),
                                            br()
                                            ),
                                   # ** Items ####
                                   tabPanel('Items',
                                            h3('Criterion validity'),
                                            p('This section requires criterion variable (e.g. future study success or future GPA in case
                                              of admission tests) which should correlate with the measurement. Criterion variable
                                              can be uploaded in ', strong('Data'), 'section. Here you can explore how the criterion correlates with individual items. '),
                                            p('In distractor analysis based on criterion variable, we are interested in how test takers
                                              select the correct answer and how the distractors (wrong answers) with respect to group based
                                              on criterion variable.'),
                                            h4('Distractor plot'),
                                            htmlOutput("validity_distractor_text"),
                                            p('With option ', strong('Combinations'), 'all item selection patterns are plotted (e.g. AB, ACD, BC). With
                                              option', strong('Distractors'), 'answers are splitted into distractors (e.g. A, B, C, D).'),
                                            fluidPage(div(class = "input-slider",
                                                          sliderInput(inputId = 'validity_group',
                                                                      label = 'Number of groups:',
                                                                      min   = 1,
                                                                      max   = 5,
                                                                      value = 3)),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(class = "input-radio",
                                                          radioButtons(inputId = 'type_validity_combinations_distractor',
                                                                       label = 'Type',
                                                                       choices = list("Combinations", "Distractors"))),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(class = "input-slider",
                                                          sliderInput(inputId = "validitydistractorSlider",
                                                                      label = "Item",
                                                                      min = 1,
                                                                      value = 1,
                                                                      max = 10,
                                                                      step = 1,
                                                                      animate = TRUE))),
                                            plotOutput('validity_distractor_plot'),
                                            downloadButton(outputId = "DB_validity_distractor_plot", label = "Download figure"),
                                            h4('Correlation of criterion variable and scored item'),
                                            p('Test for association between total score and criterion variable is based on Spearman`s \\(\\rho\\).
                                              This rank-based measure has been recommended if bivariate normal distribution is not guaranteed.
                                              The null hypothesis is that correlation is 0. '),
                                            tableOutput('validity_table_item'),
                                            htmlOutput('validity_table_item_interpretation'),
                                            h4("Selected R code"),
                                            div(code(HTML("library(ShinyItemAnalysis)&nbsp;<br>library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(\"GMAT\",&nbsp;\"GMATtest\",&nbsp;\"GMATkey\")&nbsp;<br>data&nbsp;<-&nbsp;GMATtest[,&nbsp;1:20]&nbsp;<br>data01&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>key&nbsp;<-&nbsp;GMATkey&nbsp;<br>criterion&nbsp;<-&nbsp;GMAT[,&nbsp;\"criterion\"]&nbsp;<br><br>#&nbsp;distractor&nbsp;plot&nbsp;for&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups&nbsp;<br>plotDistractorAnalysis(data,&nbsp;key,&nbsp;num.groups&nbsp;=&nbsp;3,&nbsp;item&nbsp;=&nbsp;1,&nbsp;matching&nbsp;=&nbsp;criterion)&nbsp;<br><br>#&nbsp;correlation&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>cor.test(criterion,&nbsp;data01[,&nbsp;1],&nbsp;method&nbsp;=&nbsp;\"spearman\",&nbsp;exact&nbsp;=&nbsp;F)"))),
                                            br()
                                            )
                                            ))),

             #%%%%%%%%%%%%%%%%%%%%%
             # ITEM ANALYSIS ######
             #%%%%%%%%%%%%%%%%%%%%%
             navbarMenu('Item analysis',
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
                                                           selected = "RIT"),
                                           conditionalPanel(condition = "input.DDplotDiscriminationSelect=='ULI'",
                                                            div(class = "input-slider",
                                                                sliderInput(inputId = 'DDplotNumGroupsSlider',
                                                                            label = 'Number of groups:',
                                                                            min   = 1,
                                                                            max   = 5,
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
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(psych)&nbsp;<br>library(psychometric)&nbsp;<br>library(ShinyItemAnalysis)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;difficulty&nbsp;and&nbsp;discrimination&nbsp;plot&nbsp;<br>DDplot(data,&nbsp;k&nbsp;=&nbsp;3,&nbsp;l&nbsp;=&nbsp;1,&nbsp;u&nbsp;=&nbsp;3)&nbsp;<br><br>#&nbsp;Cronbach&nbsp;alpha&nbsp;<br>psych::alpha(data)&nbsp;<br><br>#&nbsp;traditional&nbsp;item&nbsp;analysis&nbsp;table&nbsp;<br>tab&nbsp;<-&nbsp;round(data.frame(item.exam(data,&nbsp;discr&nbsp;=&nbsp;TRUE)[,&nbsp;c(4,&nbsp;1,&nbsp;5,&nbsp;2,&nbsp;3)],&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;psych::alpha(data)$alpha.drop[,&nbsp;1],&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;gDiscrim(data,&nbsp;k&nbsp;=&nbsp;3,&nbsp;l&nbsp;=&nbsp;1,&nbsp;u&nbsp;=&nbsp;3)),&nbsp;2)&nbsp;<br>colnames(tab)&nbsp;<-&nbsp;c(\"Difficulty\",&nbsp;\"SD\",&nbsp;\"Dsicrimination&nbsp;ULI\",&nbsp;\"Discrimination&nbsp;RIT\",&nbsp;\"Discrimination&nbsp;RIR\",&nbsp;\"Alpha&nbsp;Drop\",&nbsp;\"Customized&nbsp;Discrimination\")&nbsp;<br>tab"))),
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
                                                           min   = 1,
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
                                                           animate = TRUE))),
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
                        ),

             #%%%%%%%%%%%%%%%%%%%%%
             # REGRESSION #########
             #%%%%%%%%%%%%%%%%%%%%%
             navbarMenu("Regression",
                        "Dichotomous models",
                        # * LOGISTIC ####
                        tabPanel("Logistic",
                                 h3("Logistic regression on total scores"),
                                 p('Various regression models may be fitted to describe
                                   item properties in more detail.',
                                   strong('Logistic regression'),'can model dependency of probability of correct answer on total score by
                                   S-shaped logistic curve. Parameter', strong( "b0"),' describes horizontal position of the fitted curve,
                                   parameter ', strong( 'b1'),' describes its slope.'),
                                 br(),
                                 h4("Plot with estimated logistic curve"),
                                 p('Points represent proportion of correct answer with respect to total score.
                                   Their size is determined by count of respondents who achieved given level of
                                   total score.'),
                                 sliderInput("logregSlider", "Item",
                                             min = 1, value = 1, max = 10,
                                             step = 1, animate = TRUE),
                                 plotOutput('logreg_plot'),
                                 downloadButton("DB_logreg_plot", label = "Download figure"),
                                 h4("Equation"),
                                 withMathJax(),
                                 ('$$\\mathrm{P}(Y = 1|X, b_0, b_1) = \\mathrm{E}(Y|X, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 X\\right)}}{1+e^{\\left( b_{0} + b_1 X\\right) }} $$'),

                                 h4("Table of parameters"),
                                 fluidRow(column(12, align = "center", tableOutput('logreg_table'))),
                                 htmlOutput("logreg_interpretation"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>score&nbsp;<-&nbsp;apply(data,&nbsp;1,&nbsp;sum)&nbsp;#&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;score,&nbsp;family&nbsp;=&nbsp;binomial)&nbsp;<br><br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;function&nbsp;for&nbsp;plot&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;b0,&nbsp;b1){exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x)&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x))}&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(score)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;score,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(score)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(b0&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b1&nbsp;=&nbsp;coef(fit)[2]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),
                        # * LOGISTIC Z ####
                        tabPanel("Logistic Z",
                                 h3("Logistic regression on standardized total scores"),
                                 p('Various regression models may be fitted to describe
                                   item properties in more detail.',
                                   strong('Logistic regression'), 'can model dependency of probability of correct answer on
                                   standardized total score (Z-score) by S-shaped logistic curve. Parameter ', strong( 'b0'), ' describes
                                   horizontal position of the fitted curve (difficulty), parameter ', strong('b1'),' describes its slope at
                                   inflection point (discrimination). '),
                                 br(),
                                 h4("Plot with estimated logistic curve"),
                                 p('Points represent proportion of correct answer with respect to standardized
                                   total score. Their size is determined by count of respondents who achieved given
                                   level of standardized total score.'),
                                 sliderInput("zlogregSlider", "Item",
                                             min = 1, value = 1, max = 10,
                                             step = 1, animate = TRUE),
                                 plotOutput('z_logreg_plot'),
                                 downloadButton("DB_z_logreg_plot", label = "Download figure"),
                                 h4("Equation"),
                                 ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1) = \\mathrm{E}(Y|Z, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 Z\\right) }}{1+e^{\\left( b_{0} + b_1 Z\\right) }} $$'),
                                 h4("Table of parameters"),
                                 fluidRow(column(12, align = "center", tableOutput('z_logreg_table'))),
                                 htmlOutput("z_logreg_interpretation"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;zscore,&nbsp;family&nbsp;=&nbsp;binomial)&nbsp;<br><br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;function&nbsp;for&nbsp;plot&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;b0,&nbsp;b1){exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x)&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(b0&nbsp;+&nbsp;b1&nbsp;*&nbsp;x))}&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(b0&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b1&nbsp;=&nbsp;coef(fit)[2]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),
                        # * LOGISTIC IRT Z ####
                        tabPanel("Logistic IRT Z",
                                 h3("Logistic regression on standardized total scores with IRT parameterization"),
                                 p('Various regression models may be fitted to describe
                                   item properties in more detail.',
                                   strong('Logistic regression'), 'can model dependency of probability of correct answer on
                                   standardized total score (Z-score) by s-shaped logistic curve. Note change in parametrization - the IRT parametrization
                                   used here corresponds to the parametrization used in IRT models.
                                   Parameter', strong('b') , 'describes horizontal position of the fitted curve (difficulty),
                                   parameter' , strong('a') , ' describes its slope at inflection point (discrimination). '),
                                 br(),
                                 h4("Plot with estimated logistic curve"),
                                 p('Points represent proportion of correct answer with respect to standardized
                                   total score. Their size is determined by count of respondents who achieved given
                                   level of standardized total score.'),
                                 sliderInput("zlogreg_irtSlider", "Item",
                                             min = 1, value = 1, max = 10,
                                             step = 1, animate = TRUE),
                                 plotOutput('z_logreg_irt_plot'),
                                 downloadButton("DB_z_logreg_irt_plot", label = "Download figure"),
                                 h4("Equation"),
                                 ('$$\\mathrm{P}(Y = 1|Z, a, b) = \\mathrm{E}(Y|Z, a, b) = \\frac{e^{ a\\left(Z - b\\right) }}{1+e^{a\\left(Z - b\\right)}} $$'),
                                 h4("Table of parameters"),
                                 fluidRow(column(12, align = "center", tableOutput('z_logreg_irt_table'))),
                                 htmlOutput("z_logreg_irt_interpretation"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;logistic&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;glm(data[,&nbsp;1]&nbsp;~&nbsp;zscore,&nbsp;family&nbsp;=&nbsp;binomial)&nbsp;<br><br>#&nbsp;coefficients<br>coef&nbsp;<-&nbsp;c(a&nbsp;=&nbsp;coef(fit)[2],&nbsp;b&nbsp;=&nbsp;-&nbsp;coef(fit)[1]&nbsp;/&nbsp;coef(fit)[2])&nbsp;<br>coef&nbsp;&nbsp;<br><br>#&nbsp;function&nbsp;for&nbsp;plot&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b){exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(a&nbsp;=&nbsp;coef[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef[2]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),

                        # * NONLINEAR 3P IRT Z ####
                        tabPanel("Nonlinear 3P IRT Z",
                                 h3("Nonlinear three parameter regression on standardized total scores with IRT parameterization"),
                                 p('Various regression models may be fitted to describe
                                   item properties in more detail.',
                                   strong('Nonlinear regression'), 'can model dependency of probability of correct answer on
                                   standardized total score (Z-score) by s-shaped logistic curve. The IRT parametrization used here corresponds
                                   to the parametrization used in IRT models. Parameter ', strong( 'b'),' describes horizontal position of the fitted curve (difficulty),
                                   parameter ',strong( 'a'), ' describes its slope at inflection point (discrimination). This model allows for nonzero lower left
                                   asymptote ', strong( 'c'), ' (pseudo-guessing parameter). '),
                                 br(),
                                 h4("Plot with estimated nonlinear curve"),
                                 p('Points represent proportion of correct answer with respect to standardized
                                   total score. Their size is determined by count of respondents who achieved given
                                   level of standardized total score.'),
                                 sliderInput(inputId = "slider_nlr_3P_item", label = "Item",
                                             min = 1, value = 1, max = 10, step = 1, animate = TRUE),
                                 plotOutput('nlr_3P_plot'),
                                 downloadButton("DB_nlr_3P_plot", label = "Download figure"),
                                 h4("Equation"),
                                 ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1, c) = \\mathrm{E}(Y|Z, b_0, b_1, c) = c + \\left( 1-c \\right) \\cdot \\frac{e^{a\\left(Z-b\\right) }}{1+e^{a\\left(Z-b\\right) }} $$'),
                                 h4("Table of parameters"),
                                 fluidRow(column(12, align = "center", tableOutput('nlr_3P_table'))),
                                 htmlOutput("nlr_3P_interpretation"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;NLR&nbsp;3P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c){c&nbsp;+&nbsp;(1&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>fit&nbsp;<-&nbsp;nls(data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;startNLR(data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"3PLcg\",&nbsp;parameterization&nbsp;=&nbsp;\"classic\")[[1]][1:3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1))&nbsp;<br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(a&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef(fit)[2],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;c&nbsp;=&nbsp;coef(fit)[3]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),
                        # * NONLINEAR 4P IRT Z ####
                        tabPanel("Nonlinear 4P IRT Z",
                                 h3("Nonlinear four parameter regression on standardized total scores with IRT parameterization"),
                                 p('Various regression models may be fitted to describe
                                   item properties in more detail.',
                                   strong('Nonlinear four parameter regression'), 'can model dependency of probability of correct answer on
                                   standardized total score (Z-score) by s-shaped logistic curve. The IRT parametrization used here corresponds
                                   to the parametrization used in IRT models. Parameter ', strong( 'b'),' describes horizontal position of the fitted curve (difficulty),
                                   parameter ', strong( 'a'), ' describes its slope at inflection point (discrimination), pseudo-guessing parameter ', strong('c'), '
                                   is describes lower asymptote and inattention parameter ', strong('d'), 'describes upper asymptote.'),
                                 br(),
                                 h4("Plot with estimated nonlinear curve"),
                                 p('Points represent proportion of correct answer with respect to standardized
                                   total score. Their size is determined by count of respondents who achieved given
                                   level of standardized total score.'),
                                 sliderInput(inputId = "slider_nlr_4P_item", label = "Item",
                                             min = 1, value = 1, max = 10, step = 1, animate = TRUE),
                                 plotOutput('nlr_4P_plot'),
                                 downloadButton("DB_nlr_4P_plot", label = "Download figure"),
                                 h4("Equation"),
                                 ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1, c) = \\mathrm{E}(Y|Z, b_0, b_1, c) = c + \\left( d-c \\right) \\cdot \\frac{e^{a\\left(Z-b\\right) }}{1+e^{a\\left(Z-b\\right) }} $$'),
                                 h4("Table of parameters"),
                                 fluidRow(column(12, align = "center", tableOutput('nlr_4P_table'))),
                                 htmlOutput("nlr_4P_interpretation"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(ggplot2)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;NLR&nbsp;4P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d){c&nbsp;+&nbsp;(d&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>fit&nbsp;<-&nbsp;nls(data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;startNLR(data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"4PLcgdg\",&nbsp;parameterization&nbsp;=&nbsp;\"classic\")[[1]][1:4],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,&nbsp;0),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1,&nbsp;1))&nbsp;<br>#&nbsp;coefficients&nbsp;<br>coef(fit)&nbsp;<br><br>#&nbsp;empirical&nbsp;probabilities&nbsp;calculation<br>df&nbsp;<-&nbsp;data.frame(x&nbsp;=&nbsp;sort(unique(zscore)),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;y&nbsp;=&nbsp;tapply(data[,&nbsp;1],&nbsp;zscore,&nbsp;mean),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;as.numeric(table(zscore)))<br><br>#&nbsp;plot&nbsp;of&nbsp;estimated&nbsp;curve<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;x,&nbsp;y&nbsp;=&nbsp;y))&nbsp;+<br>&nbsp;&nbsp;geom_point(aes(size&nbsp;=&nbsp;size),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fill&nbsp;=&nbsp;\"darkblue\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape&nbsp;=&nbsp;21,&nbsp;alpha&nbsp;=&nbsp;0.5)&nbsp;+<br>&nbsp;&nbsp;stat_function(fun&nbsp;=&nbsp;fun,&nbsp;geom&nbsp;=&nbsp;\"line\",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;args&nbsp;=&nbsp;list(a&nbsp;=&nbsp;coef(fit)[1],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;b&nbsp;=&nbsp;coef(fit)[2],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;c&nbsp;=&nbsp;coef(fit)[3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;d&nbsp;=&nbsp;coef(fit)[4]),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;size&nbsp;=&nbsp;1,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"darkblue\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Standardized&nbsp;total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Probability&nbsp;of&nbsp;correct&nbsp;answer\")&nbsp;+<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+<br>&nbsp;&nbsp;ggtitle(\"Item&nbsp;1\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                                 br()
                                 ),
                        # * MODELS COMPARISON ####
                        tabPanel("Model comparison",
                                 h3("Logistic regression model selection"),
                                 p('Here you can compare classic 2PL logistic regression model to non-linear model
                                   item by item using some information criteria: '),
                                 tags$ul(
                                   tags$li(strong('AIC'), 'is the Akaike information criterion (Akaike, 1974), '),
                                   tags$li(strong('BIC'), 'is the Bayesian information criterion (Schwarz, 1978)')
                                 ),
                                 p('Another approach to nested models can be likelihood ratio chi-squared test.
                                   Significance level is set to 0.05. As tests are performed item by item, it is
                                   possible to use multiple comparison correction method. '),
                                 selectInput("correction_method_regrmodels", "Correction method",
                                             c("BH" = "BH",
                                               "Holm" = "holm",
                                               "Hochberg" = "hochberg",
                                               "Hommel" = "hommel",
                                               "BY" = "BY",
                                               "FDR" = "fdr",
                                               "none" = "none"),
                                             selected="none"),
                                 h4("Table of comparison statistics"),
                                 p('Rows ', strong('BEST'), 'indicate which model has the lowest value of criterion, or is the largest
                                   significant model by likelihood ratio test.'),
                                 DT::dataTableOutput('regr_comp_table'),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>Data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(Data,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br><br>#&nbsp;function&nbsp;for&nbsp;fitting&nbsp;models<br>fun&nbsp;<-&nbsp;function(x,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d){c&nbsp;+&nbsp;(d&nbsp;-&nbsp;c)&nbsp;*&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b))&nbsp;/&nbsp;(1&nbsp;+&nbsp;exp(a&nbsp;*&nbsp;(x&nbsp;-&nbsp;b)))}&nbsp;<br><br>#&nbsp;starting&nbsp;values&nbsp;for&nbsp;item&nbsp;1<br>start&nbsp;<-&nbsp;startNLR(Data,&nbsp;GMAT[,&nbsp;\"group\"],&nbsp;model&nbsp;=&nbsp;\"4PLcgdg\",&nbsp;parameterization&nbsp;=&nbsp;\"classic\")[[1]][,&nbsp;1:4]<br><br>#&nbsp;2PL&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit2PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c&nbsp;=&nbsp;0,&nbsp;d&nbsp;=&nbsp;1),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;start[1:2])&nbsp;<br>#&nbsp;NLR&nbsp;3P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit3PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d&nbsp;=&nbsp;1),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;start[1:3],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1))&nbsp;<br>#&nbsp;NLR&nbsp;4P&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit3PL&nbsp;<-&nbsp;nls(Data[,&nbsp;1]&nbsp;~&nbsp;fun(zscore,&nbsp;a,&nbsp;b,&nbsp;c,&nbsp;d),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;algorithm&nbsp;=&nbsp;\"port\",&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;start&nbsp;=&nbsp;start,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lower&nbsp;=&nbsp;c(-Inf,&nbsp;-Inf,&nbsp;0,&nbsp;0),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;upper&nbsp;=&nbsp;c(Inf,&nbsp;Inf,&nbsp;1,&nbsp;1))&nbsp;<br><br>#&nbsp;comparison&nbsp;<br>###&nbsp;AIC<br>AIC(fit2PL);&nbsp;AIC(fit3PL);&nbsp;AIC(fit4PL)&nbsp;<br>###&nbsp;BIC<br>BIC(fit2PL);&nbsp;BIC(fit3PL);&nbsp;BIC(fit4PL)&nbsp;<br>###&nbsp;LR&nbsp;test,&nbsp;using&nbsp;Benjamini-Hochberg&nbsp;correction<br>######&nbsp;2PL&nbsp;vs&nbsp;NLR&nbsp;3P<br>LRstat&nbsp;<-&nbsp;-2&nbsp;*&nbsp;(sapply(fit2PL,&nbsp;logLik)&nbsp;-&nbsp;sapply(fit3PL,&nbsp;logLik))&nbsp;<br>LRdf&nbsp;<-&nbsp;1&nbsp;<br>LRpval&nbsp;<-&nbsp;1&nbsp;-&nbsp;pchisq(LRstat,&nbsp;LRdf)&nbsp;<br>LRpval&nbsp;<-&nbsp;p.adjust(LRpval,&nbsp;method&nbsp;=&nbsp;\"BH\")&nbsp;<br>######&nbsp;NLR&nbsp;3P&nbsp;vs&nbsp;NLR&nbsp;4P<br>LRstat&nbsp;<-&nbsp;-2&nbsp;*&nbsp;(sapply(fit3PL,&nbsp;logLik)&nbsp;-&nbsp;sapply(fit4PL,&nbsp;logLik))&nbsp;<br>LRdf&nbsp;<-&nbsp;1&nbsp;<br>LRpval&nbsp;<-&nbsp;1&nbsp;-&nbsp;pchisq(LRstat,&nbsp;LRdf)&nbsp;<br>LRpval&nbsp;<-&nbsp;p.adjust(LRpval,&nbsp;method&nbsp;=&nbsp;\"BH\")"))),
                                 br()
                                 ),
                        "----",
                        "Polytomous models",
                        # * MULTINOMIAL ####
                        tabPanel("Multinomial",
                                 h3("Multinomial regression on standardized total scores"),
                                 p('Various regression models may be fitted to describe
                                   item properties in more detail.',
                                   strong('Multinomial regression'),'allows for simultaneous modelling of probability of choosing
                                   given distractors on standardized total score (Z-score).'),
                                 br(),
                                 h4("Plot with estimated curves of multinomial regression"),
                                 p('Points represent proportion of selected option with respect to standardized
                                   total score. Their size is determined by count of respondents who achieved given
                                   level of standardized total score and who selected given option.'),
                                 sliderInput("multiSlider", "Item",
                                             min = 1, value = 1, max = 10,
                                             step = 1, animate = TRUE),
                                 plotOutput('multi_plot'),
                                 downloadButton("DB_multi_plot", label = "Download figure"),
                                 h4("Equation"),
                                 uiOutput('multi_equation'),
                                 h4("Table of parameters"),
                                 fluidRow(column(12, align = "center", tableOutput('multi_table'))),
                                 strong("Interpretation:"),
                                 htmlOutput("multi_interpretation"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code(HTML("library(difNLR)&nbsp;<br>library(nnet)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;GMATtest,&nbsp;GMATkey)&nbsp;<br>zscore&nbsp;<-&nbsp;scale(apply(GMAT[,&nbsp;1:20]&nbsp;,&nbsp;1,&nbsp;sum))&nbsp;#&nbsp;standardized&nbsp;total&nbsp;score<br>data&nbsp;<-&nbsp;GMATtest[,&nbsp;1:20]&nbsp;<br>key&nbsp;<-GMATkey<br><br>#&nbsp;multinomial&nbsp;model&nbsp;for&nbsp;item&nbsp;1&nbsp;<br>fit&nbsp;<-&nbsp;multinom(relevel(data[,&nbsp;1],&nbsp;ref&nbsp;=&nbsp;paste(key[1]))&nbsp;~&nbsp;zscore)&nbsp;<br><br>#&nbsp;coefficients&nbsp;<br>coef(fit)"))),
                                 br()
                                 )
                        ),
             #%%%%%%%%%%%%%%%%%%%%%
             # IRT MODELS #########
             #%%%%%%%%%%%%%%%%%%%%%
             IRT,
             #%%%%%%%%%%%%%%%%%%%%%
             # DIF/FAIRNESS #######
             #%%%%%%%%%%%%%%%%%%%%%
             navbarMenu("DIF/Fairness",
                        # * SUMMARY ####
                        "Description",
                        tabPanel('About DIF',
                                 h3('Differential Item Functioning / Item Fairness'),
                                 p('Differential item functioning (DIF) occurs when people from different
                                   social groups (commonly gender or ethnicity) with the same underlying true
                                   ability have a different probability of answering the item correctly.
                                   If item functions differently for two groups, it is potentially unfair.
                                   In general, two type of DIF can be recognized: if the item has different
                                   difficulty for given two groups with the same discrimination, ',
                                   strong('uniform'), 'DIF is present (left figure). If the item has different
                                   discrimination and possibly also different difficulty for given two groups, ',
                                   strong('non-uniform'), 'DIF is present (right figure)'),
                                 br(),
                                 img(src = "fig_DIF_uniform.png",
                                     style = "float: left; width: 32%; margin-right: 2%; margin-left: 16%; margin-bottom: 0.5em;"),
                                 img(src = "fig_DIF_nonuniform.png",
                                     style = "float: left; width: 32%; margin-right: 16%; margin-left: 2%; margin-bottom: 0.5em;"),
                                 br(),
                                 br()
                        ),
                        "----",
                        "Used methods",
                        # * TOTAL SCORES ####
                        tabPanel("Total scores",
                                 h3("Total scores"),
                                 p('DIF is not about total scores! Two groups may have the same distribution of total scores, yet,
                                   some item may function differently for two groups. Also, one of the groups may have signifficantly
                                   lower total score, yet, it may happen that there is no DIF item!',
                                   a('(Martinkova et al., 2017). ',
                                     href = "https://www.lifescied.org/doi/10.1187/cbe.16-10-0307",
                                     target = "_blank")),
                                 h4("Summary of total scores for groups"),
                                 tableOutput('resultsgroup'),
                                 h4("Histograms of total scores for groups"),
                                 sliderInput("inSlider2group", "Cut-score", min = 1, value = 1, max = 10,
                                             step = 1, animate = TRUE),
                                 p('For selected cut-score, blue part of histogram shows respondents with total score
                                   above the cut-score, grey column shows respondents with Total Score equal
                                   to cut-score and red part of histogram shows respondents below the cut-score.'),
                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput('histbyscoregroup0'),plotOutput('histbyscoregroup1')),
                                 splitLayout(cellWidths = c("50%", "50%"), downloadButton("DP_histbyscoregroup0", label = "Download figure"),
                                             downloadButton("DP_histbyscoregroup1", label = "Download figure")),
                                 br(),
                                 h4("Selected R code"),
                                 div(code('library(difNLR)'),
                                     br(),
                                     code('data <- GMAT[, 1:20]'),
                                     br(),
                                     code('group <- GMAT[, "group"]'),
                                     br(),
                                     br(),
                                     code('# Summary table'),
                                     br(),
                                     code('sc_zero <- apply(data[group == 0, ], 1, sum); summary(sc_zero) # total scores of reference group'),
                                     br(),
                                     code('sc_one  <- apply(data[group == 1, ], 1, sum); summary(sc_one)  # total scores of focal group'),
                                     br(),
                                     code('# Histograms'),
                                     br(),
                                     code('hist(sc_zero, breaks = 0:20)'),
                                     br(),
                                     code('hist(sc_one, breaks = 0:20)')),
                                 br()
                                 ),
                        # * DELTA PLOTS ####
                        tabPanel("Delta plots",
                                 h3("Delta plot"),
                                 p('Delta plot (Angoff & Ford, 1973) compares the proportions of correct answers per
                                   item in the two groups. It displays non-linear transformation of these proportions using
                                   quantiles of standard normal distributions (so called delta scores) for each item for the two
                                   genders in a scatterplot called diagonal plot or delta plot (see Figure). Item is under
                                   suspicion of DIF if the delta point considerably departs from the diagonal. The detection
                                   threshold is either fixed to value 1.5 or based on bivariate normal approximation (Magis &
                                   Facon, 2012).'),

                                 radioButtons('type_threshold', 'Threshold',
                                              list("Fixed", "Normal")
                                 ),
                                 checkboxInput('puri_DP', 'Item purification', FALSE),
                                 conditionalPanel(
                                   condition = "input.puri_DP",
                                   selectInput("puri_DP_type", "Purification method",
                                               c("IPP1" = "IPP1",
                                                 "IPP2" = "IPP2",
                                                 "IPP3" = "IPP3"
                                               ),
                                               selected = "IPP1")),
                                 plotOutput('deltaplot'),
                                 downloadButton("DP_deltaplot", label = "Download figure"),
                                 br(),
                                 br(),
                                 verbatimTextOutput("dp_text_normal"),
                                 br(),
                                 h4("Selected R code"),
                                 div(code('library(deltaPlotR)'),
                                     br(),
                                     code('library(difNLR)'),
                                     br(),
                                     code('data(GMAT)'),
                                     br(),
                                     code('data <- GMAT[, 1:20]'),
                                     br(),
                                     code('group <- GMAT[, "group"]'),
                                     br(),
                                     br(),
                                     code('# Delta scores with fixed threshold'),
                                     br(),
                                     code('deltascores <- deltaPlot(data.frame(data, group), group = "group",
                                          focal.name = 1, thr = 1.5)'),
                                     br(),
                                     code('deltascores'),
                                     br(),
                                     code('# Delta plot'),
                                     br(),
                                     code('diagPlot(deltascores, thr.draw = T)'),
                                     br(),
                                     br(),
                                     code('# Delta scores with normal threshold'),
                                     br(),
                                     code('deltascores <- deltaPlot(data.frame(data, group), group = "group",
                                          focal.name = 1, thr = "norm", purify = F)'),
                                     br(),
                                     code('deltascores'),
                                     br(),
                                     code('# Delta plot'),
                                     br(),
                                     code('diagPlot(deltascores, thr.draw = T)')),
                                 br()
                                 ),
                        # * MANTEL-HAENSZEL ####
                        tabPanel("Mantel-Haenszel",
                                 tabsetPanel(
                                   # Summary
                                   tabPanel("Summary",
                                            h3("Mantel-Haenszel test"),
                                            p('Mantel-Haenszel test is DIF detection method based on contingency
                                              tables that are calculated for each level of total score (Mantel &
                                              Haenszel, 1959).'),
                                            h4('Summary table'),
                                            p('Here you can select ', strong('correction method'), 'for multiple comparison or ',
                                              strong('item purification.')),
                                            selectInput("correction_method_MZ_print", "Correction method",
                                                        c("BH" = "BH",
                                                          "Holm" = "holm",
                                                          "Hochberg" = "hochberg",
                                                          "Hommel" = "hommel",
                                                          "BY" = "BY",
                                                          "FDR" = "fdr",
                                                          "none" = "none"
                                                        ),
                                                        selected = "none"),
                                            checkboxInput('puri_MH', 'Item purification', FALSE),
                                            verbatimTextOutput("print_DIF_MH"),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),
                                                code('# Mantel-Haenszel test'),
                                                br(),
                                                code('fit <- difMH(Data = data, group = group, focal.name = 1,
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit')),
                                            br()
                                            ),
                                   tabPanel('Items',
                                            h3("Mantel-Haenszel test"),
                                            p('Mantel-Haenszel test is DIF detection method based on contingency
                                              tables that are calculated for each level of total score (Mantel &
                                              Haenszel, 1959).'),
                                            h4('Contingency tables and odds ratio calculation'),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          sliderInput("difMHSlider_item",
                                                                      "Item",
                                                                      animate = TRUE,
                                                                      min = 1,
                                                                      max = 10,
                                                                      value = 1,
                                                                      step = 1)),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          sliderInput("difMHSlider_score",
                                                                      "Cut-score",
                                                                      min = 0,
                                                                      max = 10,
                                                                      value = 1,
                                                                      step = 1))),
                                            fluidRow(column(12, align = "center", tableOutput('table_DIF_MH'))),
                                            uiOutput('ORcalculation'),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),
                                                code('# Contingency table for item 1 and score 12'),
                                                br(),
                                                code('df <- data.frame(data[, 1], group)'),
                                                br(),
                                                code('colnames(df) <- c("Answer", "Group")'),
                                                br(),
                                                code('df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")), "Correct")'),
                                                br(),
                                                code('df$Group <- factor(df$Group, labels = c("Reference Group", "Focal Group"))'),
                                                br(),
                                                code('score <- apply(data, 1, sum)'),
                                                br(),

                                                code('df <- df[score == 12, ]'),
                                                br(),

                                                code('tab <- dcast(data.frame(xtabs(~ Group + Answer, data = df)),
                                                     Group ~ Answer,
                                                     value.var = "Freq",
                                                     margins = T,
                                                     fun = sum)'),
                                                br(),
                                                code('tab'),
                                                br(),

                                                code('# Mantel-Haenszel estimate of OR'),
                                                br(),
                                                code('fit <- difMH(Data = data, group = group, focal.name = 1,
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit$alphaMH')),
                                            br()
                                            )
                                 )
                        ),
                        # * LOGISTIC ####
                        tabPanel("Logistic regression",
                                 tabsetPanel(
                                   # ** Summary ####
                                   tabPanel('Summary',
                                            h3('Logistic regression on total scores'),
                                            p('Logistic regression allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by adding a group
                                              specific intercept', strong('b2'), '(uniform DIF) and group specific interaction', strong('b3'), '(non-uniform DIF) into model and
                                              by testing for their significance.'),
                                            h4("Equation"),
                                            ('$$\\mathrm{P}\\left(Y_{ij} = 1 | X_i, G_i, b_0, b_1, b_2, b_3\\right) = \\frac{e^{b_0 + b_1 X_i + b_2 G_i + b_3 X_i G_i}}{1+e^{b_0 + b_1 X_i + b_2 G_i + b_3 X_i G_i}} $$'),
                                            h4("Summary table"),
                                            p('Here you can choose what', strong('type'), 'of DIF to test. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 27%; ",
                                                          radioButtons(inputId = 'type_print_DIF_logistic',
                                                                       label = 'Type',
                                                                       choices = c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'),
                                                                       selected = 'both')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_logSummary",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput(inputId = 'puri_LR',
                                                                        label = 'Item purification',
                                                                        value = FALSE))),
                                            verbatimTextOutput('print_DIF_logistic'),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),

                                                code('# Logistic regression DIF detection method'),
                                                br(),
                                                code('fit <- difLogistic(Data = data, group = group, focal.name = 1,
                                                     type = "both",
                                                     p.adjust.method = "none",
                                                     purify = F)'),
                                                br(),
                                                code('fit')),
                                            br()
                                            ),
                                   # ** Items ####
                                   tabPanel('Items',
                                            h3('Logistic regression on total scores'),
                                            p('Logistic regression allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by adding a group
                                              specific intercept', strong('b2'), '(uniform DIF) and group specific interaction', strong('b3'), '(non-uniform DIF) into model and
                                              by testing for their significance.'),
                                            h4("Plot with estimated DIF logistic curve"),
                                            p('Here you can choose what', strong('type'), 'of DIF to test. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 27%; ",
                                                          radioButtons(inputId = 'type_plot_DIF_logistic',
                                                                       label = 'Type',
                                                                       choices = c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'),
                                                                       selected = 'both')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_logItems",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput(inputId = 'puri_LR_plot',
                                                                        label = 'Item purification',
                                                                        value = FALSE)),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          sliderInput("diflogSlider", "Item",
                                                                      min = 1,
                                                                      value = 1,
                                                                      max = 10,
                                                                      step = 1,
                                                                      animate = TRUE))),
                                            p('Points represent proportion of correct answer with respect to total score.
                                              Their size is determined by count of respondents who achieved given level of
                                              total score with respect to the group membership.'),
                                            p('NOTE: Plots and tables are based on DIF logistic procedure without any correction method. '),
                                            plotOutput('plot_DIF_logistic'),
                                            downloadButton("DP_plot_DIF_logistic", label = "Download figure"),
                                            h4("Equation"),
                                            ('$$\\mathrm{P}\\left(Y_{ij} = 1 | X_i, G_i, b_0, b_1, b_2, b_3\\right) = \\frac{e^{b_0 + b_1 X_i + b_2 G_i + b_3 X_i G_i}}{1+e^{b_0 + b_1 X_i + b_2 G_i + b_3 X_i G_i}} $$'),
                                            h4("Table of parameters"),
                                            fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_logistic'))),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),

                                                code('# Logistic regression DIF detection method'),
                                                br(),
                                                code('fit <- difLogistic(Data = data, group = group, focal.name = 1,
                                                     type = "both",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit'),
                                                br(),

                                                code('# Plot of characteristic curve for item 1'),
                                                br(),
                                                code('plotDIFLogistic(data, group,
                                                     type = "both",
                                                     item =  1,
                                                     IRT = F,
                                                     p.adjust.method = "none",
                                                     purify = F)'),
                                                br(),
                                                code('# Coefficients'),
                                                br(),
                                                code('fit$logitPar')),
                                            br()
                                            )
                                            )
                                   ),

                        # # * LOGISTIC Z ####
                        # tabPanel("Logistic IRT Z",
                        #          tabsetPanel(
                        #            # ** Summary ####
                        #            tabPanel('Summary',
                        #                     h3('Logistic regression on standardized total scores with IRT parameterization'),
                        #                     p('Logistic regression allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by adding a group
                        #                       specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into model and
                        #                       by testing for their significance.'),
                        #                     h4("Equation"),
                        #                     ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) = \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)
                        #                      \\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}{1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)
                        #                      \\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                        #                     h4('Summary table'),
                        #                     p('Here you can choose what', strong('type'), 'of DIF to test. You can also select ',
                        #                       strong('correction method'), 'for multiple comparison.'),
                        #                     fluidPage(div(style = "display: inline-block; vertical-align: top; width: 27%; ",
                        #                                   radioButtons(inputId = 'type_print_DIF_logistic_IRT_Z',
                        #                                                label = 'Type',
                        #                                                choices = c("H0: Any DIF vs. H1: No DIF" = 'both',
                        #                                                            "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                        #                                                            "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'),
                        #                                                selected = 'both')),
                        #                               div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                        #                               div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                        #                                   selectInput(inputId = "correction_method_logZSummary",
                        #                                               label = "Correction method",
                        #                                               choices = c("BH" = "BH",
                        #                                                           "Holm" = "holm",
                        #                                                           "Hochberg" = "hochberg",
                        #                                                           "Hommel" = "hommel",
                        #                                                           "BY" = "BY",
                        #                                                           "FDR" = "fdr",
                        #                                                           "none" = "none"),
                        #                                               selected = "none"))),
                        #                     verbatimTextOutput('print_DIF_logistic_IRT_Z'),
                        #                     br(),
                        #                     h4("Selected R code"),
                        #                     div(code('library(difNLR)'),
                        #                         br(),
                        #                         code('library(difR)'),
                        #                         br(),
                        #                         code('data(GMAT)'),
                        #                         br(),
                        #                         code('data <- GMAT[, 1:20]'),
                        #                         br(),
                        #                         code('group <- GMAT[, "group"]'),
                        #                         br(),
                        #                         code('scaled.score <- scale(score)'),
                        #                         br(),
                        #                         br(),
                        #                         code('# Logistic regression DIF detection method'),
                        #                         br(),
                        #                         code('fit <- difLogistic(Data = data, group = group, focal.name = 1,
                        #                              type = "both",
                        #                              match = scaled.score,
                        #                              p.adjust.method = "none",
                        #                              purify = F)'),
                        #                         br(),
                        #                         code('fit')),
                        #                     br()
                        #                     ),
                        #            # ** Items ####
                        #            tabPanel('Items',
                        #                     h3('Logistic regression on standardized total scores with IRT parameterization'),
                        #                     p('Logistic regression allows for detection of uniform and non-uniform DIF by adding a group
                        #                       specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into model and
                        #                       by testing for their significance.'),
                        #                     h4("Plot with estimated DIF logistic curve"),
                        #                     p('Here you can choose what', strong('type'), 'of DIF to test. You can also select ',
                        #                       strong('correction method'), 'for multiple comparison.'),
                        #                     fluidPage(div(style = "display: inline-block; vertical-align: top; width: 27%; ",
                        #                                   radioButtons(inputId = 'type_plot_DIF_logistic_IRT_Z',
                        #                                                label = 'Type',
                        #                                                choices = c("H0: Any DIF vs. H1: No DIF" = 'both',
                        #                                                            "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                        #                                                            "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'),
                        #                                                selected = 'both')),
                        #                               div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                        #                               div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                        #                                   selectInput(inputId = "correction_method_logZItems",
                        #                                               label = "Correction method",
                        #                                               choices = c("BH" = "BH",
                        #                                                           "Holm" = "holm",
                        #                                                           "Hochberg" = "hochberg",
                        #                                                           "Hommel" = "hommel",
                        #                                                           "BY" = "BY",
                        #                                                           "FDR" = "fdr",
                        #                                                           "none" = "none"),
                        #                                               selected = "none")),
                        #                               div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                        #                               div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                        #                                   sliderInput("diflog_irtSlider", "Item",
                        #                                               min = 1,
                        #                                               value = 1,
                        #                                               max = 10,
                        #                                               step = 1,
                        #                                               animate = TRUE))),
                        #                     p('Points represent proportion of correct answer with respect to standardized
                        #                       total score. Their size is determined by count of respondents who achieved
                        #                       given level of standardized total score with respect to the group membership.'),
                        #                     p('NOTE: Plots and tables are based on DIF logistic procedure without any correction method. '),
                        #                     plotOutput('plot_DIF_logistic_IRT_Z'),
                        #                     downloadButton("DP_plot_DIF_logistic_IRT_Z", label = "Download figure"),
                        #                     h4("Equation"),
                        #                     ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                        #                      \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                        #                      {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                        #                     h4("Table of parameters"),
                        #                     fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_logistic_IRT_Z'))),
                        #                     br(),
                        #                     h4("Selected R code"),
                        #                     div(code('library(difNLR)'),
                        #                         br(),
                        #                         code('library(difR)'),
                        #                         br(),
                        #                         code('data(GMAT)'),
                        #                         br(),
                        #                         code('data <- GMAT[, 1:20]'),
                        #                         br(),
                        #                         code('group <- GMAT[, "group"]'),
                        #                         br(),
                        #                         code('scaled.score <- scale(score)'),
                        #                         br(),
                        #                         br(),
                        #                         code('# Logistic regression DIF detection method'),
                        #                         br(),
                        #                         code('fit <- difLogistic(Data = data, group = group, focal.name = 1,
                        #                              type = "both",
                        #                              match = scaled.score,
                        #                              p.adjust.method = "none",
                        #                              purify = F)'),
                        #                         br(),
                        #                         code('fit'),
                        #                         br(),
                        #
                        #                         code('# Plot of characteristic curve for item 1'),
                        #                         br(),
                        #                         code('plotDIFLogistic(data, group,
                        #                              type = "both",
                        #                              item =  1,
                        #                              IRT = T,
                        #                              p.adjust.method = "BH")'),
                        #                         br(),
                        #                         code('# Coefficients for item 1 - recalculation'),
                        #                         br(),
                        #                         code('coef_old <- fit$logitPar[1, ]'),
                        #                         br(),
                        #                         code('coef <- c()'),
                        #                         br(),
                        #                         code('# a = b1, b = -b0/b1, adif = b3, bdif = -(b1b2-b0b3)/(b1(b1+b3))'),
                        #                         br(),
                        #                         code('coef[1] <- coef_old[2]'),
                        #                         br(),
                        #                         code('coef[2] <- -(coef_old[1] / coef_old[2])'),
                        #                         br(),
                        #                         code('coef[3] <- coef_old[4]'),
                        #                         br(),
                        #                         code('coef[4] <- -(coef_old[2] * coef_old[3] + coef_old[1] * coef_old[4] ) /
                        #                              (coef_old[2] * (coef_old[2] + coef_old[4]))')),
                        #                     br()
                        #                         )
                        #                     )
                        #            ),
                        # * NONLINEAR Z ####
                        tabPanel("Generalized logistic",
                                 tabsetPanel(
                                   # ** Summary ####
                                   tabPanel('Summary',
                                            h3('Generalized logistic regression'),
                                            p('Generalized logistic regression models can be seen as proxies of IRT models for
                                              DIF detection using standardized total score as estimate of knowledge.
                                              They can allow for nonzero lower asymptote - pseudoguessing \\(c\\)',
                                              a('(Drabinova & Martinkova, 2017) ',
                                                href = "http://onlinelibrary.wiley.com/doi/10.1111/jedm.12158/full",
                                                target = "_blank"),
                                              'or upper asymptote lower than one - inattention \\(d\\). Similarly to logistic
                                              regression, also its extensions provide detection of uniform and non-uniform DIF by
                                              letting difficulty parameter \\(b\\) (uniform) and discrimination parameter \\(a\\)
                                              (non-uniform) differ for groups and by testing for significance difference in their
                                              values. Moreover, these extensions allow for testing differences in pseudoguessing and
                                              inattention parameters. '),
                                            p('With ', strong('model'), 'you can specify what parameters should be kept the same for
                                              both groups and what parameters should differ. The notation is similar to IRT models.
                                              In 3PL and 4PL models abbreviations cg or dg mean that parameters c or d are the same for
                                              both groups. With ', strong('type'), 'you can choose parameters in which difference between
                                              groups should be tested.'),
                                            h4("Equation"),
                                            p("Displayed equation is based on selected model"),
                                            uiOutput("DIF_NLR_equation_print"),
                                            h4("Summary table"),
                                            p('Here you can choose what', strong('model'), "to use and what", strong('type'), 'of DIF to test.
                                              You can also select ', strong('correction method'), 'for multiple comparison or ',
                                              strong('item purification. ')),
                                            fluidRow(column(3,
                                                            selectInput(inputId = "DIF_NLR_model_print",
                                                                        label = "Model",
                                                                        choices = c("Rasch" = "Rasch",
                                                                                    "1PL" = "1PL",
                                                                                    "2PL" = "2PL",
                                                                                    "3PLcg" = "3PLcg",
                                                                                    "3PLdg" = "3PLdg",
                                                                                    "3PLc" = "3PLc",
                                                                                    "3PLd" = "3PLd",
                                                                                    "4PLcgdg" = "4PLcgdg",
                                                                                    "4PLcgd" = "4PLcgd",
                                                                                    "4PLcdg" = "4PLcdg",
                                                                                    "4PL" = "4PL"),
                                                                        selected = "3PLcg")),
                                                     column(1,
                                                            checkboxGroupInput(inputId = 'DIF_NLR_type_print',
                                                                               label = 'Type',
                                                                               choices = c("a" = "a",
                                                                                           "b" = "b",
                                                                                           "c" = "c",
                                                                                           "d" = "d"),
                                                                               selected = c("a", "b"))),
                                                     column(3,
                                                            selectInput(inputId = "DIF_NLR_correction_method_print",
                                                                        label = "Correction method",
                                                                        choices = c("BH" = "BH",
                                                                                    "Holm" = "holm",
                                                                                    "Hochberg" = "hochberg",
                                                                                    "Hommel" = "hommel",
                                                                                    "BY" = "BY",
                                                                                    "FDR" = "fdr",
                                                                                    "none" = "none"),
                                                                        selected = "none"),
                                                            checkboxInput(inputId = 'DIF_NLR_purification_print',
                                                                          label = 'Item purification',
                                                                          value = FALSE))
                                            ),
                                            verbatimTextOutput('print_DIF_NLR'),
                                            br(),
                                            h4("Selected R code"),
                                            div(code(HTML("library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data&nbsp;<br>data(GMAT)&nbsp;<br>Data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>group&nbsp;<-&nbsp;GMAT[,&nbsp;\"group\"]&nbsp;<br><br>#&nbsp;generalized&nbsp;logistic&nbsp;regression&nbsp;DIF&nbsp;method&nbsp;<br>#&nbsp;using&nbsp;3PL&nbsp;model&nbsp;with&nbsp;the&nbsp;same&nbsp;guessing&nbsp;parameter&nbsp;for&nbsp;both&nbsp;groups&nbsp;<br>fit&nbsp;<-&nbsp;difNLR(Data&nbsp;=&nbsp;Data,&nbsp;group&nbsp;=&nbsp;group,&nbsp;focal.name&nbsp;=&nbsp;1,&nbsp;model&nbsp;=&nbsp;\"3PLcg\",&nbsp;type&nbsp;=&nbsp;\"both\",&nbsp;p.adjust.method&nbsp;=&nbsp;\"none\")&nbsp;<br>fit"))),
                                            br()
                                            ),
                                   # ** Items ####
                                   tabPanel('Items',
                                            h3('Generalized logistic regression'),
                                            p('Generalized logistic regression models can be seen as proxies of IRT models for
                                              DIF detection using standardized total score as estimate of knowledge.
                                              They can allow for nonzero lower asymptote - pseudoguessing \\(c\\)',
                                              a('(Drabinova & Martinkova, 2017) ',
                                                href = "http://onlinelibrary.wiley.com/doi/10.1111/jedm.12158/full",
                                                target = "_blank"),
                                              'or upper asymptote lower than one - inattention \\(d\\). Similarly to logistic
                                              regression, also its extensions provide detection of uniform and non-uniform DIF by
                                              letting difficulty parameter \\(b\\) (uniform) and discrimination parameter \\(a\\)
                                              (non-uniform) differ for groups and by testing for significance difference in their
                                              values. Moreover, these extensions allow for testing differences in pseudoguessing and
                                              inattention parameters. '),
                                            p('With ', strong('model'), 'you can specify what parameters should be kept the same for
                                              both groups and what parameters should differ. The notation is similar to IRT models.
                                              In 3PL and 4PL models abbreviations cg or dg mean that parameters c or d are the same for
                                              both groups. With ', strong('type'), 'you can choose parameters in which difference between
                                              groups should be tested.'),
                                            h4("Plot with estimated DIF generalized logistic curve"),
                                            p('Here you can choose what', strong('model'), "to use and what", strong('type'), 'of DIF to test.
                                              You can also select ', strong('correction method'), 'for multiple comparison or ',
                                              strong('item purification. ')),
                                            fluidRow(column(3,
                                                            selectInput(inputId = "DIF_NLR_model_plot",
                                                                        label = "Model",
                                                                        choices = c("Rasch" = "Rasch",
                                                                                    "1PL" = "1PL",
                                                                                    "2PL" = "2PL",
                                                                                    "3PLcg" = "3PLcg",
                                                                                    "3PLdg" = "3PLdg",
                                                                                    "3PLc" = "3PLc",
                                                                                    "3PLd" = "3PLd",
                                                                                    "4PLcgdg" = "4PLcgdg",
                                                                                    "4PLcgd" = "4PLcgd",
                                                                                    "4PLcdg" = "4PLcdg",
                                                                                    "4PL" = "4PL"),
                                                                        selected = "3PLcg")),
                                                     column(1,
                                                            checkboxGroupInput(inputId = 'DIF_NLR_type_plot',
                                                                               label = 'Type',
                                                                               choices = c("a" = "a",
                                                                                           "b" = "b",
                                                                                           "c" = "c",
                                                                                           "d" = "d"),
                                                                               selected = c("a", "b"))),
                                                     column(3,
                                                            selectInput(inputId = "DIF_NLR_correction_method_plot",
                                                                        label = "Correction method",
                                                                        choices = c("BH" = "BH",
                                                                                    "Holm" = "holm",
                                                                                    "Hochberg" = "hochberg",
                                                                                    "Hommel" = "hommel",
                                                                                    "BY" = "BY",
                                                                                    "FDR" = "fdr",
                                                                                    "none" = "none"),
                                                                        selected = "none"),
                                                            checkboxInput(inputId = 'DIF_NLR_purification_plot',
                                                                          label = 'Item purification',
                                                                          value = FALSE)),
                                                     column(3,
                                                            sliderInput(inputId = "DIF_NLR_item_plot",
                                                                        label = "Item",
                                                                        min = 1,
                                                                        value = 1,
                                                                        max = 10,
                                                                        step = 1,
                                                                        animate = TRUE))),
                                            p('Points represent proportion of correct answer with respect to standardized
                                              total score. Their size is determined by count of respondents who achieved
                                              given level of standardized total score with respect to the group membership.'),
                                            plotOutput('plot_DIF_NLR'),
                                            downloadButton("DP_plot_DIF_NLR", label = "Download figure"),
                                            h4("Equation"),
                                            uiOutput("DIF_NLR_equation_plot"),
                                            h4("Table of parameters"),
                                            fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_NLR'))),
                                            br(),
                                            h4("Selected R code"),
                                            div(code(HTML("library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data&nbsp;<br>data(GMAT)&nbsp;<br>Data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br>group&nbsp;<-&nbsp;GMAT[,&nbsp;\"group\"]&nbsp;<br><br>#&nbsp;generalized&nbsp;logistic&nbsp;regression&nbsp;DIF&nbsp;method&nbsp;<br>#&nbsp;using&nbsp;3PL&nbsp;model&nbsp;with&nbsp;the&nbsp;same&nbsp;guessing&nbsp;parameter&nbsp;for&nbsp;both&nbsp;groups&nbsp;<br>fit&nbsp;<-&nbsp;difNLR(Data&nbsp;=&nbsp;Data,&nbsp;group&nbsp;=&nbsp;group,&nbsp;focal.name&nbsp;=&nbsp;1,&nbsp;model&nbsp;=&nbsp;\"3PLcg\",&nbsp;type&nbsp;=&nbsp;\"both\",&nbsp;p.adjust.method&nbsp;=&nbsp;\"none\")&nbsp;<br><br>#&nbsp;plot&nbsp;of&nbsp;characteristic&nbsp;curve&nbsp;of&nbsp;item&nbsp;1&nbsp;<br>plot(fit,&nbsp;item&nbsp;=&nbsp;1)&nbsp;<br><br>#&nbsp;table&nbsp;of&nbsp;estimated&nbsp;coefficients&nbsp;<br>fit$nlrPAR"))),
                                            br()
                                            )
                                            )
                                            ),
                        # * IRT LORD ####
                        tabPanel("IRT Lord",
                                 tabsetPanel(
                                   # ** Summary ####
                                   tabPanel('Summary',
                                            h3('Lord test for IRT models'),
                                            p('Lord test (Lord, 1980) is based on IRT model
                                              (1PL, 2PL, or 3PL with the same guessing). It uses the
                                              difference between item parameters for the two groups
                                              to detect DIF. In statistical terms, Lord statistic is
                                              equal to Wald statistic.'),
                                            br(),
                                            img(src = "fig_lord_uniform.png",
                                                style = "float: left; width: 32%; margin-right: 2%; margin-left: 16%; margin-bottom: 0.5em;"),
                                            img(src = "fig_lord_nonuniform.png",
                                                style = "float: left; width: 32%; margin-right: 16%; margin-left: 2%; margin-bottom: 0.5em;"),
                                            br(),
                                            h4('Summary table'),
                                            p('Here you can choose ', strong('model'), ' to test DIF. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 10%; ",
                                                          radioButtons(inputId = 'type_print_DIF_IRT_lord',
                                                                       label = 'Model',
                                                                       choices = c("1PL" = '1PL',
                                                                                   "2PL" = '2PL',
                                                                                   "3PL" = '3PL'),
                                                                       selected = '2PL')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_DIF_IRT_lordSummary",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput('puri_Lord', 'Item purification', FALSE))),
                                            verbatimTextOutput('print_DIF_IRT_Lord'),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),
                                                code('# 1PL IRT MODEL'),
                                                br(),
                                                code('fit1PL <- difLord(Data = data, group = group, focal.name = 1,
                                                     model = "1PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit1PL'),
                                                br(),
                                                br(),
                                                code('# 2PL IRT MODEL'),
                                                br(),
                                                code('fit2PL <- difLord(Data = data, group = group, focal.name = 1,
                                                     model = "2PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit2PL'),
                                                br(),
                                                br(),
                                                code('# 3PL IRT MODEL with the same guessing for groups'),
                                                br(),
                                                code('guess <- itemParEst(data, model = "3PL")[, 3]'),
                                                br(),
                                                code('fit3PL <- difLord(Data = data, group = group, focal.name = 1,
                                                     model = "3PL", c = guess,
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit3PL')),
                                            br()
                                            ),
                                   # ** Items ####
                                   tabPanel('Items',
                                            h3('Lord test for IRT models'),
                                            p('Lord test (Lord, 1980) is based on IRT model
                                              (1PL, 2PL, or 3PL with the same guessing). It uses the
                                              difference between item parameters for the two groups
                                              to detect DIF. In statistical terms, Lord statistic is
                                              equal to Wald statistic.'),
                                            br(),
                                            h4('Plot with estimated DIF characteristic curve'),
                                            p('Here you can choose ', strong('model'), ' to test DIF. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 10%; ",
                                                          radioButtons(inputId = 'type_plot_DIF_IRT_lord',
                                                                       label = 'Model',
                                                                       choices = c("1PL" = '1PL',
                                                                                   "2PL" = '2PL',
                                                                                   "3PL" = '3PL'),
                                                                       selected = '2PL')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_DIF_IRT_lordItems",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput('puri_Lord_plot', 'Item purification', FALSE)),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          sliderInput(inputId = "difirt_lord_itemSlider",
                                                                      label = "Item",
                                                                      min = 1,
                                                                      value = 1,
                                                                      max = 10,
                                                                      step = 1,
                                                                      animate = TRUE))),
                                            p('NOTE: Plots and tables are based on larger DIF IRT model. '),
                                            plotOutput('plot_DIF_IRT_Lord'),
                                            downloadButton("DP_plot_DIF_IRT_Lord", label = "Download figure"),
                                            h4("Equation"),
                                            uiOutput('irtint_lord'),
                                            uiOutput('irteq_lord'),
                                            h4("Table of parameters"),
                                            fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_IRT_Lord'))),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),
                                                code('# 1PL IRT MODEL'),
                                                br(),
                                                code('fit1PL <- difLord(Data = data, group = group, focal.name = 1,
                                                     model = "1PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit1PL'),
                                                br(),
                                                code('# Coefficients for all items'),
                                                br(),
                                                code('tab_coef1PL <- fit1PL$itemParInit'),
                                                br(),
                                                code('# Plot of characteristic curve of item 1'),
                                                br(),
                                                code('plotDIFirt(parameters = tab_coef1PL, item = 1, test = "Lord")'),
                                                br(),
                                                br(),
                                                code('# 2PL IRT MODEL'),
                                                br(),
                                                code('fit2PL <- difLord(Data = data, group = group, focal.name = 1,
                                                     model = "2PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit2PL'),
                                                br(),
                                                code('# Coefficients for all items'),
                                                br(),
                                                code('tab_coef2PL <- fit2PL$itemParInit'),
                                                br(),
                                                code('# Plot of characteristic curve of item 1'),
                                                br(),
                                                code('plotDIFirt(parameters = tab_coef2PL, item = 1, test = "Lord")'),
                                                br(),
                                                br(),
                                                code('# 3PL IRT MODEL with the same guessing for groups'),
                                                br(),
                                                code('guess <- itemParEst(data, model = "3PL")[, 3]'),
                                                br(),
                                                code('fit3PL <- difLord(Data = data, group = group, focal.name = 1,
                                                     model = "3PL", c = guess,
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit3PL'),
                                                br(),
                                                code('# Coefficients for all items'),
                                                br(),
                                                code('tab_coef3PL <- fit3PL$itemParInit'),
                                                br(),
                                                code('# Plot of characteristic curve of item 1'),
                                                br(),
                                                code('plotDIFirt(parameters = tab_coef3PL, item = 1, test = "Lord")')),
                                            br()
                                            )
                                   )
                                   ),
                        # * IRT RAJU ####
                        tabPanel("IRT Raju",
                                 tabsetPanel(
                                   # ** Summary ####
                                   tabPanel('Summary',
                                            h3('Raju test for IRT models'),
                                            p('Raju test (Raju, 1988, 1990) is based on IRT
                                              model (1PL, 2PL, or 3PL with the same guessing). It
                                              uses the area between the item charateristic curves
                                              for the two groups to detect DIF.'),
                                            br(),
                                            img(src = "fig_raju_uniform.png",
                                                style = "float: left; width: 32%; margin-right: 2%; margin-left: 16%; margin-bottom: 0.5em;"),
                                            img(src = "fig_raju_nonuniform.png",
                                                style = "float: left; width: 32%; margin-right: 16%; margin-left: 2%; margin-bottom: 0.5em;"),
                                            br(),
                                            h4('Summary table'),
                                            p('Here you can choose ', strong('model'), ' to test DIF. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 10%; ",
                                                          radioButtons(inputId = 'type_print_DIF_IRT_raju',
                                                                       label = 'Model',
                                                                       choices = c("1PL" = '1PL',
                                                                                   "2PL" = '2PL',
                                                                                   "3PL" = '3PL'),
                                                                       selected = '2PL')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_DIF_IRT_rajuSummary",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput(inputId = 'puri_Raju',
                                                                        label = 'Item purification',
                                                                        value = FALSE))),
                                            verbatimTextOutput('print_DIF_IRT_Raju'),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),
                                                code('# 1PL IRT MODEL'),
                                                br(),
                                                code('fit1PL <- difRaju(Data = data, group = group, focal.name = 1,
                                                     model = "1PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit1PL'),
                                                br(),
                                                br(),
                                                code('# 2PL IRT MODEL'),
                                                br(),
                                                code('fit2PL <- difRaju(Data = data, group = group, focal.name = 1,
                                                     model = "2PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit2PL'),
                                                br(),
                                                br(),
                                                code('# 3PL IRT MODEL with the same guessing for groups'),
                                                br(),
                                                code('guess <- itemParEst(data, model = "3PL")[, 3]'),
                                                br(),
                                                code('fit3PL <- difRaju(Data = data, group = group, focal.name = 1,
                                                     model = "3PL", c = guess,
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit3PL')),
                                            br()
                                            ),
                                   # ** Items ####
                                   tabPanel('Items',
                                            h3('Raju test for IRT models'),
                                            p('Raju test (Raju, 1988, 1990) is based on IRT
                                              model (1PL, 2PL, or 3PL with the same guessing). It
                                              uses the area between the item charateristic curves
                                              for the two groups to detect DIF.'),
                                            br(),
                                            h4('Plot with estimated DIF characteristic curve'),
                                            p('Here you can choose ', strong('model'), ' to test DIF. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 10%; ",
                                                          radioButtons(inputId = 'type_plot_DIF_IRT_raju',
                                                                       label = 'Model',
                                                                       choices = c("1PL" = '1PL',
                                                                                   "2PL" = '2PL',
                                                                                   "3PL" = '3PL'),
                                                                       selected = '2PL')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_DIF_IRT_rajuItems",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput(inputId = 'puri_Raju_plot',
                                                                        label = 'Item purification',
                                                                        value = FALSE)),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          sliderInput(inputId = "difirt_raju_itemSlider",
                                                                      label = "Item",
                                                                      min = 1,
                                                                      value = 1,
                                                                      max = 10,
                                                                      step = 1,
                                                                      animate = TRUE))),
                                            p('NOTE: Plots and tables are based on larger DIF IRT model. '),
                                            plotOutput('plot_DIF_IRT_Raju'),
                                            downloadButton("DP_plot_DIF_IRT_Raju", label = "Download figure"),
                                            h4("Equation"),
                                            uiOutput('irtint_raju'),
                                            uiOutput('irteq_raju'),
                                            h4("Table of parameters"),
                                            fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_IRT_Raju'))),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('library(difR)'),
                                                br(),
                                                code('data(GMAT)'),
                                                br(),
                                                code('data <- GMAT[, 1:20]'),
                                                br(),
                                                code('group <- GMAT[, "group"]'),
                                                br(),
                                                br(),
                                                code('# 1PL IRT MODEL'),
                                                br(),
                                                code('fit1PL <- difRaju(Data = data, group = group, focal.name = 1,
                                                     model = "1PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit1PL'),
                                                br(),
                                                code('# Coefficients for all items'),
                                                br(),
                                                code('tab_coef1PL <- fit1PL$itemParInit'),
                                                br(),
                                                code('# Plot of characteristic curve of item 1'),
                                                br(),
                                                code('plotDIFirt(parameters = tab_coef1PL, item = 1, test = "Raju")'),
                                                br(),
                                                br(),
                                                code('# 2PL IRT MODEL'),
                                                br(),
                                                code('fit2PL <- difRaju(Data = data, group = group, focal.name = 1,
                                                     model = "2PL",
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit2PL'),
                                                br(),
                                                code('# Coefficients for all items'),
                                                br(),
                                                code('tab_coef2PL <- fit2PL$itemParInit'),
                                                br(),
                                                code('# Plot of characteristic curve of item 1'),
                                                br(),
                                                code('plotDIFirt(parameters = tab_coef2PL, item = 1, test = "Raju")'),
                                                br(),
                                                br(),
                                                code('# 3PL IRT MODEL with the same guessing for groups'),
                                                br(),
                                                code('guess <- itemParEst(data, model = "3PL")[, 3]'),
                                                br(),
                                                code('fit3PL <- difRaju(Data = data, group = group, focal.name = 1,
                                                     model = "3PL", c = guess,
                                                     p.adjust.method = "none", purify = F)'),
                                                br(),
                                                code('fit3PL'),
                                                br(),
                                                code('# Coefficients for all items'),
                                                br(),
                                                code('tab_coef3PL <- fit3PL$itemParInit'),
                                                br(),
                                                code('# Plot of characteristic curve of item 1'),
                                                br(),
                                                code('plotDIFirt(parameters = tab_coef3PL, item = 1, test = "Raju")')),
                                            br())
                                   )
                                   ),
                        # * SIBTEST ####
                        tabPanel("SIBTEST",
                                 h3("SIBTEST"),
                                 p("The SIBTEST method (Shealy and Stout, 1993) allows for detection of uniform DIF without requiring
                                   an item response model approach. Its modified version, the Crossing-SIBTEST (Chalmers, 2018; Li and Stout, 1996),
                                   focuses on detection of non-uniform DIF."),
                                 h4("Summary table"),
                                 p("Here you can choose ", strong("type"), " of DIF to be tested. With uniform DIF, SIBTEST is applied,
                                   while with non-uniform DIF, the Crossing-SIBTEST method is used instead. You can also select ",
                                   strong("correction method"), "for multiple comparison or ", strong("item purification. ")),
                                 fluidRow(column(2,
                                                 radioButtons(inputId = "DIF_SIBTEST_type",
                                                              label = "Type",
                                                              choices = c("Uniform" = "udif",
                                                                          "Non-uniform" = "nudif"),
                                                              selected = "udif")),
                                          column(3,
                                                 selectInput(inputId = "DIF_SIBTEST_correction_method",
                                                             label = "Correction method",
                                                             choices = c("BH" = "BH",
                                                                         "Holm" = "holm",
                                                                         "Hochberg" = "hochberg",
                                                                         "Hommel" = "hommel",
                                                                         "BY" = "BY",
                                                                         "FDR" = "fdr",
                                                                         "none" = "none"),
                                                             selected = "none"),
                                                 checkboxInput(inputId = "DIF_SIBTEST_purification",
                                                               label = "Item purification",
                                                               value = FALSE))),
                                 verbatimTextOutput("DIF_SIBTEST_print"),
                                 br(),
                                 h4("Selected code"),
                                 div(code(HTML("library(difNLR)<br>library(difR)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>group&nbsp;<-&nbsp;GMAT[,&nbsp;\"group\"]<br><br>#&nbsp;SIBTEST&nbsp;(uniform&nbsp;DIF)<br>fit&nbsp;<-&nbsp;difMH(Data&nbsp;=&nbsp;data,&nbsp;group&nbsp;=&nbsp;group,&nbsp;focal.name&nbsp;=&nbsp;1,&nbsp;type&nbsp;=&nbsp;\"udif\",&nbsp;p.adjust.method&nbsp;=&nbsp;\"none\",&nbsp;purify&nbsp;=&nbsp;F)<br>fit<br><br>#&nbsp;Crossing-SIBTEST&nbsp;(non-uniform&nbsp;DIF)<br>fit&nbsp;<-&nbsp;difMH(Data&nbsp;=&nbsp;data,&nbsp;group&nbsp;=&nbsp;group,&nbsp;focal.name&nbsp;=&nbsp;1,&nbsp;type&nbsp;=&nbsp;\"nudif\",&nbsp;p.adjust.method&nbsp;=&nbsp;\"none\",&nbsp;purify&nbsp;=&nbsp;F)<br>fit"))),
                                 br()),
                        # * DDF ####
                        tabPanel("DDF",
                                 tabsetPanel(
                                   # ** Summary ####
                                   tabPanel('Summary',
                                            h3('Differential Distractor Functioning with multinomial log-linear regression model'),
                                            p('Differential Distractor Functioning (DDF) occurs when people from different
                                              groups but with the same knowledge have different probability of selecting
                                              at least one distractor choice. DDF is here examined by Multinomial Log-linear
                                              Regression model with Z-score and group membership as covariates. '),
                                            h4('Equation'),
                                            p('For ', strong('K'), ' possible test choices is the probability of the correct answer for
                                              person ', strong('i'), ' with standardized total score ', strong('Z'), ' and group
                                              membership ', strong('G'),' in item ', strong('j'), 'given by the following equation: '),
                                            ('$$\\mathrm{P}(Y_{ij} = K|Z_i, G_i, b_{jl0}, b_{jl1}, b_{jl2}, b_{jl3}, l = 1, \\dots, K-1) =
                                             \\frac{1}{1 + \\sum_l e^{\\left( b_{il0} + b_{il1} Z + b_{il2} G + b_{il3} Z:G\\right)}}$$'),
                                            p('The probability of choosing distractor ', strong('k'), ' is then given by: '),
                                            ('$$\\mathrm{P}(Y_{ij} = k|Z_i, G_i, b_{jl0}, b_{jl1}, b_{jl2}, b_{jl3}, l = 1, \\dots, K-1) =
                                             \\frac{e^{\\left( b_{jk0} + b_{jk1} Z_i + b_{jk2} G_i + b_{jk3} Z_i:G_i\\right)}}
                                             {1 + \\sum_l e^{\\left( b_{jl0} + b_{jl1} Z_i + b_{jl2} G_i + b_{jl3} Z_i:G_i\\right)}}$$'),
                                            br(),
                                            h4('Summary table'),
                                            p('Here you can choose what', strong('type'), 'of DIF to test. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 27%; ",
                                                          radioButtons(inputId = 'type_print_DDF',
                                                                       label = 'Type',
                                                                       choices = c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'),
                                                                       selected = 'both')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_print_DDF",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput(inputId = 'puri_DDF_print',
                                                                        label = 'Item purification',
                                                                        value = FALSE))),
                                            verbatimTextOutput('print_DDF'),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('data(GMATtest, GMATkey)'),
                                                br(),
                                                code('Data <- GMATtest[, 1:20]'),
                                                br(),
                                                code('group <- GMATtest[, "group"]'),
                                                br(),
                                                code('key <- GMATkey'),
                                                br(),
                                                br(),
                                                code('# DDF with difNLR package'),
                                                br(),
                                                code('fit <- ddfMLR(Data, group, focal.name = 1, key, type = "both",
                                                     p.adjust.method = "none")'),
                                                br(),
                                                code('fit')),
                                            br()
                                            ),
                                   # ** Items ####
                                   tabPanel('Items',
                                            h3('Differential Distractor Functioning with multinomial log-linear regression model'),
                                            p('Differential Distractor Functioning (DDF) occurs when people from different
                                              groups but with the same knowledge have different probability of selecting
                                              at least one distractor choice. DDF is here examined by Multinomial Log-linear
                                              Regression model with Z-score and group membership as covariates. '),
                                            h4("Plot with estimated DDF curves"),
                                            p('Here you can choose what', strong('type'), 'of DIF to test. You can also select ',
                                              strong('correction method'), 'for multiple comparison or ', strong('item purification. ')),
                                            fluidPage(div(style = "display: inline-block; vertical-align: top; width: 27%; ",
                                                          radioButtons(inputId = 'type_plot_DDF',
                                                                       label = 'Type',
                                                                       choices = c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'),
                                                                       selected = 'both')),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          selectInput(inputId = "correction_method_plot_DDF",
                                                                      label = "Correction method",
                                                                      choices = c("BH" = "BH",
                                                                                  "Holm" = "holm",
                                                                                  "Hochberg" = "hochberg",
                                                                                  "Hommel" = "hommel",
                                                                                  "BY" = "BY",
                                                                                  "FDR" = "fdr",
                                                                                  "none" = "none"),
                                                                      selected = "none"),
                                                          checkboxInput(inputId = 'puri_DDF_plot',
                                                                        label = 'Item purification',
                                                                        value = FALSE)),
                                                      div(style = "display: inline-block; vertical-align: top; width: 5%; "),
                                                      div(style = "display: inline-block; vertical-align: top; width: 20%; ",
                                                          sliderInput(inputId = "ddfSlider",
                                                                      label = "Item",
                                                                      min = 1,
                                                                      value = 1,
                                                                      max = 10,
                                                                      step = 1,
                                                                      animate = TRUE))),
                                            p('Points represent proportion of selected answer with respect to standardized
                                              total score. Their size is determined by count of respondents who achieved
                                              given level of standardized total score and who selected given option with
                                              respect to the group membership.'),
                                            plotOutput('plot_DDF'),
                                            downloadButton("DP_plot_DDF", label = "Download figure"),
                                            h4("Equation"),
                                            uiOutput('DDFeq'),
                                            h4("Table of parameters"),
                                            fluidRow(column(12, align = "center", tableOutput('tab_coef_DDF'))),
                                            br(),
                                            h4("Selected R code"),
                                            div(code('library(difNLR)'),
                                                br(),
                                                code('data(GMATtest, GMATkey)'),
                                                br(),
                                                code('Data <- GMATtest[, 1:20]'),
                                                br(),
                                                code('group <- GMATtest[, "group"]'),
                                                br(),
                                                code('key <- GMATkey'),
                                                br(),
                                                br(),
                                                code('# DDF with difNLR package'),
                                                br(),
                                                code('fit <- ddfMLR(Data, group, focal.name = 1, key, type = "both",
                                                     p.adjust.method = "none")'),
                                                br(),
                                                code('# Estimated coefficients of item 1'),
                                                br(),
                                                code('fit$mlrPAR[[1]]')),
                                            br()
                                            )
                                            )
                                   )
                                            ),
             #%%%%%%%%%%%%%%%%%%%%%
             # REPORTS ############
             #%%%%%%%%%%%%%%%%%%%%%
             tabPanel("Reports",
                      h3("Download report"),
                      h4("Settings of report"),
                      p(code("ShinyItemAnalysis"), " offers an option to download a report in HTML or PDF format. PDF report
                        creation requires latest version of", a("MiKTeX", href = "https://miktex.org/howto/install-miktex",
                                                                target = "_blank"),
                        "(or other TeX distribution). If you don't have the latest installation, please, use the HTML report."),
                      p("There is an option whether to use customize settings. By checking the", strong("Customize settings"),
                        "local settings will be offered and use for each selected section of report. Otherwise the settings
                        will be taken from pages of application. You can also include your name into report as well as the name
                        of dataset which was used. "),
                      fluidRow(
                        column(2, radioButtons("report_format", "Format of report", c("HTML" = "html", "PDF" = "pdf"))),
                        column(2, checkboxInput("customizeCheck", "Customize settings", FALSE)),
                        column(2, textInput("reportAuthor", "Author")),
                        column(2, textInput("reportDataName", "Dataset"))
                      ),
                      h4("Content of report"),
                      p("Reports by default contain summary of total scores, table of standard scores, item analysis,
                        distractors plots for each item and multinomial regression plots for each item. Other analyses
                        can be selected below. "),
                      fluidRow(
                        column(8,
                               p(strong("Validity")),
                               checkboxInput("corr_report", "Correlation structure", FALSE),
                               conditionalPanel(condition = "input.customizeCheck",
                                                conditionalPanel(condition = "input.corr_report",
                                                                 div(style = "display: inline-block; vertical-align: top; width: 20%;",
                                                                     numericInput('corr_plot_clust_report',
                                                                                  label = 'Number of clusters',
                                                                                  value = 1,
                                                                                  min = 1,
                                                                                  max = 1)),
                                                                 div(style = "display: inline-block; vertical-align: top; width: 20%;",
                                                                     selectInput('corr_plot_clustmethod_report',
                                                                                 label = 'Clustering method',
                                                                                 choices = list("None" = "none",
                                                                                                "Ward's"  = "ward.D",
                                                                                                "Ward's n. 2" = "ward.D2",
                                                                                                "Single" = "single",
                                                                                                "Complete" = "complete",
                                                                                                "Average" = "average",
                                                                                                "McQuitty" = "mcquitty",
                                                                                                "Median" = "median",
                                                                                                "Centroid" = "centroid"))),
                                                                 div(style = "display: inline-block; vertical-align: top; width: 20%;",
                                                                     selectInput('corr_plot_type_of_corr_report',
                                                                                 label = 'Choose correlation',
                                                                                 choices = c("Polychoric" = "polychoric",
                                                                                             "Pearson" = "pearson",
                                                                                             "Spearman"  = "spearman"),
                                                                                 selected = "Polychoric" )))),
                               checkboxInput("predict_report", "Predictive validity", FALSE)
                        )
                      ),
                      fluidRow(
                        conditionalPanel(condition = "input.customizeCheck",
                                         column(6,
                                                p(strong("Difficulty/discrimination plot")),
                                                selectInput("DDplotDiscriminationSelect_report", "Discrimination type:",
                                                            c("ULI" = "ULI",
                                                              "RIT" = "RIT",
                                                              "RIR" = "RIR",
                                                              "none" = "none"),
                                                            selected = "ULI"),
                                                conditionalPanel(condition = "input.DDplotDiscriminationSelect_report == 'ULI'",
                                                                 splitLayout(sliderInput('DDplotNumGroupsSlider_report','Number of groups:',
                                                                                         min   = 1,
                                                                                         max   = 5,
                                                                                         value = 3),
                                                                             sliderInput("DDplotRangeSlider_report", "Which two groups to compare:",
                                                                                         min = 1,
                                                                                         max = 3,
                                                                                         step = 1,
                                                                                         value = c(1, 3)))
                                                                 )
                                                            ))
                      ),
                      fluidRow(
                        conditionalPanel(condition = "input.customizeCheck",
                                         column(6,
                                                p(strong("Distractors plots")),
                                                splitLayout(radioButtons('type_combinations_distractor_report',
                                                                         'Type',
                                                                         list("Combinations", "Distractors")),
                                                            sliderInput('distractorGroupSlider','Number of groups:',
                                                                        min   = 1,
                                                                        max   = 5,
                                                                        value = 3))))
                      ),
                      fluidRow(
                        column(4,
                               radioButtons("irt_type_report", "IRT model selection",
                                            c("None" = "none",
                                              "Rasch" = "rasch",
                                              "1PL" = "1pl",
                                              "2PL" = "2pl",
                                              "3PL" = "3pl",
                                              "4PL" = "4pl"),
                                            selected = "1pl")
                        )
                      ),

                      fluidRow(
                        column(3,
                               p(strong("DIF method selection")),
                               checkboxInput("histCheck", "None - histograms by group only", FALSE),
                               checkboxInput("deltaplotCheck", "Delta plot", FALSE),
                               checkboxInput("logregCheck", "Logistic regression", FALSE),
                               checkboxInput("multiCheck", "Multinomial regression", FALSE)
                        ),
                        conditionalPanel(condition = "input.customizeCheck",
                                         conditionalPanel(condition = "input.deltaplotCheck",
                                                          column(2, p(strong("Delta plot settings")),
                                                                 radioButtons('type_threshold_report', 'Threshold',
                                                                              list("Fixed", "Normal")
                                                                 ),
                                                                 checkboxInput('puri_DP_report', 'Item purification', FALSE),
                                                                 conditionalPanel(
                                                                   condition = "input.puri_DP_report",
                                                                   selectInput("puri_DP_type_report", "Purification method",
                                                                               c("IPP1" = "IPP1",
                                                                                 "IPP2" = "IPP2",
                                                                                 "IPP3" = "IPP3"
                                                                               ),
                                                                               selected = "IPP1")
                                                                 )
                                                          )
                                         ),
                                         conditionalPanel(condition = "input.logregCheck",
                                                          column(2, p(strong("Logistic regression settings")),
                                                                 radioButtons('type_print_DIF_logistic_report', 'Type',
                                                                              c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                                                "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                                                "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                                                              ),
                                                                              'both'
                                                                 ),
                                                                 selectInput("correction_method_log_report", "Correction method",
                                                                             c("BH" = "BH",
                                                                               "Holm" = "holm",
                                                                               "Hochberg" = "hochberg",
                                                                               "Hommel" = "hommel",
                                                                               "BY" = "BY",
                                                                               "FDR" = "fdr",
                                                                               "none" = "none"
                                                                             ),
                                                                             selected = "none"),
                                                                 checkboxInput('puri_LR_report', 'Item purification', FALSE)
                                                          )
                                         ),
                                         conditionalPanel(condition = "input.multiCheck",
                                                          column(2, p(strong("Multinomial regression settings")),
                                                                 radioButtons('type_DDF_report', 'Type',
                                                                              c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                                                "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                                                "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                                                              ),
                                                                              'both'
                                                                 ),
                                                                 selectInput("correction_method_DDF_report", "Correction method",
                                                                             c("BH" = "BH",
                                                                               "Holm" = "holm",
                                                                               "Hochberg" = "hochberg",
                                                                               "Hommel" = "hommel",
                                                                               "BY" = "BY",
                                                                               "FDR" = "fdr",
                                                                               "none" = "none"),
                                                                             selected = "none"),
                                                                 checkboxInput('puri_DDF_report', 'Item purification', FALSE)
                                                          )
                                         )
                        )
                      ),
                      p(strong("Recommendation: "), "Report generation can be faster and more reliable when you first check
                        sections of intended contents. For example, if you wish to include a ", strong("3PL IRT"),
                        " model, you can first visit ", strong("IRT models"), "section and ", strong("3PL"), " subsection."),
                      #p(strong("Warning: "), "Download of reports takes some time. Please, be patient."),
                      fluidRow(
                        column(width = 5,
                               splitLayout(cellWidths = c("45%", "55%"),
                                           actionButton("generate", "Generate report"),
                                           uiOutput("download_report_button")
                               )
                        )
                      ),
                      br(),
                      br(),
                      br()
                      ),

             #%%%%%%%%%%%%%%%%%%%%%
             # REFERENCES #########
             #%%%%%%%%%%%%%%%%%%%%%
             tabPanel("References",
                      #------------------------------------------------------------------------------------#
                      # Packages ####
                      #------------------------------------------------------------------------------------#
                      h3("R packages"),
                      HTML('<ul class = "biblio">
                           <li><code>corrplot</code>
                           Wei, T. & Simko, V. (2017).
                           R package `corrplot`: Visualization of a Correlation Matrix.
                           R package version 0.84.
                           <a href = "https://github.com/taiyun/corrplot", target = "_blank">See online.</a>
                           </li>

                           <li><code>cowplot</code>
                           Claus O. Wilke (2018).
                           cowplot: Streamlined Plot Theme and Plot Annotations for "ggplot2".
                           R package version 0.9.3.
                           <a href = " https://CRAN.R-project.org/package=cowplot", target = "_blank">See online.</a>
                           </li>

                           <li><code>CTT</code>
                           Willse, J. & Willse, T. (2018).
                           CTT: Classical Test Theory Functions.
                           R package version 2.3.2.
                           <a href = "https://CRAN.R-project.org/package=CTT", target = "_blank">See online.</a>
                           </li>

                           <li><code>data.table</code>
                           Dowle, M. & Srinivasan, A. (2018).
                           data.table: Extension of `data.frame`.
                           R package version 1.11.4.
                           <a href = "https://CRAN.R-project.org/package=data.table", target = "_blank">See online.</a>
                           </li>


                           <li><code>deltaPlotR</code>
                           Magis, D. & Facon, B. (2014).
                           deltaPlotR: An R Package for Differential Item Functioning Analysis with Angoff`s Delta Plot.
                           <i>Journal of Statistical Software, Code Snippets, 59</i>(1), 1--19.
                           <a href = "http://www.jstatsoft.org/v59/c01/", target = "_blank">See online.</a>
                           </li>

                           <li><code>difNLR</code>
                           Drabinova, A., Martinkova, P. & Zvara, K. (2018).
                           difNLR: DIF and DDF Detection by Non-Linear Regression Models.
                           R package version 1.2.2.
                           <a href = "https://CRAN.R-project.org/package=difNLR", target = "_blank">See online.</a>
                           </li>


                           <li><code>difR</code>
                           Magis, D., Beland, S., Tuerlinckx, F. & De Boeck, P. (2010).
                           A general framework and an R package for the detection of dichotomous differential item functioning.
                           <i>Behavior Research Methods, 42</i>847--862.
                           </li>

                           <li><code>DT</code>
                           Xie, Y. (2018).
                           DT: A Wrapper of the JavaScript Library `DataTables`.
                           R package version 0.4.
                           <a href = "https://CRAN.R-project.org/package=DT", target = "_blank">See online.</a>
                           </li>

                           <li><code>ggdendro</code>
                           Andrie de Vries & Brian D. Ripley (2018).
                           ggdendro: Create Dendrograms and Tree Diagrams Using "ggplot2".
                           R package version 0.1-20.
                           <a href = "https://CRAN.R-project.org/package=ggdendro", target = "_blank">See online.</a>
                           </li>

                           <li><code>ggplot2</code>
                           Wickham, H. (2016).
                           ggplot2: Elegant Graphics for Data Analysis.
                           <a href = "http://ggplot2.org", target = "_blank">See online.</a>
                           </li>

                           <li><code>gridExtra</code>
                           Auguie, B. (2017).
                           gridExtra: Miscellaneous Functions for `Grid` Graphics.
                           R package version 2.3.
                           <a href = "https://CRAN.R-project.org/package=gridExtra", target = "_blank">See online.</a>
                           </li>

                           <li><code>knitr</code>
                           Xie, Y. (2018).
                           knitr: A General-Purpose Package for Dynamic Report Generation in R.
                           R package version 1.20.
                           <a href = "https://yihui.name/knitr/", target = "_blank">See online.</a>
                           </li>

                           <li><code>lattice</code>
                           Sarkar, D. (2008).
                           Lattice: Multivariate Data Visualization with R.
                           <a href = "http://lmdvr.r-forge.r-project.org", target = "_blank">See online.</a>
                           </li>

                           <li><code>latticeExtra</code>
                           Sarkar, D. & Andrews, F. (2016).
                           latticeExtra: Extra Graphical Utilities Based on Lattice.
                           R package version 0.6-28.
                           <a href = "https://CRAN.R-project.org/package=latticeExtra", target = "_blank">See online.</a>
                           </li>

                           <li><code>ltm</code>
                           Rizopoulos, D. (2006).
                           ltm: An R package for Latent Variable Modelling and Item Response Theory Analyses.
                           <i>Journal of Statistical Software, 17</i>(5), 1--25.
                           <a href = "http://www.jstatsoft.org/v17/i05/", target = "_blank">See online.</a>
                           </li>

                           <li><code>MASS</code>
                           Venables, C. & Ripley, C. (2002).
                           Modern Applied Statistics with S.
                           <a href = "http://www.stats.ox.ac.uk/pub/MASS4", target = "_blank">See online.</a>
                           </li>

                           <li><code>mirt</code>
                           Chalmers, R. & Chalmers, P. (2012).
                           mirt: A Multidimensional Item Response Theory Package for the R Environment.
                           <i>Journal of Statistical Software, 48</i>(6), 1--29.
                           </li>

                           <li><code>moments</code>
                           Komsta, L. & Novomestky, F. (2015).
                           moments: Moments, cumulants, skewness, kurtosis and related tests.
                           R package version 0.14.
                           <a href = "https://CRAN.R-project.org/package=moments", target = "_blank">See online.</a>
                           </li>

                           <li><code>msm</code>
                           Jackson, C. & Jackson, H. (2011).
                           Multi-State Models for Panel Data: The msm Package for R.
                           <i>Journal of Statistical Software, 38</i>(8), 1--29.
                           <a href = "http://www.jstatsoft.org/v38/i08/", target = "_blank">See online.</a>
                           </li>

                           <li><code>multilevel</code>
                           Bliese, P. (2016).
                           multilevel: Multilevel Functions.
                           R package version 2.6.
                           <a href = "https://CRAN.R-project.org/package=multilevel", target = "_blank">See online.</a>
                           </li>

                           <li><code>nlme</code>
                           Pinheiro, J., Bates, D., DebRoy, S., Sarkar, D. & NULL, R. (2018).
                           nlme: Linear and Nonlinear Mixed Effects Models.
                           R package version 3.1-137.
                           <a href = "https://CRAN.R-project.org/package=nlme", target = "_blank">See online.</a>
                           </li>

                           <li><code>nnet</code>
                           Venables, C. & Ripley, C. (2002).
                           Modern Applied Statistics with S.
                           <a href = "http://www.stats.ox.ac.uk/pub/MASS4", target = "_blank">See online.</a>
                           </li>

                           <li><code>plotly</code>
                           Sievert, C., Parmer, C., Hocking, T., Chamberlain, S., Ram, K., Corvellec, M. & Despouy, P. (2017).
                           plotly: Create Interactive Web Graphics via `plotly.js`.
                           R package version 4.7.1.
                           <a href = "https://CRAN.R-project.org/package=plotly", target = "_blank">See online.</a>
                           </li>

                           <li><code>polycor</code>
                           Fox, J. (2016).
                           polycor: Polychoric and Polyserial Correlations.
                           R package version 0.7-9.
                           <a href = "https://CRAN.R-project.org/package=polycor", target = "_blank">See online.</a>
                           </li>

                           <li><code>psych</code>
                           Revelle, W. (2018).
                           psych: Procedures for Psychological, Psychometric, and Personality Research.
                           R package version 1.8.4.
                           <a href = "https://CRAN.R-project.org/package=psych", target = "_blank">See online.</a>
                           </li>

                           <li><code>psychometric</code>
                           Fletcher, T. & Fletcher, D. (2010).
                           psychometric: Applied Psychometric Theory.
                           R package version 2.2.
                           <a href = "https://CRAN.R-project.org/package=psychometric", target = "_blank">See online.</a>
                           </li>

                           <li><code>RColorBrewer</code>
                           Neuwirth, E. (2014).
                           RColorBrewer: ColorBrewer Palettes.
                           R package version 1.1-2.
                           <a href = "https://CRAN.R-project.org/package=RColorBrewer", target = "_blank">See online.</a>
                           </li>

                           <li><code>reshape2</code>
                           Wickham, H. (2007).
                           Reshaping Data with the reshape Package.
                           <i>Journal of Statistical Software, 21</i>(12), 1--20.
                           <a href = "http://www.jstatsoft.org/v21/i12/", target = "_blank">See online.</a>
                           </li>

                           <li><code>rmarkdown</code>
                           Allaire, J., Xie, Y., McPherson, J., Luraschi, J., Ushey, K., Atkins, A., Wickham, H., Cheng, J. & Chang, W. (2018).
                           rmarkdown: Dynamic Documents for R.
                           R package version 1.10.
                           <a href = "https://CRAN.R-project.org/package=rmarkdown", target = "_blank">See online.</a>
                           </li>

                           <li><code>shiny</code>
                           Chang, W., Cheng, J., Allaire, J., Xie, Y. & McPherson, J. (2018).
                           shiny: Web Application Framework for R.
                           R package version 1.1.0.
                           <a href = "https://CRAN.R-project.org/package=shiny", target = "_blank">See online.</a>
                           </li>

                           <li><code>shinyBS</code>
                           Bailey, E. (2015).
                           shinyBS: Twitter Bootstrap Components for Shiny.
                           R package version 0.61.
                           <a href = "https://CRAN.R-project.org/package=shinyBS", target = "_blank">See online.</a>
                           </li>

                           <li><code>ShinyItemAnalysis</code>
                           Martinkova, P., Drabinova, A., Leder, O. & Houdek, J. (2018).
                           ShinyItemAnalysis: Test and item analysis via shiny.
                           R package version 1.2.8.
                           <a href = "https://CRAN.R-project.org/package=ShinyItemAnalysis", target = "_blank">See online.</a>
                           </li>

                           <li><code>shinyjs</code>
                           Attali, D. (2018).
                           shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds.
                           R package version 1.0.
                           <a href = "https://CRAN.R-project.org/package=shinyjs", target = "_blank">See online.</a>
                           </li>

                           <li><code>stringr</code>
                           Wickham, H. (2018).
                           stringr: Simple, Consistent Wrappers for Common String Operations.
                           R package version 1.3.1.
                           <a href = "https://CRAN.R-project.org/package=stringr", target = "_blank">See online.</a>
                           </li>

                           <li><code>xtable</code>
                           Dahl, D. & Dahl, B. (2016).
                           xtable: Export Tables to LaTeX or HTML.
                           R package version 1.8-2.
                           <a href = "https://CRAN.R-project.org/package=xtable", target = "_blank">See online.</a>
                           </li>
                           </ul>'),
                      #------------------------------------------------------------------------------------#
                      # References ####
                      #------------------------------------------------------------------------------------#
                      h3('References'),
                      HTML('<ul class = "biblio">
                           <li>Akaike, H. (1974). A New Look at the Statistical Model Identification.
                           <i>IEEE Transactions on Automatic Control, 19</i>(6), 716-723.
                           <a href = "http://ieeexplore.ieee.org/abstract/document/1100705/",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Ames, A. J., & Penfield, R. D. (2015). An NCME Instructional Module on Item-Fit
                           Statistics for Item Response Theory Models.
                           <i>Educational Measurement: Issues and Practice, 34</i>(3), 39-48.
                           <a href = "http://onlinelibrary.wiley.com/doi/10.1111/emip.12067/full",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Andrich, D. (1978). A Rating Formulation for Ordered Response Categories.
                           <i>Psychometrika, 43</i>(4), 561-573.
                           <a href = "https://link.springer.com/article/10.1007/BF02293814",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Angoff, W. H., & Ford, S. F. (1973). Item-Race Interaction on a Test of
                           Scholastic Aptitude.
                           <i>Journal of Educational Measurement, 10</i>(2), 95-105.
                           <a href = "https://www.jstor.org/stable/1433905?seq=1#page_scan_tab_contents",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Bock, R. D. (1972). Estimating Item Parameters and Latent Ability when
                           Responses Are Scored in Two or More Nominal Categories.
                           <i>Psychometrika, 37</i>(1), 29-51.
                           <a href = "http://link.springer.com/article/10.1007/BF02291411",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Brown, W. (1910).
                           Some experimental results in the correlation of mental abilities.
                           <i>British Journal of Psychology, 1904‐1920, 3</i>(3), 296-322.
                           <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1910.tb00207.x",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Chalmers, R. P. (2018). Improving the Crossing-SIBTEST Statistic for Detecting Non-uniform DIF.
                           <i>Psychometrika, 83</i>(2), 376–386.
                           <a href = "https://link.springer.com/article/10.1007/s11336-017-9583-8",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Cronbach, L. J. (1951). Coefficient Alpha and the Internal Structure of Tests.
                           <i>Psychometrika, 16</i>(3), 297-334.
                           <a href = "https://link.springer.com/article/10.1007/BF02310555",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Drabinova, A., & Martinkova, P. (2017). Detection of Differential Item Functioning
                           with Non-Linear Regression: Non-IRT Approach Accounting for Guessing.
                           <i>Journal of Educational Measurement, 54</i>(4), 498-517
                           <a href = "http://onlinelibrary.wiley.com/doi/10.1111/jedm.12158/full",
                           target = "_blank">See online.</a>
                           </li>

                           <li> Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987).
                           Statistical inference for coefficient alpha.
                           <i>Applied Psychological Measurement 11</i>(1), 93-103.
                           <a href = "http://journals.sagepub.com/doi/abs/10.1177/014662168701100107",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Li, H.-H., and Stout, W. (1996). A New Procedure for Detection of Crossing DIF.
                           <i>Psychometrika, 61</i>(4), 647–677.
                           <a href = "https://link.springer.com/article/10.1007/BF02294041",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Lord, F. M. (1980). Applications of Item Response Theory to Practical Testing Problems.
                           Routledge.
                           </li>

                           <li>Magis, D., & Facon, B. (2012). Angoffs Delta Method Revisited: Improving DIF Detection under
                           Small Samples.
                           <i>British Journal of Mathematical and Statistical Psychology, 65</i>(2), 302-321.
                           <a href = "https://www.ncbi.nlm.nih.gov/pubmed/22500570",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Mantel, N., & Haenszel, W. (1959). Statistical Aspects of the Analysis of Data from
                           Retrospective Studies.
                           <i>Journal of the National Cancer Institute, 22</i>(4), 719-748.
                           <a href = "http://www.medicine.mcgill.ca/epidemiology/hanley/c634/stratified/Mantel_Haenszel_1.pdf",
                           target = "_blank">See online.</a>                                                                                    )),
                           </li>

                           <li>Martinkova, P., Drabinova, A., & Houdek, J. (2017). ShinyItemAnalysis: Analyza Prijimacich a
                           Jinych Znalostnich ci Psychologických Testu. [ShinyItemAnalysis: Analyzing Admission and Other
                           Educational and Psychological Tests]
                           <i>TESTFORUM, 6</i>(9), 16–35.
                           <a href = "http://testforum.cz/domains/testforum.cz/index.php/testforum/article/view/TF2017-9-129",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M.
                           (2017). Checking Equity: Why Differential Item Functioning Analysis Should Be a Routine Part
                           of Developing Conceptual Assessments.
                           <i>CBE-Life Sciences Education, 16</i>(2), rm2.
                           <a href = "https://doi.org/10.1187/cbe.16-10-0307",
                           target = "_blank">See online</a>
                           </li>

                           <li>Martinkova, P., Stepanek, L., Drabinova, A., Houdek, J., Vejrazka, M., & Stuka, C. (2017).
                           Semi-real-time Analyses of Item Characteristics for Medical School Admission Tests.
                           In
                           <i>Proceedings of the 2017 Federated Conference on Computer Science and Information Systems</i>,
                           189-194.
                           <a href="http://dx.doi.org/10.15439/2017F380",
                           target="_blank">See online.</a>
                           </li>

                           <li>Masters, G. N. (1982). A Rasch model for partial credit scoring.
                           <i>Psychometrika, 47</i>(2), 149-174.
                           <a href = "https://link.springer.com/article/10.1007/BF02296272",
                           target = "_blank">See online.</a>
                           </li>

                           <li>McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinkova, P., Cliff, W., Michael, J., ... & Wright, A. (2017).
                           Development and Validation of the Homeostasis Concept Inventory.
                           <i>CBE-Life Sciences Education, 16</i>(2), ar35.
                           <a href = "https://www.lifescied.org/doi/abs/10.1187/cbe.16-10-0305",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Muraki, E. (1992). A Generalized Partial Credit Model: Application of an EM Algorithm.
                           <i>ETS Research Report Series, 1992</i>(1)
                           <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/j.2333-8504.1992.tb01436.x",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Swaminathan, H., & Rogers, H. J. (1990). Detecting Differential Item
                           Functioning Using Logistic Regression Procedures.
                           <i>Journal of Educational Measurement, 27</i>(4), 361-370.
                           <a href = "https://www.jstor.org/stable/1434855?seq=1#page_scan_tab_contents",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Raju, N. S. (1988). The Area between Two Item Characteristic Curves.
                           <i>Psychometrika, 53</i>(4), 495-502.
                           <a href = "https://link.springer.com/article/10.1007/BF02294403",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Raju, N. S. (1990). Determining the Significance of Estimated Signed and Unsigned Areas
                           between Two Item Response Functions.
                           <i>Applied Psychological Measurement, 14</i>(2), 197-207.
                           <a href = "http://journals.sagepub.com/doi/abs/10.1177/014662169001400208",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Rasch, G. (1960) Probabilistic Models for Some Intelligence and Attainment Tests.
                           Copenhagen: Paedagogiske Institute.
                           </li>

                           <li>Revelle, W. (1979).
                           Hierarchical cluster analysis and the internal structure of tests.
                           <i>Multivariate Behavioral Research, 14</i>(1), 57-74.
                           <a href = "https://doi.org/10.1207/s15327906mbr1401_4",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Samejima, F. (1969). Estimation of Latent Ability Using a Response Pattern of Graded Scores.
                           <i>Psychometrika, 34</i>(1), 1-97
                           <a href = "https://link.springer.com/article/10.1007%2FBF03372160",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Schwarz, G. (1978). Estimating the Dimension of a Model.
                           <i>The Annals of Statistics, 6</i>(2), 461-464.
                           <a href = "https://projecteuclid.org/euclid.aos/1176344136",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Shealy, R. and Stout, W. (1993). A Model-Based Standardization Approach that
                           Separates True Bias/DIF from Group Ability Differences and Detect Test Bias/DTF
                           as well as Item Bias/DIF.
                           <i>Psychometrika, 58</i>(2), 159-194.
                           <a href = "https://link.springer.com/article/10.1007/BF02294572",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Spearman, C. (1910).
                           Correlation calculated from faulty data.
                           <i>British Journal of Psychology, 1904‐1920, 3</i>(3), 271-295.
                           <a href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8295.1910.tb00206.x",
                           target = "_blank">See online.</a>
                           </li>

                           <li>Wilson, M. (2005). Constructing Measures: An Item Response Modeling Approach.
                           </li>

                           <li>Wright, B. D., & Stone, M. H. (1979). Best Test Design. Chicago: Mesa Press.
                           </li>
                           </ul>'),
                      br()
             ),
             #%%%%%%%%%%%%%%%%%%%%%
             # SETTING #########
             #%%%%%%%%%%%%%%%%%%%%%
             tabPanel("",
                      icon = icon("fas fa-cog"),
                      h4("IRT models setting"),
                      p("Set the number of cycles for IRT 1PL, 2PL, 3PL and 4PL models."),
                      numericInput(inputId = "ncycles",
                                   label = "Number of cycles",
                                   value = 2000,
                                   min = 1,
                                   max = 999999),
                      h4("Figure downloads"),
                      p("Here you can change setting for download of figures. "),
                      fluidPage(column(2, numericInput(inputId = "setting_figures_text_size",
                                                       label = "Text size [pts]",
                                                       value = 12,
                                                       min = 6,
                                                       max = 20)),
                                column(2, numericInput(inputId = "setting_figures_height",
                                                       label = "Height [in]",
                                                       value = 4,
                                                       min = 1,
                                                       max = 16)),
                                column(2, numericInput(inputId = "setting_figures_width",
                                                       label = "Width [in]",
                                                       value = 8,
                                                       min = 1,
                                                       max = 16)),
                                column(2, numericInput(inputId = "setting_figures_dpi",
                                                       label = "Plot resolution",
                                                       value = 600,
                                                       min = 72,
                                                       max = 600))
                      ))
             #     ))
                      ))

