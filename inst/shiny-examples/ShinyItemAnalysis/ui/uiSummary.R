uiSummary <-
  navbarMenu("Summary",
             # * TOTAL SCORES ####
             tabPanel("Total scores", value = "tot_scores",
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
                      plotlyOutput('totalscores_histogram'),
                      downloadButton(outputId = "DB_totalscores_histogram", label = "Download figure"),
                      br(),
                      br(),
                      h4("Selected R code"),
                      div(code(HTML("library(difNLR)<br>library(ggplot2)<br>library(moments)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br><br>#&nbsp;total&nbsp;score&nbsp;calculation<br>score&nbsp;<-&nbsp;apply(data,&nbsp;1,&nbsp;sum)<br><br>#&nbsp;summary&nbsp;of&nbsp;total&nbsp;score&nbsp;<br>c(min(score),&nbsp;max(score),&nbsp;mean(score),&nbsp;median(score),&nbsp;sd(score),&nbsp;skewness(score),&nbsp;kurtosis(score))<br><br>#&nbsp;colors&nbsp;by&nbsp;cut-score<br>cut&nbsp;<-&nbsp;median(score)&nbsp;#&nbsp;cut-score&nbsp;<br>color&nbsp;<-&nbsp;c(rep(\"red\",&nbsp;cut&nbsp;-&nbsp;min(score)),&nbsp;\"gray\",&nbsp;rep(\"blue\",&nbsp;max(score)&nbsp;-&nbsp;cut))<br>df&nbsp;<-&nbsp;data.frame(score)<br><br>#&nbsp;histogram<br>ggplot(df,&nbsp;aes(score))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_histogram(binwidth&nbsp;=&nbsp;1,&nbsp;fill&nbsp;=&nbsp;color,&nbsp;col&nbsp;=&nbsp;\"black\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Number&nbsp;of&nbsp;respondents\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
                      br()
                      ),
             # * STANDARD SCORES ####
             tabPanel("Standard scores", value = "stan_scores",
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
                      )
