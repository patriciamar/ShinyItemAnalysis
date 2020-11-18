uiSummary <-
  navbarMenu(
    "Summary",
    # * TOTAL SCORES ####
    tabPanel("Total scores",
      value = "tot_scores",
      h3("Analysis of total scores"),
      p("Total score, also known as raw score or sum score, is the easiest measure of latent trait being measured.
                      The total score is calculated as the sum of their item scores.
                      In binary items, the total score corresponds to the total number of correct answers."),
      h4("Summary table"),
      p(
        "Table below summarizes basic descriptive statistics for the total scores including number of respondents \\(n\\),
                        minimum and maximum,", htmlOutput("totalscores_tooltip_mean", inline = TRUE), "median,",
        htmlOutput("totalscores_tooltip_sd", inline = TRUE), "\\(SD \\), ",
        htmlOutput("totalscores_tooltip_skewness", inline = TRUE), "and",
        htmlOutput("totalscores_tooltip_kurtosis", inline = TRUE),
        "The skewness for normally distributed scores is near the value of 0 and the kurtosis is near the value of 3. "
      ),
      tableOutput("totalscores_table"),
      h4("Histogram of total score"),
      fluidPage(div(
        class = "input-slider",
        sliderInput(
          inputId = "slider_totalscores_histogram",
          label = "Cut-score",
          min = 0,
          max = 10,
          value = 1,
          step = 1
        )
      )),
      p("For selected cut-score, blue part of histogram shows respondents with total score
                        above the cut-score, grey column shows respondents with total score equal
                        to the cut-score and red part of histogram shows respondents below the cut-score."),
      plotlyOutput("totalscores_histogram"),
      downloadButton(outputId = "DB_totalscores_histogram", label = "Download figure"),
      br(),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(difNLR)<br>library(ggplot2)<br>library(psych)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br><br>#&nbsp;total&nbsp;score&nbsp;calculation<br>score&nbsp;<-&nbsp;rowSums(data)<br><br>#&nbsp;summary&nbsp;of&nbsp;total&nbsp;score&nbsp;<br>tab&nbsp;<-&nbsp;describe(score)[,&nbsp;c(\"n\",&nbsp;\"min\",&nbsp;\"max\",&nbsp;\"mean\",&nbsp;\"median\",&nbsp;\"sd\",&nbsp;\"skew\",&nbsp;\"kurtosis\")]<br>tab$kurtosis&nbsp;<-&nbsp;tab$kurtosis&nbsp;+&nbsp;3<br>tab<br><br>#&nbsp;colors&nbsp;by&nbsp;cut-score<br>cut&nbsp;<-&nbsp;median(score)&nbsp;#&nbsp;cut-score&nbsp;<br>color&nbsp;<-&nbsp;c(rep(\"red\",&nbsp;cut&nbsp;-&nbsp;min(score)),&nbsp;\"gray\",&nbsp;rep(\"blue\",&nbsp;max(score)&nbsp;-&nbsp;cut))<br>df&nbsp;<-&nbsp;data.frame(score)<br><br>#&nbsp;histogram<br>ggplot(df,&nbsp;aes(score))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_histogram(binwidth&nbsp;=&nbsp;1,&nbsp;fill&nbsp;=&nbsp;color,&nbsp;col&nbsp;=&nbsp;\"black\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Number&nbsp;of&nbsp;respondents\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_app()"))),
      br()
    ),
    # * STANDARD SCORES ####
    tabPanel("Standard scores",
      value = "stan_scores",
      h3("Standard scores"),
      withMathJax(p(
        strong("Total score"), "is calculated as the sum of item scores.", br(),
        strong("Percentile"), "indicates the value below which a percentage of observations
                        falls, e.g. a individual score at the 80th percentile means that the individual score
                        is the same or higher than the scores of 80% of all respondents. ", br(),
        strong("Success rate"), "is the percentage of scores obtained, e.g. if the maximum points of test
                        is equal to 20, minimum is 0, and individual score is 12 then success rate is \\(12 / 20 = 0.6\\), i.e. 60%.", br(),
        strong("Z-score"), "or also standardized score is a linear transformation of total
                        score, defined as \\(Z = \\frac{X - \\bar{X}}{s}\\), where \\(X\\) is total score,
                        \\(\\bar{X}\\) is the sample mean of total scores and \\(s\\) is the
                        standard deviation. The Z-score has a mean of 0 and and sample variance of 1. ", br(),
        strong("T-score"), "is transformed Z-score with a mean of 50 and standard deviation
                        of 10. The T-score is calculated as \\(T = (Z \\cdot 10) + 50\\). "
      )), br(),
      # Formulae for help:
      # (p(strong('Total score'), 'of person \\(p\\) is calculated as the sum of item scores \\(X_p = \\sum_{i=1}^{I}X_{pi}\\)'
      # \\(\\bar{X} = \\frac{1}{n}\\sum_{p=1}^{n}X_p\\) is the sample mean of total scores
      h4("Table by score"),
      tableOutput("standardscores_table"),
      br(),
      downloadButton(
        outputId = "DB_standardscores_table",
        label = "Download table"
      ),
      br(),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(difNLR)&nbsp;<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;scores&nbsp;calculations&nbsp;(unique&nbsp;values)<br>score&nbsp;<-&nbsp;rowSums(data)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Total&nbsp;score&nbsp;<br>tosc&nbsp;<-&nbsp;sort(unique(score))&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Levels&nbsp;of&nbsp;total&nbsp;score&nbsp;<br>perc&nbsp;<-&nbsp;ecdf(sc)(tosc)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Percentiles&nbsp;<br>sura&nbsp;<-&nbsp;100&nbsp;*&nbsp;(tosc&nbsp;/&nbsp;max(score))&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;Success&nbsp;rate&nbsp;<br>zsco&nbsp;<-&nbsp;sort(unique(scale(score)))&nbsp;&nbsp;&nbsp;#&nbsp;Z-score&nbsp;<br>tsco&nbsp;<-&nbsp;50&nbsp;+&nbsp;10&nbsp;*&nbsp;zsco&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#&nbsp;T-score"))),
      br()
    )
  )
