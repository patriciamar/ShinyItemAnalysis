uiReliability <-
  navbarMenu(
    "Reliability",
    "Description",
    # * RELIABILITY ####
    tabPanel("Reliability",
      value = "reliability_description",
      h3("Reliability"),
      p("We are typically interested in unobserved true score \\(T\\), but have available only the
                        observed score \\(X\\) which is contaminated by some measurement error \\(e\\), such that
                        \\(X = T + e\\) and error term is uncorrelated with the true score."),
      h4("Equation"),
      p("Reliability is defined as squared correlation of the true and observed score"),
      withMathJax(),
      ("$$\\text{rel}(X) = \\text{cor}(T, X)^2$$"),
      p("Equivalently, reliability can be re-expressed as the ratio of the true score variance
                        to total observed variance"),
      withMathJax(),
      ("$$\\text{rel}(X) = \\frac{\\sigma^2_T}{\\sigma^2_X}$$"),
      br()
    ),
    "----",
    "Used methods",
    # * SPEARMAN-BROWN FORMULA ####
    tabPanel(
      "Spearman-Brown formula",
      h3("Spearman-Brown formula"),
      h4("Equation"),
      p("For test with \\(I\\) items total score is calculated as \\(X = X_1 + ... + X_I\\).
                        Let \\(\\text{rel}(X)\\) be the reliability of the test. For a test consisting of
                        \\(I^*\\) items (equally precise, measuring the same construct),  that is for test which is
                        \\(m = \\frac{I^*}{I}\\) times longer/shorter, the reliability would be"),
      withMathJax(),
      ("$$\\text{rel}(X^*) = \\frac{m\\cdot \\text{rel}(X)}{1 + (m - 1)\\cdot\\text{rel}(X)}.$$"),
      p("Spearman-Brown formula can be used to determine reliability of a test with similar items but of
                        different number of items. It can also be used to determine necessary number of items to achieve
                        desired reliability."),
      p("In calculations below", strong("reliability of original data"), "is by
                        default set to value of Cronbach's \\(\\alpha\\) of the dataset currentli in use. ", strong("Number of items in original data"), "is
                        by default set to number of items of dataset currently in use. "),
      fluidRow(
        column(
          3,
          numericInput(
            inputId = "reliability_SBformula_reliability_original",
            label = "Reliability of original data",
            max = 0,
            min = 1,
            value = 0.7
          )
        ),
        column(
          3,
          numericInput(
            inputId = "reliability_SBformula_items_original",
            label = "Number of items in original data",
            min = 1,
            step = 1,
            value = 20
          )
        )
      ),
      # br(),
      h4("Estimate of reliability with different number of items"),
      p("Here you can calculate estimate of reliability of a test consisting of different number of
                        items (equally precise, measuring the same construct). "),
      fluidRow(column(
        3,
        numericInput(
          inputId = "reliability_SBformula_items_new",
          label = "Number of items in new data",
          min = 1,
          step = 1,
          value = 30
        )
      )),
      uiOutput("reliability_SBformula_reliability_text"),
      # br(),
      h4("Necessary number of items for required level of reliability"),
      p("Here you can calculate necessary number of items (equally precise, measuring the same construct)
                        to gain required level of reliability. "),
      fluidRow(column(
        3,
        numericInput(
          inputId = "reliability_SBformula_reliability_new",
          label = "Reliability of new data",
          max = 0,
          min = 1,
          value = 0.8
        )
      )),
      uiOutput("reliability_SBformula_items_text"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(psychometrics)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;reliability&nbsp;of&nbsp;original&nbsp;data<br>rel.original&nbsp;<-&nbsp;psychometric::alpha(data)<br>#&nbsp;number&nbsp;of&nbsp;items&nbsp;in&nbsp;original&nbsp;data<br>items.original&nbsp;<-&nbsp;ncol(data)<br><br><br>#&nbsp;number&nbsp;of&nbsp;items&nbsp;in&nbsp;new&nbsp;data<br>items.new&nbsp;<-&nbsp;30<br>#&nbsp;ratio&nbsp;of&nbsp;tests&nbsp;lengths<br>m&nbsp;<-&nbsp;items.new/items.original<br>#&nbsp;determining&nbsp;reliability<br>psychometric::SBrel(Nlength&nbsp;=&nbsp;m,&nbsp;rxx&nbsp;=&nbsp;rel.original)<br><br><br>#&nbsp;desired&nbsp;reliability<br>rel.new&nbsp;<-&nbsp;0.8<br>#&nbsp;determining&nbsp;test&nbsp;length<br>(m.new&nbsp;<-&nbsp;psychometric::SBlength(rxxp&nbsp;=&nbsp;rel.new,&nbsp;rxx&nbsp;=&nbsp;rel.original))<br>#&nbsp;number&nbsp;of&nbsp;required&nbsp;items<br>m.new*items.original"))),
      br(),
      br()
    ),
    # * SPLIT-HALF METHOD ####
    tabPanel(
      "Split-half method",
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
      ("$$\\text{rel}(X) = \\frac{m\\cdot \\text{cor}(X^*_1, X^*_2)}{1 + (m - 1)\\cdot\\text{cor}(X^*_1, X^*_2)} =
                       \\frac{2\\cdot \\text{cor}(X^*_1, X^*_2)}{1 + \\text{cor}(X^*_1, X^*_2)}$$"),
      p(
        "Below you can choose from different split-half approaches. ",
        strong("First-last"), "method uses correlation between the first half of items and the second
                        half of items.", strong("Even-odd"), "includes even items into the first subset and odd items
                        into the second one. ", strong("Random"), "method performs random split of items, thus the
                        resulting estimate may be different for each call. ", strong("Revelle's \\(\\beta\\)"), "is
                        actually the worst split-half (Revelle, 1979). Estimate is here calculated as the lowest split-half
                        reliability of by default 10,000 random splits. Finally, ", strong("Average"), "considers by default
                        10,000 split halves and averages the resulting estimates. Number of split halves can be changed below.
                        In case of odd number of items, first subset contains one more item than second one."
      ),
      uiOutput("reliability_splithalf_allpossible_text"),
      br(),
      fluidRow(
        column(
          3,
          withMathJax(),
          selectInput(
            inputId = "reliability_splithalf_method",
            label = "Split half method",
            choices = c(
              "First-last" = "firstlast",
              "Even-odd" = "evenodd",
              "Random" = "random",
              "Revelle's beta" = "worst",
              "Average" = "average"
            ),
            selected = "First_last"
          )
        ),
        column(
          4,
          numericInput(
            inputId = "reliability_splithalf_number",
            label = textOutput("reliability_splithalf_number_label"),
            value = 10000,
            min = 1,
            step = 1
          )
        )
      ),
      conditionalPanel(
        condition = "input.reliability_splithalf_method != 'average'",
        uiOutput("reliability_splithalf_text"),
        br()
      ),
      h4("Reliability estimate with confidence interval"),
      p(
        "Estimate of reliability for ", strong("First-last"), ", ", strong("Even-odd"), ", ", strong("Random"), "and",
        strong("Revelle's \\(\\beta\\)"), "is calculated using Spearman-Brown formula. Confidence interval is based on
                        confidence interval of correlation using delta method. Estimate of reliability for ", strong("Average"),
        "method is mean value of sampled reliabilities and confidence interval is confidence interval of this mean. "
      ),
      uiOutput("reliability_splithalf_table"),
      br(),
      h4("Histogram of reliability estimates"),
      p("Histogram is based on selected number of split halves estimates (10,000 by default).
                        The current estimate is highlighted by red colour."),
      plotlyOutput("reliability_splithalf_histogram"),
      downloadButton("DB_reliability_splithalf_histogram"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(psych)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br><br>#&nbsp;First-last&nbsp;splitting<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;1:10]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;11:20]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;Even-odd&nbsp;splitting<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;seq(1,&nbsp;20,&nbsp;2)]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;seq(2,&nbsp;20,&nbsp;2)]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;Random&nbsp;splitting<br>samp&nbsp;<-&nbsp;sample(1:20,&nbsp;10)<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;samp]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;setdiff(1:20,&nbsp;samp)]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;Minimum&nbsp;of&nbsp;10,000&nbsp;split-halves&nbsp;(Revelle's&nbsp;beta)<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE)<br>items1&nbsp;<-&nbsp;which(split$minAB[,&nbsp;'A']&nbsp;==&nbsp;1)<br>items2&nbsp;<-&nbsp;which(split$minAB[,&nbsp;'B']&nbsp;==&nbsp;1)<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;items1]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;items2]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;apply(df1,&nbsp;1,&nbsp;sum)<br>ts2&nbsp;<-&nbsp;apply(df2,&nbsp;1,&nbsp;sum)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearmann-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;calculation&nbsp;of&nbsp;CI<br>z.r&nbsp;<-&nbsp;0.5*log((1&nbsp;+&nbsp;cor.x)/(1&nbsp;-&nbsp;cor.x))<br>n&nbsp;<-&nbsp;length(ts1)<br>z.low&nbsp;<-&nbsp;z.r&nbsp;-&nbsp;1.96&nbsp;*&nbsp;sqrt(1/(n&nbsp;-&nbsp;3))<br>z.upp&nbsp;<-&nbsp;z.r&nbsp;+&nbsp;1.96&nbsp;*&nbsp;sqrt(1/(n&nbsp;-&nbsp;3))<br><br>cor.low&nbsp;<-&nbsp;(exp(2*z.low)&nbsp;-&nbsp;1)/(exp(2*z.low)&nbsp;+&nbsp;1)<br>cor.upp&nbsp;<-&nbsp;(exp(2*z.upp)&nbsp;-&nbsp;1)/(exp(2*z.upp)&nbsp;+&nbsp;1)<br><br>rel.x&nbsp;<-&nbsp;2*cor.x/(1&nbsp;+&nbsp;cor.x)<br>rel.low&nbsp;<-&nbsp;2*cor.low/(1&nbsp;+&nbsp;cor.low)<br>rel.upp&nbsp;<-&nbsp;2*cor.upp/(1&nbsp;+&nbsp;cor.upp)<br><br><br>#&nbsp;Average&nbsp;10,000&nbsp;split-halves<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE)<br>(rel.x&nbsp;<-&nbsp;mean(split$raw))<br><br>#&nbsp;Average&nbsp;all&nbsp;split-halves<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE,&nbsp;brute&nbsp;=&nbsp;TRUE)<br>(rel.x&nbsp;<-&nbsp;mean(split$raw))<br><br>#&nbsp;calculation&nbsp;of&nbsp;CI<br>n&nbsp;<-&nbsp;length(split$raw)<br>rel.low&nbsp;<-&nbsp;rel.x&nbsp;-&nbsp;1.96&nbsp;*&nbsp;sd(split$raw)/sqrt(n)<br>rel.upp&nbsp;<-&nbsp;rel.x&nbsp;+&nbsp;1.96&nbsp;*&nbsp;sd(split$raw)/sqrt(n)"))),
      br()
    ),
    # * CRONBACH'S ALPHA ####
    tabPanel("Cronbach's \\(\\alpha\\)",
      value = "cronbach",
      h3("Cronbach's \\(\\alpha\\)"),
      p("Cronbach's \\(\\alpha\\) is an estimate of internal consistency of a psychometric test.
                        It is a function of the number of items in a test, the average covariance
                        between item-pairs, and the variance of the total score (Cronbach, 1951)."),
      h4("Equation"),
      p("For test with \\(I\\) items where \\(X = X_1 + ... + X_I\\) is a total score,
                        \\(\\sigma^2_X\\) its variance and \\(\\sigma^2_{X_i}\\) variances of items,
                        Cronbach's \\(\\alpha\\) is given by following equation"),
      withMathJax(),
      ("$$\\alpha = \\frac{I}{I-1}\\left(1 - \\frac{\\sum_{i = 1}^I \\sigma^2_{X_i}}{\\sigma^2_X}\\right)$$"),
      h4("Estimate with confidence interval"),
      p("Confidence interval is based on F distribution as proposed by Feldt et al. (1987)."),
      tableOutput("reliability_cronbachalpha_table"),
      # h3("McDonald's \\(\\omega\\)"),
      # p(),
      # br(),
      h4("Selected R code"),
      div(code(HTML("library(psychometric)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;Cronbach's&nbsp;alpha&nbsp;with&nbsp;confidence&nbsp;interval<br>a&nbsp;<-&nbsp;psychometric::alpha(data)<br>psychometric::alpha.CI(a,&nbsp;N&nbsp;=&nbsp;nrow(data),&nbsp;k&nbsp;=&nbsp;ncol(data),&nbsp;level&nbsp;=&nbsp;0.95)"))),
      br()
    ),
    # * INTRA-CLASS CORRELATION ####
    # tabPanel("Intra-class correlation",
    #   value = "ICC",
    #   h3("Intra-class correlation"),
    #   p("More generally, Cronbach's \\(\\alpha\\) is equivalent to intraclass correlation under one of the ANOVA models."),
    #   tableOutput("reliability_icc_table"), # preview, under construction
    #   h4("Selected R code"),
    #   div(code(HTML("library(psychometric)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI)<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;Cronbach's&nbsp;alpha&nbsp;with&nbsp;confidence&nbsp;interval<br>a&nbsp;<-&nbsp;psychometric::alpha(data)<br>psychometric::alpha.CI(a,&nbsp;N&nbsp;=&nbsp;nrow(data),&nbsp;k&nbsp;=&nbsp;ncol(data),&nbsp;level&nbsp;=&nbsp;0.95)"))),
    #   br()
    # ),

    # * RANGE-RESTRICTED RELIABILITY ####
    tabPanel("Restricted range",
      value = "rr_irr",
      h3("Range-restricted reliability"),
      p(
        "This section illustrates the issue of range-restricted reliability and the difficulties with the maximum
        likelihood estimation, described in more detail in the context of inter-rater reliability in grant proposal review in ",
        a("Erosheva, Martinkova & Lee (2021)",
          href = "https://rss.onlinelibrary.wiley.com/loi/1467985x",
          target = "_blank", .noWS = "outside"
        ),
        ". To replicate their examples, select the ", code("AIBS"), "toy dataset in the ", strong("Data"), "section."
      ),
      p(
        "Below, you may select the ratio and type of range restriction given by the ", strong("proportion of ratees"),
        ". A ratee is the object or subject that is being evaluated. It could be a proposal application in grant review
        (as is the case in the ", code("AIBS"), "dataset), a student in educational assessment,
        a job application in hiring, a patient in a medical study, etc.
        Further, you may select the ", strong("direction"), "of restriction (top or bottom).
        The left plot illustrates the variability in ratings for the whole dataset outshading the data which would be lost
        due to range-restriction. The right plot provides the estimates of the calculated inter-rater reliability estimates,
        defined by intraclass corelation in the simplest model including the ratee effect only.
        The ", strong("number of bootstrap samples"), "is used to calculate the 95% confidence interval of the
        restricted-range reliability estimate plotted in the right figure."
      ),
      fluidRow(
        column(
          2,
          sliderInput(
            inputId = "reliability_restricted_proportion",
            label = "Proportion of ratees",
            min = 0,
            max = 100,
            step = 1,
            value = 100, animate = animationOptions(2000), post = "%"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "reliability_restricted_direction",
            label = "Direction",
            choices = c("top", "bottom"),
            selected = "top"
          )
        ),
        column(
          2,
          numericInput(
            inputId = "reliability_restricted_bootsamples",
            label = "Num. of bootstrap samples",
            value = 10,
            min = 3,
            max = 1000
          )
        ),
        column(
          2,
          actionButton(
            inputId = "reliability_restricted_clear",
            label = "Clear everything",
            icon = icon("eraser"),
            style = "margin-top:25px"
          )
        )
      ),
      fluidRow(
        column(6, plotlyOutput("reliability_restricted_caterpillarplot")),
        column(6, plotlyOutput("reliability_restricted_iccplot"))
      ),
      # tableOutput("res_entries"),
      br(),

      # plotlyOutput("irr3_plt"),
      textOutput("icc_text"),
      h4("Selected R code"),
      code(HTML("library(ShinyItemAnalysis)<br><br>#&nbsp;estimate&nbsp;inter-rater&nbsp;reliability&nbsp;(ICC)&nbsp;for&nbsp;complete&nbsp;AIBS&nbsp;dataset<br>ICCrestricted(Data&nbsp;=&nbsp;AIBS,&nbsp;case&nbsp;=&nbsp;\"ID\",&nbsp;var&nbsp;=&nbsp;\"Score\",&nbsp;rank&nbsp;=&nbsp;\"ScoreRankAdj\")<br><br>#&nbsp;estimate&nbsp;range-restricted&nbsp;ICC<br>ICCrestricted(<br>&nbsp;&nbsp;Data&nbsp;=&nbsp;AIBS,&nbsp;case&nbsp;=&nbsp;\"ID\",&nbsp;var&nbsp;=&nbsp;\"Score\",&nbsp;rank&nbsp;=&nbsp;\"ScoreRankAdj\",<br>&nbsp;&nbsp;sel&nbsp;=&nbsp;0.8,&nbsp;dir&nbsp;=&nbsp;\"bottom\"<br>)<br><br>#&nbsp;estimate&nbsp;all&nbsp;possible&nbsp;top-restricted&nbsp;subsets,&nbsp;save&nbsp;them&nbsp;to&nbsp;a&nbsp;tibble<br>all_top_restricted&nbsp;<-&nbsp;purrr::map_dfr(<br>&nbsp;&nbsp;2:72,<br>&nbsp;&nbsp;~&nbsp;ICCrestricted(<br>&nbsp;&nbsp;&nbsp;&nbsp;Data&nbsp;=&nbsp;AIBS,&nbsp;case&nbsp;=&nbsp;\"ID\",&nbsp;var&nbsp;=&nbsp;\"Score\",&nbsp;rank&nbsp;=&nbsp;\"ScoreRankAdj\",<br>&nbsp;&nbsp;&nbsp;&nbsp;sel&nbsp;=&nbsp;.x,&nbsp;nsim&nbsp;=&nbsp;10<br>&nbsp;&nbsp;)<br>)<br><br>#&nbsp;plot<br>all_top_restricted&nbsp;%>%<br>&nbsp;&nbsp;ggplot(aes(prop_sel,&nbsp;y&nbsp;=&nbsp;ICC1,&nbsp;ymin&nbsp;=&nbsp;ICC1_LCI,&nbsp;ymax&nbsp;=&nbsp;ICC1_UCI))&nbsp;+<br>&nbsp;&nbsp;geom_pointrange()&nbsp;+<br>&nbsp;&nbsp;scale_x_continuous(labels&nbsp;=&nbsp;scales::percent)&nbsp;+<br>&nbsp;&nbsp;coord_cartesian(ylim&nbsp;=&nbsp;c(0,&nbsp;1))")),
      br()
    )
  )
