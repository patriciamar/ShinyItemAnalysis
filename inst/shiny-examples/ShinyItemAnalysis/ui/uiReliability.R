uiReliability <-
  navbarMenu(
    "Reliability",
    # "Description",
    ## * RELIABILITY ####
    # tabPanel("Reliability",
    #  value = "reliability_description",
    #  h3("Reliability"),
    #  p("We are typically interested in the unobserved true score \\(T\\), but normally have available only the
    #                    observed score \\(X\\) which is contaminated by some measurement error \\(e\\), so that
    #                    \\(X = T + e\\) and the error term is uncorrelated with the true score."),
    #  h4("Equation"),
    #  p("Reliability is defined as the squared correlation of the true and observed score"),
    #  withMathJax(),
    #  ("$$\\text{rel}(X) = \\text{cor}(T, X)^2$$"),
    #  p("Equivalently, reliability can be re-expressed as the ratio of the true score variance
    #                    to the total observed variance"),
    #  withMathJax(),
    #  ("$$\\text{rel}(X) = \\frac{\\sigma^2_T}{\\sigma^2_X}$$"),
    #  br()
    # ),
    # "----",
    # "Used methods",
    # * SPEARMAN-BROWN FORMULA ####
    tabPanel(
      "Spearman-Brown formula",
      h3("Spearman-Brown formula"),
      h4("Equation"),
      p("Let \\(\\text{rel}(X)\\) be the reliability of the test composed of \\(I\\) equally precise
                        items measuring the same construct, \\(X = X_1 + ... + X_I\\).
                        Then for a test consisting of
                        \\(I^*\\) such items,  that is for a test which is
                        \\(m = \\frac{I^*}{I}\\) times longer/shorter, the reliability would be"),
      withMathJax(),
      ("$$\\text{rel}(X^*) = \\frac{m\\cdot \\text{rel}(X)}{1 + (m - 1)\\cdot\\text{rel}(X)}.$$"),
      p("The Spearman-Brown formula can be used to determine reliability of a test with with a
                        different number of equally precise items measuring the same construct.
                        It can also be used to determine the necessary number of items to achieve
                        desired reliability."),
      p("In the calculations below, ", strong("reliability of original data"), "is by
                        default set to the value of Cronbach's \\(\\alpha\\) for the dataset currently in use. The ",
                        strong("number of items in the original data"), "is
                        by default set to the number of items in the dataset currently in use. "),
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
      p("Here you can calculate an estimate of reliability for a test consisting of a different number of
                        items. "),
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
      p("Here you can calculate the necessary number of items
                        to gain the required level of reliability. "),
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
      div(code(HTML("library(psychometric)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;reliability&nbsp;of&nbsp;original&nbsp;data<br>rel.original&nbsp;<-&nbsp;psychometric::alpha(data)<br>#&nbsp;number&nbsp;of&nbsp;items&nbsp;in&nbsp;original&nbsp;data<br>items.original&nbsp;<-&nbsp;ncol(data)<br><br>#&nbsp;number&nbsp;of&nbsp;items&nbsp;in&nbsp;new&nbsp;data<br>items.new&nbsp;<-&nbsp;30<br>#&nbsp;ratio&nbsp;of&nbsp;tests&nbsp;lengths<br>m&nbsp;<-&nbsp;items.new&nbsp;/&nbsp;items.original<br>#&nbsp;determining&nbsp;reliability<br>SBrel(Nlength&nbsp;=&nbsp;m,&nbsp;rxx&nbsp;=&nbsp;rel.original)<br><br>#&nbsp;desired&nbsp;reliability<br>rel.new&nbsp;<-&nbsp;0.8<br>#&nbsp;determining&nbsp;test&nbsp;length<br>(m.new&nbsp;<-&nbsp;SBlength(rxxp&nbsp;=&nbsp;rel.new,&nbsp;rxx&nbsp;=&nbsp;rel.original))<br>#&nbsp;number&nbsp;of&nbsp;required&nbsp;items<br>m.new&nbsp;*&nbsp;items.original<br>"))),
      br(),
      br()
    ),
    # * SPLIT-HALF METHOD ####
    tabPanel(
      "Split-half method",
      h3("Split-half method"),
      p("The split-half method uses the correlation between two subscores for an estimation of reliability.
                        The underlying assumption is that the two halves of the test (or even all items on the test) are
                        equally precise and measure the same underlying construct. The Spearman-Brown formula is then used to
                        correct the estimate for the number of items."),
      h4("Equation"),
      p("For a test with \\(I\\) items total score is calculated as \\(X = X_1 + ... + X_I\\).
                        Let \\(X^*_1\\) and \\(X^*_2\\) be total scores calculated from items found only in the first
                        and second subsets. The estimate of reliability is then given by the Spearman-Brown formula (Spearman, 1910; Brown, 1910)
                        with \\(m = 2\\)."),
      withMathJax(),
      ("$$\\text{rel}(X) = \\frac{m\\cdot \\text{cor}(X^*_1, X^*_2)}{1 + (m - 1)\\cdot\\text{cor}(X^*_1, X^*_2)} =
                       \\frac{2\\cdot \\text{cor}(X^*_1, X^*_2)}{1 + \\text{cor}(X^*_1, X^*_2)}$$"),
      p(
        "You can choose below from different split-half approaches. The ",
        strong("First-last"), "method uses a correlation between the first half of items and the second
                        half of items. The ", strong("Even-odd"), "method places even numbered items into the first subset and odd numbered items
                        into the second one. The ", strong("Random"), "method performs a random split of items, thus the
                        resulting estimate may be different for each call. Out of a specified number of random splits (10,000 by default),
                        the ", strong("Worst"), " method selects the lowest estimate and the ", strong("Average"), "method calculates the
                        average. In the case of an odd number of items, the first subset contains one more item than the second one."
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
              "Worst" = "worst",
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
        "The estimate of reliability for ", strong("First-last"), ", ", strong("Even-odd"), ", ", strong("Random"), "and",
        strong("Worst"), "is calculated using the Spearman-Brown formula. The confidence interval is based on a
        confidence interval of correlation using the delta method. The estimate of reliability for the ", strong("Average"),
        "method is a mean value of sampled reliabilities and the confidence interval is the confidence interval of this mean. "
      ),
      uiOutput("reliability_splithalf_table"),
      br(),
      h4("Histogram of reliability estimates"),
      p("A histogram is based on a selected number of split halves estimates (10,000 by default).
                        The current estimate is highlighted by a red colour."),
      plotlyOutput("reliability_splithalf_histogram"),
      downloadButton("DB_reliability_splithalf_histogram"),
      br(),
      h4("Selected R code"),
      div(code(HTML("library(psych)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br><br>#&nbsp;first-last&nbsp;splitting<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;1:10]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;11:20]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;rowSums(df1)<br>ts2&nbsp;<-&nbsp;rowSums(df2)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearman-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.x&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;even-odd&nbsp;splitting<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;seq(1,&nbsp;20,&nbsp;2)]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;seq(2,&nbsp;20,&nbsp;2)]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;rowSums(df1)<br>ts2&nbsp;<-&nbsp;rowSums(df2)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearman-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.x&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;random&nbsp;splitting<br>samp&nbsp;<-&nbsp;sample(1:20,&nbsp;10)<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;samp]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;setdiff(1:20,&nbsp;samp)]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;rowSums(df1)<br>ts2&nbsp;<-&nbsp;rowSums(df2)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearman-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.x&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;minimum&nbsp;of&nbsp;10,000&nbsp;split-halves&nbsp;(Revelle's&nbsp;beta)<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE)<br>items1&nbsp;<-&nbsp;which(split$minAB[,&nbsp;\"A\"]&nbsp;==&nbsp;1)<br>items2&nbsp;<-&nbsp;which(split$minAB[,&nbsp;\"B\"]&nbsp;==&nbsp;1)<br>df1&nbsp;<-&nbsp;HCI[,&nbsp;items1]<br>df2&nbsp;<-&nbsp;HCI[,&nbsp;items2]<br>#&nbsp;total&nbsp;score&nbsp;calculation<br>ts1&nbsp;<-&nbsp;rowSums(df1)<br>ts2&nbsp;<-&nbsp;rowSums(df2)<br>#&nbsp;correlation<br>cor.x&nbsp;<-&nbsp;cor(ts1,&nbsp;ts2)<br>#&nbsp;apply&nbsp;Spearman-Brown&nbsp;formula&nbsp;to&nbsp;estimate&nbsp;reliability<br>(rel.x&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.x&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.x))<br><br>#&nbsp;calculation&nbsp;of&nbsp;CI<br>z.r&nbsp;<-&nbsp;0.5&nbsp;*&nbsp;log((1&nbsp;+&nbsp;cor.x)&nbsp;/&nbsp;(1&nbsp;-&nbsp;cor.x))<br>n&nbsp;<-&nbsp;length(ts1)<br>z.low&nbsp;<-&nbsp;z.r&nbsp;-&nbsp;1.96&nbsp;*&nbsp;sqrt(1&nbsp;/&nbsp;(n&nbsp;-&nbsp;3))<br>z.upp&nbsp;<-&nbsp;z.r&nbsp;+&nbsp;1.96&nbsp;*&nbsp;sqrt(1&nbsp;/&nbsp;(n&nbsp;-&nbsp;3))<br><br>cor.low&nbsp;<-&nbsp;(exp(2&nbsp;*&nbsp;z.low)&nbsp;-&nbsp;1)&nbsp;/&nbsp;(exp(2&nbsp;*&nbsp;z.low)&nbsp;+&nbsp;1)<br>cor.upp&nbsp;<-&nbsp;(exp(2&nbsp;*&nbsp;z.upp)&nbsp;-&nbsp;1)&nbsp;/&nbsp;(exp(2&nbsp;*&nbsp;z.upp)&nbsp;+&nbsp;1)<br><br>rel.x&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.x&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.x)<br>rel.low&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.low&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.low)<br>rel.upp&nbsp;<-&nbsp;2&nbsp;*&nbsp;cor.upp&nbsp;/&nbsp;(1&nbsp;+&nbsp;cor.upp)<br><br>#&nbsp;average&nbsp;10,000&nbsp;split-halves<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE)<br>(rel.x&nbsp;<-&nbsp;mean(split$raw))<br><br>#&nbsp;average&nbsp;all&nbsp;split-halves<br>split&nbsp;<-&nbsp;psych::splitHalf(HCI[,&nbsp;1:20],&nbsp;raw&nbsp;=&nbsp;TRUE,&nbsp;brute&nbsp;=&nbsp;TRUE)<br>(rel.x&nbsp;<-&nbsp;mean(split$raw))<br><br>#&nbsp;calculation&nbsp;of&nbsp;CI<br>n&nbsp;<-&nbsp;length(split$raw)<br>rel.low&nbsp;<-&nbsp;rel.x&nbsp;-&nbsp;1.96&nbsp;*&nbsp;sd(split$raw)&nbsp;/&nbsp;sqrt(n)<br>rel.upp&nbsp;<-&nbsp;rel.x&nbsp;+&nbsp;1.96&nbsp;*&nbsp;sd(split$raw)&nbsp;/&nbsp;sqrt(n)<br>"))),
      br()
    ),
    # * CRONBACH'S ALPHA ####
    tabPanel("Cronbach's \\(\\alpha\\)",
      value = "cronbach",
      h3("Cronbach's \\(\\alpha\\)"),
      p("Cronbach's \\(\\alpha\\) is an estimate of the internal consistency of a psychometric test.
                        It is a function of the number of items in a test, the average covariance
                        between item-pairs, and the variance of the total score (Cronbach, 1951)."),
      h4("Equation"),
      p("For a test with \\(I\\) items where \\(X = X_1 + ... + X_I\\) is a total score,
                        \\(\\sigma^2_X\\) its variance and \\(\\sigma^2_{X_i}\\) variances of items,
                        Cronbach's \\(\\alpha\\) is given by following equation"),
      withMathJax(),
      ("$$\\alpha = \\frac{I}{I-1}\\left(1 - \\frac{\\sum_{i = 1}^I \\sigma^2_{X_i}}{\\sigma^2_X}\\right)$$"),
      h4("Estimate with confidence interval"),
      p("A confidence interval is based on F distribution as proposed by Feldt et al. (1987)."),
      tableOutput("reliability_cronbachalpha_table"),
      # h3("McDonald's \\(\\omega\\)"),
      # p(),
      # br(),
      h4("Selected R code"),
      div(code(HTML("library(psychometric)<br><br>#&nbsp;loading&nbsp;data<br>data(HCI,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br>data&nbsp;<-&nbsp;HCI[,&nbsp;1:20]<br><br>#&nbsp;Cronbach's&nbsp;alpha&nbsp;with&nbsp;confidence&nbsp;interval<br>a&nbsp;<-&nbsp;psychometric::alpha(data)<br>psychometric::alpha.CI(a,&nbsp;N&nbsp;=&nbsp;nrow(data),&nbsp;k&nbsp;=&nbsp;ncol(data),&nbsp;level&nbsp;=&nbsp;0.95)<br>"))),
      br()
    ),
    # * INTRA-CLASS CORRELATION ####
    # tabPanel("Intra-class correlation",
    #   value = "ICC",
    #   h3("Intra-class correlation"),
    #   p("More generally, Cronbach's \\(\\alpha\\) is equivalent to intraclass correlation under one of the ANOVA models."),
    #   tableOutput("reliability_icc_table"), # preview, under construction
    #   h4("Selected R code"),
    #   div(code(HTML("library(psych)<br>library(tidyverse)<br><br>#&nbsp;loading&nbsp;and&nbsp;formatting&nbsp;data<br>data(AIBS,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br>AIBSwide&nbsp;<-&nbsp;AIBS&nbsp;%>%<br>&nbsp;&nbsp;pivot_wider(ID,&nbsp;values_from&nbsp;=&nbsp;Score,&nbsp;names_from&nbsp;=&nbsp;RevCode)&nbsp;%>%<br>&nbsp;&nbsp;select(-ID)<br>head(AIBSwide)<br><br>ICC(AIBSwide)<br>"))),
    #   br()
    # ),

    # * RANGE-RESTRICTED RELIABILITY ####
    tabPanel("Restricted range",
      value = "rr_irr",
      h3("Range-restricted reliability"),
      p(
        "This section illustrates the issue of range-restricted reliability and the difficulties with maximum
        likelihood estimation, described in more detail in the context of inter-rater reliability in grant proposal review in
        Erosheva, Martinkova & Lee (accepted).
        To replicate their examples, select the ", code("AIBS"), "toy dataset in the ", strong("Data"), "section."
      ),
      p(
        "Below, you may select the ratio and type of range restriction given by the ", strong("proportion of rated subjects/objects."),
        " It could be a grant proposal application in grant review (as is the case in the ", code("AIBS"), "dataset),
        a student in educational assessment, a job application in hiring, a patient in a medical study, etc.
        Further, you may select the ", strong("direction"), "of restriction (top or bottom).
        The left plot illustrates the variability in ratings for the whole dataset outshading the data which would be lost
        due to range-restriction. The right plot provides the estimates of the calculated inter-rater reliability estimates,
        defined by intraclass corelation in the simplest model including the ratee effect only.
        The estimates are accompanied by a bootstrapped 95% confidence interval; see the settings section for further details
        on used number of bootstrapped samples."
      ),
      fluidRow(
        column(
          2,
          sliderInput(
            inputId = "reliability_restricted_proportion",
            label = "Proportion",
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
        column(6, plotlyOutput("reliability_restricted_iccplot")),
        style = "
    padding-bottom: 15px;"
      ),
      fluidRow(
        column(
          6,
          downloadButton("DB_reliability_restricted_caterpillarplot", label = "Download figure")
        ),
        column(
          6,
          downloadButton("DB_reliability_restricted_iccplot", label = "Download figure"),
          downloadButton("DB_reliability_restricted_iccdata", label = "Download data")
        )
      ),
      br(),
      textOutput("icc_text"),
      h4("Selected R code"),
      code(includeText("sc/reliability/restr_range.R")),
      br()
    )
  )
