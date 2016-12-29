#library(shinythemes)

ui=tagList(
  # BOOTSTRAP THEME SELECTOR FOR SHINY APP (requieres shinythemes package)
  #shinythemes:themeSelector(),
navbarPage(title="TEST AND ITEM ANALYSIS",
           collapsible=TRUE,
           footer=list(
             # online version
             div(class="panel-footer",
                 p(strong("ShinyItemAnalysis Version 1.1.0")),
                 p("Download ShinyItemAnalysis R package from ",
                   a(strong("CRAN"), href = "https://cran.rstudio.com/web/packages/ShinyItemAnalysis/",
                     target = "_blank"), "to run analysis faster!"),
                # br(),
                 p("Project was supported by grant funded by Czech Science Foundation under number ",
                   a("GJ15-15856Y",
                     href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
                     target = "_blank")),
                 p(" Copyright 2016  Patricia Martinkova, Adela Drabinova, Ondrej Leder and Jakub Houdek"),
                # br(),
                 div(
                   HTML('<p style="color:black; font-size: 9pt">
                        Older version 0.1 <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV01/">
                       is available here. </a>
                        </p>')
                 ),
                 div(
                   HTML('<p style="color:black; font-size: 9pt">
                        Older version 0.2 <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV02/">
                       is available here. </a>
                        </p>')
                 ),
                div(
                  HTML('<p style="color:black; font-size: 9pt">
                       Older version 1.0.0 <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV100/">
                       is available here. </a>
                       </p>')
                  ),
                # br(),
                 p(textOutput('counter'))
                # br()
             )
           ),
           theme="bootstrap.css",
          #####################
          # SIDE PANEL UPLOADS####
          #####################
          # sidebarLayout(
          #   sidebarPanel(
          #     # online version
          #     p(strong("ShinyItemAnalysis Version 1.0")),
          #     p("Download ShinyItemAnalysis R package from ",
          #       a(strong("CRAN"), href = "https://cran.rstudio.com/web/packages/ShinyItemAnalysis/",
          #         target = "_blank"), "to run analysis faster!"),
          #     br(),
          #     p("Project was supported by grant funded by Czech Science Foundation under number ",
          #       a("GJ15-15856Y",
          #         href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
          #         target = "_blank")),
          #     p(" Copyright 2016  Patricia Martinkova, Ondrej Leder, Adela Drabinova and Jakub Houdek"),
          #     br(),
          #     div(
          #       HTML('<p style="color:black; font-size: 9pt">
          #            Older version 0.1 <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV01/">
          #           is available here. </a>
          #            </p>')
          #     ),
          #     div(
          #       HTML('<p style="color:black; font-size: 9pt">
          #            Older version 0.2 <a href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV02/">
          #           is available here. </a>
          #            </p>')
          #     ),
          #     br(),
          #     p(textOutput('counter')),
          #     br(),
          #
          #     # # CRAN version
          #     # p(strong("ShinyItemAnalysis Version 1.0")),
          #     # p("Try ShinyItemAnalysis ",
          #     #   a(strong("online"), href = "https://shiny.cs.cas.cz/ShinyItemAnalysis", target = "_blank")),
          #     # br(),
          #     # p("Project was supported by grant funded by Czech Science Foundation under number ",
          #     #   a("GJ15-15856Y",
          #     #     href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
          #     #     target = "_blank")),
          #     # p(" Copyright 2016  Patricia Martinkova, Adela Drabinova, Ondrej Leder and Jakub Houdek"),
          #     # br(),
          #
          #     width = 3
          #     ),
            #########################
            # MAIN PANEL ####
            #########################
            # mainPanel(
            # tabsetPanel(
                ########################
                # SUMMARY #####
                ########################
                navbarMenu("Summary",
                     # TOTAL SCORES
                    tabPanel("Total Scores",
                  br(),
                  p(code('ShinyItemAnalysis'), 'provides analysis of educational tests (such as admission tests)
                    and its items. For demonstration purposes, 20-item dataset', code('GMAT'),' from
                    R ', code('library(difNLR)'),' is used. You can change the dataset
                    (and try your own one) on page', strong('Data.')),
                  h3("Analysis of Total Scores"),
                  h4("Summary Table"),
                  tableOutput('results'),
                  h4("Histogram of Total Score"),
                  #uiOutput("slider2"),
                  sliderInput("inSlider2", "Cut-Score", min = 0, max = 10,
                              value = 1, step = 1),
                  p('For selected cut-score, blue part of histogram shows students with total score
                    above the cut-score, grey column shows students with Total Score equal
                    to cut-score and red part of histogram shows students below the cut-score.'),
                  plotOutput('histogram_totalscores'),
                  downloadButton("DP_histogram_totalscores", label = "Download figure"),
                  br(),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      code('score <- apply(data, 1, sum) # Total score'),
                      br(),
                      br(),
                      code('# Summary of total score'),
                      br(),
                      code('summary(score)'),
                      br(),
                      code('# Histogram'),
                      br(),
                      code('hist(score, breaks = 0:ncol(data)) ')),
                  br()
                  ),
                  # STANDARD SCORES
                  tabPanel("Standard Scores",
                           h4("Table by Score"),
                           tableOutput('percentile'),
                           br(),
                           h4("Selected R code"),
                           div(code('library(difNLR)'),
                               br(),
                               code('data(GMAT)'),
                               br(),
                               code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                               br(),
                               code('score <- apply(data, 1, sum) # Total score'),
                               br(),
                               br(),
                               code('tosc <- sort(unique(score)) # Levels of total score'),
                               br(),
                               code('perc <- cumsum(prop.table(table(score))) # Percentiles'),
                               br(),
                               code('sura <- 100 * (tosc / max(score)) # Success rate'),
                               br(),
                               code('zsco <- sort(unique(scale(score))) # Z-score'),
                               br(),
                               code('tsco <- 50 + 10 * zsco # T-score')),
                           br()
                           ),
                  # CORRELATION STRUCTURE
                  tabPanel("Correlation Structure",
                           h4("Correlation Plot"),
                           plotOutput('corr_plot'),
                           br(),
                           h4("Scree Plot"),
                           plotOutput('scree_plot'),
                           h4("Selected R code"),
                           div(code('library(corrplot, difNLR, psych)'),
                               br(),
                               code('data(GMAT)'),
                               br(),
                               code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                               br(),
                               br(),
                               code('# Correlation plot'),
                               br(),
                               code('corP <- polychoric(data)'),
                               br(),
                               code('corrplot(corP$rho)'),
                               br(),
                               code('corP$rho'),
                               br(),
                               br(),
                               code('# Scree plot'),
                               br(),
                               code('plot(1:length(eigen(corP$rho)$values), eigen(corP$rho)$values,
                                    ylab = "Eigen value")'),
                               br(),
                               code('lines(1:length(eigen(corP$rho)$values), eigen(corP$rho)$values)'),
                               br(),
                               code('eigen(corP$rho)')),
                           br()
                  )
                  #)
                ),
                ###########################
                # TRADITIONAL ANALYSIS ####
                ###########################
                navbarMenu('Traditional Analysis',
                  # ITEM ANALYSIS
                  tabPanel("Item Analysis",
                  br(),
                  h3("Traditional Item Analysis"),
                  p('Traditional item analysis uses proportions of correct answers or correlations to estimate item properties.'),
                  h4("Item Difficulty/Discrimination Graph"),
                  p("Displayed is difficulty (red) and discrimination (blue)
                    for all items. Items are ordered by difficulty. "),
                  p(strong("Difficulty"),' of items is estimated as percent of students who answered correctly to that item.'),
                  p(strong("Discrimination"),' is described by difference of percent correct
                    in upper and lower third of students (Upper-Lower Index, ULI). By rule of thumb it should not be lower than 0.2
                    (borderline in the plot), except for very easy or very difficult items.'),

                  plotOutput('difplot'),
                  downloadButton("DP_difplot", label = "Download figure"),
                  h4("Traditional Item Analysis"),
                  h3("Cronbach's alpha"),
                  p("Chronbach's alpha is an estimate of the reliability of a psychometric test. It is a function
                    of the number of items in a test, the average covariance between item-pairs, and the variance
                    of the total score (Cronbach, 1951)."),
                  tableOutput('cronbachalpha'),
                  h3("Traditional Item Analysis Table"),
                  p(strong('Explanation: Difficulty'), ' - Difficulty of item is estimated as percent
                    of students who answered correctly to that item. ', strong('SD'),' - standard deviation, ',
                    strong('RIT'), ' - Pearson correlation between item and Total score, ', strong('RIR'),'
                     - Pearson correlation between item and rest of items, ', strong('ULI'),'
                     - Upper-Lower Index, ', strong('Alpha Drop'),' - Cronbach\'s alpha of test without given item.'),
                  tableOutput('itemexam'),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      br(),
                      code('# Difficulty and discrimination plot'),
                      br(),
                      code('DDplot(data)'),
                      br(),
                      code('# Table'),
                      br(),
                      code('tab <- round(data.frame(item.exam(data, discr = TRUE)[, c(4, 1, 5, 2, 3)],
                           psych::alpha(data)$alpha.drop[, 1]), 2)'),
                      br(),
                      code('tab')),
                  br()
                  ),
                  # DISTRACTORS
                  tabPanel("Distractors",
                     br(),
                     p('Traditional item analysis uses proportions of correct answers or correlations to estimate item properties.'),
                     h3("Distractor Analysis"),
                     p('In distractor analysis, we are interested in how test takers select
                       the correct answer and how the distractors (wrong answers) were able
                       to function effectively by drawing the test takers away from the correct answer.'),

                    htmlOutput("text_distractor"),
                    br(),
                    sliderInput('gr','Number of groups:',
                      min   = 1,
                      max   = 5,
                      value = 3
                    ),
                    fluidRow(column(12, align = "center", tableOutput('tab_distractor_by_group'))),
                    br(),
                    plotOutput('hist_distractor_by_group'),
                    downloadButton("DP_hist_distractor_by_group", label = "Download figure"),
                    br(),
                    radioButtons('type_combinations_distractor', 'Type',
                                 list("Combinations", "Distractors")
                    ),
                    # uiOutput("distractorSliderUI"),
                    sliderInput("distractorSlider", "Item Slider", min=1, value=1, max=10,
                                step=1, animate=TRUE),
                    h4("Distractors Plot"),
                    plotOutput('graf'),
                    downloadButton("DP_graf", label = "Download figure"),
                    h4("Table with Counts"),
                    fluidRow(column(12, align = "center", tableOutput('tab_counts_distractor'))),
                    h4("Table with Proportions"),
                    fluidRow(column(12, align = "center", tableOutput('tab_props_distractor'))),
                    br(),
                    h4("Selected R code"),
                    div(code('library(difNLR)'),
                        br(),
                        code('data(GMATtest)'),
                        br(),
                        code('data  <- GMATtest[, colnames(GMATtest) != "group"]'),
                        br(),
                        code('data(GMATkey)'),
                        br(),
                        code('key  <- GMATkey'),
                        br(),
                        br(),
                        code('# Combinations - plot for item 1 and 3 groups'),
                        br(),
                        code('plotDistractorAnalysis(data, key, num.group = 3, item = 1,
                                               multiple.answers = T)'),
                        br(),
                        code('# Distractors - plot for item 1 and 3 groups'),
                        br(),
                        code('plotDistractorAnalysis(data, key, num.group = 3, item = 1,
                             multiple.answers = F)'),
                        br(),
                        code('# Table with counts and margins - item 1 and 3 groups'),
                        br(),
                        code('DA <- DistractorAnalysis(data, key, num.groups = 3)[[1]]'),
                        br(),
                        code('dcast(as.data.frame(DA), response ~ score.level, sum, margins = T, value.var = "Freq")'),
                        br(),
                        code('# Table with proportions - item 1 and 3 groups'),
                        br(),
                        code('DistractorAnalysis(data, key, num.groups = 3, p.table = T)[[1]]'),
                        br(),
                        code('tab')),
                    br()
                    )
                 # )
                ),

                #####################
                # REGRESSION ########
                #####################
                navbarMenu("Regression",
                # LOGISTIC
                tabPanel("Logistic",
                  h3("Logistic Regression on Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Logistic regression'),'can model dependency of probability of correct answer on total score by
                    s-shaped logistic curve. Parameter', strong( "b0"),' describes horizontal position of the fitted curve,
                    parameter ', strong( 'b1'),' describes its slope.'),
                  br(),
                  h4("Plot with Estimated Logistic Curve"),
                  p('Points represent proportion of correct answer with respect to total score.
  Their size is determined by count of respondents who answered item correctly.'),
                 # uiOutput("logregSliderUI"),
                  sliderInput("logregSlider", "Item Slider", min=1, value=1, max=10,
                              step=1, animate=TRUE),
                  plotOutput('logreg'),
                  downloadButton("DP_logref", label = "Download figure"),
                  h4("Equation"),
                  withMathJax(),
                  ('$$\\mathrm{P}(Y = 1|X, b_0, b_1) = \\mathrm{E}(Y|X, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 X\\right)}}{1+e^{\\left( b_{0} + b_1 X\\right) }} $$'),

                  h4("Table of parameters"),
                  fluidRow(column(12, align = "center", tableOutput('logregtab'))),
                  htmlOutput("logisticint"),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      code('score  <- apply(data, 1, sum)'),
                      br(),
                      br(),
                      code('# Logistic model for item 1'),
                      br(),
                      code('fit <- glm(data[, 1] ~ score, family = binomial)'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)'),
                      br(),
                      code('# Function for plot'),
                      br(),
                      code('fun <- function(x, b0, b1){exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}'),
                      br(),
                      code('# Plot of estimated curve'),
                      br(),
                      code('curve(fun(x, b0 = coef(fit)[1], b1 = coef(fit)[2]), 0, 20,
                        xlab = "Total score",
                        ylab = "Probability of correct answer",
                           ylim = c(0, 1))')),
                  br()
                ),
                # LOGISTIC Z
                tabPanel("Logistic Z",
                  h3("Logistic Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Logistic regression'), 'can model dependency of probability of correct answer on
                    standardized total score (Z-score) by s-shaped logistic curve. Parameter ', strong( 'b0'), ' describes
                    horizontal position of the fitted curve (difficulty), parameter ', strong('b1'),' describes its slope at
                    inflection point (discrimination). '),
                  br(),
                  h4("Plot with Estimated Logistic Curve"),
                  p('Points represent proportion of correct answer with respect to standardized
total score. Their size is determined by count of respondents who answered item correctly.'),
                 # uiOutput("zlogregSliderUI"),
                  sliderInput("zlogregSlider", "Item Slider", min=1, value=1, max=10,
                              step=1, animate=TRUE),
                  plotOutput('zlogreg'),
                  downloadButton("DP_zlogreg", label = "Download figure"),
                  h4("Equation"),
                  ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1) = \\mathrm{E}(Y|Z, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 Z\\right) }}{1+e^{\\left( b_{0} + b_1 Z\\right) }} $$'),
                  h4("Table of parameters"),
                  fluidRow(column(12, align = "center", tableOutput('zlogregtab'))),
                  htmlOutput("zlogisticint"),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      code('stand.score  <- scale(apply(data, 1, sum))'),
                      br(),
                      br(),
                      code('# Logistic model for item 1'),
                      br(),
                      code('fit <- glm(data[, 1] ~ stand.score, family = binomial)'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)'),
                      br(),
                      code('# Function for plot'),
                      br(),
                      code('fun <- function(x, b0, b1){exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}'),
                      br(),
                      code('# Plot of estimated curve'),
                      br(),
                      code('curve(fun(x, b0 = coef(fit)[1], b1 = coef(fit)[2]), -3, 3,
                           xlab = "Standardized total score",
                           ylab = "Probability of correct answer",
                           ylim = c(0, 1))')),
                  br()
                  ),
                # LOGISTIC IRT Z
                tabPanel("Logistic IRT Z",
                  h3("Logistic Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                    strong('Logistic regression'), 'can model dependency of probability of correct answer on
                    standardized total score (Z-score) by s-shaped logistic curve. Note change in parametrization - the IRT parametrization
                    used here corresponds to the parametrization used in IRT models.
                    Parameter', strong('b') , 'describes horizontal position of the fitted curve (difficulty),
                    parameter' , strong('a') , ' describes its slope at inflection point (discrimination). '),
                  br(),
                  h4("Plot with Estimated Logistic Curve"),
                  p('Points represent proportion of correct answer with respect to standardized
total score. Their size is determined by count of respondents who answered item correctly.'),
                  #uiOutput("zlogreg_irtSliderUI"),
                  sliderInput("zlogreg_irtSlider", "Item Slider", min=1, value=1, max=10,
                              step=1, animate=TRUE),
                  plotOutput('zlogreg_irt'),
                  downloadButton("DP_zlogreg_irt", label = "Download figure"),
                  h4("Equation"),
                  ('$$\\mathrm{P}(Y = 1|Z, a, b) = \\mathrm{E}(Y|Z, a, b) = \\frac{e^{ a\\left(Z - b\\right) }}{1+e^{a\\left(Z - b\\right)}} $$'),
                  h4("Table of parameters"),
                  fluidRow(column(12, align = "center", tableOutput('zlogregtab_irt'))),
                  htmlOutput("zlogisticint_irt"),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      code('stand.score  <- scale(apply(data, 1, sum))'),
                      br(),
                      br(),
                      code('# Logistic model for item 1'),
                      br(),
                      code('fit <- glm(data[, 1] ~ stand.score, family = binomial)'),
                      br(),
                      code('# Coefficients - tranformation'),
                      br(),
                      code('coef <- c(a = coef(fit)[2], b = - coef(fit)[1] / coef(fit)[2])'),
                      br(),
                      code('coef'),
                      br(),
                      code('# Function for plot'),
                      br(),
                      code('fun <- function(x, a, b){exp(a * (x - b)) / (1 + exp(a * (x - b)))}'),
                      br(),
                      code('# Plot of estimated curve'),
                      br(),
                      code('curve(fun(x, a = coef[1], b = coef[2]), -3, 3,
                           xlab = "Standardized total score",
                           ylab = "Probability of correct answer",
                           ylim = c(0, 1))')),
                  br()
                  ),

                # NONLINEAR Z
                tabPanel("Nonlinear IRT Z",
                  h3("Nonlinear Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Nonlinear regression'), 'can model dependency of probability of correct answer on
                  standardized total score (Z-score) by s-shaped logistic curve. The IRT parametrization used here corresponds
                  to the parametrization used in IRT models. Parameter ', strong( 'b'),' describes horizontal position of the fitted curve (difficulty),
                  parameter ',strong( 'a'), ' describes its slope at inflection point (discrimination). This model allows for nonzero lower left asymptote ',strong( 'c'),'
                  (pseudo-guessing). '),
                  br(),
                  h4("Plot with Estimated Nonlinear Curve"),
                  p('Points represent proportion of correct answer with respect to standardized
total score. Their size is determined by count of respondents who answered item correctly.'),
                 # uiOutput("nlsSliderUI"),
                  sliderInput("nlsSlider", "Item Slider", min=1, value=1, max=10,
                              step=1, animate=TRUE),
                  plotOutput('nlsplot'),
                  downloadButton("DP_nlsplot", label = "Download figure"),
                  h4("Equation"),
                  ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1, c) = \\mathrm{E}(Y|Z, b_0, b_1, c) = c + \\left( 1-c \\right) \\cdot \\frac{e^{a\\left(Z-b\\right) }}{1+e^{a\\left(Z-b\\right) }} $$'),
                  h4("Table of parameters"),
                  fluidRow(column(12, align = "center", tableOutput('nonlinearztab'))),
                  htmlOutput("nonlinearint"),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      code('stand.score  <- scale(apply(data, 1, sum))'),
                      br(),
                      br(),
                      code('# NLR model for item 1'),
                      br(),
                      code('fun <- function(x, a, b, c){c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))}'),
                      br(),
                      code('fit <- nls(data[, 1] ~ fun(stand.score, a, b, c), algorithm = "port",
                           start = startNLR(data, GMAT[, "group"])[1, 1:3])'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)'),
                      br(),
                      code('# Plot of estimated curve'),
                      br(),
                      code('curve(fun(x, a = coef(fit)[1], b = coef(fit)[2], c = coef(fit)[3]), -3, 3,
                           xlab = "Standardized total score",
                           ylab = "Probability of correct answer",
                           ylim = c(0, 1))')),
                  br()
                  ),
                # MULTINOMIAL
                tabPanel("Multinomial",
                  h3("Multinomial Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Multinomial regression'),'allows for simultaneous modelling of probability of choosing
                  given distractors on standardized total score (Z-score).'),
                  br(),
                  h4("Plot with Estimated Curves of Multinomial Regression"),
                  p('Points represent proportion of selected option with respect to standardized
total score. Their size is determined by count of respondents who selected given option.'),
                  # uiOutput("multiSliderUI"),
                  sliderInput("multiSlider", "Item Slider", min=1, value=1, max=10,
                              step=1, animate=TRUE),
                  plotOutput('multiplot'),
                  downloadButton("DP_multiplot", label = "Download figure"),
                  h4("Equation"),
                  # ('$$\\mathrm{P}(Y = A|Z, b_{A0}, b_{A1}) = \\mathrm{P}(Y = D|Z, b_{D0}, b_{D1})\\cdot e^{\\left( b_{A0} + b_{A1} Z\\right) }$$'),
                  # ('$$\\mathrm{P}(Y = B|Z, b_{B0}, b_{B1}) = \\mathrm{P}(Y = D|Z, b_{D0}, b_{D1})\\cdot e^{\\left( b_{B0} + b_{B1} Z\\right) }$$'),
                  # ('$$\\mathrm{P}(Y = C|Z, b_{C0}, b_{C1}) = \\mathrm{P}(Y = D|Z, b_{D0}, b_{D1})\\cdot e^{\\left( b_{C0} + b_{C1} Z\\right) }$$'),
                  uiOutput('multieq'),
                  h4("Table of parameters"),
                  fluidRow(column(12, align = "center", tableOutput('multitab'))),
                  strong("Interpretation:"),
                  htmlOutput("multiint"),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('library(nnet)'),
                      br(),
                      code('data(GMAT)'),
                      br(),
                      code('data.scored  <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      code('stand.score  <- scale(apply(data, 1, sum))'),
                      br(),
                      code('data(GMATtest)'),
                      br(),
                      code('data  <- GMATtest[, colnames(GMATtest) != "group"]'),
                      br(),
                      code('data(GMATkey)'),
                      br(),
                      code('key  <- GMATkey'),

                      br(),
                      br(),
                      code('# multinomial model for item 1'),
                      br(),
                      code('fit <- multinom(relevel(data[, 1], ref = paste(key[1])) ~ stand.score)'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)')),
                  br()
                  )
                ),
                ###################
                # IRT MODELS ######
                ###################
                navbarMenu("IRT models",
                # 1PL (RASCH)
                tabPanel("1PL (Rasch)",
                  h3("One Parameter Item Response Theory Model"),
                  p('Item Response Theory (IRT) models are mixed-effect regression models in which
                    student ability (theta) is assumed to be a random effect and is estimated together with item
                    paramters. Ability (theta) is often assumed to follow normal distibution.'),
                  p('In',
                    strong('1PL IRT model,'), 'all items are assumed to have the same slope in inflection point – the
                    same discrimination', strong('a.'), 'Items can differ in location of their inflection point – in item difficulty',
                    strong('b.'), 'More restricted version of this model, the
                    ',strong('Rasch model,'),'assumes discrimination', strong('a'), 'is equal to 1.'),
                  h4("Equation"),
                  ('$$\\mathrm{P}\\left(Y_{ij} = 1\\vert \\theta_{i}, a, b_{j} \\right) =  \\frac{e^{a\\left(\\theta_{i}-b_{j}\\right) }}{1+e^{a\\left(\\theta_{i}-b_{j}\\right) }} $$'),
                  h4("Item Characteristic Curves"),
                  plotOutput('rasch'),
                  downloadButton("DP_rasch", label = "Download figure"),
                  h4("Item information curves"),
                  plotOutput('raschiic'),
                  downloadButton("DP_raschiic", label = "Download figure"),
                  h4("Test information function"),
                  plotOutput('raschtif'),
                  downloadButton("DP_raschtif", label = "Download figure"),
                  h4("Table of parameters"),
                  tableOutput('raschcoef'),
                  h4('Factor Scores vs. Standardized Total Scores'),
                  plotOutput('raschFactor'),
                  downloadButton("DP_raschFactor", label = "Download figure"),
                  br(),
                  h4("Selected R code"),
                  div(code('data(GMAT)'),
                      br(),
                      code('data <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      br(),
                      code('# Model'),
                      br(),
                      code('fit <- rasch(data)'),
                      br(),
                      code('# Item Characteristic Curves'),
                      br(),
                      code('plot(fit)'),
                      br(),
                      code('# Item Information Curves'),
                      br(),
                      code('plot(fit, type = "IIC")'),
                      br(),
                      code('# Test Information Function'),
                      br(),
                      code('plot(fit, items = 0, type = "IIC")'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)'),
                      br(),
                      code('# Factor scores vs Standardized total scores'),
                      br(),
                      code('df1  <- ltm::factor.scores(fit, return.MIvalues = T)$score.dat'),
                      br(),
                      code('FS   <- as.vector(df1[, "z1"])'),
                      br(),
                      code('df2  <- df1'),
                      br(),
                      code('df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL'),
                      br(),
                      code('STS <- as.vector(scale(apply(df2, 1, sum)))'),
                      br(),
                      code('df  <- data.frame(FS, STS)'),
                      br(),
                      code('plot(FS ~ STS, data = df,
         xlab = "Standardized total score",
         ylab = "Factor score")')),
                  br()
                  ),
                # 2PL
                tabPanel("2PL ",
                  h3("Two Parameter Item Response Theory Model"),
                  p('Item Response Theory (IRT) models are mixed-effect regression models in which
                    student ability (theta) is assumed to be a random effect and is estimated together with item
                    paramters. Ability (theta) is often assumed to follow normal distibution.'),
                  p(strong('2PL IRT model,'), 'allows for different slopes in inflection point – different
                    discriminations', strong('a.'), 'Items can also differ in location of their inflection point – in item difficulty',
                    strong('b.')),
                  h4("Equation"),
                  ('$$\\mathrm{P}\\left(Y_{ij} = 1\\vert \\theta_{i}, a_{j}, b_{j}\\right) =  \\frac{e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }}{1+e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }} $$'),

                  h4("Item Characteristic Curves"),
                  plotOutput('twoparam'),
                  downloadButton("DP_twoparam", label = "Download figure"),
                  h4("Item information curves"),
                  plotOutput("twoparamiic"),
                  downloadButton("DP_twoparamiic", label = "Download figure"),
                  h4("Test information function"),
                  plotOutput("twoparamtif"),
                  downloadButton("DP_twoparamtif", label = "Download figure"),
                  h4("Table of parameters"),
                  tableOutput('twoparamcoef'),
                  h4('Factor Scores vs. Standardized Total Scores'),
                  plotOutput('twoFactor'),
                  downloadButton("DP_twoFactor", label = "Download figure"),
                  br(),
                  h4("Selected R code"),
                  div(code('data(GMAT)'),
                      br(),
                      code('data <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      br(),
                      code('# Model'),
                      br(),
                      code('fit <- ltm(data ~ z1)'),
                      br(),
                      code('# Item Characteristic Curves'),
                      br(),
                      code('plot(fit)'),
                      br(),
                      code('# Item Information Curves'),
                      br(),
                      code('plot(fit, type = "IIC")'),
                      br(),
                      code('# Test Information Function'),
                      br(),
                      code('plot(fit, items = 0, type = "IIC")'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)'),
                      br(),
                      code('# Factor scores vs Standardized total scores'),
                      br(),
                      code('df1  <- ltm::factor.scores(fit, return.MIvalues = T)$score.dat'),
                      br(),
                      code('FS   <- as.vector(df1[, "z1"])'),
                      br(),
                      code('df2  <- df1'),
                      br(),
                      code('df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL'),
                      br(),
                      code('STS <- as.vector(scale(apply(df2, 1, sum)))'),
                      br(),
                      code('df  <- data.frame(FS, STS)'),
                      br(),
                      code('plot(FS ~ STS, data = df,
         xlab = "Standardized total score",
         ylab = "Factor score")')),
                  br()
                  ),
                # 3PL
                tabPanel("3PL ",
                  h3("Three Parameter Item Response Theory Model"),
                  p('Item Response Theory (IRT) models are mixed-effect regression models in which
                    student ability (theta) is assumed to be a random effect and is estimated together with item
                    paramters. Ability (theta) is often assumed to follow normal distibution.'),
                  p(strong('3PL IRT model,'), 'allows for different discriminations of items', strong('a,'),
                    'different item difficulties',
                    strong('b,'), 'and allows also for nonzero left asymptote – pseudo-guessing', strong('c.')),
                  h4("Equation"),
                  ('$$\\mathrm{P}\\left(Y_{ij} = 1\\vert \\theta_{i}, a_{j}, b_{j}, c_{j} \\right) = c_{j} + \\left(1 - c_{j}\\right) \\cdot \\frac{e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }}{1+e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }} $$'),

                  h4("Item characterisic curves"),
                  plotOutput('threeparam'),
                  downloadButton("DP_threeparam", label = "Download figure"),
                  h4("Item information curves"),
                  plotOutput("threeparamiic"),
                  downloadButton("DP_threeparamiic", label = "Download figure"),
                  h4("Test information function"),
                  plotOutput("threeparamtif"),
                  downloadButton("DP_threeparamtif", label = "Download figure"),
                  h4("Table of parameters"),
                  tableOutput("threeparamcoef"),
                  h4('Factor Scores vs. Standardized Total Scores'),
                  plotOutput('threeFactor'),
                  downloadButton("DP_threeFactor", label = "Download figure"),
                  br(),
                  h4("Selected R code"),
                  div(code('data(GMAT)'),
                      br(),
                      code('data <- GMAT[, colnames(GMAT) != "group"]'),
                      br(),
                      br(),
                      code('# Model'),
                      br(),
                      code('fit <- tpm(data)'),
                      br(),
                      code('# Item Characteristic Curves'),
                      br(),
                      code('plot(fit)'),
                      br(),
                      code('# Item Information Curves'),
                      br(),
                      code('plot(fit, type = "IIC")'),
                      br(),
                      code('# Test Information Function'),
                      br(),
                      code('plot(fit, items = 0, type = "IIC")'),
                      br(),
                      code('# Coefficients'),
                      br(),
                      code('coef(fit)'),
                      br(),
                      code('# Factor scores vs Standardized total scores'),
                      br(),
                      code('df1  <- ltm::factor.scores(fit, return.MIvalues = T)$score.dat'),
                      br(),
                      code('FS   <- as.vector(df1[, "z1"])'),
                      br(),
                      code('df2  <- df1'),
                      br(),
                      code('df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL'),
                      br(),
                      code('STS <- as.vector(scale(apply(df2, 1, sum)))'),
                      br(),
                      code('df  <- data.frame(FS, STS)'),
                      br(),
                      code('plot(FS ~ STS, data = df,
         xlab = "Standardized total score",
         ylab = "Factor score")')),
                  br()
                  )
                ),
                ###################
                # DIF/FAIRNESS ####
                ###################
                navbarMenu("DIF/Fairness",
                "Used methods",
                # TOTAL SCORES
                tabPanel("Total Scores",
                  h3("Total Scores"),
                  p('DIF is not about total scores! Two groups may have the same distribution of total scores, yet,
                    some item may function differently for two groups. Also, one of the groups may have signifficantly
                    lower total score, yet, it may happen that there is no DIF item!'),
                  h4("Summary of Total Scores for Groups"),
                  tableOutput('resultsgroup'),
                  h4("Histograms of Total Scores for Groups"),
                  #uiOutput("slider2group"),
                  sliderInput("inSlider2group", "Cut-Score", min=1, value=1, max=10,
                              step=1, animate=TRUE),
                  p('For selected cut-score, blue part of histogram shows students with total score
                    above the cut-score, grey column shows students with Total Score equal
                    to cut-score and red part of histogram shows students below the cut-score.'),
                  plotOutput('histbyscoregroup0'),
                  downloadButton("DP_histbyscoregroup0", label = "Download figure"),
                  plotOutput('histbyscoregroup1'),
                  downloadButton("DP_histbyscoregroup1", label = "Download figure"),
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
                # DELTA PLOTS
                tabPanel("Delta Plots",
                  h3("Delta Plot"),
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

                  plotOutput('deltaplot'),
                  downloadButton("DP_deltaplot", label = "Download figure"),
                  verbatimTextOutput("dp_text_normal"),
                  br(),
                  h4("Selected R code"),
                  div(code('library(difNLR)'),
                      br(),
                      code('library(deltaPlotR)'),
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
                         focal.name = 1, thr = "norm")'),
                      br(),
                      code('deltascores'),
                      br(),
                      code('# Delta plot'),
                      br(),
                      code('diagPlot(deltascores, thr.draw = T)')),
                  br()
                  ),
                # MANTEL-HAENSZEL
                tabPanel("Mantel-Haenszel",
                         tabsetPanel(
                         # Summary
                           tabPanel("Summary",
                                     h3("Mantel-Haenszel Test"),
                                     p('Mantel-Haenszel test is DIF detection method based on contingency
                                       tables that are calculated for each level of total score (Mantel &
                                       Haenszel, 1959).'),
                                     selectInput("correction_method_MZ_print", "Correction method",
                                                 c("BH" = "BH",
                                                   "Holm" = "holm",
                                                   "Hochberg" = "hochberg",
                                                   "Hommel" = "hommel",
                                                   "BY" = "BY",
                                                   "FDR" = "fdr",
                                                   "none" = "none"
                                                 ),
                                                 selected="BH"),
                                     verbatimTextOutput("print_DIF_MH"),
                                     br(),
                                    h4("Selected R code"),
                                    div(code('library(difNLR)'),
                                        br(),
                                        code('library(deltaPlotR)'),
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
             p.adjust.method = "BH")'),
                                        br(),
                                        code('fit')),
                                    br()
                           ),
                           tabPanel('Items',
                                    h3("Mantel-Haenszel Test"),
                                    p('Mantel-Haenszel test is DIF detection method based on contingency
                                       tables that are calculated for each level of total score (Mantel &
                                       Haenszel, 1959).'),
                                    h4('Contingency Tables'),
                                    #uiOutput("difMHSlider_itemUI"),
                                    sliderInput("difMHSlider_item", "Item", animate = TRUE,
                                                min = 1, max = 10, value = 1, step = 1),
                                    #uiOutput("difMHSlider_score"),
                                    sliderInput("difMHSlider_score", "Cut-Score", min = 0, max = 10,
                                                value = 1, step = 1),
                                    fluidRow(column(12, align = "center", tableOutput('table_DIF_MH'))),
                                    uiOutput('ORcalculation'),
                                    br(),
                                    h4("Selected R code"),
                                    div(code('library(difNLR)'),
                                        br(),
                                        code('library(deltaPlotR)'),
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
                                             p.adjust.method = "BH")'),
                                        br(),
                                        code('fit$alphaMH')),
                                    br()
                           )
                )
                ),
                # LOGISTIC
                tabPanel("Logistic",
                         tabsetPanel(
                           # Summary
                           tabPanel('Summary',
                                    h3('Logistic regression'),
                                    p('Logistic regression allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by adding a group
                                      specific intercept', strong('b2'), '(uniform DIF) and group specific interaction', strong('b3'), '(non-uniform DIF) into model and
                                      by testing for their significance.'),
                                    h4("Equation"),
                                    ('$$\\mathrm{P}\\left(Y_{ij} = 1 | X_i, G_i, b_0, b_1, b_2, b_3\\right) = \\frac{e^{b_0 + b_1 X_i + b_2 G_i + b_3 X_i G_i}}{1+e^{b_0 + b_1 X_i + b_2 G_i + b_3 X_i G_i}} $$'),
                                    radioButtons('type_print_DIF_logistic', 'Type',
                                                 c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                                 ),
                                                 'both'
                                    ),
                                    selectInput("correction_method_logSummary", "Correction method",
                                                c("BH" = "BH",
                                                  "Holm" = "holm",
                                                  "Hochberg" = "hochberg",
                                                  "Hommel" = "hommel",
                                                  "BY" = "BY",
                                                  "FDR" = "fdr",
                                                  "none" = "none"
                                                ),
                                                selected="BH"),
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
                   p.adjust.method = "BH")'),
                                        br(),
                                        code('fit')),
                                    br()
                                    ),
                           # Items
                           tabPanel('Items',
                                    h3('Logistic regression'),
                                    p('Logistic regression allows for detection of uniform and non-uniform DIF by adding a group
                        specific intercept', strong('b2'), '(uniform DIF) and group specific interaction', strong('b3'), '(non-uniform DIF) into model and
                        by testing for their significance.'),
                                    h4("Plot with Estimated DIF Logistic Curve"),
                                    p('Points represent proportion of correct answer with respect to standardized
total score. Their size is determined by count of respondents who answered item correctly.'),
                                    radioButtons('type_plot_DIF_logistic', 'Type',
                                                 c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                                 ),
                                                 'both'
                                    ),
                                    selectInput("correction_method_logItems", "Correction method",
                                                c("BH" = "BH",
                                                  "Holm" = "holm",
                                                  "Hochberg" = "hochberg",
                                                  "Hommel" = "hommel",
                                                  "BY" = "BY",
                                                  "FDR" = "fdr",
                                                  "none" = "none"),
                                                selected="BH"),
                                    # uiOutput("diflogSliderUI"),
                                    sliderInput("diflogSlider", "Item Slider", min=1, value=1, max=10,
                                                step=1, animate=TRUE),
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
                   p.adjust.method = "BH")'),
                                        br(),
                                        code('fit'),
                                        br(),

                                        code('# Plot of characteristic curve for item 1'),
                                        br(),
                                        code('plotDIFLogistic(data, group,
                type = "both",
                item =  1,
                IRT = F,
                p.adjust.method = "BH")'),
                                        br(),
                                        code('# Coefficients'),
                                        br(),
                                        code('fit$logitPar')),
                                    br()
                           )
                           )
                     ),

                # LOGISTIC Z
                tabPanel("Logistic IRT Z",
                  tabsetPanel(
                    # Summary
                    tabPanel('Summary',
                      h3('Logistic regression'),
                      p('Logistic regression allows for detection of uniform and non-uniform DIF (Swaminathan & Rogers, 1990) by adding a group
                        specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into model and
                        by testing for their significance.'),
                      h4("Equation"),
                      ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) = \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)
                       \\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}{1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)
                       \\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                      radioButtons('type_print_DIF_logistic_IRT_Z', 'Type',
                                   c("H0: Any DIF vs. H1: No DIF" = 'both',
                                     "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                     "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                     ),

                                   'both'
                                   ),
                      selectInput("correction_method_logzZSummary", "Correction method",
                                  c("BH" = "BH",
                                    "Holm" = "holm",
                                    "Hochberg" = "hochberg",
                                    "Hommel" = "hommel",
                                    "BY" = "BY",
                                    "FDR" = "fdr",
                                    "none" = "none"),
                                  selected="BH"),
                      verbatimTextOutput('print_DIF_logistic_IRT_Z'),
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
                          code('scaled.score <- scale(score)'),
                          br(),
                          br(),
                          code('# Logistic regression DIF detection method'),
                          br(),
                          code('fit <- difLogistic(Data = data, group = group, focal.name = 1,
                   type = "both",
                   match = scaled.score,
                   p.adjust.method = "BH")'),
                          br(),
                          code('fit')),
                      br()
                      ),
                    # Items
                    tabPanel('Items',
                      h3('Logistic regression'),
                      p('Logistic regression allows for detection of uniform and non-uniform DIF by adding a group
                        specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into model and
                        by testing for their significance.'),
                      h4("Plot with Estimated DIF Logistic Curve"),
                      p('Points represent proportion of correct answer with respect to standardized
total score. Their size is determined by count of respondents who answered item correctly.'),
                      radioButtons('type_plot_DIF_logistic_IRT_Z', 'Type',
                                   c("H0: Any DIF vs. H1: No DIF" = 'both',
                                     "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                     "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                     ),
                                   'both'
                                   ),
                      selectInput("correction_method_logZItems", "Correction method",
                                  c("BH" = "BH",
                                    "Holm" = "holm",
                                    "Hochberg" = "hochberg",
                                    "Hommel" = "hommel",
                                    "BY" = "BY",
                                    "FDR" = "fdr",
                                    "none" = "none"),
                                  selected="BH"),
                      # uiOutput("diflog_irtSliderUI"),
                      sliderInput("diflog_irtSlider", "Item Slider", min=1, value=1, max=10,
                                  step=1, animate=TRUE),
                      plotOutput('plot_DIF_logistic_IRT_Z'),
                      downloadButton("DP_plot_DIF_logistic_IRT_Z", label = "Download figure"),
                      h4("Equation"),
                      ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                       \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                       {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_logistic_IRT_Z'))),
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
                          code('scaled.score <- scale(score)'),
                          br(),
                          br(),
                          code('# Logistic regression DIF detection method'),
                          br(),
                          code('fit <- difLogistic(Data = data, group = group, focal.name = 1,
                               type = "both",
                               match = scaled.score,
                               p.adjust.method = "BH")'),
                          br(),
                          code('fit'),
                          br(),

                          code('# Plot of characteristic curve for item 1'),
                          br(),
                          code('plotDIFLogistic(data, group,
                                          type = "both",
                               item =  1,
                               IRT = T,
                               p.adjust.method = "BH")'),
                          br(),
                          code('# Coefficients for item 1 - recalculation'),
                          br(),
                          code('coef_old <- fit$logitPar[1, ]'),
                          br(),
                          code('coef <- c()'),
                          br(),
                          code('# a = b1, b = -b0/b1, adif = b3, bdif = -(b1b2-b0b3)/(b1(b1+b3))'),
                          br(),
                          code('coef[1] <- coef_old[2]'),
                          br(),
                          code('coef[2] <- -(coef_old[1] / coef_old[2])'),
                          br(),
                          code('coef[3] <- coef_old[4]'),
                          br(),
                          code('coef[4] <- -(coef_old[2] * coef_old[3] + coef_old[1] * coef_old[4] ) /
                            (coef_old[2] * (coef_old[2] + coef_old[4]))')),
                          br()
                      )
                    )
                  ),
                # NONLINEAR Z
                tabPanel("Nonlinear Z",
                  tabsetPanel(
                    # Summary
                    tabPanel('Summary',
                      h3('Nonlinear regression'),
                      p('Nonlinear regression model allows for nonzero lower asymptote - pseudoguessing',
                        strong('c.'), 'Similarly to logistic regression, also nonlinear regression allows for
                        detection of uniform and non-uniform DIF by adding a group specific intercept',
                        strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'),
                        '(non-uniform DIF) into the model and by testing for their significance.'),
                      h4("Equation"),
                      ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, c_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                       c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                       {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                      radioButtons('type_print_DIF_NLR', 'Type',
                                   c("H0: Any DIF vs. H1: No DIF" = 'both',
                                     "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                     "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                     ),
                                   'both'
                                   ),
                      selectInput("correction_method_nlrSummary", "Correction method",
                                  c("BH" = "BH",
                                    "Holm" = "holm",
                                    "Hochberg" = "hochberg",
                                    "Hommel" = "hommel",
                                    "BY" = "BY",
                                    "FDR" = "fdr",
                                    "none" = "none"),
                                  selected="BH"),
                      verbatimTextOutput('print_DIF_NLR'),
                      br(),
                      h4("Selected R code"),
                      div(code('library(difNLR)'),
                          br(),
                          code('data(GMAT)'),
                          br(),
                          code('Data <- GMAT[, 1:20]'),
                          br(),
                          code('group <- GMAT[, "group"]'),
                          br(),
                          br(),
                          code('# Nonlinear regression DIF method'),
                          br(),
                          code('fit <- difNLR(Data = Data, group = group, focal.name = 1,
model = "3PLcg", type = "both", p.adjust.method = "BH")'),
                          br(),
                          code('fit')),
                      br()
                      ),
                    # Items
                    tabPanel('Items',
                      h3('Nonlinear regression'),
                      p('Nonlinear regression model allows for nonzero lower asymptote - pseudoguessing',
                        strong('c.'), 'Similarly to logistic regression, also nonlinear regression allows
                        for detection of uniform and non-uniform DIF (Drabinova & Martinkova, 2016) by
                        adding a group specific intercept', strong('bDIF'), '(uniform DIF) and group specific
                        interaction', strong('aDIF'), '(non-uniform DIF) into the model and by testing for
                        their significance.'),
                      h4("Plot with Estimated DIF Nonlinear Curve"),
                      p('Points represent proportion of correct answer with respect to standardized
total score. Their size is determined by count of respondents who answered item correctly.'),
                      radioButtons('type_plot_DIF_NLR', 'Type',
                                   c("H0: Any DIF vs. H1: No DIF" = 'both',
                                     "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                     "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                     ),
                                   'both'
                                   ),
                      selectInput("correction_method_nlrItems", "Correction method",
                                  c("BH" = "BH",
                                    "Holm" = "holm",
                                    "Hochberg" = "hochberg",
                                    "Hommel" = "hommel",
                                    "BY" = "BY",
                                    "FDR" = "fdr",
                                    "none" = "none"),
                                  selected="BH"),
                      #goback
                      # uiOutput("difnlrSliderUI"),
                      sliderInput("difnlrSlider", "Item Slider", min=1, value=1, max=10,
                                  step=1, animate=TRUE),
                      plotOutput('plot_DIF_NLR'),
                      downloadButton("DP_plot_DIF_NLR", label = "Download figure"),
                      h4("Equation"),
                      ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, c_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                       c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                       {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                      h4("Table of parameters"),
                      fluidRow(column(12, align = "center", tableOutput('tab_coef_DIF_NLR'))),
                      br(),
                      h4("Selected R code"),
                      div(code('library(difNLR)'),
                          br(),
                          code('data(GMAT)'),
                          br(),
                          code('Data <- GMAT[, 1:20]'),
                          br(),
                          code('group <- GMAT[, "group"]'),
                          br(),
                          br(),
                          code('# Nonlinear regression DIF method'),
                          br(),
                          code('fit <- difNLR(Data = Data, group = group, focal.name = 1,
model = "3PLcg", type = "both", p.adjust.method = "BH")'),
                          br(),
                          code('# Plot of characteristic curve of item 1'),
                          br(),
                          code('plot(fit, item = 1)'),
                          br(),
                          code('# Coefficients'),
                          br(),
                          code('fit$nlrPAR')),
                      br()
                      )
                    )
                ),
                # IRT LORD
                tabPanel("IRT Lord",
                  tabsetPanel(
                    tabPanel('Summary',
                             h3('Lord Test'),
                             p('Lord test (Lord, 1980) is based on IRT model
                                  (1PL, 2PL, or 3PL with the same guessing). It uses the
                                  difference between item parameters for the two groups
                                  to detect DIF. In statistical terms, Lord statistic is
                                  equal to Wald statistic.'),
                             br(),
                             img(src = 'lord_udif.png', width = 380, align = "left"),
                             img(src = 'lord_nudif.png', width = 380, align = "right"),
                             radioButtons('type_print_DIF_IRT_lord', 'Model',
                                          c("1PL" = '1PL',
                                            "2PL" = '2PL',
                                            "3PL" = '3PL'
                                          ),
                                          '2PL'
                             ),
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
                                 code('# 2PL IRT MODEL'),
                                 br(),
                                 code('fit <- difLord(Data = data, group = group, focal.name = 1,
               model = "2PL",
               p.adjust.method = "BH")'),
                                 br(),
                                 code('fit')),
                             br()
                             ),
                  tabPanel('Items',
                           h3('Lord Test'),
                           p('Lord test (Lord, 1980) is based on IRT model
                             (1PL, 2PL, or 3PL with the same guessing). It uses the
                             difference between item parameters for the two groups
                             to detect DIF. In statistical terms, Lord statistic is
                             equal to Wald statistic.'),
                           br(),
                           h3('Plot with Estimated DIF Characteristic Curve'),
                           radioButtons('type_plot_DIF_IRT_lord', 'Model',
                                        c("1PL" = '1PL',
                                          "2PL" = '2PL',
                                          "3PL" = '3PL'
                                        ),
                                        '2PL'
                           ),
                           #uiOutput("difirt_lord_itemSliderUI"),
                           sliderInput("difirt_lord_itemSlider", "Item Slider", min=1, value=1, max=10,
                                       step=1, animate=TRUE),
                           plotOutput('plot_DIF_IRT_Lord'),
                           downloadButton("DP_plot_DIF_IRT_Lord", label = "Download figure"),
                           h4("Equation"),
                           uiOutput('irtint_lord'),
                           uiOutput('irteq_lord'),
                       #     p('As the parameters are estimated in ', code("difR"), 'package separately for
                       #                groups, there is one equation for each group. Parameters ', strong('aR'), ' and ',
                       #       strong('bR'), 'are discrimination and difficulty for reference group. Parameters ', strong('aF'), ' and ',
                       #       strong('bF'), 'are discrimination and difficulty for reference group. Parameter ', strong('c'), ' is
                       #                a common guessing parameter.'),
                       #     ('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, a_{Rj}, b_{Rj}, c_j\\right) =
                       # c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}}
                       # {1+e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}} $$'),
                       #     ('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, a_{Fj}, b_{Fj}, c_j\\right) =
                       # c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}}
                       # {1+e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}} $$'),
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
                               code('# 2PL IRT MODEL'),
                               br(),
                               code('fit <- difLord(Data = data, group = group, focal.name = 1,
               model = "2PL",
               p.adjust.method = "BH")'),
                               br(),
                               code('fit'),
                               br(),
                               code('# Coefficients for item 1'),
                               br(),
                               code('tab_coef <- fit$itemParInit[c(1, ncol(data) + 1), 1:2]'),
                               br(),
                               code('# Plot of characteristic curve of item 1'),
                               br(),
                               code('plotDIFirt(parameters = tab_coef, item = 1)')),
                           br()
                           )
                  )
                  ),
                # IRT RAJU
                tabPanel("IRT Raju",
                         tabsetPanel(
                           tabPanel('Summary',
                                    h3('Raju Test'),
                                    p('Raju test (Raju, 1988, 1990) is based on IRT
                                      model (1PL, 2PL, or 3PL with the same guessing). It
                                      uses the area between the item charateristic curves
                                      for the two groups to detect DIF.'),
                                    br(),
                                    img(src = 'raju_udif.png', width = 380, align = "left"),
                                    img(src = 'raju_nudif.png', width = 380, align = "right"),
                                    radioButtons('type_print_DIF_IRT_raju', 'Model',
                                                 c("1PL" = '1PL',
                                                   "2PL" = '2PL',
                                                   "3PL" = '3PL'
                                                 ),
                                                 '2PL'
                                    ),
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
                                        code('# 2PL IRT MODEL'),
                                        br(),
                                        code('fit <- difRaju(Data = data, group = group, focal.name = 1,
               model = "2PL",
               p.adjust.method = "BH")'),
                                        br(),
                                        code('fit')),
                                    br()),
                           tabPanel('Items',
                                    h3('Raju Test'),
                                    p('Raju test (Raju, 1988, 1990) is based on IRT
                                      model (1PL, 2PL, or 3PL with the same guessing). It
                                      uses the area between the item charateristic curves
                                      for the two groups to detect DIF.'),
                                    br(),
                                    h3('Plot with Estimated DIF Characteristic Curve'),
                                    radioButtons('type_plot_DIF_IRT_raju', 'Model',
                                                 c("1PL" = '1PL',
                                                   "2PL" = '2PL',
                                                   "3PL" = '3PL'
                                                 ),
                                                 '2PL'
                                    ),
                                    #uiOutput("difirt_raju_itemSliderUI"),
                                    sliderInput("difirt_raju_itemSlider", "Item Slider", min=1, value=1, max=10,
                                                step=1, animate=TRUE),
                                    plotOutput('plot_DIF_IRT_Raju'),
                                    downloadButton("DP_plot_DIF_IRT_Raju", label = "Download figure"),
                                    h4("Equation"),
                                    uiOutput('irtint_raju'),
                                    uiOutput('irteq_raju'),
                       #              p('As the parameters are estimated in ', code("difR"), 'package separately for
                       #                groups, there is one equation for each group. Parameters ', strong('aR'), ' and ',
                       #                strong('bR'), 'are discrimination and difficulty for reference group. Parameters ', strong('aF'), ' and ',
                       #                strong('bF'), 'are discrimination and difficulty for reference group. Parameter ', strong('c'), ' is
                       #                a common guessing parameter.'),
                       #              ('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 0, a_{Rj}, b_{Rj}, c_j\\right) =
                       # c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}}
                       # {1+e^{a_{Rj} \\left(\\theta_i - b_{Rj} \\right)}} $$'),
                       #              ('$$\\mathrm{P}\\left(Y_{ij} = 1 | \\theta_i, G_i = 1, a_{Fj}, b_{Fj}, c_j\\right) =
                       # c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}}
                       # {1+e^{a_{Fj} \\left(\\theta_i - b_{Fj} \\right)}} $$'),

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
                                        code('# 2PL IRT MODEL'),
                                        br(),
                                        code('fit <- difRaju(Data = data, group = group, focal.name = 1,
               model = "2PL",
               p.adjust.method = "BH")'),
                                        br(),
                                        code('fit'),
                                        br(),
                                        code('# Coefficients for item 1'),
                                        br(),
                                        code('tab_coef <- fit$itemParInit[c(1, ncol(data) + 1), 1:2]'),
                                        br(),
                                        code('# Plot of characteristic curve of item 1'),
                                        br(),
                                        code('plotDIFirt(parameters = tab_coef, item = 1, test = "Raju")')),
                                    br())
                           )
                       ),
                # DDF
                tabPanel("DDF",
                         tabsetPanel(
                           # Summary
                           tabPanel('Summary',
                                    h3('Differential Distractor Functioning with Multinomial Log-linear Regression Model'),
                                    p('Here will be description...'),
                                    h4("Equation"),
                                    p('Here will be equation...'),
                                    radioButtons('type_print_DDF', 'Type',
                                                 c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                                 ),
                                                 'both'
                                    ),
                                    selectInput("correction_method_print_DDF", "Correction method",
                                                c("BH" = "BH",
                                                  "Holm" = "holm",
                                                  "Hochberg" = "hochberg",
                                                  "Hommel" = "hommel",
                                                  "BY" = "BY",
                                                  "FDR" = "fdr",
                                                  "none" = "none"),
                                                selected = "BH"),
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
                                             p.adjust.method = "BH")'),
                                        br(),
                                        code('fit')),
                                    br()
                           ),
                           # Items
                           tabPanel('Items',
                                    h3('Differential Distractor Functioning with Multinomial Log-linear Regression Model'),
                                    p('Here will be description...'),
                                    h4("Plot with Estimated DDF Curves"),
                                    p('Points represent proportion of selected answer with respect to standardized
                                      total score. Size of points is determined by count of respondents who chose particular
                                      answer.'),
                                    radioButtons('type_plot_DDF', 'Type',
                                                 c("H0: Any DIF vs. H1: No DIF" = 'both',
                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                                 ),
                                                 'both'
                                    ),
                                    selectInput("correction_method_plot_DDF", "Correction method",
                                                c("BH" = "BH",
                                                  "Holm" = "holm",
                                                  "Hochberg" = "hochberg",
                                                  "Hommel" = "hommel",
                                                  "BY" = "BY",
                                                  "FDR" = "fdr",
                                                  "none" = "none"),
                                                selected = "BH"),

                                    sliderInput("ddfSlider", "Item Slider", min = 1, value = 1, max = 10,
                                                step = 1, animate = TRUE),
                                    plotOutput('plot_DDF'),
                                    downloadButton("DP_plot_DDF", label = "Download figure"),
                                    h4("Equation"),
                                    p('Here will be equation'),
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
                             p.adjust.method = "BH")'),
                                        br(),
                                        code('# Estimated coefficients of item 1'),
                                        br(),
                                        code('fit$nlrPAR[[1]]')),
                                    br()
                                    )
                         )
                ),

                "----",
                # SUMMARY
                "Description",
                tabPanel('About DIF',
                         h3('Differential Item Functioning / Item Fairness'),
                         p('Differential item functioning (DIF) occurs when people from different
                           groups (commonly gender or ethnicity) with the same underlying true
                           ability have a different probability of answering the item correctly.
                           If item functions differently for two groups, it is potentially unfair.
                           In general, two type of DIF can be recognized: if the item has different
                           difficulty for given two groups with the same discrimination, ',
                           strong('uniform'), 'DIF is present (left figure). If the item has different
                           discrimination and possibly also different difficulty for given two groups, ',
                           strong('non-uniform'), 'DIF is present (right figure)'),
                         br(),
                         fluidRow(
                           column(6, align="center",
                                  img(src = 'fig_NLR_uniformDIF.png', width = 380)),
                           column(6, align="center",
                                  img(src = 'fig_NLR_nonuniformDIF.png', width = 380))
                         ),
                         br(),
                         br()
                )
                ),
                ###################
                # DATA ############
                ###################
                tabPanel("Data",
                  h3("Data"),
                  p('For demonstration purposes, 20-item dataset ' , code("GMAT"),'
                    and dataset', code("GMATkey"),' from R ', code('library(difNLR)'),' are used.
                    On this page, you may select one of three dataset offered in ', code('difNLR'),
                    'package or you may upload your own dataset (see below). To return to demonstration dataset,
                    refresh this page in your browser' , strong("(F5)"), '.'),
                  p('Used dataset ', code("GMAT"), ' is generated based on parameters of real Graduate Management
                    Admission Test (GMAT) data set (Kingston et al., 1985). However, first two items were
                    generated to function differently in uniform and non-uniform way respectively.
                    The data set represents responses of 2,000 subjects to multiple-choice test of 20 items.
                    The distribution of total scores is the same for both groups. '),
                  p('Dataset ', code("GMAT2"), ' is also generated based on parameters of GMAT (Kingston et al., 1985). Again,
                    first two items were generated to function differently in uniform and non-uniform way respectively.
                    The data set represents responses of 1,000 subjects to multiple-choice test of 20 items. '),
                  p('Dataset ', code("Medical"), ' is a subset of real admission test to medical school. First item was previously
                    detected as functioning differently. The data set represents responses of
                    1,407 subjects (484 males, 923 females) to multiple-choice test of 20 items. For more details of item selection see
                    Drabinova & Martinkova (2016).'),
                  br(),
                  selectInput("dataSelect", "Select dataset",
                              c("GMAT" = "GMAT_difNLR",
                                "GMAT2" = "GMAT2_difNLR",
                                "Medical 20 DIF" = "difMedical_difNLR",
                                "Medical 100" = "dataMedical_ShinyItemAnalysis"
                              ),
                              selected="GMAT_difNLR"),
                  h4("Upload your own datasets"),
                  p('Main dataset should contain responses of individual students (rows) to given items (collumns).
                    Header may contain item names, no row names should be included. If responses are in ABC format,
                    the key provides correct respomse for each item. If responses are scored 0-1, key is vecor of 1s.'),
                  fluidRow(
                    column(4, offset = 0, fileInput(
                      'data', 'Choose data (csv file)',
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain',
                                 '.csv',
                                 '.tsv'
                                 )
                      )
                      ),
                    column(4, offset = 1, fileInput(
                      'key', 'Choose key (csv file)',
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain',
                                 '.csv',
                                 '.tsv'
                                 )
                      )
                      ),
                    column(4, fileInput(
                      'groups', 'Choose groups for DIF (optional)',
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain',
                                 '.csv',
                                 '.tsv'
                                 )
                      )
                      )
                    ),
                  tags$hr(),
                  h4("Data Specification"),
                  fluidRow(
                    column(1, offset = 0, checkboxInput('header', 'Header', TRUE)),
                    column(3, offset = 1, radioButtons('sep', 'Separator',
                                                       c(Comma = ',',
                                                         Semicolon = ';',
                                                         Tab = '\t'
                                                         ),
                                                       ','
                                                       )
                           ),
                    column (3, offset = 0, radioButtons('quote', 'Quote',
                                                        c(None = '',
                                                          'Double Quote' = '"',
                                                          'Single Quote' = "'"
                                                          ),
                                                        '"'
                                                        )
                            )
                    ),
                  tags$hr(),
                  h4("Data Check"),
                  dataTableOutput('headdata'),
                  h4("Key (correct answers)"),
                  dataTableOutput('key'),
                  h4("Scored Test"),
                  dataTableOutput('sc01')
                  ),

                ########################
                # REPORTS ##############
                ########################
                tabPanel("Reports",
                         h3("Download Report"),
                         p("This shiny app also offers an option to download a report in HTML or PDF format."),
                         p("PDF report creation requires latest version of MiKTeX (or other TeX distribution).
                           If you don't have the latest installation, please, use the HTML report"),
                         # sliderInput("reportSlider", "Choose item for Report", min=1, value=1, max=10,
                         #             step=1, animate=TRUE),
                         radioButtons("report_format", "Format of Report",
                                     c("HTML" = "html",
                                       "PDF" = "pdf")),
                         radioButtons("irt_type_report", "IRT Model selection",
                                      c("1PL" = "1pl",
                                        "2PL" = "2pl",
                                        "3PL" = "3pl"),
                                      selected = "1pl"),
                         downloadButton("report", "Generate Report"),
                         p(strong("Warning"), ": download of Reports takes some time. Please, be patient.")
                         ),

                ########################
                # ABOUT ################
                ########################
                tabPanel("About",
                  br(),
                  strong('Version'),
                  p("ShinyItemAnalysis Version 1.0"),
                  p('ShinyItemAnalysis Version 0.1 is available',
                    a("here.", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV01/",
                      target = "_blank")),
                  br(),
                  strong('Description'),
                  p('ShinyItemAnalysis provides analysis of tests and their items.
                    It is based on the', a("Shiny", href = "http://www.rstudio.com/shiny/", target = "_blank"),
                    'R package. '),
                  br(),
                  strong('Data'),
                  p('For demonstration purposes, practice dataset from', code('library(difNLR)'),'is used.
                    On page', strong("Data"), 'you may select your own dataset '),
                  br(),
                  strong('List of Packages Used'),
                  br(),
                  code('library(corrplot)'), br(),
                  code('library(CTT)'), br(),
                  code('library(deltaPlotR)'), br(),
                  code('library(difNLR)'), br(),
                  code('library(difR)'), br(),
                  code('library(foreign)'), br(),
                  code('library(ggplot2)'), br(),
                  code('library(gridExtra)'), br(),
                  code('library(ltm)'), br(),
                  code('library(moments)'), br(),
                  code('library(nnet)'), br(),
                  code('library(psych)'), br(),
                  code('library(psychometric)'), br(),
                  code('library(reshape2)'), br(),
                  code('library(shiny)'), br(),
                  code('library(shinyAce)'), br(),
                  code('library(stringr)'), br(),
                  code('library(rmarkdown)'), br(),
                  br(),
                  strong('Authors'),
                  br(),

                  img(src = 'patricia.jpg', width = 70),
                  p(a("Patricia Martinkova, Institute of Computer Science, Czech Academy of Sciences",
                      href = "http://www.cs.cas.cz/martinkova/", target = "_blank")),

                  img(src = 'adela.jpg', width = 70),
                  p("Adela Drabinova"),

                  img(src = 'leder.png', width = 70),
                  p(a("Ondrej Leder", href = "https://www.linkedin.com/in/ond%C5%99ej-leder-3864b1119",
                      target = "_blank")),

                  img(src = 'jakub.jpg', width = 70),
                  p("Jakub Houdek"),

                  strong('Bug Reports'),
                  p("If you discover a problem with this application please contact the project maintainer
                    at martinkova(at)cs.cas.cz "
                    ),
                  strong('Acknowledgments'),
                  p(" Project was supported by grant funded by Czech Science foundation under number ",
                    a("GJ15-15856Y", href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
                      target = "_blank")
                    ),
                  br(),
                  strong('License'),
                  p(" Copyright 2016  Patricia Martinkova, Adela Drabinova, Ondrej Leder and Jakub Houdek"),
                  p(" This program is free software you can redistribute it and or modify it under the terms of the GNU
                    General Public License as published by the Free Software Foundation either version 3 of the License or
                    at your option any later version."
                    ),
                  p("This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
                    even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
                    Public License for more details."
                    ),
                  br(),
                  br()
                  ),
                  ########################
                  # REFERENCES ###########
                  ########################
                  tabPanel("References",
                    br(),
                    strong('References'),
                    br(),
                    br(),
                    p('Angoff, W. H., & Ford, S. F. (1973). Item‐Race Interaction on a Test of
                      Scholastic Aptitude. Journal of Educational Measurement, 10(2), 95-105.'
                      ),
                    p('Cronbach, L. J. (1951). Coefficient alpha and the internal structure
                      of tests. psychometrika, 16(3), 297-334.'
                      ),
                    p("Drabinova, A., & Martinkova, P. (2016). Detection of Differential Item
                      Functioning Based on Non-Linear Regression. Technical Report",
                      a("V-1229", href = "https://goo.gl/R3dpJ5}", target = "_blank"), "."
                      ),
                    p("Lord, F. M. (1980). Applications of Item Response Theory to Practical Testing Problems.
                      Routledge."
                      ),
                    p("Magis, D., & Facon, B. (2012). Angoff's delta method revisited:
                      Improving DIF detection under small samples. British Journal of
                      Mathematical and Statistical Psychology, 65(2), 302-321.
                      "),
                    p("Mantel, N., & Haenszel, W. (1959). Statistical Aspects of the Analysis of Data from
                      Retrospective Studies. Journal of the National Cancer Institute, 22 (4), 719-748."
                      ),
                    p("Swaminathan, H., & Rogers, H. J. (1990). Detecting Differential Item
                      Functioning Using Logistic Regression Procedures. Journal of Educational
                      Measurement, 27(4), 361-370."
                      ),
                    p("Raju, N. S. (1988). The Area between Two Item Characteristic Curves. Psychometrika,
                      53 (4), 495-502."
                      ),
                    p("Raju, N. S. (1990). Determining the Significance of Estimated Signed and Unsigned Areas
                    between Two Item Response Functions. Applied Psychological Measurement, 14 (2), 197-207."
                      ),
                    # p("Vlckova, K. (2014). Test and Item Fairness (Unpublished master's thesis)."
                    #   ),
                    br(),
                    br()
                  )
#)
#)
)
)
