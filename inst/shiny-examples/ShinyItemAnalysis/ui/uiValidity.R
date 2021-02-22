uiValidity <-
  navbarMenu(
    "Validity",
    # * CORRELATION STRUCTURE ####
    tabPanel(
      "Correlation structure",
      h3("Correlation structure"),
      h4("Correlation heat map"),
      p(
        "A correlation heat map displays selected type of",
        HTML("<b>correlations</b>"), "between items. The size and shade of circles indicate how much the
                        items are correlated (larger and darker circle mean greater correlations).
                        The color of circles indicates in which way the items are correlated - a blue
                        color means possitive correlation and a red color means negative correlation.
                        A correlation heat map can be reordered using a hierarchical",
        HTML("<b>clustering method</b>"), "selected below. With a ", HTML("<b>number  of clusters</b>"), "larger than 1, the rectangles representing
                        clusters are drawn. The values of a correlation heatmap may be displayed and also downloaded."
      ),
      fluidRow(
        column(2, selectInput(
          inputId = "type_of_corr",
          label = "Choose correlation",
          choices = c(
            "Pearson" = "pearson",
            "Spearman" = "spearman",
            "Polychoric" = "polychoric"
          ),
          selected = "polychoric"
        )),
        column(2, selectInput(
          inputId = "corr_plot_clustmethod",
          label = "Clustering method",
          choices = list(
            "None" = "none",
            "Ward's" = "ward.D",
            "Ward's n. 2" = "ward.D2",
            "Single" = "single",
            "Complete" = "complete",
            "Average" = "average",
            "McQuitty" = "mcquitty",
            "Median" = "median",
            "Centroid" = "centroid"
          ),
          selected = "none"
        )),
        column(2, numericInput(
          inputId = "corr_plot_clust",
          label = "Number of clusters",
          value = 0,
          min = 0,
          max = 1
        )),
        column(
          2, br(),
          checkboxInput("show_corr",
            label = "Display correlation values",
            value = FALSE
          )
        ),
        column(
          2,
          numericInput("corr_plot_labs_size",
            label = "Values display size",
            min = 1, value = 3, max = 10, step = .1
          )
        )
      ),
      conditionalPanel(
        condition = "input.type_of_corr == 'pearson'",
        p(HTML("<b>Pearson correlation coefficient</b>"), "describes the strength and direction of a linear relationship between
                                         two random variables \\(X\\) and \\(Y\\). It is given by formula"),
        withMathJax(),
        ("$$\\rho = \\frac{cov(X,Y)}{\\sqrt{var(X)}\\sqrt{var(Y)}}.$$"),
        p("Sample Pearson corelation coefficient may be calculated as"),
        withMathJax(),
        ("$$ r = \\frac{\\sum_{i = 1}^{n}(x_{i} - \\bar{x})(y_{i} - \\bar{y})}{\\sqrt{\\sum_{i = 1}^{n}(x_{i} - \\bar{x})^2}\\sqrt{\\sum_{i = 1}^{n}(y_{i} - \\bar{y})^2}}$$"),
        p("Pearson correlation coefficient has a value between -1 and +1. Sample correlation of -1 and +1 correspond to all data points lying exactly on a line
                                         (decreasing in case of negative linear correlation -1 and increasing for +1). If the coefficient is
                                         equal to 0, it means there is no linear relationship between the two variables.")
      ),
      conditionalPanel(
        condition = "input.type_of_corr == 'polychoric'",
        p("A ", HTML("<b>polychoric/tetrachoric correlation</b>"), "between two ordinal/binary variables is calculated from their contingency table,
                                         under the assumption that the ordinal variables dissect continuous latent variables that are bivariate normal.")
      ),
      conditionalPanel(
        condition = "input.type_of_corr == 'spearman'",
        p("The ", HTML("<b>Spearman's rank correlation coefficient</b>"), "describes the strength and the direction of a monotonic relationship between random variables \\(X\\)
                                         and \\(Y\\), i.e. the dependence between the rankings of two variables. It is given by formula"),
        withMathJax(),
        ("$$\\rho = \\frac{cov(rg_{X},rg_{Y})}{\\sqrt{var(rg_{X})}\\sqrt{var(rg_{Y})}},$$"),
        p("where \\(rg_{X}\\) and \\(rg_{Y}\\) are the transformed random variables \\(X\\) and \\(Y\\) into ranks, i.e, the  Spearman correlation coefficient is the Pearson correlation coefficient between the ranked variables."),
        p("The sample Spearman correlation is calculated by converting \\(X\\) and \\(Y\\) to ranks (average ranks are used in case of ties) and by applying the sample Pearson correlation formula. If both the \\(X\\) and \\(Y\\) have \\(n\\) unique ranks, i.e. there are no ties, then the sample correlation coefficient is given by formula"),
        withMathJax(),
        ("$$ r = 1 - \\frac{6\\sum_{i = 1}^{n}d_i^{2}}{n(n-1)}$$"),
        p("where \\(d = rg_{X} - rg_{Y}\\) is the difference between two ranks and \\(n\\) is size of \\(X\\) and \\(Y\\).
                                         Spearman rank correlation coefficient has value between -1 and 1, where 1  means identity of ranks
                                         of the variables and -1 means reverse ranks of the two variables.
                                         In case of no repeated values, Spearman correlation of +1 or -1 means that all data points are lying exactly on some monotone line.
                                         If the Spearman coefficient is equal to 0, it means there is no tendency for \\(Y\\) to either increase or decrease with \\(X\\) increasing.")
      ),
      p(
        HTML("<b>Clustering methods.</b>"),
        "Ward's method aims at finding compact clusters based on minimizing the within-cluster
                        sum of squares.
                        Ward's n. 2 method uses squared disimilarities.
                        The Single method connects clusters with their nearest neighbours, i.e. the distance between
                        two clusters is calculated as the minimum of the distance of observations in one cluster and
                        observations in the other clusters.
                        Complete linkage with the farthest neighbours, on the other hand, uses the maximum of distance.
                        The Average linkage method uses the distance based on a weighted average of the individual distances.
                        The McQuitty method uses an unweighted average.
                        The Median linkage calculates the distance as the median of distance between an observation
                        in one cluster and observation in another cluster.
                        The Centroid method uses the distance between centroids of clusters. "
      ),
      uiOutput("corr_na_alert"),
      br(),
      fluidRow(column(8, offset = 2, plotlyOutput("corr_plot", height = "700px"))),
      br(),
      downloadButton(outputId = "DB_corr_plot", label = "Download figure"),
      # download correlation matrix button
      tags$style(HTML("#corr_matrix { margin: 10px }")),
      downloadButton(outputId = "corr_matrix", label = "Download matrix"),
      br(),
      conditionalPanel(
        condition = "input.corr_plot_clustmethod != 'none'",
        h4("Dendrogram"),
        plotlyOutput("dendrogram_plot"),
        downloadButton(outputId = "DB_dendrogram", label = "Download figure")
      ),
    ),
    # * FACTOR ANALYSIS ####
    tabPanel("Factor analysis",
             h3("Factor analysis"),
             h4("Scree plot"),
             p('A scree plot displays the eigenvalues associated with an component or a factor in descending order
               versus the number of the component or factor. Location of a bend (an elbow) suggests a suitable number of factors.'),
             plotOutput('scree_plot'),
             downloadButton(outputId = "DB_scree_plot", label = "Download figure"),
             h4("Selected R code"),
             div(code(HTML("library(difNLR)&nbsp;<br>library(psych)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT)&nbsp;<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]&nbsp;<br><br>#&nbsp;scree&nbsp;plot&nbsp;<br>ev&nbsp;<-&nbsp;eigen(corP$rho)$values&nbsp;#&nbsp;eigen&nbsp;values<br>df&nbsp;<-&nbsp;data.frame(comp&nbsp;=&nbsp;1:length(ev),&nbsp;ev)<br><br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;comp,&nbsp;y&nbsp;=&nbsp;ev))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_point()&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Eigen&nbsp;value\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Component&nbsp;number\")&nbsp;+<br>&nbsp;&nbsp;theme_app()"))),
             br()
             ),
    # * PREDICTIVE VALIDITY ####
    tabPanel(
      "Criterion validity",
      tabsetPanel(
        # ** Summary ####
        tabPanel("Summary",
          value = "val_summary",
          h3("Criterion validity"),
          p("Depending on the criterion variable, different types of criterion validity may be examined. As an example,
                                   a correlation between the test score and the future study success or future GPA may
                                   be  used as a proof of predictive validity in the case of admission tests. A criterion
                                   variable may be uploaded in the ", strong("Data"), "section."),

          h4("Descriptive plots of criterion variable on total score"),
          p("Total scores are plotted according to a criterion variable. Boxplot or scatterplot is displayed
                                   depending on the type of criterion variable - whether it is discrete or continuous. Scatterplot is
                                   provided with a red linear regression line. "),
          plotlyOutput("validity_plot"),
          downloadButton(outputId = "DB_validity_plot", label = "Download figure"),
          h4("Correlation of criterion variable and total score"),
          p("A test for association between the total score and the criterion variable is based on Spearman`s \\(\\rho\\).
                                   This rank-based measure has been recommended if bivariate normal distribution is not guaranteed.
                                   The null hypothesis being tested states that correlation is 0. "),
          tableOutput("validity_table"),
          htmlOutput("validity_table_interpretation"),
          h4("Selected R code"),
          div(code(HTML("library(ggplot2)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>score&nbsp;<-&nbsp;rowSums(data)&nbsp;#&nbsp;total&nbsp;score&nbsp;calculation<br>criterion&nbsp;<-&nbsp;GMAT[,&nbsp;\"criterion\"]&nbsp;#&nbsp;criterion&nbsp;variable<br><br>#&nbsp;number&nbsp;of&nbsp;respondents&nbsp;in&nbsp;each&nbsp;criterion&nbsp;level<br>size&nbsp;<-&nbsp;as.factor(criterion)<br>levels(size)&nbsp;<-&nbsp;table(as.factor(criterion))<br>size&nbsp;<-&nbsp;as.numeric(paste(size))<br>df&nbsp;<-&nbsp;data.frame(score,&nbsp;criterion,&nbsp;size)<br><br>#&nbsp;descriptive&nbsp;plots<br>###&nbsp;boxplot,&nbsp;for&nbsp;discrete&nbsp;criterion<br>ggplot(df,&nbsp;aes(y&nbsp;=&nbsp;score,&nbsp;x&nbsp;=&nbsp;as.factor(criterion),&nbsp;fill&nbsp;=&nbsp;as.factor(criterion)))&nbsp;+<br>&nbsp;&nbsp;geom_boxplot()&nbsp;+<br>&nbsp;&nbsp;geom_jitter(shape&nbsp;=&nbsp;16,&nbsp;position&nbsp;=&nbsp;position_jitter(0.2))&nbsp;+<br>&nbsp;&nbsp;scale_fill_brewer(palette&nbsp;=&nbsp;\"Blues\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Criterion&nbsp;group\")&nbsp;+<br>&nbsp;&nbsp;ylab(\"Total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;coord_flip()&nbsp;+<br>&nbsp;&nbsp;theme_app()<br>###&nbsp;scatterplot,&nbsp;for&nbsp;continuous&nbsp;criterion<br>ggplot(df,&nbsp;aes(x&nbsp;=&nbsp;score,&nbsp;y&nbsp;=&nbsp;criterion))&nbsp;+<br>&nbsp;&nbsp;geom_point()&nbsp;+<br>&nbsp;&nbsp;ylab(\"Criterion&nbsp;variable\")&nbsp;+<br>&nbsp;&nbsp;xlab(\"Total&nbsp;score\")&nbsp;+<br>&nbsp;&nbsp;geom_smooth(<br>&nbsp;&nbsp;&nbsp;&nbsp;method&nbsp;=&nbsp;lm,<br>&nbsp;&nbsp;&nbsp;&nbsp;se&nbsp;=&nbsp;FALSE,<br>&nbsp;&nbsp;&nbsp;&nbsp;color&nbsp;=&nbsp;\"red\"<br>&nbsp;&nbsp;)&nbsp;+<br>&nbsp;&nbsp;theme_app()<br><br>#&nbsp;test&nbsp;for&nbsp;association&nbsp;between&nbsp;total&nbsp;score&nbsp;and&nbsp;criterion&nbsp;variable<br>cor.test(criterion,&nbsp;score,&nbsp;method&nbsp;=&nbsp;\"pearson\",&nbsp;exact&nbsp;=&nbsp;FALSE)<br>"))),
          br()
        ),
        # ** Items ####
        tabPanel("Items",
          value = "crit_val_items",
          h3("Criterion validity"),
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
          p("A test for association between the total score and criterion variable is based on Spearman`s \\(\\rho\\).
                                   This rank-based measure has been recommended if bivariate normal distribution is not guaranteed.
                                   The null hypothesis is that correlation is 0. "),
          tableOutput("validity_table_item"),
          htmlOutput("validity_table_item_interpretation"),
          h4("Selected R code"),
          div(code(HTML("library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;GMATtest,&nbsp;GMATkey,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br>data&nbsp;<-&nbsp;GMATtest[,&nbsp;1:20]<br>data_binary&nbsp;<-&nbsp;GMAT[,&nbsp;1:20]<br>key&nbsp;<-&nbsp;GMATkey<br>criterion&nbsp;<-&nbsp;GMAT[,&nbsp;\"criterion\"]<br><br>#&nbsp;item&nbsp;difficulty&nbsp;/&nbsp;criterion&nbsp;validity&nbsp;plot<br>DDplot(data_binary,&nbsp;criterion&nbsp;=&nbsp;criterion,&nbsp;val_type&nbsp;=&nbsp;\"simple\")<br><br>#&nbsp;distractor&nbsp;plot&nbsp;for&nbsp;item&nbsp;1&nbsp;and&nbsp;3&nbsp;groups<br>plotDistractorAnalysis(data,&nbsp;key,&nbsp;num.groups&nbsp;=&nbsp;3,&nbsp;item&nbsp;=&nbsp;1,&nbsp;criterion&nbsp;=&nbsp;criterion)<br><br>#&nbsp;test&nbsp;for&nbsp;association&nbsp;between&nbsp;total&nbsp;score&nbsp;and&nbsp;criterion&nbsp;variable&nbsp;for&nbsp;item&nbsp;1<br>cor.test(criterion,&nbsp;data_binary[,&nbsp;1],&nbsp;method&nbsp;=&nbsp;\"pearson\",&nbsp;exact&nbsp;=&nbsp;FALSE)<br>"))),
          br()
        )
      )
    )
  )
