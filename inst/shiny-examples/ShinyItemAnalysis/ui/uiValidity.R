uiValidity <-
  navbarMenu(
    "Validity",
    # * PREDICTIVE VALIDITY ####
    # ** Summary ####
    tabPanel(
      "Criterion validity",
      value = "val_summary",
      h3("Criterion validity"),
      p(
        "Depending on the criterion variable, different types of criterion validity may be examined. As an example, a correlation between the test score and the future study success or future GPA may be  used as a proof of predictive validity in the case of admission tests. A criterion variable may be uploaded in the ", strong("Data"), "section."
      ),
      h4("Descriptive plots of criterion variable on total score"),
      p(
        "Total scores are plotted according to a criterion variable. Boxplot or scatterplot is displayed depending on the type of criterion variable - whether it is discrete or continuous. Scatterplot is provided with a red linear regression line. "
      ),
      plotlyOutput("validity_plot"),
      downloadButton(outputId = "DB_validity_plot", label = "Download figure"),
      h4("Correlation of criterion variable and total score"),
      p(
        "An association between the total score and the criterion variable can be estimated using Pearson product-moment correlation coefficient ", em("r"), ". The null hypothesis being tested states that correlation is exactly 0. "
      ),
      uiOutput("validity_table"),
      br(),
      htmlOutput("validity_table_interpretation"),
      br(),
      h4("Selected R code"),
      code(includeText("sc/validity/crit_val.R"))
    ),
    # * CORRELATION STRUCTURE ####
    tabPanel(
      "Correlation structure",
      h3("Correlation structure"),
      h4("Correlation heat map"),
      p(
        "A correlation heat map displays selected type of",
        HTML("<b>correlations</b>"),
        "between items. The size and shade of circles indicate how much the items are correlated (larger and darker circle mean greater correlations). The color of circles indicates in which way the items are correlated - a blue color means possitive correlation and a red color means negative correlation. A correlation heat map can be reordered using a hierarchical",
        HTML("<b>clustering method</b>"),
        "selected below. With a ",
        HTML("<b>number  of clusters</b>"),
        "larger than 1, the rectangles representing clusters are drawn. The values of a correlation heatmap may be displayed and also downloaded."
      ),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "type_of_corr",
            label = "Choose correlation",
            choices = c(
              "Pearson" = "pearson",
              "Spearman" = "spearman",
              "Polychoric" = "polychoric"
            ),
            selected = "pearson"
          )
        ),
        column(
          2,
          selectInput(
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
          )
        ),
        column(
          2,
          numericInput(
            inputId = "corr_plot_clust",
            label = "Number of clusters",
            value = 0,
            min = 0,
            max = 1
          )
        ),
        column(
          2,
          br(),
          checkboxInput("show_corr",
            label = "Display correlation values",
            value = FALSE
          )
        ),
        column(
          2,
          numericInput(
            "corr_plot_labs_size",
            label = "Values display size",
            min = 1,
            value = 3,
            max = 10,
            step = .1
          )
        )
      ),
      conditionalPanel(
        condition = "input.type_of_corr == 'pearson'",
        p(
          HTML("<b>Pearson correlation coefficient</b>"),
          "describes the strength and direction of a linear relationship between two random variables \\(X\\) and \\(Y\\). It is given by formula"
        ),
        withMathJax(),
        (
          "$$\\rho = \\frac{cov(X,Y)}{\\sqrt{var(X)}\\sqrt{var(Y)}}.$$"
        ),
        p("Sample Pearson corelation coefficient may be calculated as"),
        withMathJax(),
        (
          "$$ r = \\frac{\\sum_{i = 1}^{n}(x_{i} - \\bar{x})(y_{i} - \\bar{y})}{\\sqrt{\\sum_{i = 1}^{n}(x_{i} - \\bar{x})^2}\\sqrt{\\sum_{i = 1}^{n}(y_{i} - \\bar{y})^2}}$$"
        ),
        p(
          "Pearson correlation coefficient has a value between -1 and +1. Sample correlation of -1 and +1 correspond to all data points lying exactly on a line (decreasing in case of negative linear correlation -1 and increasing for +1). If the coefficient is equal to 0, it means there is no linear relationship between the two variables."
        )
      ),
      conditionalPanel(
        condition = "input.type_of_corr == 'polychoric'",
        p(
          "A ",
          HTML("<b>polychoric/tetrachoric correlation</b>"),
          "between two ordinal/binary variables is calculated from their contingency table, under the assumption that the ordinal variables dissect continuous latent variables that are bivariate normal."
        )
      ),
      conditionalPanel(
        condition = "input.type_of_corr == 'spearman'",
        p(
          "The ",
          HTML("<b>Spearman's rank correlation coefficient</b>"),
          "describes the strength and the direction of a monotonic relationship between random variables \\(X\\) and \\(Y\\), i.e. the dependence between the rankings of two variables. It is given by formula"
        ),
        withMathJax(),
        (
          "$$\\rho = \\frac{cov(rg_{X},rg_{Y})}{\\sqrt{var(rg_{X})}\\sqrt{var(rg_{Y})}},$$"
        ),
        p(
          "where \\(rg_{X}\\) and \\(rg_{Y}\\) are the transformed random variables \\(X\\) and \\(Y\\) into ranks, i.e, the  Spearman correlation coefficient is the Pearson correlation coefficient between the ranked variables."
        ),
        p(
          "The sample Spearman correlation is calculated by converting \\(X\\) and \\(Y\\) to ranks (average ranks are used in case of ties) and by applying the sample Pearson correlation formula. If both the \\(X\\) and \\(Y\\) have \\(n\\) unique ranks, i.e. there are no ties, then the sample correlation coefficient is given by formula"
        ),
        withMathJax(),
        ("$$ r = 1 - \\frac{6\\sum_{i = 1}^{n}d_i^{2}}{n(n-1)}$$"),
        p(
          "where \\(d = rg_{X} - rg_{Y}\\) is the difference between two ranks and \\(n\\) is size of \\(X\\) and \\(Y\\). Spearman rank correlation coefficient has value between -1 and 1, where 1  means identity of ranks of the variables and -1 means reverse ranks of the two variables. In case of no repeated values, Spearman correlation of +1 or -1 means that all data points are lying exactly on some monotone line. If the Spearman coefficient is equal to 0, it means there is no tendency for \\(Y\\) to either increase or decrease with \\(X\\) increasing."
        )
      ),
      p(
        HTML("<b>Clustering methods.</b>"),
        "Ward's method aims at finding compact clusters based on minimizing the within-cluster sum of squares. Ward's n. 2 method uses squared disimilarities. The Single method connects clusters with their nearest neighbours, i.e. the distance between two clusters is calculated as the minimum of the distance of observations in one cluster and observations in the other clusters. Complete linkage with the farthest neighbours, on the other hand, uses the maximum of distance. The Average linkage method uses the distance based on a weighted average of the individual distances. The McQuitty method uses an unweighted average. The Median linkage calculates the distance as the median of distance between an observation in one cluster and observation in another cluster. The Centroid method uses the distance between centroids of clusters. "
      ),
      uiOutput("corr_na_alert"),
      br(),
      fluidRow(column(
        8,
        offset = 2, plotlyOutput("corr_plot", height = "700px")
      )),
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
      br(), br(),
      h4("Selected R code"),
      code(includeText("sc/validity/cor_struct.R"))
    ),
    # * FACTOR ANALYSIS ####
    tabPanel(
      "Factor analysis",
      value = "factanal",
      h3("Factor analysis"),
      h4("Finding the optimal number of factors/components"),
      p(
        "A scree plot below displays two sets of the eigenvalues associated with the factors/components in descending order. Location of a bend (an elbow) of", span("the \"real\" part", style = "font-weight: bold; color: red;"), "can be considered indicative to the suitable number of factors (Catell, 1966). Another rule, as proposed by Kaiser (1960), discards all factors or components with the eigenvalue less than or equal to 0 or 1, respectively (the information of a single average item)."
      ),
      p(
        "A more complex approach called a parallel analysis (Horn, 1965) compares the eigenvalues of the real data correlation matrix with the eigenvalues (or more precisely, 95th percentiles of their sampling distributions) obtained from simulated zero-factor random matrices. The number of factors/components with the eigenvalue bigger than the eigenvalue at the first (leftmost) curves crossing is then the optimal number to extract in factor or principal component analysis. According to Bartholomew et al. (2011), the number of components is a good guide to the number of factors given the relationship between the PCA and FA."
      ),
      fluidRow(
        column(
          2,
          selectInput(
            "validity_factor_pa_method",
            "Eigenvalues from",
            c(
              "FA" = "fa",
              "PCA" = "pca",
              "Both" = "both"
            ),
            selected = "pca"
          )
        ),
        column(2, selectInput("validity_factor_pa_cor",
          "Correlation method",
          choices = c(
            "Pearson" = "pearson",
            # "Tetrachoric" = "tetrachoric",
            "Polychoric" = "polychoric"
          )
        )),
        column(
          7, br(),
          helpText("Method used to compute the correlation matrix. For ordinal datasets with only a few categories, polychoric option is recommended. The choice is automatically forwarded to the EFA below.")
        )
      ),
      plotlyOutput("validity_factor_screeplot"),
      downloadButton(outputId = "DB_scree_plot", label = "Download figure"),
      br(),
      br(),
      h4("Interpretation"),
      textOutput("validity_factor_number"),
      br(),
      h4("Exploratory factor analysis"),
      p("Once the optimal number of factors is found, the exploratory factor analysis (EFA) itself may be conducted. The number of factors found by the parallel analysis is offered as the default value. You can select the preffered factor rotation of the solution or hide the loadings outside interest. There is also an option to sort items by their importance on each factor. Below the loadings table, there is factor summary with proportion of variance each of the factor explains, as well as the list of common model fit indices."),
      fluidRow(
        column(
          2,
          selectInput("validity_factor_cor_efa",
            "Correlation method",
            choices = c(
              "Pearson" = "cor",
              # "Tetrachoric" = "tetrachoric",
              "Polychoric" = "poly"
            ), multiple = FALSE
          )
        ),
        column(
          2,
          numericInput("validity_factor_nfactors",
            "Number of factors to extract",
            value = 1,
            min = 1, step = 1
          )
        ),
        column(
          2,
          selectInput("validity_factor_rotation",
            "Rotation",
            choices = c(
              "None" = "none",
              "Varimax" = "varimax",
              "Quartimax" = "quartimax",
              "Promax" = "promax",
              "Oblimin" = "oblimin",
              "Simplimax" = "simplimax"
            ), selected = "oblimin"
          )
        ),
        column(
          2,
          numericInput("validity_factor_hide",
            "Hide loadings below",
            value = .3,
            min = 0, max = 1, step = .1
          )
        ),
        column(
          2, br(),
          checkboxInput(
            "validity_factor_sort",
            "Sort items", FALSE
          )
        ),
      ),
      tableOutput("validity_factor_loadings"),
      downloadButton("DB_validity_factor_loadings", label = "Download table"),
      br(), br(),
      h4("Factor summary"),
      tableOutput("validity_factor_varex"), br(),
      h4("Model fit"),
      uiOutput("validity_factor_efa_fit"), br(),

      h4("Factor scores"),
      DTOutput("validity_factor_fscores"),

      downloadButton("DB_validity_factor_fscores", label = "Download table"),
      br(), br(),
      h4("Selected R code"),
      code(includeText("sc/validity/fact_anal.R"))
    )
  )
