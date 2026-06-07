uiPolyGPCM <- tabPanel(
  "Generalized partial credit model",
  value = "gpcm",
  tabsetPanel(
    id = "gpcm_tabs",

    # common header -----------------------------------------------------------
    header = tagList(
      # hide everything in the header on the comparison tab
      conditionalPanel(
        "input.gpcm_tabs !== 'gpcm_comparison'",

        h3("Generalized partial credit model"),

        p(
          "The Generalized Partial Credit Model (GPCM; Muraki, 1992) models the",
          "probability of a successful transition from one score category to the",
          "next using the 2PL IRT model. Category probabilities are defined as",
          "ratios of category-related exponential functions to their sum. The GPCM",
          "belongs to the class of",
          strong("divide-by-total models"),
          "(also called adjacent-category logit models)."
        ),
        p(
          "Two restricted versions are available via the",
          strong("Model"),
          "selector below:"
        ),
        tags$ul(
          tags$li(
            strong("Partial Credit Model"),
            "(",
            strong("PCM", .noWS = c("before", "after")),
            ";",
            "Masters, 1982): the slope parameter is fixed to",
            "\\(a = 1\\) for all items, yielding a 1PL-based polytomous model."
          ),
          tags$li(
            strong("Rating Scale Model"),
            "(",
            strong("RSM", .noWS = c("before", "after")),
            ";",
            "Andrich, 1978): assumes the same set of",
            "\\(K\\) response categories for every item, with threshold parameters",
            "decomposed into a common response-threshold component \\(\\lambda_k\\)",
            "and an item-specific location parameter \\(b_i\\)."
          )
        ),

        # model and parametrization selectors
        fluidRow(
          column(
            3,
            selectInput(
              inputId = "IRT_gpcm_model",
              label = "Model",
              choices = c(
                "GPCM" = "gpcm",
                "PCM" = "pcm",
                "RSM" = "rsm"
              )
            )
          ),
          column(
            3,
            selectInput(
              inputId = "IRT_gpcm_parametrization",
              label = "Parametrization",
              choices = c(
                "IRT (slope/threshold)" = "irt",
                "Intercept-slope" = "intercept_slope"
              )
            )
          )
        ),

        # equations
        h4("Equation"),
        p(
          "For item \\(i\\) with highest score \\(K_i\\), the probability of",
          "person \\(p\\) with latent trait \\(\\theta_p\\) obtaining exactly",
          "score \\(k\\) is:"
        ),
        div(
          style = "margin-bottom: 25px;",

          # GPCM IRT
          conditionalPanel(
            "input.IRT_gpcm_model == 'gpcm' && input.IRT_gpcm_parametrization == 'irt'",
            p(
              "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
              "= \\frac{\\exp \\sum_{t=0}^{k} a_i(\\theta_p - b_{it})}{",
              "\\sum_{r=0}^{K_i} \\exp \\sum_{t=0}^{r} a_i(\\theta_p - b_{it})}$$"
            ),
            p(
              "where \\(a_i > 0\\) is the item discrimination and",
              "\\(b_{i1}, \\ldots, b_{iK_i}\\) are the threshold parameters.",
              "By convention \\(b_{i0} = 0\\)."
            )
          ),

          # GPCM intercept-slope
          conditionalPanel(
            "input.IRT_gpcm_model == 'gpcm' && input.IRT_gpcm_parametrization == 'intercept_slope'",
            p(
              "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
              "= \\frac{\\exp \\sum_{t=1}^{k} (\\beta_{0it} + \\beta_{1i}\\theta_p)}{",
              "\\sum_{r=0}^{K_i} \\exp \\sum_{t=1}^{r} (\\beta_{0it} + \\beta_{1i}\\theta_p)}$$"
            ),
            p(
              "where \\(\\beta_{1i} = a_i\\) is the slope and",
              "\\(\\beta_{0it} = -a_i b_{it}\\) is the intercept for threshold \\(t\\).",
              "By convention \\(\\beta_{0i0} = 0\\)."
            )
          ),

          # PCM IRT
          conditionalPanel(
            "input.IRT_gpcm_model == 'pcm' && input.IRT_gpcm_parametrization == 'irt'",
            p(
              "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
              "= \\frac{\\exp \\sum_{t=0}^{k} (\\theta_p - b_{it})}{",
              "\\sum_{r=0}^{K_i} \\exp \\sum_{t=0}^{r} (\\theta_p - b_{it})}$$"
            ),
            p(
              "where \\(b_{i1}, \\ldots, b_{iK}\\) are the category transition threshold (difficulty)",
              "parameters for item \\(i\\). By convention \\(b_{i0} = 0\\).",
              "The PCM is the GPCM with discrimination fixed to \\(a_i = 1\\) for all items."
            )
          ),

          # PCM intercept-slope
          conditionalPanel(
            "input.IRT_gpcm_model == 'pcm' && input.IRT_gpcm_parametrization == 'intercept_slope'",
            p(
              "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
              "= \\frac{\\exp \\sum_{t=1}^{k} (\\beta_{0it} + \\theta_p)}{",
              "\\sum_{r=0}^{K_i} \\exp \\sum_{t=1}^{r} (\\beta_{0it} + \\theta_p)}$$"
            ),
            p(
              "where \\(\\beta_{0ik} = -b_{ik}\\) are the intercept parameters for item \\(i\\),",
              "\\(k = 1, \\ldots, K\\). By convention \\(\\beta_{0i0} = 0\\).",
              "The PCM is the GPCM with the slope parameter fixed to \\(\\beta_{1i} = 1\\) for all items."
            )
          ),

          # RSM IRT
          conditionalPanel(
            "input.IRT_gpcm_model == 'rsm' && input.IRT_gpcm_parametrization == 'irt'",
            p(
              "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
              "= \\frac{\\exp \\sum_{t=0}^{k} (\\theta_p - b_i - \\lambda_t)}{",
              "\\sum_{r=0}^{K} \\exp \\sum_{t=0}^{r} (\\theta_p - b_i - \\lambda_t)}$$"
            ),
            p(
              "The RSM decomposes each threshold into an item-specific location",
              "\\(b_i\\) and a common response-threshold offset \\(\\lambda_t\\),",
              "with \\(\\lambda_0 = 0\\) fixed by constraint.",
              "All items need to share the same \\(K\\) response categories."
            )
          ),

          # RSM intercept-slope
          conditionalPanel(
            "input.IRT_gpcm_model == 'rsm' && input.IRT_gpcm_parametrization == 'intercept_slope'",
            p(
              "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
              "= \\frac{\\exp \\sum_{t=0}^{k} (\\beta_{i0} + \\beta_{t} + \\theta_p)}{",
              "\\sum_{r=0}^{K} \\exp \\sum_{t=0}^{r} (\\beta_{i0} + \\beta_{t} + \\theta_p)}$$"
            ),
            p(
              "where \\(\\beta_{i0} = -b_i\\) is the item intercept for item \\(i\\) and",
              "\\(\\beta_t = -\\lambda_t\\), \\(t = 1, \\ldots, K\\) are the",
              "category-specific threshold intercepts common for all items.",
              "By convention \\(\\beta_{t0} = 0\\) and \\(\\beta_{1i} = 1\\) are fixed."
            )
          )
        )
      ) # end conditionalPanel (hide on comparison tab)
    ),

    ## Summary subtab ---------------------------------------------------------
    tabPanel(
      "Summary",
      value = "gpcm_summary",
      fluidRow(
        column(
          12,

          div(
            style = "margin-bottom: 25px;",
            h4("Item characteristic curves"),
            p(
              "For item characteristic curves please see the",
              strong("Items"),
              "subtab.",
              "(Plotting all items at once would result in visual clutter.)"
            )
          ),

          # IIC
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curves"),
            plotlyOutput("IRT_gpcm_summary_iic"),
            downloadButton(
              outputId = "IRT_gpcm_summary_iic_download",
              label = "Download figure"
            )
          ),

          # TIC
          div(
            style = "margin-bottom: 25px;",
            h4("Test information curve and SE"),
            plotlyOutput("IRT_gpcm_summary_tic"),
            downloadButton(
              outputId = "IRT_gpcm_summary_tic_download",
              label = "Download figure"
            )
          ),

          # parameter table
          div(
            style = "margin-bottom: 25px;",
            h4("Table of parameters"),
            # Note on fixed/absent parameters (for developer reference):
            # GPCM IRT:          b_{i0} = 0 fixed (baseline), not shown. Shown: a_i, b_{i1}..b_{i,K-1}
            # GPCM int-slope:    beta_{0i0} = 0 fixed, not shown. Shown: beta_{1i}, beta_{0i1}..beta_{0i,K-1}
            # PCM IRT:           b_{i0} = 0 fixed, a_i = 1 fixed (blank SE). Shown: a_i, b_{i1}..b_{i,K-1}
            # PCM int-slope:     beta_{0i0} = 0 fixed, beta_{1i} = 1 fixed (blank SE).
            # RSM IRT:           lambda_0 = 0 fixed (not shown), b_1 = 0 fixed (blank SE), a_i = 1 (blank SE).
            # RSM int-slope:     beta_{t0} = 0 fixed (not shown), beta_{i0} first item = 0 (blank SE), beta_{1i} = 1 (blank SE).
            p(
              "Estimates of item parameters can be displayed using the IRT or intercept/slope",
              strong("parametrization"),
              ", which can be selected at the top of this tab.",
              "Parameter estimates are complemented by",
              "\\(S\\text{-}X^2\\) item fit statistics (Orlando & Thissen, 2000).",
              "\\(S\\text{-}X^2\\) statistics are computed only when no missing data are present."
            ),
            tableOutput("IRT_gpcm_summary_coef"),
            downloadButton(
              outputId = "IRT_gpcm_summary_coef_download",
              label = "Download table"
            )
          ),

          # ability estimates
          div(
            style = "margin-bottom: 25px;",
            h4("Ability estimates"),
            p(
              "This table shows the scores of the first six respondents.",
              "Click",
              strong("Download abilities"),
              "to obtain scores for all respondents."
            ),
            tableOutput("IRT_gpcm_summary_ability"),
            downloadButton(
              outputId = "IRT_gpcm_summary_ability_download",
              label = "Download abilities",
              style = "margin-bottom: 25px;"
            ),
            textOutput("IRT_gpcm_summary_ability_correlation_text"),
            plotlyOutput("IRT_gpcm_summary_ability_plot"),
            downloadButton(
              outputId = "IRT_gpcm_summary_ability_plot_download",
              label = "Download figure"
            )
          )
        )
      )
    ),

    ## Items subtab -----------------------------------------------------------
    tabPanel(
      "Items",
      value = "gpcm_items",
      fluidRow(
        column(
          12,
          sliderInput(
            inputId = "IRT_gpcm_items",
            label = "Item",
            min = 1,
            value = 1,
            max = 20,
            step = 1,
            animate = TRUE
          ),

          # ICC side-by-side with parameter table
          # Note on fixed/absent parameters (for developer reference):
          # GPCM IRT:          b_{i0} = 0 fixed (baseline), not shown. Shown: a_i, b_{i1}..b_{i,K-1}
          # GPCM int-slope:    beta_{0i0} = 0 fixed, not shown. Shown: beta_{1i}, beta_{0i1}..beta_{0i,K-1}
          # PCM IRT:           b_{i0} = 0 fixed, a_i = 1 fixed (blank SE). Shown: a_i, b_{i1}..b_{i,K-1}
          # PCM int-slope:     beta_{0i0} = 0 fixed, beta_{1i} = 1 fixed (blank SE).
          # RSM IRT:           lambda_0 = 0 fixed (not shown), b_1 = 0 fixed (blank SE), a_i = 1 (blank SE).
          # RSM int-slope:     beta_{t0} = 0 fixed (not shown), beta_{i0} first item = 0 (blank SE), beta_{1i} = 1 (blank SE).
          fluidRow(
            style = "margin-bottom: 25px;",
            column(
              7,
              h4("Item characteristic curves"),
              plotlyOutput("IRT_gpcm_items_icc"),
              downloadButton(
                outputId = "IRT_gpcm_items_icc_download",
                label = "Download figure"
              )
            ),
            column(
              5,
              h4("Table of parameters"),
              p(
                "Estimates of item parameters using the selected",
                strong("parametrization"),
                "."
              ),
              tableOutput("IRT_gpcm_items_coef")
            )
          ),

          # IIC
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curve"),
            plotlyOutput("IRT_gpcm_items_iic"),
            downloadButton(
              outputId = "IRT_gpcm_items_iic_download",
              label = "Download figure"
            )
          ),

          # expected score curve
          div(
            style = "margin-bottom: 25px;",
            h4("Expected score curve"),
            plotlyOutput("IRT_gpcm_items_esc"),
            downloadButton(
              outputId = "IRT_gpcm_items_esc_download",
              label = "Download figure"
            )
          )
        )
      )
    ),

    ## Model comparison subtab -----------------------------------------------
    tabPanel(
      "Model comparison",
      value = "gpcm_comparison",
      fluidRow(
        column(
          12,
          h3("Model comparison"),
          p(
            "The",
            strong("Generalized Partial Credit Model"),
            "(GPCM; Muraki, 1992)",
            "models the probability of a successful transition from one score category to the",
            "next using the 2PL IRT model. The GPCM belongs to the class of",
            strong("divide-by-total models"),
            "(also called adjacent-category logit models).",
            "The",
            strong("Partial Credit Model"),
            "(PCM; Masters, 1982)",
            "is its restricted version with the slope parameter fixed to \\(a = 1\\) for all items.",
            "The",
            strong("Rating Scale Model"),
            "(RSM; Andrich, 1978)",
            "further assumes the same set of \\(K\\) response categories for every item.",
            "RSM is omitted if items have differing numbers of response categories."
          ),
          p(
            "Models are nested as GPCM \\(\\supset\\) PCM \\(\\supset\\) RSM.",
            "They can be compared using information criteria and likelihood ratio tests:"
          ),
          tags$ul(
            tags$li(
              strong("AIC"),
              "— Akaike information criterion (Akaike, 1974),"
            ),
            tags$li(
              strong("BIC"),
              "— Bayesian information criterion (Schwarz, 1978),"
            ),
            tags$li(
              strong("LR statistic"),
              "— likelihood ratio test statistic,",
              "with degrees of freedom (df) and p-value.",
              "GPCM has no comparison (most general model)."
            )
          ),
          h4("Table of model fit"),
          p(
            "Row",
            strong("BEST"),
            "indicates which model has the lowest AIC and BIC.",
            "For the LRT, the",
            strong("BEST"),
            "row shows the least",
            "restricted model that provides a significant improvement in fit."
          ),
          DT::dataTableOutput("IRT_gpcm_comparison"),
          br(),
          downloadButton(
            outputId = "IRT_gpcm_comparison_download",
            label = "Download table"
          )
        )
      )
    ),

    # footer ------------------------------------------------------------------
    footer = tagList(
      h4("Selected R code"),
      code(includeText("sc/irt/gpcm_comparison.R"))
    )
  )
)
