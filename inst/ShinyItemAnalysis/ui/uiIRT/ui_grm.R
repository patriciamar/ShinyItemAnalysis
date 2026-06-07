uiPolyGRM <- tabPanel(
  "Graded response model",
  value = "grm",
  tabsetPanel(

    # common header -----------------------------------------------------------
    header = tagList(
      h3("Graded response model"),

      p(
        "The Graded Response Model (GRM; Samejima, 1970) uses the 2PL IRT model to",
        "describe cumulative probabilities, i.e., the probability of scoring",
        "higher than each threshold. Category probabilities are then obtained as",
        "differences between two subsequent cumulative probabilities. The GRM",
        "belongs to the class of",
        strong("difference models"),
        "(also called cumulative logit models), and is suitable for ordinal",
        "polytomous items such as Likert-scale responses."
      ),

      # parametrization selector
      fluidRow(
        column(
          3,
          selectInput(
            inputId = "IRT_grm_parametrization",
            label   = "Parametrization",
            choices = c(
              "IRT (slope/threshold)"    = "irt",
              "Intercept-slope"          = "intercept_slope"
            )
          )
        )
      ),

      # equation block
      h4("Equation"),
      p(
        "For item \\(i\\) with \\(K_i\\) ordered response categories",
        "\\(k = 0, 1, \\ldots, K_i\\), the",
        strong("cumulative probability"),
        "that person \\(p\\) with latent trait \\(\\theta_p\\) scores",
        "at least \\(k\\) is:"
      ),
      div(
        style = "margin-bottom: 25px;",

        # IRT parametrization
        conditionalPanel(
          "input.IRT_grm_parametrization == 'irt'",
          p(
            "$$\\pi_{pik}^* = \\mathrm{P}(Y_{pi} \\geq k \\mid \\theta_p)",
            "= \\frac{e^{a_i(\\theta_p - b_{ik})}}{1 + e^{a_i(\\theta_p - b_{ik})}}$$"
          ),
          p(
            "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
            "= \\pi_{pik}^* - \\pi_{pi,k+1}^*$$"
          ),
          p(
            "where \\(a_i > 0\\) is the common discrimination parameter and",
            "\\(b_{i1} < b_{i2} < \\cdots < b_{iK_i}\\) are the ordered",
            "threshold (difficulty) parameters.",
            "By convention \\(\\pi_{pi0}^* = 1\\) and \\(\\pi_{pi,K_i+1}^* = 0\\)."
          )
        ),

        # Intercept-slope parametrization
        conditionalPanel(
          "input.IRT_grm_parametrization == 'intercept_slope'",
          p(
            "$$\\pi_{pik}^* = \\mathrm{P}(Y_{pi} \\geq k \\mid \\theta_p)",
            "= \\frac{e^{\\beta_{0ik} + \\beta_{1i}\\theta_p}}{1 + e^{\\beta_{0ik} + \\beta_{1i}\\theta_p}}$$"
          ),
          p(
            "$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k \\mid \\theta_p)",
            "= \\pi_{pik}^* - \\pi_{pi,k+1}^*$$"
          ),
          p(
            "where \\(\\beta_{0ik} = -a_i b_{ik}\\) is the intercept for threshold \\(k\\) and",
            "\\(\\beta_{1i} = a_i\\) is the slope.",
            "By convention \\(\\pi_{pi0}^* = 1\\) and \\(\\pi_{pi,K_i+1}^* = 0\\)."
          )
        )
      )
    ),

    ## Summary subtab ---------------------------------------------------------
    tabPanel(
      "Summary",
      value = "grm_summary",
      fluidRow(
        column(
          12,

          div(
            style = "margin-bottom: 25px;",
            h4("Item characteristic curves"),
            p(
              "For item characteristic curves please see the",
              strong("Items"), "subtab.",
              "(Plotting all items at once would result in visual clutter.)"
            )
          ),

          # IIC
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curves"),
            plotlyOutput("IRT_grm_summary_iic"),
            downloadButton(
              outputId = "IRT_grm_summary_iic_download",
              label    = "Download figure"
            )
          ),

          # TIC
          div(
            style = "margin-bottom: 25px;",
            h4("Test information curve and SE"),
            plotlyOutput("IRT_grm_summary_tic"),
            downloadButton(
              outputId = "IRT_grm_summary_tic_download",
              label    = "Download figure"
            )
          ),

          # parameter table
          div(
            style = "margin-bottom: 25px;",
            h4("Table of parameters"),
            # Note on fixed/absent parameters (for developer reference):
            # IRT:          b_{i0} absent — P(Y>=0|theta) = 1 structurally, no parameter needed.
            #               Shown: a_i, b_{i1}, ..., b_{i,K-1}
            # Intercept-slope: beta_{0i0} absent for the same reason.
            #               Shown: beta_{1i}, beta_{0i1}, ..., beta_{0i,K-1}
            p(
              "Estimates of item parameters can be displayed using the IRT or intercept/slope",
              strong("parametrization"),
              ", which can be selected at the top of this tab.",
              "Parameter estimates are complemented by",
              "\\(S\\text{-}X^2\\) item fit statistics (Orlando & Thissen, 2000).",
              "\\(S\\text{-}X^2\\) statistics are computed only when no missing data are present."
            ),
            tableOutput("IRT_grm_summary_coef"),
            downloadButton(
              outputId = "IRT_grm_summary_coef_download",
              label    = "Download table"
            )
          ),

          # ability estimates
          div(
            style = "margin-bottom: 25px;",
            h4("Ability estimates"),
            p(
              "This table shows the scores of the first six respondents.",
              "Click", strong("Download abilities"), "to obtain scores for all respondents."
            ),
            tableOutput("IRT_grm_summary_ability"),
            downloadButton(
              outputId = "IRT_grm_summary_ability_download",
              label    = "Download abilities",
              style    = "margin-bottom: 25px;"
            ),
            textOutput("IRT_grm_summary_ability_correlation_text"),
            plotlyOutput("IRT_grm_summary_ability_plot"),
            downloadButton(
              outputId = "IRT_grm_summary_ability_plot_download",
              label    = "Download figure"
            )
          )
        )
      )
    ),

    ## Items subtab -----------------------------------------------------------
    tabPanel(
      "Items",
      value = "grm_items",
      fluidRow(
        column(
          12,
          sliderInput(
            inputId = "IRT_grm_items",
            label   = "Item",
            min     = 1,
            value   = 1,
            max     = 20,
            step    = 1,
            animate = TRUE
          ),

          # CPC + ICC side by side
          div(
            style = "margin-bottom: 25px;",
            p(
              "The GRM is built on cumulative probabilities",
              "\\(\\mathrm{P}(Y \\geq k \\mid \\theta)\\)",
              "modelled by the 2PL IRT model (left).",
              "Category probabilities (right) are differences between adjacent",
              "cumulative curves. Colours match across both plots."
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              div(
                h4("Cumulative probability curves"),
                plotlyOutput("IRT_grm_items_cpc"),
                downloadButton(
                  outputId = "IRT_grm_items_cpc_download",
                  label    = "Download figure"
                )
              ),
              div(
                h4("Item characteristic curves"),
                plotlyOutput("IRT_grm_items_icc"),
                downloadButton(
                  outputId = "IRT_grm_items_icc_download",
                  label    = "Download figure"
                )
              )
            )
          ),


          # IIC
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curve"),
            plotlyOutput("IRT_grm_items_iic"),
            downloadButton(
              outputId = "IRT_grm_items_iic_download",
              label    = "Download figure"
            )
          ),

          # expected score curve
          div(
            style = "margin-bottom: 25px;",
            h4("Expected score curve"),
            plotlyOutput("IRT_grm_items_esc"),
            downloadButton(
              outputId = "IRT_grm_items_esc_download",
              label    = "Download figure"
            )
          ),

          # parameter table for selected item
          div(
            style = "margin-bottom: 25px;",
            h4("Table of parameters"),
            # Note on fixed/absent parameters (for developer reference):
            # IRT:          b_{i0} absent — P(Y>=0|theta) = 1 structurally, no parameter needed.
            #               Shown: a_i, b_{i1}, ..., b_{i,K-1}
            # Intercept-slope: beta_{0i0} absent for the same reason.
            #               Shown: beta_{1i}, beta_{0i1}, ..., beta_{0i,K-1}
            tableOutput("IRT_grm_items_coef")
          )
        )
      )
    ),

    # footer ------------------------------------------------------------------
    footer = tagList(
      h4("Selected R code"),
      code(includeText("sc/irt/grm.R"))
    )
  )
)
