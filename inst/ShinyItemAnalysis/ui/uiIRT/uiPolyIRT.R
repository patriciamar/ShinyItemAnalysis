uiPolyIRT <- tabPanel(
  "Nominal response model",
  value = "bock",
  tabsetPanel(


    # common header -----------------------------------------------------------
    header = tagList(
      h3("Nominal response model"),

      # tab description
      p(
        "The Nominal Response Model (NRM) was introduced by Bock (1972) as a way",
        "to model responses to items with two or more nominal  categories. This model",
        "is suitable for multiple-choice items with no particular ordering of",
        "distractors. It is also a generalization of some models for ordinal",
        "data, e.g., Generalized Partial Credit Model (GPCM) or its restricted",
        "versions Partial Credit Model (PCM) and Rating Scale Model (RSM)."
      ),
      # global NRM parametrization selector
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "IRT_bock_parametrization",
            label = "Parametrization",
            c(
              BLIS = "blis",
              BLIRT = "blirt",
              Bock = "bock",
              "Thissen et al." = "thissen"
            )
          )
        ),
      ),
      # equation commentary text and the equation itself
      h4("Equation"),
      p(
        "For ", strong("\\(K_i\\)"),
        " possible test choices, the probability of selecting distractor ",
        strong("\\(k\\)"), " by person ", strong("\\(p\\)"), " with latent trait",
        strong("\\(\\theta_p\\)"), " in item ", strong("\\(i\\)"),
        "is given by the following equation: "
      ),
      div(
        style = "margin-bottom: 25px;",

        # use conditional panels (let the client do the work and do not burden server)

        # BLIS
        conditionalPanel(
          "input.IRT_bock_parametrization == \"blis\"",
          p("$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ {\\beta_0}_{ik} +  {\\beta_1}_{ik}\\theta_p  }}{\\sum_{l=0}^{K_i} e^{ {\\beta_0}_{il} +  {\\beta_1}_{il}\\theta_p }}$$"),
          p("with constrains", "\\({\\beta_1}_{i0} = 0\\)", "and", "\\({\\beta_0}_{i0} = 0\\).")
        ),
        # BLIRT
        conditionalPanel(
          "input.IRT_bock_parametrization == \"blirt\"",
          p("$$ \\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ a_{ik}( \\theta_p - b_{ik} ) }} {\\sum_{l=0}^{K_i} e^{ a_{il}( \\theta_p - b_{il} ) }}$$"),
          p("with constrains", "\\(a_{i0} = 0\\)", "and", "\\(b_{i0} = 0\\).")
        ),
        # Bock
        conditionalPanel(
          "input.IRT_bock_parametrization == \"bock\"",
          p("$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ \\alpha_{ik}\\theta_p +c_{ik} }}{\\sum_{l=0}^{K_i} e^{\\alpha_{il}\\theta_p +c_{il}}}$$"),
          p("with constrains", "\\(\\sum_{k=0}^{K_i}a_k = 0\\)", "and", "\\(\\sum_{k=0}^{K_i}c_k = 0\\).")
        ),
        # Thissen et al.
        conditionalPanel(
          "input.IRT_bock_parametrization == \"thissen\"",
          p("$$ \\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ a^*_i a_{ik}^s \\theta_p + c_{ik} }}{\\sum_{l=0}^{K_i} e^{ a^*_i a_{il}^s \\theta_p + c_{il}  }}$$"),
          p("with constrains", "\\(a_{i0}^s = 0\\),", "\\(a_{iK}^s = K\\)", "and", "\\(d_{i0} = 0\\), where \\(a_{i}^*\\) is \"the overall slope\" parameter for item \\(i\\) and \\(a_{ik}^s\\) is \"the scoring function\" for response \\(k\\) (Thissen et al., 2010).")
        ),
      )
    ),


    ## summary tab -------------------------------------------------------------

    tabPanel("Summary",
      value = "bock_mod",
      fluidRow(
        column(
          12,
          div(
            style = "margin-bottom: 25px;",
            h4("Item characteristic curves"),
            p(
              "For item characteristic curves please see the", strong("Items"), "subtab.",
              "(Plotting all items at once would result in a visual clutter.)"
            )
          ),


          # ICC plot ----------------------------------------------------------------
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curves"),
            plotlyOutput("IRT_bock_summary_iic"),
            downloadButton(
              outputId = "IRT_bock_summary_iic_download",
              label = "Download figure"
            )
          ),


          # test information and SE -------------------------------------------------
          div(
            style = "margin-bottom: 25px;",
            h4("Test information curve and SE"),
            plotlyOutput("IRT_bock_summary_tic"),
            downloadButton(
              outputId = "IRT_bock_summary_tic_download",
              label = "Download figure"
            )
          ),


          # table of parameters -----------------------------------------------------
          div(
            style = "margin-bottom: 25px;",
            h4("Table of parameters"),
            tableOutput("IRT_bock_summary_coef"),
            downloadButton(
              outputId = "IRT_bock_summary_coef_download",
              label = "Download table"
            )
          ),

          # ability estimates -------------------------------------------------------
          div(
            style = "margin-bottom: 25px;",
            h4("Ability estimates"),
            p(
              "This table shows the response score of only six respondents.",
              "If you want to see scores for all respondents, click on the",
              strong("Download abilities"), " button. "
            ),
            tableOutput("IRT_bock_summary_ability"),
            downloadButton(
              outputId = "IRT_bock_summary_ability_download",
              label = "Download abilities",
              style = "margin-bottom: 25px;"
            ),
            # correlation text
            textOutput("IRT_bock_summary_ability_correlation_text"),
            # z-scores -- f-scores scatterplot
            plotlyOutput("IRT_bock_summary_ability_plot"),
            downloadButton(
              outputId = "IRT_bock_summary_ability_plot_download",
              label = "Download figure"
            ),
          )
        )
      )
    ),


    ## items tab ---------------------------------------------------------------

    tabPanel("Items",
      value = "bock_it",
      fluidRow(
        column(
          12,
          # item selector slider (to be updated in server part to suit the current data set)
          sliderInput(
            inputId = "IRT_bock_items",
            label = "Item to draw the plot for",
            min = 1, value = 1, max = 20,
            step = 1, animate = TRUE
          ),

          ### ICC plot ----------------------------------------------------------------
          div(
            style = "margin-bottom: 25px;",
            h4("Item characteristic curves"),
            plotlyOutput("IRT_bock_items_icc"),
            downloadButton(
              outputId = "IRT_bock_items_icc_download",
              label = "Download figure"
            )
          ),


          ### IIC plot -------------------------------------------------------------
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curves"),
            plotlyOutput("IRT_bock_items_iic"),
            downloadButton(
              outputId = "IRT_bock_items_iic_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            # ** Estimated parameters ####
            h4("Table of parameters"),
            tableOutput("IRT_bock_items_coef")
          )
        )
      )
    ),


    # common footer -----------------------------------------------------------

    footer = tagList(
      h4("Selected R code"),
      code(includeText("sc/irt/bock.R"))
    )
  )
)
