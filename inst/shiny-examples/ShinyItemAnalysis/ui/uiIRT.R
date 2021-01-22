source("ui/uiIRT/uiDIRT.R")

uiIRT <- navbarMenu(
  "IRT models",
  "Dichotomous models",
  # * RASCH ####
  tabPanel(
    "Rasch",
    tabsetPanel(
      # ** RASCH - MODEL ####
      tabPanel("Model",
        value = "rasch_mod",
        h3("Rasch model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which
                                        respondent ability \\(\\theta_p\\) is assumed to be latent and is estimated together with item
                                        paramters. "),
        p("In", strong("Rasch model"), "(Rasch, 1960), all items are assumed to have the same slope in inflection point while they may
                                        differ in location of the item characteristic curves and their inflection points. Model parameters
                                        are estimated using marginal maximum likelihood method. Ability \\(\\theta_p\\) of respondent \\(p\\)
                                        is assumed to follow normal distribution with freely estimated variance. "),
        # *** Equations ####
        h4("Equations"),
        p("Item characteristic function \\(\\pi_{pi} = \\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right)\\) describes probability of correct
                                        answer for given item \\(i\\). Item information  function \\(\\mathrm{I}_i(\\theta_p)\\) describes how well item
                                        discriminates from two nearby ability levels, i.e., how much information it provides for the given ability.
                                        Test information  function \\(\\mathrm{T}(\\theta_p)\\) sums up all item informations and thus describes the information
                                        of the whole test. The inverse of the test information is standard error (SE) of measurement. "),
        p("Equation and estimated item parameters can be displayed using the IRT or classical - intercept/slope ", strong("parametrization.")),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "irt_rasch_parametrization",
              label = "Parametrization",
              choices = c(
                "IRT" = "irt",
                "Intercept/slope" = "classical"
              )
            )
          )
        ),
        uiOutput("irt_rasch_icc_equation", inline = T),
      "$$\\mathrm{I}_i(\\theta_p) =  \\pi_{pi} (1 - \\pi_{pi})$$",
      "$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m \\pi_{pi} (1 - \\pi_{pi})$$",
        uiOutput("irt_rasch_equation_interpretation"),
        uiOutput("irt_rasch_model_converged"),
        # *** Plots ####
        h4("Item characteristic curves"),
        plotlyOutput("irt_rasch_icc"),
        downloadButton(
          outputId = "irt_rasch_icc_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("irt_rasch_iic"),
        downloadButton(
          outputId = "irt_rasch_iic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Test information curve and SE"),
        plotlyOutput("irt_rasch_tic"),
        downloadButton(
          outputId = "irt_rasch_tic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # *** Estimated parameters ####
        h4("Table of estimated parameters"),

        p("Estimates of item parameters can be displayed using the IRT or classical - intercept/slope ", strong("parametrization,"), "which can be
                                        selected at the top of this tab. Parameter estimates are completed by SX2 item fit statistics (Orlando &
                                        Thissen, 2000). SX2 statistics are computed
                                        only when no missing data are present."),
        tableOutput("irt_rasch_coef"),
        downloadButton(
          outputId = "irt_rasch_coef_download",
          label = "Download table"
        ),
        br(),
        br(),
        # *** Ability estimates ####
        h4("Ability estimates"),
        p("This table shows the response and factor scores of only six respondents. If you want to see scores for
                                        all respondents, click on", strong("Download abilities"), "button."),
        tableOutput("irt_rasch_factors"),
        downloadButton(
          outputId = "irt_rasch_factors_download",
          label = "Download abilities"
        ),
        br(),
        br(),
        h4("Scatter plot of factor scores and standardized total scores"),
        textOutput("irt_rasch_factors_correlation"),
        plotlyOutput("irt_rasch_factors_plot"),
        downloadButton(
          outputId = "irt_rasch_factors_plot_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # *** Wright map ####
        h4("Wright map"),
        p("Wright map (Wilson, 2005; Wright & Stone, 1979), also called item-person map, is a graphical tool
                                        to display person ability estimates and item parameters. The person side
                                        (left) represents histogram of estimated abilities of respondents.
                                        The item side (right) displays estimates of difficulty parameters of individual items. "),
        plotOutput("irt_rasch_wrightmap"),
        downloadButton(
          outputId = "irt_rasch_wrightmap_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # *** Selected R code ####
        h4("Selected R code"),
        div(code(HTML("library(mirt)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;fitting&nbsp;Rasch&nbsp;model<br>fit&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"Rasch\",&nbsp;SE&nbsp;=&nbsp;TRUE)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"trace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infotrace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infoSE\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;classical&nbsp;intercept-slope&nbsp;parametrization<br>coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;IRT&nbsp;parametrization<br><br>#&nbsp;item&nbsp;fit&nbsp;statistics<br>itemfit(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>fs&nbsp;<-&nbsp;as.vector(fscores(fit))<br>sts&nbsp;<-&nbsp;as.vector(scale(rowSums(GMAT[,&nbsp;1:20])))<br>plot(fs&nbsp;~&nbsp;sts,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(fs,&nbsp;sts)<br><br>#&nbsp;Wright&nbsp;map<br>b&nbsp;<-&nbsp;coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)$items[,&nbsp;\"b\"]<br>ggWrightMap(fs,&nbsp;b)"))),
        br()
      ),
      # ** RASCH - ITEMS ####
      tabPanel("Items",
        value = "rasch_it",
        h3("Rasch model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters. "),
        p("In", strong("Rasch model"), "(Rasch, 1960), all items are assumed to have the same slope in inflection point while they may
                                        differ in location of the item characteristic curves and their inflection points. Model parameters
                                        are estimated using marginal maximum likelihood method. Ability \\(\\theta_p\\) of respondent \\(p\\)
                                        is assumed to follow normal distribution with freely estimated variance. "),
        # *** Equations ####
        h4("Equations"),
        p("Item characteristic function \\(\\pi_{pi} = \\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right)\\) describes probability of correct
                                        answer for given item \\(i\\). Item information  function \\(\\mathrm{I}_i(\\theta_p)\\) describes how well item
                                        discriminates from two nearby ability levels, i.e., how much information it provides for the given ability. "),
        p("Equation and estimated item parameters can be displayed using the IRT or classical - intercept/slope ", strong("parametrization.")),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = "irt_rasch_item_parametrization",
              label = "Parametrization",
              choices = c(
                "IRT" = "irt",
                "Intercept/slope" = "classical"
              )
            )
          )
        ),
        fluidRow(column(12, align = "center", uiOutput("irt_rasch_item_icc_equation"))),
        ("$$\\mathrm{I}_i(\\theta_p) =  \\pi_{pi} (1 - \\pi_{pi})$$"),
        uiOutput("irt_rasch_item_equation_interpretation"),
        # *** Plots ####
        h4("Item characteristic curves"),
        sliderInput(
          inputId = "irt_rasch_item_slider", label = "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("irt_rasch_item_icc"),
        downloadButton(
          outputId = "irt_rasch_item_icc_download",
          label = "Download figure"
        ),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("irt_rasch_item_iic"),
        downloadButton(
          outputId = "irt_rasch_item_iic_download",
          label = "Download figure"
        ),
        br(),
        br(),
        # *** Estimated parameters ####
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 is computed only when no missing data are present. In such a case consider using imputed dataset!"),
        tableOutput("irt_rasch_item_coef")
      )
    )
  ),
  # * 1PL ####
  tabPanel(
    "1PL",
    tabsetPanel(
      tabPanel("Model",
        value = "1pl_mod",
        h3("1PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed to be latent
                                        and is estimated together with item paramters. "),
        p("In", strong("One Parameter Logistic (1PL) IRT model,"), "all items are assumed to have the same slope in inflection point, i.e., the same discrimination \\(a\\).
                                        Its value corresponds to standard deviation of ability estimates in Rasch model. Items can differ in location
                                        of their inflection point, i.e., in item difficulty parameters \\(b_i\\). Model parameters are estimated using
                                        marginal maximum likelihood method. Ability \\(\\theta_p\\) is assumed to follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = \\frac{e^{a\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  a^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m a^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        uiOutput("irt_1PL_model_converged"),
        h4("Item characteristic curves"),
        plotlyOutput("oneparamirt_mirt"),
        downloadButton("DP_oneparamirt_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("oneparamirtiic_mirt"),
        downloadButton("DP_oneparamirtiic_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Test information function"),
        plotlyOutput("oneparamirttif_mirt"),
        downloadButton("DP_oneparamirttif_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("coef_oneparamirt_mirt"),
        downloadButton(
          outputId = "download_1pl_table",
          label = "Download table"
        ),
        br(),
        br(),
        h4("Ability estimates"),
        p("This table shows the response score of only six respondents. If you want to see scores for all respondents, click on ", strong("Download abilities"), " button."),
        tableOutput("one_PL_abilities"),
        downloadButton(
          outputId = "download_onePL_abilities",
          label = "Download abilities"
        ),
        br(),
        br(),
        h4("Scatter plot of factor scores and standardized total scores"),
        textOutput("oneparamirtFactorCor_mirt"),
        plotlyOutput("oneparamirtFactor_mirt"),
        downloadButton("DP_oneparamirtFactor_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Wright map"),
        p("Wright map (Wilson, 2005; Wright & Stone, 1979), also called item-person map, is a graphical tool
                                        to display person ability estimates and item parameters. The person side
                                        (left) represents histogram of estimated abilities of respondents.
                                        The item side (right) displays estimates of difficulty parameters of individual items. "),
        plotOutput("oneparamirtWrightMap_mirt"),
        downloadButton("DP_oneparamirtWM_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Selected R code"),
        div(code(HTML("library(ltm)<br>library(mirt)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;fitting&nbsp;1PL&nbsp;model<br>fit&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],<br>&nbsp;&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"2PL\",<br>&nbsp;&nbsp;constrain&nbsp;=&nbsp;list((1:20)&nbsp;+&nbsp;seq(0,&nbsp;(20&nbsp;-&nbsp;1)&nbsp;*&nbsp;3,&nbsp;3)),&nbsp;SE&nbsp;=&nbsp;TRUE<br>)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"trace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infotrace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infoSE\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;classical&nbsp;intercept-slope&nbsp;parametrization<br>coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;IRT&nbsp;parametrization<br><br>#&nbsp;item&nbsp;fit&nbsp;statistics<br>itemfit(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>fs&nbsp;<-&nbsp;as.vector(fscores(fit))<br>sts&nbsp;<-&nbsp;as.vector(scale(rowSums(GMAT[,&nbsp;1:20])))<br>plot(fs&nbsp;~&nbsp;sts,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(fs,&nbsp;sts)<br><br>#&nbsp;Wright&nbsp;map<br>b&nbsp;<-&nbsp;coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)$items[,&nbsp;\"b\"]<br>ggWrightMap(fs,&nbsp;b)<br><br>#&nbsp;you&nbsp;can&nbsp;also&nbsp;use&nbsp;the&nbsp;ltm&nbsp;package<br>#&nbsp;fitting&nbsp;1PL&nbsp;model<br>fit&nbsp;<-&nbsp;rasch(GMAT[,&nbsp;1:20])<br>#&nbsp;for&nbsp;Rasch&nbsp;model&nbsp;use<br>#&nbsp;fit&nbsp;<-&nbsp;rasch(GMAT[, 1:20],&nbsp;constraint&nbsp;=&nbsp;cbind(ncol(GMAT[, 1:20])&nbsp;+&nbsp;1,&nbsp;1))<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"IIC\")<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;items&nbsp;=&nbsp;0,&nbsp;type&nbsp;=&nbsp;\"IIC\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>df1&nbsp;<-&nbsp;ltm::factor.scores(fit,&nbsp;return.MIvalues&nbsp;=&nbsp;TRUE)$score.dat<br>FS&nbsp;<-&nbsp;as.vector(df1[,&nbsp;\"z1\"])<br>df2&nbsp;<-&nbsp;df1<br>df2$Obs&nbsp;<-&nbsp;df2$Exp&nbsp;<-&nbsp;df2$z1&nbsp;<-&nbsp;df2$se.z1&nbsp;<-&nbsp;NULL<br>STS&nbsp;<-&nbsp;as.vector(scale(rowSums(df2[,&nbsp;1:20])))<br>df&nbsp;<-&nbsp;data.frame(FS,&nbsp;STS)<br>plot(FS&nbsp;~&nbsp;STS,&nbsp;data&nbsp;=&nbsp;df,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(FS,&nbsp;STS)"))),
        br()
      ),
      tabPanel("Items",
        value = "1pl_it",
        h3("1PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed to be latent
                                        and is estimated together with item paramters. "),
        p("In", strong("One Parameter Logistic (1PL) IRT model,"), "all items are assumed to have the same slope in inflection point, i.e., the same discrimination \\(a\\).
                                        Its value corresponds to standard deviation of ability estimates in Rasch model. Items can differ in location
                                        of their inflection point, i.e., in item difficulty parameters \\(b_i\\). Model parameters are estimated using
                                        marginal maximum likelihood method. Ability \\(\\theta_p\\) is assumed to follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = \\frac{e^{a\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  a^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        # ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m a^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        h4("Item characteristic curves"),
        sliderInput("onePLSliderChar", "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("oneparamirt_mirt_tab"),
        downloadButton("DP_oneparamirt_mirt_tab", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("oneparamirtiic_mirt_tab"),
        downloadButton("DP_oneparamirtiic_mirt_tab", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("tab_coef_oneparamirt_mirt")
      )
    )
  ),
  # * 2PL ####
  tabPanel(
    "2PL",
    tabsetPanel(
      tabPanel("Model",
        value = "2pl_mod",
        h3("2PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters."),
        p(strong("Two Parameter Logistic (2PL) IRT model"), " allows for different slopes in inflection point, i.e., different discrimination parameters \\(a_i\\).
                                        Items can also differ in location of their inflection point, i.e., in item difficulty parameters
                                        \\(b_i\\). Model parameters are estimated using marginal maximum likelihood method. Ability
                                        \\(\\theta_p\\) is assumed to follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = \\frac{e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  a_i^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m a_i^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        uiOutput("irt_2PL_model_converged"),
        h4("Item characteristic curves"),
        plotlyOutput("twoparamirt_mirt"),
        downloadButton("DP_twoparamirt_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("twoparamirtiic_mirt"),
        downloadButton("DP_twoparamirtiic_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Test information function"),
        plotlyOutput("twoparamirttif_mirt"),
        downloadButton("DP_twoparamirttif_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("coef_twoparamirt_mirt"),
        downloadButton(
          outputId = "download_2pl_table",
          label = "Download table"
        ),
        br(),
        br(),
        h4("Ability estimates"),
        p("This table shows the response score of only six respondents. If you want to see scores for all respondents, click on ", strong("Download abilities"), " button."),
        tableOutput("two_PL_abilities"),
        br(),
        downloadButton(
          outputId = "download_twoPL_abilities",
          label = "Download abilities"
        ),
        br(),
        br(),
        h4("Scatter plot of factor scores and standardized total scores"),
        textOutput("twoparamirtFactorCor_mirt"),
        plotOutput("twoparamirtFactor_mirt"),
        downloadButton("DP_twoparamirtFactor_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Selected R code"),
        div(code(HTML("library(ltm)<br>library(mirt)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;fitting&nbsp;2PL&nbsp;model<br>fit&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"2PL\",&nbsp;SE&nbsp;=&nbsp;TRUE)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"trace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infotrace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infoSE\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;classical&nbsp;intercept-slope&nbsp;parametrization<br>coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;IRT&nbsp;parametrization<br><br>#&nbsp;item&nbsp;fit&nbsp;statistics<br>itemfit(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>fs&nbsp;<-&nbsp;as.vector(fscores(fit))<br>sts&nbsp;<-&nbsp;as.vector(scale(rowSums(GMAT[,&nbsp;1:20])))<br>plot(fs&nbsp;~&nbsp;sts,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(fs,&nbsp;sts)<br><br>#&nbsp;you&nbsp;can&nbsp;also&nbsp;use&nbsp;the&nbsp;ltm&nbsp;package<br>#&nbsp;fitting&nbsp;2PL&nbsp;model<br>fit&nbsp;<-&nbsp;ltm(GMAT[,&nbsp;1:20]&nbsp;~&nbsp;z1,&nbsp;IRT.param&nbsp;=&nbsp;TRUE)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"IIC\")<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;items&nbsp;=&nbsp;0,&nbsp;type&nbsp;=&nbsp;\"IIC\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>df1&nbsp;<-&nbsp;ltm::factor.scores(fit,&nbsp;return.MIvalues&nbsp;=&nbsp;TRUE)$score.dat<br>FS&nbsp;<-&nbsp;as.vector(df1[,&nbsp;\"z1\"])<br>df2&nbsp;<-&nbsp;df1<br>df2$Obs&nbsp;<-&nbsp;df2$Exp&nbsp;<-&nbsp;df2$z1&nbsp;<-&nbsp;df2$se.z1&nbsp;<-&nbsp;NULL<br>STS&nbsp;<-&nbsp;as.vector(scale(rowSums(df2[,&nbsp;1:20])))<br>df&nbsp;<-&nbsp;data.frame(FS,&nbsp;STS)<br>plot(FS&nbsp;~&nbsp;STS,&nbsp;data&nbsp;=&nbsp;df,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(FS,&nbsp;STS)"))),
        br()
      ),
      tabPanel("Items",
        value = "2pl_it",
        h3("2PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters."),
        p(strong("Two Parameter Logistic (2PL) IRT model"), " allows for different slopes in inflection point, i.e., different discrimination parameters \\(a_i\\).
                                        Items can also differ in location of their inflection point, i.e., in item difficulty parameters
                                        \\(b_i\\). Model parameters are estimated using marginal maximum likelihood method. Ability
                                        \\(\\theta_p\\) is assumed to follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = \\frac{e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  a_i^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        # ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m a_i^2 \\pi_{pi} (1 - \\pi_{pi})$$"),
        h4("Item characteristic curves"),
        sliderInput("twoPLSliderChar", "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("twoparamirt_mirt_tab"),
        downloadButton("DP_twoparamirt_mirt_tab", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("twoparamirtiic_mirt_tab"),
        downloadButton("DP_twoparamirtiic_mirt_tab", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("tab_coef_twoparamirt_mirt")
      )
    )
  ),
  # * 3PL ####
  tabPanel(
    "3PL",
    tabsetPanel(
      tabPanel("Model",
        value = "3pl_mod",
        h3("3PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters. "),
        p(strong("Three Parameter Logistic (3PL) IRT model"), " allows for different discriminations of items \\(a_i\\), different item difficulties \\(b_i\\)
                                        and allows also for nonzero left asymptote, pseudo-guessing \\(c_i\\). Model parameters are
                                        estimated using marginal maximum likelihood method. Ability \\(\\theta_p\\) is assumed to
                                        follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = c_i + (1 - c_i) \\cdot \\frac{e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (1 - \\pi_{pi})}{(1 - c_i^2) \\pi_{pi}}$$"),
        ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (1 - \\pi_{pi})}{(1 - c_i^2) \\pi_{pi}}$$"),
        uiOutput("irt_3PL_model_converged"),
        h4("Item characteristic curves"),
        plotlyOutput("threeparamirt_mirt"),
        downloadButton("DP_threeparamirt_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("threeparamirtiic_mirt"),
        downloadButton("DP_threeparamirtiic_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Test information function"),
        plotlyOutput("threeparamirttif_mirt"),
        downloadButton("DP_threeparamirttif_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("coef_threeparamirt_mirt"),
        downloadButton(
          outputId = "download_3pl_table",
          label = "Download table"
        ),
        br(),
        br(),
        h4("Ability estimates"),
        p("This table shows the response score of only six respondents. If you want to see scores for all respondents, click on ", strong("Download abilities"), " button."),
        tableOutput("three_PL_abilities"),
        downloadButton(
          outputId = "download_threePL_abilities",
          label = "Download abilities"
        ),
        br(),
        br(),
        h4("Scatter plot of factor scores and standardized total scores"),
        textOutput("threeparamirtFactorCor_mirt"),
        plotOutput("threeparamirtFactor_mirt"),
        downloadButton("DP_threeparamirtFactor_mirt", label = "Download figure"),
        br(),
        br(),
        h4("Selected R code"),
        div(code(HTML("library(ltm)<br>library(mirt)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;fitting&nbsp;3PL&nbsp;model<br>fit&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"3PL\",&nbsp;SE&nbsp;=&nbsp;TRUE)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"trace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infotrace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infoSE\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;classical&nbsp;intercept-slope&nbsp;parametrization<br>coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;IRT&nbsp;parametrization<br><br>#&nbsp;item&nbsp;fit&nbsp;statistics<br>itemfit(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>fs&nbsp;<-&nbsp;as.vector(fscores(fit))<br>sts&nbsp;<-&nbsp;as.vector(scale(rowSums(GMAT[,&nbsp;1:20])))<br>plot(fs&nbsp;~&nbsp;sts,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(fs,&nbsp;sts)<br><br>#&nbsp;you&nbsp;can&nbsp;also&nbsp;use&nbsp;the&nbsp;ltm&nbsp;package<br>#&nbsp;fitting&nbsp;3PL&nbsp;model<br>fit&nbsp;<-&nbsp;tpm(GMAT[,&nbsp;1:20],&nbsp;IRT.param&nbsp;=&nbsp;TRUE)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"IIC\")<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;items&nbsp;=&nbsp;0,&nbsp;type&nbsp;=&nbsp;\"IIC\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>df1&nbsp;<-&nbsp;ltm::factor.scores(fit,&nbsp;return.MIvalues&nbsp;=&nbsp;TRUE)$score.dat<br>FS&nbsp;<-&nbsp;as.vector(df1[,&nbsp;\"z1\"])<br>df2&nbsp;<-&nbsp;df1<br>df2$Obs&nbsp;<-&nbsp;df2$Exp&nbsp;<-&nbsp;df2$z1&nbsp;<-&nbsp;df2$se.z1&nbsp;<-&nbsp;NULL<br>STS&nbsp;<-&nbsp;as.vector(scale(rowSums(df2[,&nbsp;1:20])))<br>df&nbsp;<-&nbsp;data.frame(FS,&nbsp;STS)<br>plot(FS&nbsp;~&nbsp;STS,&nbsp;data&nbsp;=&nbsp;df,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(FS,&nbsp;STS)"))),
        br()
      ),
      tabPanel("Items",
        value = "3pl_it",
        h3("3PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters. "),
        p(strong("Three Parameter Logistic (3PL) IRT model"), " allows for different discriminations of items \\(a_i\\), different item difficulties \\(b_i\\)
                                        and allows also for nonzero left asymptote, pseudo-guessing \\(c_i\\). Model parameters are
                                        estimated using marginal maximum likelihood method. Ability \\(\\theta_p\\) is assumed to
                                        follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = c_i + (1 - c_i) \\cdot \\frac{e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (1 - \\pi_{pi})}{(1 - c_i^2) \\pi_{pi}}$$"),
        # ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (1 - \\pi_{pi})}{(1 - c_i^2) \\pi_{pi}}$$"),
        uiOutput("irt_3PL_model_converged_tab"),
        h4("Item characteristic curves"),
        sliderInput("threePLSliderChar", "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("threeparamirt_mirt_tab"),
        downloadButton("DP_threeparamirt_mirt_tab", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("threeparamirtiic_mirt_tab"),
        downloadButton("DP_threeparamirtiic_mirt_tab", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("tab_coef_threeparamirt_mirt")
      )
    )
  ),
  # * 4PL ####
  tabPanel(
    "4PL",
    tabsetPanel(
      tabPanel("Model",
        value = "4pl_mod",
        h3("4PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters.  "),
        p(strong("Four Parameter Logistic (4PL) IRT model"), " allows for different discriminations of items \\(a_i\\), different item difficulties \\(b_i\\),
                                        nonzero left asymptotes, pseudo-guessing \\(c_i\\) and also for upper asymptote lower than one,
                                        i.e, inattention parameter \\(d_i\\). Model parameters are estimated using marginal maximum
                                        likelihood method. Ability \\(\\theta_p\\) is assumed to follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = c_i + (d_i - c_i) \\cdot \\frac{e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (d_i - \\pi_{pi})^2}{(d_i - c_i^2) \\pi_{pi} (1 - \\pi_{pi})}$$"),
        ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (d_i - \\pi_{pi})^2}{(d_i - c_i^2) \\pi_{pi} (1 - \\pi_{pi})}$$"),
        uiOutput("irt_4PL_model_converged"),
        h4("Item characteristic curves"),
        plotlyOutput("irt_4PL_icc"),
        downloadButton("DB_irt_4PL_icc", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("irt_4PL_iic"),
        downloadButton("DB_irt_4PL_iic", label = "Download figure"),
        br(),
        br(),
        h4("Test information function"),
        plotlyOutput("irt_4PL_tif"),
        downloadButton("DB_irt_4PL_tif", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("coef_irt_4PL"),
        downloadButton(
          outputId = "download_4pl_table",
          label = "Download table"
        ),
        br(),
        br(),
        h4("Ability estimates"),
        p("This table shows the response score of only six respondents. If you want to see scores for all respondents, click on ", strong("Download abilities"), " button."),
        tableOutput("four_PL_abilities"),
        downloadButton(
          outputId = "download_fourPL_abilities",
          label = "Download abilities"
        ),
        br(),
        br(),
        h4("Scatter plot of factor scores and standardized total scores"),
        textOutput("irt_4PL_factorscores_correlation"),
        plotOutput("irt_4PL_factorscores_plot"),
        downloadButton("DB_irt_4PL_factorscores_plot", label = "Download figure"),
        br(),
        br(),
        h4("Selected R code"),
        div(code(HTML("library(mirt)<br>library(ShinyItemAnalysis)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;fitting&nbsp;4PL&nbsp;model<br>fit&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"4PL\",&nbsp;SE&nbsp;=&nbsp;TRUE)<br><br>#&nbsp;item&nbsp;characteristic&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"trace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infotrace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infoSE\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;classical&nbsp;intercept-slope&nbsp;parametrization<br>coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;IRT&nbsp;parametrization<br><br>#&nbsp;item&nbsp;fit&nbsp;statistics<br>itemfit(fit)<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>fs&nbsp;<-&nbsp;as.vector(fscores(fit))<br>sts&nbsp;<-&nbsp;as.vector(scale(rowSums(GMAT[,&nbsp;1:20])))<br>plot(fs&nbsp;~&nbsp;sts,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(fs,&nbsp;sts)"))),
        br()
      ),
      tabPanel("Items",
        value = "4pl_it",
        h3("4PL IRT model"),
        p("Item Response Theory (IRT) models are mixed-effect regression models in which respondent ability \\(\\theta_p\\) is assumed
                                        to be latent and is estimated together with item paramters.  "),
        p(strong("Four Parameter Logistic (4PL) IRT model"), " allows for different discriminations of items \\(a_i\\), different item difficulties \\(b_i\\),
                                        nonzero left asymptotes, pseudo-guessing \\(c_i\\) and also for upper asymptote lower than one,
                                        i.e, inattention parameter \\(d_i\\). Model parameters are estimated using marginal maximum
                                        likelihood method. Ability \\(\\theta_p\\) is assumed to follow standard normal distribution. "),
        h4("Equations"),
        ("$$\\mathrm{P}\\left(Y_{pi} = 1\\vert \\theta_{p}\\right) =  \\pi_{pi} = c_i + (d_i - c_i) \\cdot \\frac{e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}{1 + e^{a_i\\left(\\theta_{p} - b_{i}\\right)}}$$"),
        ("$$\\mathrm{I}_i(\\theta_p) =  \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (d_i - \\pi_{pi})^2}{(d_i - c_i^2) \\pi_{pi} (1 - \\pi_{pi})}$$"),
        # ("$$\\mathrm{T}(\\theta_p) =  \\sum_{i = 1}^m \\mathrm{I}_i(\\theta_p) = \\sum_{i = 1}^m \\frac{a_i^2 (\\pi_{pi} - c_i)^2 (d_i - \\pi_{pi})^2}{(d_i - c_i^2) \\pi_{pi} (1 - \\pi_{pi})}$$"),
        uiOutput("irt_4PL_model_converged_item"),
        h4("Item characteristic curves"),
        sliderInput("fourPLSliderChar", "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("irt_4PL_icc_item_tab"),
        downloadButton("DB_irt_4PL_icc_tab", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("irt_4PL_iic_item_tab"),
        downloadButton("DB_irt_4PL_iic_tab", label = "Download figure"),
        br(),
        br(),
        h4("Table of estimated parameters"),
        p("Estimates of parameters are completed by SX2 item fit statistics (Orlando & Thissen, 2000).
                                        SX2 statistics are computed only when no missing data are present."),
        tableOutput("tab_coef_irt_4PL")
      )
    )
  ),
  # * MODEL COMPARISON ####
  tabPanel("Model comparison",
    value = "irt_mod_comp",
    h3("IRT model selection"),
    withMathJax(),
    p("Item Response Theory (IRT) models are mixed-effect regression models in which
                             respondent ability \\(\\theta\\) is assumed to be latent and is estimated together with item
                             paramters. Model parameters are estimated using marginal maximum likelihood (MML) method,
                             in 1PL, 2PL, 3PL and 4PL IRT models, ability \\(\\theta\\) is assumed to follow standard normal distribution."),
    p("IRT models can be compared by several information criteria: "),
    tags$ul(
      tags$li(strong("AIC"), "is the Akaike information criterion (Akaike, 1974), "),
      tags$li(strong("AICc"), "is AIC with a correction for finite sample size, "),
      tags$li(strong("BIC"), "is the Bayesian information criterion (Schwarz, 1978)."),
      tags$li(strong("SABIC"), "is the Sample-sized adjusted BIC criterion, ")
    ),
    # p('Another approach to compare IRT models can be likelihood ratio chi-squared test.
    # Significance level is set to 0.05.'),
    h4("Table of comparison statistics"),
    # p('Row ', strong('BEST'), 'indicates which model has the lowest value of criterion, or is the largest
    # significant model by likelihood ratio test.'),
    p("Row ", strong("BEST"), "indicates which model has the lowest value of given information criterion."),
    tableOutput("irtcomparison"),
    tags$style(type = "text/css", "#irtcomparison tr:last-child {font-weight:bold;}"),
    br(),
    h4("Selected R code"),
    div(code(HTML("library(mirt)<br><br>#&nbsp;loading&nbsp;data<br>data(GMAT,&nbsp;package&nbsp;=&nbsp;\"difNLR\")<br><br>#&nbsp;1PL&nbsp;IRT&nbsp;model<br>fit1PL&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;constrain&nbsp;=&nbsp;list((1:20)&nbsp;+&nbsp;seq(0,&nbsp;(20&nbsp;-&nbsp;1)&nbsp;*&nbsp;3,&nbsp;3)),&nbsp;itemtype&nbsp;=&nbsp;\"2PL\")<br>#&nbsp;2PL&nbsp;IRT&nbsp;model<br>fit2PL&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"2PL\")<br>#&nbsp;3PL&nbsp;IRT&nbsp;model<br>fit3PL&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"3PL\")<br>#&nbsp;4PL&nbsp;IRT&nbsp;model<br>fit4PL&nbsp;<-&nbsp;mirt(GMAT[,&nbsp;1:20],&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"4PL\")<br><br>#&nbsp;comparison<br>anova(fit1PL,&nbsp;fit2PL)<br>anova(fit2PL,&nbsp;fit3PL)<br>anova(fit3PL,&nbsp;fit4PL)"))),
    br()
  ),
  "----",
  "Polytomous models",
  # * BOCK'S NOMINAL MODEL ####
  tabPanel("Bock's nominal model",
    value = "bock",
    tabsetPanel(
      tabPanel("Model",
        value = "bock_mod",
        h3("Bock's nominal IRT model"),
        p("The Nominal Response Model (NRM) was introduced by Bock (1972) as a way to model responses to items with two or more nominal
                                        categories. This model is suitable for multiple-choice items with no particular ordering of
                                        distractors. It is also generalization of some models for ordinal data, e.g., Generalized Partial
                                        Credit Model (GPCM) or its restricted versions Partial Credit Model (PCM) and Rating Scale Model
                                        (RSM)."),
        h4("Equations"),
        withMathJax(
          "For ", strong("\\(K_i\\)"), " possible test choices the probability of the choice ", strong("\\(k\\)"), " for person ",
          strong("\\(p\\)"), " with latent trait", strong("\\(\\theta_p\\)"), " in item ", strong("\\(i\\)"),
          "is given by the following equation: "
        ),
        ("$$\\mathrm{P}(Y_{pi} = k|\\theta_p) =
                                       \\frac{e^{(ak_{i(k - 1)} a_{i1} \\theta_p + d_{i(k - 1)})}}{\\sum_l e^{(al_{i(l - 1)} a_{i1} \\theta_p + d_{i(l - 1)})}}$$"),
        br(),
        h4("Item characteristic curves"),
        plotlyOutput("bock_CC"),
        downloadButton("DP_bock_CC", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("bock_IIC"),
        downloadButton("DP_bock_IIC", label = "Download figure"),
        br(),
        br(),
        h4("Test information function"),
        plotlyOutput("bock_TIF"),
        downloadButton("DP_bock_TIF", label = "Download figure"),
        br(),
        br(),
        h4("Table of parameters"),
        tableOutput("coef_bock"),
        h4("Ability estimates"),
        p("This table shows the response score of only six respondents. If you want to see scores for all respondents, click on", strong("Download abilities"), " button. "),
        tableOutput("bock_abilities"),
        downloadButton("DP_bock_ability", label = "Download abilities"),
        br(),
        br(),
        h4("Scatter plot of factor scores and standardized total scores"),
        textOutput("bockFactorCorInput_mirt"),
        plotOutput("bock_factor"),
        downloadButton("DP_bock_factor", label = "Download figure"),
        br(),
        br(),
        h4("Selected R code"),
        div(code(HTML("library(mirt)<br><br>#&nbsp;loading&nbsp;data<br>data(HCItest,&nbsp;HCI,&nbsp;package&nbsp;=&nbsp;\"ShinyItemAnalysis\")<br>HCInumeric&nbsp;<-&nbsp;HCItest[,&nbsp;1:20]<br>HCInumeric[]&nbsp;<-&nbsp;sapply(HCInumeric,&nbsp;as.numeric)<br><br>#&nbsp;model<br>fit&nbsp;<-&nbsp;mirt(HCInumeric,&nbsp;model&nbsp;=&nbsp;1,&nbsp;itemtype&nbsp;=&nbsp;\"nominal\")<br><br>#&nbsp;item&nbsp;response&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"trace\")<br>#&nbsp;item&nbsp;information&nbsp;curves<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infotrace\",&nbsp;facet_items&nbsp;=&nbsp;FALSE)<br>#&nbsp;test&nbsp;information&nbsp;curve<br>plot(fit,&nbsp;type&nbsp;=&nbsp;\"infoSE\")<br><br>#&nbsp;estimated&nbsp;parameters<br>coef(fit,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;classical&nbsp;intercept-slope&nbsp;parametrization<br>coef(fit,&nbsp;IRTpars&nbsp;=&nbsp;TRUE,&nbsp;simplify&nbsp;=&nbsp;TRUE)&nbsp;#&nbsp;IRT&nbsp;parametrization<br><br>#&nbsp;factor&nbsp;scores&nbsp;vs&nbsp;standardized&nbsp;total&nbsp;scores<br>fs&nbsp;<-&nbsp;as.vector(fscores(fit))<br>sts&nbsp;<-&nbsp;as.vector(scale(rowSums(HCI[,&nbsp;1:20])))<br>plot(fs&nbsp;~&nbsp;sts,&nbsp;xlab&nbsp;=&nbsp;\"Standardized&nbsp;total&nbsp;score\",&nbsp;ylab&nbsp;=&nbsp;\"Factor&nbsp;score\")<br>cor(fs,&nbsp;sts)"))),
        br(),
        br()
      ),
      tabPanel("Items",
        value = "bock_it",
        h3("Bock's nominal IRT model"),
        p("The Nominal Response Model (NRM) was introduced by Bock (1972) as a way to model responses to items with two or more nominal
                                        categories. This model is suitable for multiple-choice items with no particular ordering of
                                        distractors. It is also generalization of some models for ordinal data, e.g., Generalized Partial
                                        Credit Model (GPCM) or its restricted versions Partial Credit Model (PCM) and Rating Scale Model
                                        (RSM)."),
        h4("Equations"),
        withMathJax(
          "For ", strong("\\(K_i\\)"), " possible test choices the probability of the choice ", strong("\\(k\\)"), " for person ",
          strong("\\(p\\)"), " with latent trait", strong("\\(\\theta_p\\)"), " in item ", strong("\\(i\\)"),
          "is given by the following equation: "
        ),
        ("$$\\mathrm{P}(Y_{pi} = k|\\theta_p) =
                                       \\frac{e^{(ak_{i(k - 1)} a_{i1} \\theta_p + d_{i(k - 1)})}}{\\sum_l e^{(al_{i(l - 1)} a_{i1} \\theta_p + d_{i(l - 1)})}}$$"),
        br(),
        h4("Item characteristic curves"),
        sliderInput("bockSlider", "Item",
          min = 1, value = 1, max = 20,
          step = 1, animate = TRUE
        ),
        plotlyOutput("bock_CC_tab"),
        downloadButton("DP_bock_CC_tab", label = "Download figure"),
        br(),
        br(),
        h4("Item information curves"),
        plotlyOutput("bock_IIC_tab"),
        downloadButton("DP_bock_IIC_tab", label = "Download figure"),
        br(),
        br(),
        h4("Table of parameters"),
        fluidRow(column(12, align = "center", tableOutput("tab_coef_bock"))),
        br(),
        br()
      )
    )
  ),
  "----",
  "Training",
  # * TRAINING  ####
  # ** Dichotomous models ####
  uiDIRT,
  # ** Polytomous models ####
  tabPanel(
    "Polytomous models",
    tabsetPanel(
      # *** Intro ####
      tabPanel("Intro",
        value = "polytom_intro",
        h3("Polytomous models"),
        p("Polytomous models are used when partial score is possible, or when items are graded
                                        on Likert scale (e.g. from Totally disagree to Totally agree); some polytomous
                                        models can also be used when analyzing multiple-choice items.  In this section you
                                        can explore item response functions of some polytomous models."),
        br(),
        p("Two main classes of polytomous IRT models are considered:"),
        p(strong("Difference models"), "are defined by setting mathematical form to cumulative
                                        probabilities, while category probabilities are calculated as their difference.
                                        These models are also sometimes called", strong("cumulative logit models"), "as they
                                        set linear form to cumulative logits."),
        p("As an example, ", strong("Graded Response Model"), "(GRM; Samejima, 1970) uses 2PL
                                        IRT model to describe cumulative probabilities (probabilities to obtain score higher
                                        than 1, 2, 3, etc.). Category probabilities are then described as differences of two
                                        subsequent cumulative probabilities. "), br(),
        p("For", strong("divide-by-total models"), "response category probabilities are defined
                                        as the ratio between category-related functions and their sum. "),
        p(
          "In", strong("Generalized Partial Credit Model"), "(GPCM; Muraki, 1992), probability
                                        of the successful transition from one category score to the next category score is
                                        modelled by 2PL IRT model, while ", strong("Partial Credit Model"), "(PCM; Masters, 1982)
                                        uses 1PL IRT model to describe this probability. Even more restricted version, the",
          strong("Rating Scale Model"), "(RSM; Andrich, 1978) assumes exactly the same K response
                                        categories for each item and threshold parameters which can be split into a response-threshold
                                        parameter and an item-specific location parameter. These models are also sometimes called
                                        ", strong("adjacent-category logit models"), "as they set linear form to adjacent logits."
        ),
        p(
          "To model distractor properties in multiple-choice items,", strong("Nominal Response Model"),
          "(NRM; Bock, 1972) can be used. NRM is an IRT analogy of multinomial regression model. This
                                        model is also generalization of GPCM/PCM/RSM ordinal models. NRM is also sometimes called
                                        ", strong("baseline-category logit model"), "as it sets linear form to log of odds of selecting given category
                                        to selecting a baseline category. Baseline can be chosen arbitrary, although usually the correct
                                        answer or the first answer is chosen."
        )
      ),
      # *** Graded response model ####
      tabPanel("Graded response model",
        value = "polytom_grm",
        h3("Graded response model"),
        p("Graded response model (GRM; Samejima, 1970) uses 2PL IRT model to describe cumulative probabilities
                                        (probabilities to obtain score higher than 1, 2, 3, etc.). Category probabilities are then described
                                        as differences of two subsequent cumulative probabilities. "),
        p("It belongs to class of difference models, which are defined by setting mathematical form to cumulative
                                        probabilities, while category probabilities are calculated as their difference. These models are also
                                        sometimes called cumulative logit models, as they set linear form to cumulative logits."),

        HTML("<div class='pb' style='page-break-after:always'></div>"),

        h4("Parameters"),
        p("Select number of responses and difficulty for cummulative probabilities \\(b\\) and common
                                        discrimination parameter \\(a\\). Cummulative probability \\(P(Y \\geq 0)\\) is always equal to 1
                                        and it is not displayed, corresponding category probability \\(P(Y = 0)\\) is displayed with black color."),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          numericInput(
            inputId = "irt_training_grm_numresp",
            label = "Highest score",
            value = 4,
            min = 2,
            max = 6
          )
        ),
        br(),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          sliderInput(
            inputId = "irt_training_grm_a",
            label = "\\(a\\) - discrimination",
            value = 1,
            min = 0,
            max = 4,
            step = 0.1
          )
        ),
        br(),
        uiOutput("irt_training_grm_sliders"),
        br(),
        h4("Equations"),
        ("$$\\pi_k* = \\mathrm{P}\\left(Y \\geq k \\vert \\theta, a, b_k\\right) = \\frac{e^{a\\left(\\theta-b\\right) }}{1+e^{a\\left(\\theta-b\\right) }} $$"),
        ("$$\\pi_k =\\mathrm{P}\\left(Y = k \\vert \\theta, a, b_k, b_{k+1}\\right) = \\pi_k* - \\pi_{k+1}* $$"),
        ("$$\\mathrm{E}\\left(Y \\vert \\theta, a, b_1, \\dots, b_K\\right) = \\sum_{k = 0}^K k\\pi_k$$"),
        h4("Plots"),
        splitLayout(
          cellWidths = c("33%", "33%", "33%"),
          plotlyOutput("irt_training_grm_plot_cummulative"),
          plotlyOutput("irt_training_grm_plot_category"),
          plotlyOutput("irt_training_grm_plot_expected")
        ),
        splitLayout(
          cellWidths = c("33%", "33%", "33%"),
          downloadButton("DB_irt_training_grm_plot_cummulative", label = "Download figure"),
          downloadButton("DB_irt_training_grm_plot_category", label = "Download figure"),
          downloadButton("DB_irt_training_grm_plot_expected", label = "Download figure")
        ),
        br(),
        #------------------------------------------------------------------------------------#
        # **** Exercise ####
        #------------------------------------------------------------------------------------#
        h4("Exercise "),
        p("Consider item following graded response model rated \\(0-1-2-3\\), with discrimination \\(a = 1\\) and
										 difficulties \\(b_{1} = \u2212 0.5\\), \\(b_{2} = 1\\) and \\(b_{3} = 1.5\\)."),
        tags$ul(
          tags$li(
            "Calculate probabilities of obtaining \\(k\\) and more points for specific level of ability \\(\\theta\\)",
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 0 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_1a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_1e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_1e_answer"), ""
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 1 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_2a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_2e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_2e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 2 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_3a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_3e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_3e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k \\geq 3 \\)"),
              numericInput(
                inputId = "irt_training_grm_1_4a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4a_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4b_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4c_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4d_answer"),
              numericInput(
                inputId = "irt_training_grm_1_4e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_1_4e_answer")
            ), ""
          ),

          tags$li(
            "Calculate probabilities of obtaining  exactly \\(k\\) points for specific level of ability \\(\\theta\\)",
            bsButton(
              inputId = "irt_training_grm_1_help",
              label = "", icon = icon("question"),
              style = "info", size = "extra-small"
            ),
            bsPopover(
              id = "irt_training_grm_1_help",
              title = "Help",
              content = "What should be the sum in the columns?",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 0\\) "),
              numericInput(
                inputId = "irt_training_grm_2_1a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_1e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_1e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 1\\) "),
              numericInput(
                inputId = "irt_training_grm_2_2a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_2e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_2e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 2\\) "),
              numericInput(
                inputId = "irt_training_grm_2_3a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_3e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_3e_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "7%", "4%", "38%"),
              strong("\\(k = 3\\) "),
              numericInput(
                inputId = "irt_training_grm_2_4a",
                label = "\\(\\theta = -2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4a_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4b",
                label = "\\(\\theta = -1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4b_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4c",
                label = "\\(\\theta = 0\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4c_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4d",
                label = "\\(\\theta = 1\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4d_answer"),
              numericInput(
                inputId = "irt_training_grm_2_4e",
                label = "\\(\\theta = 2\\)", value = 0
              ),
              htmlOutput("irt_training_grm_2_4e_answer")
            ), ""
          ),
          tags$li(
            "What is the expected item score for specific level of ability \\(\\theta\\)?",
            bsButton(
              inputId = "irt_training_grm_2_help",
              label = "", icon = icon("question"),
              style = "info", size = "extra-small"
            ),
            bsPopover(
              id = "irt_training_grm_2_help",
              title = "Help",
              content = "Look at the third figure.",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -2\\)"),
              numericInput(
                inputId = "irt_training_grm_3_1a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_1a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -1\\)"),
              numericInput(
                inputId = "irt_training_grm_3_2a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_2a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 0\\)"),
              numericInput(
                inputId = "irt_training_grm_3_3a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_3a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 1\\)"),
              numericInput(
                inputId = "irt_training_grm_3_4a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_4a_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 2\\)"),
              numericInput(
                inputId = "irt_training_grm_3_5a",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_grm_3_5a_answer")
            ),
            div(
              style = "display: inline-block; float: right; width: 200px;",
              htmlOutput("irt_training_grm_answer"),
              actionButton(
                inputId = "irt_training_grm_1_submit",
                label = "Submit answers", width = "80%"
              )
            ), ""
          )
        ),
        br(),
        h4("Selected R code"),
        div(code(HTML("library(ggplot2)&nbsp;<br>library(data.table)&nbsp;<br><br>#&nbsp;setting&nbsp;parameters&nbsp;<br>a&nbsp;<-&nbsp;1&nbsp;<br>b&nbsp;<-&nbsp;c(-1.5,&nbsp;-1,&nbsp;-0.5,&nbsp;0)&nbsp;<br>theta&nbsp;<-&nbsp;seq(-4,&nbsp;4,&nbsp;0.01)&nbsp;<br><br>#&nbsp;calculating&nbsp;cummulative&nbsp;probabilities&nbsp;<br>ccirt&nbsp;<-&nbsp;function(theta,&nbsp;a,&nbsp;b){&nbsp;return(1/(1&nbsp;+&nbsp;exp(-a*(theta&nbsp;-&nbsp;b))))&nbsp;}&nbsp;<br>df1&nbsp;<-&nbsp;data.frame(sapply(1:length(b),&nbsp;function(i)&nbsp;ccirt(theta,&nbsp;a,&nbsp;b[i]))&nbsp;,&nbsp;theta)<br>df1&nbsp;<-&nbsp;melt(df1,&nbsp;id.vars&nbsp;=&nbsp;\"theta\")&nbsp;<br><br>#&nbsp;plotting&nbsp;cummulative&nbsp;probabilities&nbsp;<br>ggplot(data&nbsp;=&nbsp;df1,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;value,&nbsp;col&nbsp;=&nbsp;variable))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Cummulative&nbsp;probability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Cummulative&nbsp;probabilities\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;scale_color_manual(\"\",&nbsp;values&nbsp;=&nbsp;c(\"red\",&nbsp;\"yellow\",&nbsp;\"green\",&nbsp;\"blue\"),&nbsp;labels&nbsp;=&nbsp;paste0(\"P(Y&nbsp;>=&nbsp;\",&nbsp;1:4,&nbsp;\")\"))&nbsp;<br><br>#&nbsp;calculating&nbsp;category&nbsp;probabilities&nbsp;<br>df2&nbsp;<-&nbsp;data.frame(1,&nbsp;sapply(1:length(b),&nbsp;function(i)&nbsp;ccirt(theta,&nbsp;a,&nbsp;b[i])))&nbsp;<br>df2&nbsp;<-&nbsp;data.frame(sapply(1:length(b),&nbsp;function(i)&nbsp;df2[,&nbsp;i]&nbsp;-&nbsp;df2[,&nbsp;i+1]),&nbsp;df2[,&nbsp;ncol(df2)],&nbsp;theta)&nbsp;<br>df2&nbsp;<-&nbsp;melt(df2,&nbsp;id.vars&nbsp;=&nbsp;\"theta\")&nbsp;<br><br>#&nbsp;plotting&nbsp;category&nbsp;probabilities&nbsp;<br>ggplot(data&nbsp;=&nbsp;df2,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;value,&nbsp;col&nbsp;=&nbsp;variable))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Category&nbsp;probability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Category&nbsp;probabilities\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;scale_color_manual(\"\",&nbsp;values&nbsp;=&nbsp;c(\"black\",&nbsp;\"red\",&nbsp;\"yellow\",&nbsp;\"green\",&nbsp;\"blue\"),&nbsp;labels&nbsp;=&nbsp;paste0(\"P(Y&nbsp;>=&nbsp;\",&nbsp;0:4,&nbsp;\")\"))<br><br>#&nbsp;calculating&nbsp;expected&nbsp;item&nbsp;score<br>df3&nbsp;<-&nbsp;data.frame(1,&nbsp;sapply(1:length(b),&nbsp;function(i)&nbsp;ccirt(theta,&nbsp;a,&nbsp;b[i])))&nbsp;<br>df3&nbsp;<-&nbsp;data.frame(sapply(1:length(b),&nbsp;function(i)&nbsp;df3[,&nbsp;i]&nbsp;-&nbsp;df3[,&nbsp;i+1]),&nbsp;df3[,&nbsp;ncol(df3)])<br>df3&nbsp;<-&nbsp;data.frame(exp&nbsp;=&nbsp;as.matrix(df3)&nbsp;%*%&nbsp;0:4,&nbsp;theta)<br><br>#&nbsp;plotting&nbsp;category&nbsp;probabilities&nbsp;<br>ggplot(data&nbsp;=&nbsp;df3,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;exp))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Expected&nbsp;item&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Expected&nbsp;item&nbsp;score\")"))),
        br(),
        br()
      ),
      # *** Generalized partial credit model ####
      tabPanel("Generalized partial credit model",
        value = "polytom_gpcm",
        h3("Generalized partial credit model"),
        p("In Generalized Partial Credit Model (GPCM; Muraki, 1992), probability of the successful transition
                                        from one category score to the next category score is modelled by 2PL IRT model. The response category
                                        probabilities are then ratios between category-related functions (cumulative sums of exponentials)
                                        and their sum."),
        p("Two simpler models can be derived from GPCM by restricting some parameters: Partial Credit Model
                                        (PCM; Masters, 1982) uses 1PL IRT model to describe this probability, thus parameters \\(\\alpha = 1\\).
                                        Even more restricted version, the Rating Scale Model (RSM; Andrich, 1978) assumes exactly the same
                                        K response categories for each item and threshold parameters which can be split into a response-threshold
                                        parameter \\(\\lambda_t\\) and an item-specific location parameter \\(\\delta_i\\). These models are
                                        also sometimes called adjacent logit models, as they set linear form to adjacent logits."),

        HTML("<div class='pb' style='page-break-after:always'></div>"),
        h4("Parameters"),
        p("Select number of responses and their threshold parameters \\(\\delta\\)  and common
                                        discrimination parameter \\(\\alpha\\). With \\(\\alpha = 1\\) you get PCM. Numerator of \\(\\pi_0 = P(Y = 0)\\) is
                                        set to 1 and \\(\\pi_0\\) is displayed with black color."),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          numericInput(
            inputId = "irt_training_gpcm_numresp",
            label = "Highest score",
            value = 4,
            min = 2,
            max = 6
          )
        ),
        br(),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          sliderInput(
            inputId = "irt_training_gpcm_a",
            label = "\\(\\alpha\\) - discrimination",
            value = 1,
            min = 0,
            max = 4,
            step = 0.1
          )
        ),
        br(),
        uiOutput("irt_training_gpcm_sliders"),
        br(),
        h4("Equations"),
        ("$$\\pi_k =\\mathrm{P}\\left(Y = k \\vert \\theta\\right) = \\frac{\\exp\\sum_{t = 0}^k \\alpha(\\theta - \\delta_t)}{\\sum_{r = 0}^K\\exp\\sum_{t = 0}^r \\alpha(\\theta - \\delta_t)} $$"),
        ("$$\\mathrm{E}\\left(Y \\vert \\theta\\right) = \\sum_{k = 0}^K k\\pi_k$$"),
        h4("Plots"),
        splitLayout(
          cellWidths = c("50%", "50%"),
          plotlyOutput("irt_training_gpcm_plot"),
          plotlyOutput("irt_training_gpcm_plot_expected")
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("DB_irt_training_gpcm_plot", label = "Download figure"),
          downloadButton("DB_irt_training_gpcm_plot_expected", label = "Download figure")
        ),
        br(),
        #------------------------------------------------------------------------------------#
        # ***** Exercise ####
        #------------------------------------------------------------------------------------#
        h4("Exercise"),
        p("Consider  item  following  generalized  partial  credit  model  rated  \\(0-1-2\\),
									  with  discrimination \\(a =  1\\)  andthreshold parameters \\(d_{1} = \u2212 1\\) and \\(d_{2} = 1\\)."),
        tags$ul(
          tags$li(
            "For what ability levels do the category probability curves cross?",
            splitLayout(
              cellWidths = c("90%", "5%", "5%"),
              checkboxGroupInput(
                inputId = "irt_training_gpcm_1",
                label = NULL,
                choices = c(
                  "\\(\\theta = -4\\)" = 1,
                  "\\(\\theta = -3\\)" = 2,
                  "\\(\\theta = -2\\)" = 3,
                  "\\(\\theta = -1\\)" = 4,
                  "\\(\\theta = 0\\)" = 5,
                  "\\(\\theta = 1\\)" = 6,
                  "\\(\\theta = 2\\)" = 7,
                  "\\(\\theta = 3\\)" = 8,
                  "\\(\\theta = 4\\)" = 9
                ),
                inline = TRUE
              ),
              uiOutput("irt_training_gpcm_1_answer")
            )
          ),
          tags$li(
            "What is the expected item score for these ability levels?",
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_2_1",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_2_1_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 0\\)"),
              numericInput(
                inputId = "irt_training_gpcm_2_2",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_2_2_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_2_3",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_2_3_answer")
            ), ""
          ),
          tags$li(
            "Change discrimination to \\(a = 2\\).  Do the category probability curves cross for the same ability levels?",
            splitLayout(
              cellWidths = c("21%", "4%", "75%"),
              radioButtons(
                inputId = "irt_training_gpcm_3",
                label = NULL,
                choices = c("Yes", "No"),
                inline = TRUE
              ),
              uiOutput("irt_training_gpcm_3_answer")
            ), ""
          ),
          tags$li(
            "How did the expected item score change for these ability levels? ",
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = -1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_4_1",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_4_1_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 0\\)"),
              numericInput(
                inputId = "irt_training_gpcm_4_2",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_4_2_answer")
            ),
            splitLayout(
              cellWidths = c("7%", "7%", "4%", "82%"),
              strong("\\(\\theta = 1.5\\)"),
              numericInput(
                inputId = "irt_training_gpcm_4_3",
                label = NULL,
                value = 0
              ),
              htmlOutput("irt_training_gpcm_4_3_answer")
            ),
            div(
              style = "display: inline-block; float: right; width: 200px;",
              htmlOutput("irt_training_gpcm_answer"),
              actionButton(
                inputId = "irt_training_gpcm_1_submit",
                label = "Submit answers", width = "80%"
              )
            ), ""
          )
        ),
        h4("Selected R code"),
        div(code(HTML("library(ggplot2)&nbsp;<br>library(data.table)&nbsp;<br><br>#&nbsp;setting&nbsp;parameters&nbsp;<br>a&nbsp;<-&nbsp;1&nbsp;<br>d&nbsp;<-&nbsp;c(-1.5,&nbsp;-1,&nbsp;-0.5,&nbsp;0)&nbsp;<br>theta&nbsp;<-&nbsp;seq(-4,&nbsp;4,&nbsp;0.01)&nbsp;<br><br>#&nbsp;calculating&nbsp;category&nbsp;probabilities&nbsp;<br>ccgpcm&nbsp;<-&nbsp;function(theta,&nbsp;a,&nbsp;d){&nbsp;a*(theta&nbsp;-&nbsp;d)&nbsp;}&nbsp;<br>df&nbsp;<-&nbsp;sapply(1:length(d),&nbsp;function(i)&nbsp;ccgpcm(theta,&nbsp;a,&nbsp;d[i]))&nbsp;<br>pk&nbsp;<-&nbsp;sapply(1:ncol(df),&nbsp;function(k)&nbsp;apply(as.data.frame(df[,&nbsp;1:k]),&nbsp;1,&nbsp;sum))&nbsp;<br>pk&nbsp;<-&nbsp;cbind(0,&nbsp;pk)&nbsp;<br>pk&nbsp;<-&nbsp;exp(pk)&nbsp;<br>denom&nbsp;<-&nbsp;apply(pk,&nbsp;1,&nbsp;sum)&nbsp;<br>df&nbsp;<-&nbsp;&nbsp;apply(pk,&nbsp;2,&nbsp;function(x)&nbsp;x/denom)<br>df1&nbsp;<-&nbsp;melt(data.frame(df,&nbsp;theta),&nbsp;id.vars&nbsp;=&nbsp;\"theta\")&nbsp;<br><br>#&nbsp;plotting&nbsp;category&nbsp;probabilities&nbsp;<br>ggplot(data&nbsp;=&nbsp;df1,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;value,&nbsp;col&nbsp;=&nbsp;variable))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Category&nbsp;probability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Category&nbsp;probabilities\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;scale_color_manual(\"\",&nbsp;values&nbsp;=&nbsp;c(\"black\",&nbsp;\"red\",&nbsp;\"yellow\",&nbsp;\"green\",&nbsp;\"blue\"),&nbsp;labels&nbsp;=&nbsp;paste0(\"P(Y&nbsp;=&nbsp;\",&nbsp;0:4,&nbsp;\")\"))<br><br>#&nbsp;calculating&nbsp;expected&nbsp;item&nbsp;score<br>df2&nbsp;<-&nbsp;data.frame(exp&nbsp;=&nbsp;as.matrix(df)&nbsp;%*%&nbsp;0:4,&nbsp;theta)<br>#&nbsp;plotting&nbsp;expected&nbsp;item&nbsp;score&nbsp;<br>ggplot(data&nbsp;=&nbsp;df2,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;exp))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Expected&nbsp;item&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Expected&nbsp;item&nbsp;score\")"))),
        br(),
        br()
      ),
      # *** Nominal response model ####
      tabPanel("Nominal response model",
        value = "polytom_nrm",
        h3("Nominal response model"),
        p("In Nominal Response Model (NRM; Bock, 1972), probability of selecting given category over baseline
                                        category is modelled by 2PL IRT model. This model is also sometimes called baseline-category logit
                                        model, as it sets linear form to log of odds of selecting given category to selecting a baseline category.
                                        Baseline can be chosen arbitrary, although usually the correct answer or the first answer is chosen.
                                        NRM model is generalization of GPCM model by setting item-specific and category-specific intercept and
                                        slope parameters."),

        HTML("<div class='pb' style='page-break-after:always'></div>"),
        h4("Parameters"),
        p("Select number of distractors and their threshold parameters  \\(\\delta\\) and discrimination parameters \\(\\alpha\\).
                                        Parameters of \\(\\pi_0 = P(Y = 0)\\) are set to zeros and \\(\\pi_0\\) is displayed with black color."),
        div(
          style = "display: inline-block; vertical-align: middle; width: 18%;",
          numericInput(
            inputId = "irt_training_nrm_numresp",
            label = "Number of distractors",
            value = 4,
            min = 2,
            max = 6
          )
        ),
        br(),
        uiOutput("irt_training_nrm_sliders"),
        br(),
        h4("Equations"),
        ("$$\\pi_k =\\mathrm{P}\\left(Y = k \\vert \\theta\\right) = \\frac{\\exp(\\alpha_k\\theta + \\delta_k)}{\\sum_{r = 0}^K\\exp(\\alpha_r\\theta + \\delta_r)} $$"),
        h4("Plots"),
        plotlyOutput("irt_training_nrm_plot"),
        downloadButton("DB_irt_training_nrm_plot", label = "Download figure"),
        h4("Selected R code"),
        div(code(HTML("library(ggplot2)&nbsp;<br>library(data.table)&nbsp;<br><br>#&nbsp;setting&nbsp;parameters&nbsp;<br>a&nbsp;<-&nbsp;c(2.5,&nbsp;2,&nbsp;1,&nbsp;1.5)&nbsp;<br>d&nbsp;<-&nbsp;c(-1.5,&nbsp;-1,&nbsp;-0.5,&nbsp;0)&nbsp;<br>theta&nbsp;<-&nbsp;seq(-4,&nbsp;4,&nbsp;0.01)&nbsp;<br><br>#&nbsp;calculating&nbsp;category&nbsp;probabilities&nbsp;<br>ccnrm&nbsp;<-&nbsp;function(theta,&nbsp;a,&nbsp;d){&nbsp;exp(d&nbsp;+&nbsp;a*theta)&nbsp;}&nbsp;<br>df&nbsp;<-&nbsp;sapply(1:length(d),&nbsp;function(i)&nbsp;ccnrm(theta,&nbsp;a[i],&nbsp;d[i]))&nbsp;<br>df&nbsp;<-&nbsp;data.frame(1,&nbsp;df)&nbsp;<br>denom&nbsp;<-&nbsp;apply(df,&nbsp;1,&nbsp;sum)&nbsp;<br>df&nbsp;<-&nbsp;apply(df,&nbsp;2,&nbsp;function(x)&nbsp;x/denom)&nbsp;<br>df1&nbsp;<-&nbsp;melt(data.frame(df,&nbsp;theta),&nbsp;id.vars&nbsp;=&nbsp;\"theta\")&nbsp;<br><br>#&nbsp;plotting&nbsp;category&nbsp;probabilities&nbsp;<br>ggplot(data&nbsp;=&nbsp;df1,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;value,&nbsp;col&nbsp;=&nbsp;variable))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Category&nbsp;probability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;1)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Category&nbsp;probabilities\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;scale_color_manual(\"\",&nbsp;values&nbsp;=&nbsp;c(\"black\",&nbsp;\"red\",&nbsp;\"yellow\",&nbsp;\"green\",&nbsp;\"blue\"),&nbsp;labels&nbsp;=&nbsp;paste0(\"P(Y&nbsp;=&nbsp;\",&nbsp;0:4,&nbsp;\")\"))<br><br>#&nbsp;calculating&nbsp;expected&nbsp;item&nbsp;score<br>df2&nbsp;<-&nbsp;data.frame(exp&nbsp;=&nbsp;as.matrix(df)&nbsp;%*%&nbsp;0:4,&nbsp;theta)<br><br>#&nbsp;plotting&nbsp;expected&nbsp;item&nbsp;score<br>ggplot(data&nbsp;=&nbsp;df2,&nbsp;aes(x&nbsp;=&nbsp;theta,&nbsp;y&nbsp;=&nbsp;exp))&nbsp;+&nbsp;<br>&nbsp;&nbsp;geom_line()&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlab(\"Ability\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylab(\"Expected&nbsp;item&nbsp;score\")&nbsp;+&nbsp;<br>&nbsp;&nbsp;xlim(-4,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;ylim(0,&nbsp;4)&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme_bw()&nbsp;+&nbsp;<br>&nbsp;&nbsp;theme(text&nbsp;=&nbsp;element_text(size&nbsp;=&nbsp;14),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.major&nbsp;=&nbsp;element_blank(),&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;panel.grid.minor&nbsp;=&nbsp;element_blank())&nbsp;+&nbsp;<br>&nbsp;&nbsp;ggtitle(\"Expected&nbsp;item&nbsp;score\")"))),
        br(),
        br()
      )
    )
  )
)
