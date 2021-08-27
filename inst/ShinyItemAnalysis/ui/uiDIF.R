source("ui/uiDIF/logistic.R", local = TRUE)
source("ui/uiDIF/cumulative.R", local = TRUE)
source("ui/uiDIF/adjacent.R", local = TRUE)
source("ui/uiDIF/multinomial.R", local = TRUE)
source("ui/uiDIF/uiTDIF.R")

uiDIF <-
  navbarMenu(
    "DIF/Fairness",
    # DESCRIPTION ####
    "Description",
    # * ABOUT DIF and DDF ####
    tabPanel(
      "About DIF and DDF",
      h3("Differential Item/Distractor Functioning"),
      p("Differential item functioning (DIF) occurs when respondents from different
                        social groups (such as those defined by gender or ethnicity) with the same underlying
                        ability have a different probability of answering the item correctly or
                        endorsing the item. If some item functions differently for two groups,
                        it is potentially unfair and should be checked for wording.
                        In general, two types of DIF can be distinguished: The ", strong("uniform"), "DIF
                        describes a situation when the item advantages one of the groups at all levels of
                        the latent ability (left figure). In such a case, the item has different difficulty
                        (location parameters) for two given groups, while the item discrimination is the same.
                        Contrary, the ", strong("non-uniform"), "DIF (right figure) means that the item
                        advantages one of the groups at lower ability levels, and the other group at higher
                        ability levels. In this case, the item has different discrimination (slope) parameters
                        and possibly also different difficulty parameters for the two given groups."),
      br(),
      img(
        src = "fig_DIF_uniform.svg",
        style = "float: left; width: 32%; margin-right: 2%; margin-left: 16%; margin-bottom: 0.5em;"
      ),
      img(
        src = "fig_DIF_nonuniform.svg",
        style = "float: left; width: 32%; margin-right: 16%; margin-left: 2%; margin-bottom: 0.5em;"
      ),
      br(),
      p(
        "Differential distractor functioning (DDF) occurs when respondents from different
                        groups but with the same latent ability have a different probability of selecting
                        at least one distractor choice. Again, two types of DDF can be distinguished - ",
        strong("uniform"), " (left figure below) and ", strong("non-uniform"), " DDF (right figure below)."
      ),
      img(
        src = "fig_DDF_uniform.svg",
        style = "float: left; width: 32%; margin-right: 2%; margin-left: 16%; margin-bottom: 0.5em;"
      ),
      img(
        src = "fig_DDF_nonuniform.svg",
        style = "float: left; width: 32%; margin-right: 16%; margin-left: 2%; margin-bottom: 0.5em;"
      )
    ),
    # * TOTAL SCORES ####
    tabPanel(
      "Observed scores",
      h3("Observed scores"),
      p(
        "DIF analysis may come to a different conclusion than a test of group differences in total scores.
                        Two groups may have the same distribution of total scores, yet, some items may function differently
                        for the two groups. Also, one of the groups may have a significantly lower total score, yet, it may
                        happen that there is no DIF item ",
        a("(Martinkova et al., 2017). ",
          href = "https://doi.org/10.1187/cbe.16-10-0307",
          target = "_blank"
        ),
        "This section examines the differences in observed scores only. Explore further DIF sections to analyze
                        differential item functioning."
      ),
      p(
        "In DIF analysis, the groups are compared in functioning of items with respect to respondent ability.
                        In many methods, observed ability such as the standardized total score is used as the matching criterion.
                        DIF can also be explored with respect to other observed score or criterion.
                        For example, to analyze instructional sensitivity, ",
        a("Martinkova et al. (2020)",
          href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
          target = "_blank"
        ),
        " analyzed differential item functioning in change (DIF-C) by analyzing DIF on Grade 9 item answers
                          while matching on Grade 6 total scores of the same respondents in a longitudinal setting
                          (see toy data ", code("Learning to Learn 9"), " in the Data section)."
      ),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "DIF_total_matching",
            label = "Observed score",
            choices = c(
              "Total score" = "score",
              "Uploaded" = "uploaded"
            ),
            selected = "score"
          )
        )
      ),
      h4("Summary of ", textOutput("DIF_total_matching_title1", inline = T), " for groups"),
      tableOutput("DIF_total_table"),
      h4("Histograms of ", textOutput("DIF_total_matching_title2", inline = T), " for groups"),
      plotlyOutput("DIF_total_hist"),
      downloadButton("DB_DIF_total_hist", label = "Download figure"),
      br(),
      h4("Comparison of ", textOutput("DIF_total_matching_title3", inline = T)),
      tableOutput("DIF_total_ttest"),
      HTML(paste0(
        "Notes: A test for the difference in ", textOutput("DIF_total_matching_title4", inline = T), " between
                      the reference and the focal group is based on the Welch two sample t-test. ", br(),
        strong("Diff. (CI)"), " - difference in the means of ",
        textOutput("DIF_total_matching_title5", inline = T), " with a 95% confidence interval, ",
        strong("\\(t\\)-value"), " - test statistic, ", strong("df"), " - degrees of freedom, ",
        strong("\\(p\\)-value"), " - value lower than 0.05 means a significant difference in the ",
        textOutput("DIF_total_matching_title6", inline = T), " between the reference and the focal group."
      )),
      br(),
      h4("Selected R code"),
      code(includeText("sc/dif/tot_scores.R"))
    ),
    # DICHOTOMOUS METHODS ####
    "----",
    "Dichotomous methods",
    # * DELTA PLOTS ####
    tabPanel(
      "Delta plot",
      h3("Delta plot"),
      p("A delta plot (Angoff & Ford, 1973) compares the proportions of correct answers per
                        item in the two groups. It displays non-linear transformation of these proportions using
                        quantiles of standard normal distributions (so-called delta scores) for each item for the two
                        groups in a scatterplot called diagonal plot or delta plot (see Figure below). An item is under
                        suspicion of DIF if the delta point departs considerably from the main axis of the ellipsoid formed
                        by the delta scores. "),
      h4("Method specification"),
      p("The ", strong("detection threshold"), " is either fixed to the value of 1.5 or it is based on bivariate
                        normal approximation (Magis & Facon, 2012). The", strong("item purification"), "algorithms offered when
                        using the threshold based on normal approximation are as follows: IPP1 uses the threshold obtained
                        after the first run in all following runs, IPP2 updates only the slope parameter of the threshold formula
                        and thus lessens the impact of DIF items, IPP3 adjusts every single parameter and completely discards the
                        effect of items flagged as DIF from the computation of the threshold (for further details see Magis & Facon,
                        2013). When using the fixed threshold and item purification, this threshold (1.5) stays the same henceforward
                        during the purification algorithm."),
      fluidRow(
        column(
          1,
          radioButtons(
            inputId = "type_threshold",
            label = "Threshold",
            choices = list("Fixed", "Normal")
          )
        ),
        column(
          2, br(),
          checkboxInput(
            inputId = "puri_DP",
            label = "Item purification",
            value = FALSE
          )
        ),
        column(
          2,
          conditionalPanel(
            condition = "input.puri_DP && input.type_threshold == 'Normal'",
            selectInput(
              inputId = "puri_DP_type",
              label = "Purification method",
              choices = c(
                "IPP1" = "IPP1",
                "IPP2" = "IPP2",
                "IPP3" = "IPP3"
              ),
              selected = "IPP1"
            )
          )
        )
      ),
      h4("Delta plot"),
      plotlyOutput("deltaplot"),
      downloadButton("DP_deltaplot", label = "Download figure"),
      br(), br(),
      h4("Summary table"),
      p("A summary table contains information about the proportions of correct answers in the reference and the focal group together
                        with their transformations into delta scores. It also includes the distances of delta scores from the main axis of the
                        ellipsoid formed by delta scores. "),
      strong(textOutput("dp_dif_items")),
      br(),
      fluidRow(column(12, align = "left", tableOutput("coef_dp_table"))),
      fluidRow(column(12, align = "left", uiOutput("note_dp"))),
      br(),
      fluidRow(column(2, downloadButton(
        outputId = "download_dp_table",
        label = "Download table"
      ))),
      br(),
      h4("Purification process"),
      textOutput("dp_puri_info"),
      br(),
      tags$head(tags$style("#dp_puri_table  {white-space: nowrap;  }")),
      fluidRow(column(12, align = "center", tableOutput("dp_puri_table"))),
      conditionalPanel("input.puri_DP == 1", downloadButton(outputId = "download_dp_puri", label = "Download table"), br(), br()),
      h4("Selected R code"),
      code(includeText("sc/dif/delta_plt.R"))
    ),
    # * MANTEL-HAENSZEL ####
    tabPanel(
      "Mantel-Haenszel",
      tabsetPanel(
        # Summary
        tabPanel(
          "Summary",
          h3("Mantel-Haenszel test"),
          p("The Mantel-Haenszel test is a DIF detection method based on contingency tables which are calculated for each
                                    level of the total score (Mantel & Haenszel, 1959)."),
          h4("Method specification"),
          p(
            "Here you can select a ", strong("correction method"), " for multiple comparison, and/or ",
            strong("item purification.")
          ),
          fluidRow(
            column(
              2,
              selectInput(
                inputId = "DIF_MH_summary_correction",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              )
            ),
            column(
              2, br(),
              checkboxInput(
                inputId = "DIF_MH_summary_purification",
                label = "Item purification",
                value = FALSE
              )
            )
          ),
          h4("Summary table"),
          p("The summary table contains information about Mantel-Haenszel \\(\\chi^2\\) statistics, corresponding
                                   \\(p\\)-values considering selected adjustement, and significance codes. Moreover, this table offers values
                                   of Mantel-Haenszel estimates of the odds ratio \\(\\alpha_{\\mathrm{MH}}\\), which incorporate all levels of
                                   the total score, and their transformations into D-DIF indices \\(\\Delta_{\\mathrm{MH}} =
                                   -2.35 \\log(\\alpha_{\\mathrm{MH}})\\) to evaluate DIF effect size. "),
          strong(textOutput("mh_dif_items")),
          br(),
          fluidRow(column(12, align = "left", tableOutput("coef_mh_table"))),
          fluidRow(column(12, align = "left", uiOutput("note_mh"))),
          br(),
          fluidRow(column(2, downloadButton(outputId = "download_mh_table", label = "Download table"))),
          br(),
          h4("Purification process"),
          textOutput("mh_puri_info"),
          br(),
          tags$head(tags$style("#mh_puri_table  {white-space: nowrap;  }")),
          fluidRow(column(12, align = "center", tableOutput("mh_puri_table"))),
          conditionalPanel("input.DIF_MH_summary_purification == 1", downloadButton(outputId = "download_mh_puri", label = "Download table"), br(), br()),
          br(),
          h4("Selected R code"),
          code(includeText("sc/dif/mh.R"))
        ),
        tabPanel("Items",
          value = "mh_it",
          h3("Mantel-Haenszel test"),
          p("The Mantel-Haenszel test is a DIF detection method based on contingency tables which are calculated for each
                                    level of the total score (Mantel & Haenszel, 1959)."),
          h4("Contingency tables and odds ratio calculation"),
          p("For the selected", strong("item"), "and for the selected", strong("level of the total score"), "you can
                                   display a contingency table and calculate the odds ratio of answering an item correctly. This can be compared
                                   to the Mantel-Haenszel estimate of odds ratio \\(\\alpha_{\\mathrm{MH}}\\), which incorporates all levels of the total score.
                                   Further, \\(\\alpha_{\\mathrm{MH}}\\) can be transformed into the Mantel-Haenszel D-DIF index
                                   \\(\\Delta_{\\mathrm{MH}}\\) to evaluate the DIF effect size. "),
          fluidRow(
            column(
              2,
              sliderInput(
                inputId = "DIF_MH_items_item",
                label = "Item",
                animate = animationOptions(interval = 1600),
                min = 1,
                max = 10,
                value = 1,
                step = 1
              )
            ),
            column(
              2,
              sliderInput(
                inputId = "DIF_MH_items_score",
                label = "Score level",
                min = 0,
                max = 10,
                value = 1,
                step = 1
              )
            )
          ),
          fluidRow(column(12, align = "center", tableOutput("DIF_MH_items_table"))),
          uiOutput("DIF_MH_items_interpretation"),
          br(),
          h4("Selected R code"),
          code(includeText("sc/dif/mh_it.R"))
        )
      )
    ),
    # * SIBTEST ####
    tabPanel(
      "SIBTEST",
      h3("SIBTEST"),
      p("The SIBTEST method (Shealy & Stout, 1993) allows for detection of uniform DIF without requiring
                        an item response model. Its modified version, the Crossing-SIBTEST (Chalmers, 2018; Li & Stout, 1996),
                        focuses on detection of non-uniform DIF."),
      h4("Method specification"),
      p("Here you can choose the ", strong("type"), " of DIF to test. With uniform DIF, SIBTEST is applied, while with non-uniform DIF,
                        the Crossing-SIBTEST method is used instead. You can also select the ", strong("correction method"), " for multiple comparisons
                        or", strong("item purification.")),
      fluidRow(
        column(
          2,
          radioButtons(
            inputId = "DIF_SIBTEST_type",
            label = "Type",
            choices = c(
              "Uniform" = "udif",
              "Non-uniform" = "nudif"
            ),
            selected = "udif"
          )
        ),
        column(
          2,
          selectInput(
            inputId = "DIF_SIBTEST_correction",
            label = "Correction method",
            choices = c(
              "Benjamini-Hochberg" = "BH",
              "Benjamini-Yekutieli" = "BY",
              "Bonferroni" = "bonferroni",
              "Holm" = "holm",
              "Hochberg" = "hochberg",
              "Hommel" = "hommel",
              "None" = "none"
            ),
            selected = "none"
          )
        ),
        column(
          2, br(),
          checkboxInput(
            inputId = "DIF_SIBTEST_purification",
            label = "Item purification",
            value = FALSE
          )
        )
      ),
      h4("Summary table"),
      p("This summary table contains estimates of \\(\\beta\\) together with standard errors
                      (only available when testing uniform DIF), corresponding \\(\\chi^2\\)-statistics
                        with \\(p\\)-values considering selected adjustement, and significance codes. "),
      uiOutput("DIF_SIBTEST_NA_alert"),
      # verbatimTextOutput("DIF_SIBTEST_print"),
      strong(textOutput("sibtest_dif_items")),
      br(),
      tags$head(tags$style("#coef_sibtest_dif  {white-space: nowrap;}")),
      fluidRow(column(12, align = "left", tableOutput("coef_sibtest_dif"))),
      fluidRow(column(12, align = "left", uiOutput("note_sibtest"))),
      br(),
      fluidRow(column(2, downloadButton(outputId = "download_sibtest_dif", label = "Download table"))),
      br(),
      h4("Purification process"),
      textOutput("dif_sibtest_puri_info"),
      br(),
      tags$head(tags$style("#dif_sibtest_puri_table  {white-space: nowrap;}")),
      fluidRow(column(12, align = "center", tableOutput("dif_sibtest_puri_table"))),
      conditionalPanel(
        "input.DIF_SIBTEST_purification == 1",
        downloadButton(outputId = "download_sibtest_dif_puri", label = "Download table"), br(), br()
      ),
      h4("Selected code"),
      code(includeText("sc/dif/sibtest.R"))
    ),
    # * LOGISTIC ####
    ui_DIF_logistic,
    # * GENERALIZED LOGISTIC ####
    tabPanel(
      "Generalized logistic",
      tabsetPanel(
        # ** Summary ####
        tabPanel(
          "Summary",
          h3("Generalized logistic regression"),
          p(
            "Generalized logistic regression models are extensions of a logistic regression method which
                                   account for the possibility of guessing by allowing for nonzero lower asymptote - pseudo-guessing \\(c_i\\)",
            a("(Drabinova & Martinkova, 2017) ",
              href = "https://doi.org/10.1111/jedm.12158",
              target = "_blank"
            ),
            "or an upper asymptote lower than one - inattention \\(d_i\\). Similarly to logistic
                                   regression, its extensions also provide detection of uniform and non-uniform DIF by
                                   letting the difficulty parameter \\(b_i\\) (uniform) and the discrimination parameter \\(a_i\\)
                                   (non-uniform) differ for groups and by testing for the difference in their
                                   values. Moreover, these extensions allow for testing differences in pseudo-guessing and
                                   inattention parameters and they can be seen as proxies of 3PL and 4PL IRT models for
                                   DIF detection."
          ),
          h4("Method specification"),
          p(
            "Here you can specify the assumed ", strong("model."), "In 3PL and 4PL models, the abbreviations \\(c_{g}\\) or \\(d_{g}\\)
                                    mean that parameters \\(c_i\\) or \\(d_i\\) are assumed to be the same for both groups, otherwise they are allowed to differ.
                                    With ", strong("type"), "you can specify the type of DIF to be tested by choosing the parameters in which a difference between
                                    groups should be tested. You can also select", strong("correction method"), " for multiple comparison or",
            strong("item purification. ")
          ),
          p(
            "Finally, you may change the ", strong("Observed score."), " While matching on the standardized total score is typical, the upload
                                   of other Observed scores is possible in the ", strong("Data "), "section. Using a pre-test (standardized) total score allows
                                   for testing differential item functioning in change (DIF-C) to provide proofs of instructional sensitivity ",
            a("(Martinkova et al., 2020),",
              href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank"
            ), "also see", code("Learning To Learn 9"), " toy dataset."
          ),
          fluidRow(
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_model_print",
                label = "Model",
                choices = c(
                  "Rasch" = "Rasch",
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PLcg" = "3PLcg",
                  "3PLdg" = "3PLdg",
                  "3PLc" = "3PLc",
                  "3PLd" = "3PLd",
                  "4PLcgdg" = "4PLcgdg",
                  "4PLcgd" = "4PLcgd",
                  "4PLcdg" = "4PLcdg",
                  "4PL" = "4PL"
                ),
                selected = "3PLcg"
              )
            ),
            column(
              1,
              checkboxGroupInput(
                inputId = "DIF_NLR_type_print",
                label = "Type",
                choices = c(
                  "\\(a\\)" = "a",
                  "\\(b\\)" = "b",
                  "\\(c\\)" = "c",
                  "\\(d\\)" = "d"
                ),
                selected = c("\\(a\\)", "b")
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_correction_method_print",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "DIF_NLR_purification_print",
                label = "Item purification",
                value = FALSE
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_summary_matching",
                label = "Observed score",
                choices = c("Standardized total score" = "zscore"),
                selected = "zscore"
              )
            )
          ),
          h4("Equation"),
          p("The displayed equation is based on the model selected below"),
          fluidRow(column(12, align = "center", uiOutput("DIF_NLR_equation_print"))),
          h4("Summary table"),
          p("This summary table contains information about DIF test statistic \\(LR(\\chi^2)\\), corresponding \\(p\\)-values
                                   considering selected adjustement, and significance codes. This table also provides estimated parameters for
                                   the best fitted model for each item. Note that \\(a_{iG_p}\\) (and also other parameters) from the equation
                                   above consists of a parameter for the reference group and a parameter for the difference between focal and reference
                                   groups, i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\), where \\(G_{p} = 0\\) for the reference group and \\(G_{p} = 1\\)
                                   for the focal group, as stated in the table below. "),
          uiOutput("DIF_NLR_na_alert"),
          strong(textOutput("nlr_dif_items")),
          br(),
          tags$head(tags$style("#coef_nlr_dif  {white-space: nowrap;}")),
          fluidRow(column(12, align = "left", tableOutput("coef_nlr_dif"))),
          fluidRow(column(12, align = "left", uiOutput("note_nlr"))),
          br(),
          fluidRow(column(2, downloadButton(outputId = "download_nlr_dif", label = "Download table"))),
          br(),
          h4("Purification process"),
          textOutput("dif_nlr_puri_info"),
          br(),
          tags$head(tags$style("#dif_nlr_puri_table  {white-space: nowrap;}")),
          fluidRow(column(12, align = "center", tableOutput("dif_nlr_puri_table"))),
          conditionalPanel(
            "input.DIF_NLR_purification_print == 1",
            downloadButton(outputId = "download_nlr_dif_puri", label = "Download table"), br(), br()
          ),
          br(),
          h4("Selected R code"),
          code(includeText("sc/dif/nlr.R"))
        ),
        # ** Items ####
        tabPanel("Items",
          value = "glr_it",
          h3("Generalized logistic regression"),
          p(
            "Generalized logistic regression models are extensions of a logistic regression method which
                                   account for the possibility of guessing by allowing for nonzero lower asymptote - pseudo-guessing \\(c_i\\)",
            a("(Drabinova & Martinkova, 2017) ",
              href = "https://doi.org/10.1111/jedm.12158",
              target = "_blank"
            ),
            "or an upper asymptote lower than one - inattention \\(d_i\\). Similarly to logistic
                                   regression, its extensions also provide detection of uniform and non-uniform DIF by
                                   letting the difficulty parameter \\(b_i\\) (uniform) and the discrimination parameter \\(a_i\\)
                                   (non-uniform) differ for groups and by testing for the difference in their
                                   values. Moreover, these extensions allow for testing differences in pseudo-guessing and
                                   inattention parameters and they can be seen as proxies of 3PL and 4PL IRT models for
                                   DIF detection."
          ),
          h4("Method specification"),
          p(
            "Here you can specify the assumed ", strong("model."), "In 3PL and 4PL models, the abbreviations \\(c_{g}\\) or \\(d_{g}\\)
                                    mean that parameters \\(c\\) or \\(d\\) are assumed to be the same for both groups, otherwise they are allowed to differ.
                                    With ", strong("type"), "you can specify the type of DIF to be tested by choosing the parameters in which a difference between
                                    groups should be tested. You can also select", strong("correction method"), " for multiple comparison or",
            strong("item purification. ")
          ),
          p(
            "Finally, you may change the ", strong("Observed score."), " While matching on the standardized total score is typical, the upload
                                   of other observed scores is possible in the ", strong("Data "), " section. Using a pre-test (standardized) total score allows
                                   for testing differential item functioning in change (DIF-C) to provide proofs of instructional sensitivity ",
            a("(Martinkova et al., 2020),",
              href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank"
            ), "also see", code("Learning To Learn 9"), " toy dataset. For selected", strong("item"),
            "you can display plot of its characteristic curves and table of its estimated parameters with standard errors. "
          ),
          fluidRow(
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_model_plot",
                label = "Model",
                choices = c(
                  "Rasch" = "Rasch",
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PLcg" = "3PLcg",
                  "3PLdg" = "3PLdg",
                  "3PLc" = "3PLc",
                  "3PLd" = "3PLd",
                  "4PLcgdg" = "4PLcgdg",
                  "4PLcgd" = "4PLcgd",
                  "4PLcdg" = "4PLcdg",
                  "4PL" = "4PL"
                ),
                selected = "3PLcg"
              )
            ),
            column(
              1,
              withMathJax(),
              checkboxGroupInput(
                inputId = "DIF_NLR_type_plot",
                label = "Type",
                choices = c(
                  "\\(a\\)" = "a",
                  "\\(b\\)" = "b",
                  "\\(c\\)" = "c",
                  "\\(d\\)" = "d"
                ),
                selected = c("a", "b")
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_correction_method_plot",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "DIF_NLR_purification_plot",
                label = "Item purification",
                value = FALSE
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_items_matching",
                label = "Observed score",
                choices = c("Standardized total score" = "zscore"),
                selected = "zscore"
              )
            ),
            column(
              2,
              sliderInput(
                inputId = "DIF_NLR_item_plot",
                label = "Item",
                min = 1,
                value = 1,
                max = 10,
                step = 1,
                animate = animationOptions(interval = 1600)
              )
            )
          ),
          h4("Plot with estimated DIF generalized logistic curve"),
          p("Points represent a proportion of the correct answer (empirical probabilities) with respect to the observed score.
                                   Their size is determined by the count of respondents who achieved a given level of observed score with respect
                                   to the group membership."),
          plotlyOutput("plot_DIF_NLR"),
          downloadButton("DP_plot_DIF_NLR", label = "Download figure"),
          h4("Equation"),
          fluidRow(column(12, align = "center", uiOutput("DIF_NLR_equation_plot"))),
          h4("Table of parameters"),
          p("This table summarizes estimated item parameters together with their standard errors. Note that \\(a_{iG_p}\\) (and also other
                                   parameters) from the equation above consists of a parameter for the reference group and a parameter for the difference between
                                   focal and reference groups, i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\), where \\(G_{p} = 0\\) for the reference group and
                                   \\(G_{p} = 1\\) for the focal group, as stated in the table below. "),
          fluidRow(column(12, align = "center", tableOutput("tab_coef_DIF_NLR"))),
          br(),
          h4("Selected R code"),
          code(includeText("sc/dif/nlr_it.R"))
        )
      )
    ),
    # * IRT LORD ####
    tabPanel(
      "IRT Lord",
      tabsetPanel(
        # ** Summary ####
        tabPanel(
          "Summary",
          h3("Lord test for IRT models"),
          p("To detect DIF, the Lord test (Lord, 1980) compares item parameters of a selected IRT model, fitted separately
                                    on data of the two groups. The model is either 1PL, 2PL, or 3PL with guessing, which is the same for the two
                                    groups. In the case of the 3PL model, the guessing parameter is estimated based on the whole dataset and is
                                    subsequently considered fixed. In statistical terms, the Lord statistic is equal to the Wald statistic."),
          h4("Method specification"),
          p("Here you can choose the underlying IRT ", strong("model"), " used to test DIF. You can also select the ", strong("correction method"), " for multiple comparisons,
                                   and/or", strong("item purification.")),
          fluidRow(
            column(
              1,
              radioButtons(
                inputId = "type_print_DIF_IRT_lord",
                label = "Model",
                choices = c(
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PL" = "3PL"
                ),
                selected = "2PL"
              )
            ),
            column(
              2,
              selectInput(
                inputId = "correction_method_DIF_IRT_lordSummary",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "puri_Lord",
                label = "Item purification",
                value = FALSE
              )
            )
          ),
          h4("Equation"),
          uiOutput("DIF_Lord_interpretation_summary"),
          fluidRow(column(12, align = "center", uiOutput("DIF_Lord_equation_summary"))),
          h4("Summary table"),
          p("This summary table contains information about Lord's \\(\\chi^2\\)-statistics, corresponding \\(p\\)-values
                                   considering selected adjustment, and significance codes. The table also provides estimated parameters for
                                   both groups. Note that item parameters might slightly differ even for non-DIF items as two seperate models are
                                   fitted, however this difference is non-significant. Also note that under the 3PL model, the guessing parameter
                                   \\(c\\) is estimated from the whole dataset, and is considered fixed in the final models, thus no standard error
                                   is displayed."),
          uiOutput("DIF_IRT_LORD_na_alert"),
          strong(textOutput("lord_dif_items")),
          br(),
          tags$head(tags$style("#coef_lord_dif  {white-space: nowrap;}")),
          fluidRow(column(12, align = "left", tableOutput("coef_lord_dif"))),
          fluidRow(column(12, align = "left", uiOutput("note_lord"))),
          br(),
          fluidRow(column(2, downloadButton(outputId = "download_lord_dif", label = "Download table"))),
          br(),
          h4("Purification process"),
          textOutput("dif_lord_puri_info"),
          br(),
          tags$head(tags$style("#dif_lord_puri_table  {white-space: nowrap;}")),
          fluidRow(column(12, align = "center", tableOutput("dif_lord_puri_table"))),
          conditionalPanel(
            "input.puri_Lord == 1",
            downloadButton(outputId = "download_lord_dif_puri", label = "Download table"), br(), br()
          ),
          h4("Selected R code"),
          code(includeText("sc/dif/lord.R"))
        ),
        # ** Items ####
        tabPanel("Items",
          value = "lord_it",
          h3("Lord test for IRT models"),
          p("To detect DIF, the Lord test (Lord, 1980) compares item parameters of a selected IRT model, fitted separately
                                    on data of the two groups. The model is either 1PL, 2PL, or 3PL with guessing which is the same for the two
                                    groups. In the case of the 3PL model, the guessing parameter is estimated based on the whole dataset and is
                                    subsequently considered fixed. In statistical terms, the Lord statistic is equal to the Wald statistic."),
          h4("Method specification"),
          p("Here you can choose an underlying IRT ", strong("model"), " used to test DIF. You can also select a ", strong("correction method"), " for multiple comparison,
                                   and/or", strong("item purification."), "For a selected", strong("item"), "you can display the plot of its characteristic curves and the table
                                   of its estimated parameters with standard errors. "),
          fluidPage(
            column(
              1,
              radioButtons(
                inputId = "type_plot_DIF_IRT_lord",
                label = "Model",
                choices = c(
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PL" = "3PL"
                ),
                selected = "2PL"
              )
            ),
            column(
              2,
              selectInput(
                inputId = "correction_method_DIF_IRT_lordItems",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput("puri_Lord_plot", "Item purification", FALSE)
            ),
            column(
              2,
              sliderInput(
                inputId = "difirt_lord_itemSlider",
                label = "Item",
                min = 1,
                value = 1,
                max = 10,
                step = 1,
                animate = animationOptions(interval = 1600)
              )
            )
          ),
          h4("Plot with estimated DIF characteristic curve"),
          p("Note that plots might differ slightly even for non-DIF items as two seperate models are fitted, however this difference
                                   is non-significant. "),
          plotlyOutput("plot_DIF_IRT_Lord"),
          downloadButton("DP_plot_DIF_IRT_Lord", label = "Download figure"),
          h4("Equation"),
          uiOutput("irtint_lord"),
          fluidRow(column(12, align = "center", uiOutput("irteq_lord"))),
          h4("Table of parameters"),
          p("The table summarizes estimated item parameters together with standard errors. Note that item parameters might differ slightly
                                 even for non-DIF items as two seperate models are fitted, however this difference is non-significant.
                                 Also note that under the 3PL model, the guessing parameter \\(c\\) is estimated from the whole dataset, and
                                 is considered fixed in the final models, thus no standard error is displayed."),
          fluidRow(column(12, align = "center", tableOutput("tab_coef_DIF_IRT_Lord"))),
          br(),
          h4("Selected R code"),
          code(includeText("sc/dif/lord_it.R"))
        )
      )
    ),
    # * IRT RAJU ####
    tabPanel(
      "IRT Raju",
      tabsetPanel(
        # ** Summary ####
        tabPanel(
          "Summary",
          h3("Raju test for IRT models"),
          p("To detect DIF, the Raju test (Raju, 1988, 1990) uses the area between the item charateristic curves of the selected
                                    IRT model, fitted separately with data of the two groups. The model is either 1PL, 2PL, or 3PL with guessing
                                    which is the same for the two groups. In the case of the 3PL model, the guessing parameter is estimated based on the whole dataset and is
                                    subsequently considered fixed."),
          h4("Method specification"),
          p("Here you can choose an underlying IRT ", strong("model"), " used to test DIF. You can also select the ", strong("correction method"), " for multiple comparison,
                                   and/or", strong("item purification.")),
          fluidPage(
            column(
              1,
              radioButtons(
                inputId = "type_print_DIF_IRT_raju",
                label = "Model",
                choices = c(
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PL" = "3PL"
                ),
                selected = "2PL"
              )
            ),
            column(
              2,
              selectInput(
                inputId = "correction_method_DIF_IRT_rajuSummary",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "puri_Raju",
                label = "Item purification",
                value = FALSE
              )
            )
          ),
          h4("Equation"),
          uiOutput("DIF_Raju_interpretation_summary"),
          fluidRow(column(12, align = "center", uiOutput("DIF_Raju_equation_summary"))),
          h4("Summary table"),
          p("This summary table contains information about Raju's \\(Z\\)-statistics, corresponding \\(p\\)-values
                                   considering selected adjustement, and significance codes. The table also provides estimated parameters for
                                   both groups. Note that item parameters might differ slightly even for non-DIF items as the two seperate models are
                                   fitted, however this difference is non-significant. Also note that under the 3PL model, the guessing parameter \\(c\\) is estimated from the whole dataset, and
                                   is considered fixed in the final models, thus no standard error is displayed."),
          # verbatimTextOutput('print_DIF_IRT_Raju'),
          uiOutput("DIF_IRT_Raju_na_alert"),
          strong(textOutput("raju_dif_items")),
          br(),
          tags$head(tags$style("#coef_raju_dif  {white-space: nowrap;}")),
          fluidRow(column(12, align = "left", tableOutput("coef_raju_dif"))),
          fluidRow(column(12, align = "left", uiOutput("note_raju"))),
          br(),
          fluidRow(column(2, downloadButton(outputId = "download_raju_dif", label = "Download table"))),
          br(),
          h4("Purification process"),
          textOutput("dif_raju_puri_info"),
          br(),
          tags$head(tags$style("#dif_raju_puri_table  {white-space: nowrap;}")),
          fluidRow(column(12, align = "center", tableOutput("dif_raju_puri_table"))),
          conditionalPanel(
            "input.puri_Raju == 1",
            downloadButton(outputId = "download_raju_dif_puri", label = "Download table"), br(), br()
          ),
          h4("Selected R code"),
          code(includeText("sc/dif/raju.R"))
        ),
        # ** Items ####
        tabPanel("Items",
          value = "raju_it",
          h3("Raju test for IRT models"),
          p("To detect DIF, the Raju test (Raju, 1988, 1990) uses the area between the item charateristic curves of the selected
                                    IRT model, fitted separately with data of the two groups. The model is either 1PL, 2PL, or 3PL with guessing
                                    which is the same for the two groups. In the case of the 3PL model, the guessing parameter is estimated based on the whole dataset and is
                                    subsequently considered fixed."),
          h4("Method specification"),
          p("Here you can choose an underlying IRT ", strong("model"), " used to test DIF. You can also select the ", strong("correction method"), " for multiple comparison,
                                   and/or", strong("item purification."), "For a selected", strong("item"), "you can display the plot of its characteristic curves and the table
                                   of its estimated parameters with standard errors. "),
          fluidPage(
            column(
              1,
              radioButtons(
                inputId = "type_plot_DIF_IRT_raju",
                label = "Model",
                choices = c(
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PL" = "3PL"
                ),
                selected = "2PL"
              )
            ),
            column(
              2,
              selectInput(
                inputId = "correction_method_DIF_IRT_rajuItems",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "puri_Raju_plot",
                label = "Item purification",
                value = FALSE
              )
            ),
            column(
              2,
              sliderInput(
                inputId = "difirt_raju_itemSlider",
                label = "Item",
                min = 1,
                value = 1,
                max = 10,
                step = 1,
                animate = animationOptions(interval = 1600)
              )
            )
          ),
          h4("Plot with estimated DIF characteristic curve"),
          p("Note that plots might differ slightly even for non-DIF items as two seperate models are fitted, however this difference
                                   is non-significant. "),
          plotlyOutput("plot_DIF_IRT_Raju"),
          downloadButton("DP_plot_DIF_IRT_Raju", label = "Download figure"),
          h4("Equation"),
          uiOutput("irtint_raju"),
          fluidRow(column(12, align = "center", uiOutput("irteq_raju"))),
          h4("Table of parameters"),
          p("This table summarizes the estimated item parameters together with the standard errors. Note that item parameters might differ slightly
                                    even for non-DIF items as two seperate models are fitted, however this difference is non-significant.
                                    Also note that under the 3PL model, the guessing parameter \\(c\\) is estimated from the whole dataset, and
                                    is considered fixed in the final models, thus no standard error is available."),
          fluidRow(column(12, align = "center", tableOutput("tab_coef_DIF_IRT_Raju"))),
          br(),
          h4("Selected R code"),
          code(includeText("sc/dif/raju_it.R"))
        )
      )
    ),
    # * METHOD COMPARISON ####
    tabPanel(
      "Method comparison",
      h3("Method comparison"),
      p("Here you can compare all offered DIF detection methods. In the table below, columns represent DIF detection methods, and rows represent item numbers. If
                        the method detects an item as DIF, value 1 is assigned to that item, otherwise 0 is assigned. In the case that any method fails to converge or cannot be
                        fitted, NA is displayed instead of 0/1 values. Available methods: "),
      tags$ul(
        tags$li(strong("Delta"), "is delta plot method (Angoff & Ford, 1973; Magis & Facon, 2012),"),
        tags$li(strong("MH"), "is Mantel-Haenszel test (Mantel & Haenszel, 1959), "),
        tags$li(strong("LR"), "is logistic regression (Swaminathan & Rogers, 1990),"),
        tags$li(strong("NLR"), "is generalized (non-linear) logistic regression (Drabinova & Martinkova, 2017),"),
        tags$li(strong("LORD"), "is Lord chi-square test (Lord, 1980),"),
        tags$li(strong("RAJU"), "is Raju area method (Raju, 1990),"),
        tags$li(strong("SIBTEST"), "is SIBTEST (Shealy & Stout, 1993) and crossing-SIBTEST method (Chalmers, 2018; Li & Stout, 1996). ")
        # tags$li(strong('DDF'), 'is differential distractor functioning with multinomial log-linear regression model. ')
      ),
      h3("Table with method comparison"),
      p("Settings for individual methods (Observed score, type of DIF to be tested,
                        correction method, item purification) are taken from the subsection pages of given methods.
                        In case your settings are not unified, you can set some of them below. Note that changing
                        the options globaly can be computationaly demanding. This especially applies for a purification request.
                        To see the complete setting of all analyses, please refer to the note below the table.
                        The last column shows how many methods detect a certain item as DIF.
                        The last row shows how many items are detected as DIF by a certain method. "),
      tags$div(tags$style("#same_dmv {color:red; }")),
      htmlOutput("same_dmv"),
      tags$div(tags$style("#same_puri {color:orange; }")),
      htmlOutput("same_puri"),
      tags$div(tags$style("#same_corr {color:orange; }")),
      htmlOutput("same_corr"),
      conditionalPanel("output.unify_methods_condition == 1", br()),
      fluidRow(
        column(
          2,
          selectInput(
            "mc_dmv",
            "Observed score",
            c(
              "as is" = "asis",
              "(standardized) total scores" = "score"
            ),
            "asis"
          )
        ),
        column(
          2,
          selectInput(
            "mc_puri",
            "Item purification",
            c(
              "as is" = "asis",
              "yes" = "purify",
              "no" = "dontpurify"
            ),
            "asis"
          )
        ),
        column(
          3,
          selectInput(
            "mc_corr",
            "Correction method",
            c(
              "as is" = "asis",
              "Benjamini-Hochberg" = "BH",
              "Benjamini-Yekutieli" = "BY",
              "Bonferroni" = "bonferroni",
              "Holm" = "holm",
              "Hochberg" = "hochberg",
              "Hommel" = "hommel",
              "none" = "none"
            ),
            "asis"
          )
        ),
        column(
          1, br(),
          actionButton("unify_button", "Apply setting")
        )
      ),
      # br(),
      fluidRow(column(12, align = "left", tableOutput("method_comparison_table"))),
      fluidRow((column(12, align = "left", uiOutput("mc_settings"))))
    ),
    # POLYTOMOUS METHODS ####
    "----",
    "Polytomous methods",
    # * CUMULATIVE ####
    ui_DIF_cumulative,
    # * ADJACENT ####
    ui_DIF_adjacent,
    # * MULTINOMIAL ####
    ui_DIF_multinomial,
    "----",
    "Training",
    # * TRAINING  ####
    uiTDIF,

    # Modules
    "---",
    "Modules",
    tabPanel(tags$a("DIF-C",
                    href = "https://shiny.cs.cas.cz/ShinyItemAnalysis-module-DIF-C-ordinal/",
                    target = "_blank", .noWS = "outside"
    ))
  )
