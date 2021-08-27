# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# REPORTS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Name of dataset in report ####
dataName <- reactive({
  dataset$name
})

observe({
  updateTextInput(
    session = session,
    inputId = "reportDataName",
    value = paste(dataName(), "dataset")
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * VALIDITY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Criterion present ####
criterionPresent <- reactive({
  (any(dataset$criterion != "missing") | is.null(dataset$criterion))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT MODELS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model ####
report_IRT_binary_model <- reactive({
  if (input$report_IRT_binary_model == "none") {
    fit <- NULL
  } else {
    fit <- switch(input$report_IRT_binary_model,
      "Rasch" = IRT_binary_model_rasch(),
      "1PL" = IRT_binary_model_1pl(),
      "2PL" = IRT_binary_model_2pl(),
      "3PL" = IRT_binary_model_3pl(),
      "4PL" = IRT_binary_model_4pl()
    )
  }
  fit
})

# ** ICC equation ####
# so far it takes parametrization setting from the IRT section
report_IRT_binary_equation <- reactive({
  if (input$IRT_binary_summary_parametrization == "irt") {
    txt1 <- switch(input$report_IRT_binary_model,
                   "none" = "",
                   "Rasch" = "{(\\theta_p - b_i)}",
                   "1PL" = "{a(\\theta_p - b_i)}",
                   "2PL" = "{a_i(\\theta_p - b_i)}",
                   "3PL" = "{a_i(\\theta_p - b_i)}",
                   "4PL" = "{a_i(\\theta_p - b_i)}"
    )
  } else {
    txt1 <- switch(input$report_IRT_binary_model,
                   "none" = "",
                   "Rasch" = "{\\beta_{i0} + \\theta_p}",
                   "1PL" = "{\\beta_{i0} + \\beta_{1} \\theta_p}",
                   "2PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}",
                   "3PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}",
                   "4PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}"
    )
  }

  txt2 <- switch(input$report_IRT_binary_model,
                 "none" = "",
                 "Rasch" = "",
                 "1PL" = "",
                 "2PL" = "",
                 "3PL" = "c_i + (1 - c_i)",
                 "4PL" = "c_i + (d_i - c_i)"
  )

  if (input$report_IRT_binary_model == "none") {
    txt <- ""
  } else {
    txt <- paste0(
      "$$\\mathrm{P}(Y_{pi} = 1|\\theta_p) = \\pi_{pi} = ", txt2,
      "\\frac{e^", txt1, "}{1 + e^", txt1, "}$$"
    )
  }
  txt
})

# ** Wright map ####
# in reports there is Wright map based on 1PL model, does not matter which
# IRT model is selected
report_IRT_binary_wrightmap <- reactive({
  if (input$report_IRT_binary_model == "none") {
    g <- ""
  } else {
    fit <- IRT_binary_model_1pl()
    fscore <- as.vector(fscores(fit))
    b <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items[, "b"]

    g <- ggWrightMap(fscore, b, item.names = item_names())
  }
  g
})

# ** Plot of ICC ####
report_IRT_binary_icc <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    mod_item_names <- fit@Data$data %>% colnames()

    d <- map2_dfr(
      mod_item_names,
      item_names(), # names from user
      ~ tibble(
        Ability = IRT_thetas_for_plots(), # vector only
        Probability = probtrace(extract.item(fit, .x), IRT_thetas_for_plots())[, 2], # ascending probs
        Item = .y,
      )
    )
    d$Item <- factor(d$Item, levels = item_names())

    g <- d %>% ggplot(aes(x = Ability, y = Probability, color = Item)) +
      geom_line() +
      ylab("Probability of correct answer") +
      theme_app()
  }
  g
})

# ** Plot of IIC ####
report_IRT_binary_iic <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    mod_item_names <- fit@Data$data %>% colnames()

    d <- map2_dfr(
      mod_item_names,
      item_names(), # names from user
      ~ tibble(
        Ability = IRT_thetas_for_plots(), # vector only
        Information = iteminfo(extract.item(fit, .x), IRT_thetas_for_plots()),
        Item = .y,
      )
    )
    d$Item <- factor(d$Item, levels = item_names())

    g <- d %>% ggplot(aes(x = Ability, y = Information, color = Item)) +
      geom_line() +
      theme_app()
  }
  g
})

# ** Plot of TIC ####
report_IRT_binary_tic <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    plt <- plot(fit, type = "infoSE")

    vals <- plt$panel.args
    x <- vals[[1]]$x
    y <- vals[[1]]$y
    df <- data.frame(cbind(Ability = x, Information = y))

    df$SE <- 1 / sqrt(df$Information)

    g <- ggplot(data = df, aes(x = Ability)) +
      geom_line(aes(y = Information, col = "info")) +
      geom_line(aes(y = SE, col = "se")) +
      scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
      scale_y_continuous("Information",
        sec.axis = sec_axis(~., name = "SE")
      ) +
      theme(axis.title.y = element_text(color = "pink")) +
      theme_app()
  }
  g
})

# ** Table of parameters ####
report_IRT_binary_coef <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    tab <- ""
  } else {
    n <- length(item_names())

    # parametrization should be added as option into reports
    IRTpars <- input$IRT_binary_summary_parametrization == "irt"

    par_tab <- coef(fit, IRTpars = IRTpars, simplify = TRUE)$items
    if (dim(fit@vcov)[1] > 1) {
      se_list <- coef(fit, IRTpars = IRTpars, printSE = TRUE)
      se_tab <- do.call(rbind, lapply(1:nrow(par_tab), function(i) se_list[[i]]["SE", ]))
    } else {
      se_tab <- cbind(rep(NA, nrow(par_tab)), NA, NA, NA)
    }

    tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]

    tab_fit <- itemfit(fit)[, 2:4]
    if (!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
      cat("ERROR : ", conditionMessage(e), "\n")
    }))) {
      tab <- data.frame(tab, itemfit(fit)[, 2:4])
      colnames(tab)[9:11] <- c("SX2-value", "df", "p-value")
    } else {
      tab <- data.frame(tab, cbind("-", "-", "-"))
      colnames(tab)[9:11] <- c("SX2-value", "df", "p-value")
    }

    if (IRTpars) {
      colnames(tab)[1:8] <- paste0(
        c("", "SE("),
        paste0("%%mathit{", rep(c("a", "b", "c", "d"), each = 2), "}%%"),
        c("", ")")
      )
    } else {
      colnames(tab)[1:8] <- paste0(
        c("", "SE("),
        paste0("%%mathit{", rep(c("\\beta_{1}", "\\beta_{0}", "c", "d"), each = 2), "}%%"),
        c("", ")")
      )
      tab <- tab[, c(3:4, 1:2, 5:8, 9:11)]
    }

    rownames(tab) <- item_names()
    tab
  }
  tab
})

# ** Ability estimates plot ####
report_IRT_binary_ability_plot <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    fscore <- as.vector(fscores(fit))
    zscore <- z_score()

    df <- data.frame(fscore, zscore)

    g <- ggplot(df, aes_string("zscore", "fscore")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_app() +
      theme(
        legend.box.just = "left",
        legend.justification = c(1, 0),
        legend.position = c(1, 0),
        legend.box = "vertical",
        legend.key.size = unit(1, "lines"),
        legend.text.align = 0,
        legend.title.align = 0
      )
  }
  g
})

# ** Ability estimates table ####
report_IRT_binary_ability_table <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    tab <- ""
  } else {
    score <- as.vector(total_score())
    zscore <- as.vector(z_score())
    fscore <- fscores(fit)

    tab <- data.frame(score, zscore, fscore)
    tab <- data.frame(
      Min = sapply(tab, min, na.rm = TRUE),
      Max = sapply(tab, max, na.rm = TRUE),
      Mean = sapply(tab, mean, na.rm = TRUE),
      Median = sapply(tab, median, na.rm = TRUE),
      SD = sapply(tab, sd, na.rm = TRUE),
      Skewness = sapply(tab, ShinyItemAnalysis:::skewness),
      Kurtosis = sapply(tab, ShinyItemAnalysis:::kurtosis)
    )
    rownames(tab) <- c("Total Scores", "Z-Scores", "F-scores")
  }
  tab
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DIF ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Group present ####
groupPresent <- reactive({
  (any(dataset$group != "missing") | is.null(dataset$group))
})

# * Observed score / DIF matching present ####
DIFmatchingPresent <- reactive({
  (any(dataset$DIFmatching != "missing") | is.null(dataset$DIFmatching))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * PROGRESS BAR ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Progress bar ####
observeEvent(input$generate, {
  withProgress(message = "Creating content", value = 0, style = "notification", {
    list( # header
      author = input$reportAuthor,
      dataset = input$reportDataName,
      # datasets
      a = nominal(),
      k = key(),
      itemNames = item_names(),
      # total scores
      incProgress(0.05),
      results = t(totalscores_table_Input()),
      histogram_totalscores = totalscores_histogram_Input(),
      cutScore = input$slider_totalscores_histogram,
      # standard scores
      standardscores_table = standardscores_table_Input(),
      incProgress(0.05),
      # validity section
      corr_plot = {
        if (input$corr_report) {
          if (input$customizeCheck) {
            corr_plot_Input_report()
          } else {
            corr_plot_Input()
          }
        } else {
          ""
        }
      },
      corr_plot_numclust = ifelse(input$customizeCheck, input$corr_plot_clust_report, input$corr_plot_clust),
      corr_plot_clustmethod = ifelse(input$customizeCheck, input$corr_plot_clustmethod_report, input$corr_plot_clustmethod),
      corr_type = ifelse(input$customizeCheck, input$corr_plot_type_of_corr_report, input$type_of_corr),
      # scree_plot = {
      #   if (input$corr_report) {
      #     scree_plot_Input()
      #   } else {
      #     ""
      #   }
      # },
      isCriterionPresent = criterionPresent(),
      validity_check = input$predict_report,
      validity_plot = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_plot_Input()
          } else {
            ""
          }
        }
      },
      validity_table = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_table_Input()
          } else {
            ""
          }
        }
      },
      incProgress(0.05),
      # item analysis
      DDplot = report_itemanalysis_DDplot(),
      DDplotRange1 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[1]], input$itemanalysis_DDplot_range_slider[[1]]),
      DDplotRange2 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[2]], input$itemanalysis_DDplot_range_slider[[2]]),
      DDplotNumGroups = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_groups_slider, input$itemanalysis_DDplot_groups_slider),
      DDplotDiscType = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_discrimination, input$itemanalysis_DDplot_discrimination),
      itemexam = report_itemanalysis_table(),
      cronbachs_alpha_table = reliability_cronbachalpha_table_Input(),
      incProgress(0.05),
      # distractors
      distractor_plot = report_distractor_plot(),
      type_distractor_plot = input$report_distractor_type,
      distractor_plot_legend_length = report_distractor_plot_legend_length(),
      incProgress(0.25),
      # regression
      multiplot = report_regression_multinomial_plot(),
      incProgress(0.05),
      # irt
      irt_wrightmap = report_IRT_binary_wrightmap(),
      irt_equation = report_IRT_binary_equation(),
      irt_model = input$report_IRT_binary_model,
      irt_parametrization = input$IRT_binary_summary_parametrization,
      irt_icc = report_IRT_binary_icc(),
      irt_iic = report_IRT_binary_iic(),
      irt_tic = report_IRT_binary_tic(),
      irt_coef = report_IRT_binary_coef(),
      irt_ability_plot = report_IRT_binary_ability_plot(),
      irt_ability_table = report_IRT_binary_ability_table(),
      incProgress(0.25),
      # DIF
      ### presence of group vector
      isGroupPresent = groupPresent(),
      ### histograms by group
      histCheck = input$histCheck,
      DIF_total_table = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_table_Input()
          }
        }
      },
      DIF_total_hist = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_hist_Input()
          }
        }
      },
      DIF_total_ttest = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_ttest_Input()
          }
        }
      },
      ### delta plot
      deltaplotCheck = input$deltaplotCheck,
      deltaplot = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            deltaplotInput_report()
          }
        }
      },
      DP_text_normal = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            deltaGpurn_report()
          }
        }
      },
      ### Mantel-Haenszel
      MHCheck = input$MHCheck,
      DIF_MH_print = {
        if (groupPresent()) {
          if (input$MHCheck) {
            report_DIF_MH_model()
          }
        }
      },
      ### logistic regression
      logregCheck = input$logregCheck,
      DIF_logistic_plot = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_plot()
          }
        }
      },
      DIF_logistic_print = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_model()
          }
        }
      },
      ### DDF multinomial
      multiCheck = input$multiCheck,
      DIF_multinomial_print = {
        if (groupPresent()) {
          if (input$multiCheck) {
            DIF_multinomial_model_report()
          }
        }
      },
      DIF_multinomial_plot = {
        if (groupPresent()) {
          if (input$multiCheck) {
            DIF_multinomial_plot_report()
          }
        }
      },
      incProgress(0.25),
      ### sessionInfo
      sessionInfo = input$include_session
    )
  })

  output$download_report_button <- renderUI({
    if (is.null(input$generate)) {
      return(NULL)
    }
    downloadButton(
      outputId = "report",
      label = "Download report",
      class = "btn btn-primary"
    )
  })
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DOWNLOAD REPORT ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Download report ####
output$report <- downloadHandler(
  filename = reactive({
    paste0("report.", input$report_format)
  }),
  content = function(file) {
    reportPath <- file.path(getwd(), paste0("report", input$report_format, ".Rmd"))
    parameters <- list( # header
      author = input$reportAuthor,
      dataset = input$reportDataName,
      # datasets
      a = nominal(),
      k = key(),
      itemNames = item_names(),
      # total scores
      totalscores_table = t(totalscores_table_Input()),
      histogram_totalscores = totalscores_histogram_Input(),
      cutScore = input$slider_totalscores_histogram,
      # standard scores
      standardscores_table = standardscores_table_Input(),
      # validity section
      corr_plot = {
        if (input$corr_report) {
          if (input$customizeCheck) {
            corr_plot_Input_report()
          } else {
            corr_plot_Input()
          }
        } else {
          ""
        }
      },
      corr_plot_numclust = ifelse(input$customizeCheck, input$corr_plot_clust_report, input$corr_plot_clust),
      corr_plot_clustmethod = ifelse(input$customizeCheck, input$corr_plot_clustmethod_report, input$corr_plot_clustmethod),
      corr_type = ifelse(input$customizeCheck, input$corr_plot_type_of_corr_report, input$type_of_corr),
      # scree_plot = {
      #   if (input$corr_report) {
      #     scree_plot_Input()
      #   } else {
      #     ""
      #   }
      # },
      isCriterionPresent = criterionPresent(),
      validity_check = input$predict_report,
      validity_plot = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_plot_Input()
          } else {
            ""
          }
        }
      },
      validity_table = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_table_Input()
          } else {
            ""
          }
        }
      },
      # item analysis
      DDplot = report_itemanalysis_DDplot(),
      DDplotRange1 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[1]], input$itemanalysis_DDplot_range_slider[[1]]),
      DDplotRange2 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[2]], input$itemanalysis_DDplot_range_slider[[2]]),
      DDplotNumGroups = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_groups_slider, input$itemanalysis_DDplot_groups_slider),
      DDplotDiscType = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_discrimination, input$itemanalysis_DDplot_discrimination),
      itemexam = report_itemanalysis_table(),
      cronbachs_alpha_table = reliability_cronbachalpha_table_Input(),
      # distractors
      distractor_plot = report_distractor_plot(),
      type_distractor_plot = input$report_distractor_type,
      distractor_plot_legend_length = report_distractor_plot_legend_length(),
      # regression
      multiplot = report_regression_multinomial_plot(),
      # irt
      irt_wrightmap = report_IRT_binary_wrightmap(),
      irt_equation = report_IRT_binary_equation(),
      irt_model = input$report_IRT_binary_model,
      irt_parametrization = input$IRT_binary_summary_parametrization,
      irt_icc = report_IRT_binary_icc(),
      irt_iic = report_IRT_binary_iic(),
      irt_tic = report_IRT_binary_tic(),
      irt_coef = report_IRT_binary_coef(),
      irt_ability_plot = report_IRT_binary_ability_plot(),
      irt_ability_table = report_IRT_binary_ability_table(),
      # DIF
      ### presence of group vector
      isGroupPresent = groupPresent(),
      ### histograms by groups
      histCheck = input$histCheck,
      DIF_total_table = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_table_Input()
          }
        }
      },
      DIF_total_hist = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_hist_Input()
          }
        }
      },
      DIF_total_ttest = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_ttest_Input()
          }
        }
      },
      ### delta plot
      deltaplotCheck = input$deltaplotCheck,
      DIF_deltaplot = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            deltaplotInput_report()
          }
        }
      },
      DIF_deltaplot_text = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            deltaGpurn_report()
          }
        }
      },
      ### Mantel-Haenszel
      MHCheck = input$MHCheck,
      DIF_MH_print = {
        if (groupPresent()) {
          if (input$MHCheck) {
            report_DIF_MH_model()
          }
        }
      },
      ### logistic regression
      logregCheck = input$logregCheck,
      DIF_logistic_plot = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_plot()
          }
        }
      },
      DIF_logistic_print = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_model()
          }
        }
      },
      ### multinomial regression
      multiCheck = input$multiCheck,
      DIF_multinomial_print = {
        if (groupPresent()) {
          if (input$multiCheck) {
            DIF_multinomial_model_report()
          }
        }
      },
      DIF_multinomial_plot = {
        if (groupPresent()) {
          if (input$multiCheck) {
            DIF_multinomial_plot_report()
          }
        }
      },
      ### sessionInfo
      sessionInfo = input$include_session
    )
    rmarkdown::render(reportPath,
      output_file = file,
      params = parameters, envir = new.env(parent = globalenv())
    )
  }
)

# source('tests/helper_functions/markdown_render.R', local = TRUE)
#
# exportTestValues(report = report_react())
