# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# IRT MODELS WITH MIRT ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Thetas vector for plotting ####
# length.out effectively specifies the "resolution" of plotted lines
IRT_thetas_for_plots <- reactive({
  seq(-6, 6, length.out = 500)
})

# ** Helper function for consistent colors ####
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BINARY MODELS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ####

IRT_binary <- reactiveValues(
  model = NULL,
  parametrization = NULL
)

# ** Updating model ####
observeEvent(input$IRT_binary_summary_model, {
  IRT_binary$model <- input$IRT_binary_summary_model
})
observeEvent(input$IRT_binary_items_model, {
  IRT_binary$model <- input$IRT_binary_items_model
})
observeEvent(IRT_binary$model, {
  if (IRT_binary$model != input$IRT_binary_summary_model) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "IRT_binary_summary_model",
      selected = IRT_binary$model
    )
  }
  if (IRT_binary$model != input$IRT_binary_items_model) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "IRT_binary_items_model",
      selected = IRT_binary$model
    )
  }
})

# ** Updating parametrization ####
observeEvent(input$IRT_binary_summary_parametrization, {
  IRT_binary$parametrization <- input$IRT_binary_summary_parametrization
})
observeEvent(input$IRT_binary_items_parametrization, {
  IRT_binary$parametrization <- input$IRT_binary_items_parametrization
})
observeEvent(IRT_binary$parametrization, {
  if (IRT_binary$parametrization != input$IRT_binary_summary_parametrization) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "IRT_binary_summary_parametrization",
      selected = IRT_binary$parametrization
    )
  }
  if (IRT_binary$parametrization != input$IRT_binary_items_parametrization) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "IRT_binary_items_parametrization",
      selected = IRT_binary$parametrization
    )
  }
})

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())
  updateSliderInput(
    session = session,
    inputId = "IRT_binary_items",
    max = item_count
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MODEL ####

# ** Rasch model ####
IRT_binary_model_rasch <- reactive({
  data <- binary()
  fit <- mirt(
    data, model = 1, itemtype = "Rasch",
    SE = TRUE, verbose = FALSE
  )
  fit
})

# ** 1PL model ####
IRT_binary_model_1pl <- reactive({
  data <- binary()
  s <- paste(
    "F = 1-", ncol(data), "\n",
    "CONSTRAIN = (1-", ncol(data), ", a1)"
  )
  model <- mirt.model(s)
  fit <- mirt(
    data, model = model, itemtype = "2PL",
    SE = TRUE, verbose = FALSE
  )
  fit
})

# ** 2PL model ####
IRT_binary_model_2pl <- reactive({
  data <- binary()
  fit <- mirt(
    data, model = 1, itemtype = "2PL",
    SE = TRUE, verbose = FALSE
  )
  fit
})

# ** 3PL model ####
IRT_binary_model_3pl <- reactive({
  data <- binary()
  fit <- mirt(
    data, model = 1, itemtype = "3PL",
    SE = TRUE, verbose = FALSE
  )
  fit
})

# ** 4PL model ####
IRT_binary_model_4pl <- reactive({
  data <- binary()
  fit <- mirt(
    data, model = 1, itemtype = "4PL",
    SE = TRUE, verbose = FALSE
  )
  fit
})

# ** Model ####
IRT_binary_model <- reactive({
  fit <- switch(input$IRT_binary_summary_model,
    "Rasch" = IRT_binary_model_rasch(),
    "1PL" = IRT_binary_model_1pl(),
    "2PL" = IRT_binary_model_2pl(),
    "3PL" = IRT_binary_model_3pl(),
    "4PL" = IRT_binary_model_4pl()
  )
  fit
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ####

# ** Model description ####
IRT_binary_summary_model_description <- reactive({
  if (input$IRT_binary_summary_parametrization == "irt") {
    txt <- switch(input$IRT_binary_summary_model,
      "Rasch" =
        paste0("In the <b>Rasch model</b> (Rasch, 1960), the items may differ only in their difficulty parameters \\(b_i\\),
             which are represented by the locations of the item characteristic curve inflection points.
             All items are assumed to have the same discrimination of 1, represented by the slope in the inflection point.
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) of respondent \\(p\\) is assumed to follow normal distribution
             with freely estimated variance. "),
      "1PL" =
        paste0("In the <b>One Parameter Logistic (1PL) IRT model</b>, the items may differ only in their difficulty parameters \\(b_i\\),
             which are represented by the locations of the item characteristic curve inflection points.
             All items are assumed to have the same discrimination \\(a\\), represented by the slope in the inflection point.
             Its value corresponds to the standard deviation of ability estimates in the Rasch model.
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) of respondent \\(p\\) is assumed to follow standard normal
             distribution. "),
      "2PL" =
        paste0("The <b>Two Parameter Logistic (2PL) IRT model</b> allows for different difficulty parameters \\(b_i\\),
             which are represented by the locations of the item characteristic curve inflection points,
             and items can also differ in their discrimination parameters \\(a_i\\), represented by the slope in the inflection point.
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) of respondent \\(p\\) is assumed to follow standard normal
             distribution. "),
      "3PL" =
        paste0("The <b>Three Parameter Logistic (3PL) IRT model </b> allows for different item difficulties \\(b_i\\),
             different discriminations of items \\(a_i\\), and also for nonzero left asymptotes, the pseudo-guessing \\(c_i\\).
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) is assumed to  follow standard normal distribution. "),
      "4PL" =
        paste0("The <b>Four Parameter Logistic (4PL) IRT model </b> allows for different item difficulties \\(b_i\\),
             different discriminations of items \\(a_i\\), nonzero left asymptotes, the pseudo-guessing \\(c_i\\),
             and also for upper asymptote lower than one, i.e, inattention parameters \\(d_i\\).
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) is assumed to follow standard normal distribution. ")
    )
  } else {
    txt <- switch(input$IRT_binary_summary_model,
      "Rasch" =
        paste0("In the <b>Rasch model</b> (Rasch, 1960), the items may differ only in their difficulty parameters \\(b_i\\),
             which are represented by the locations of the item characteristic curve inflection points.
             All items are assumed to have the same discrimination of 1, represented by the slope in the inflection point.
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) of respondent \\(p\\) is assumed to follow normal distribution
             with freely estimated variance. "),
      "1PL" =
        paste0("In the <b>One Parameter Logistic (1PL) IRT model</b>, the items may differ only in their difficulty parameters \\(b_i\\),
             which are represented by the locations of the item characteristic curve inflection points.
             All items are assumed to have the same discrimination \\(a\\), represented by the slope in the inflection point.
             Its value corresponds to the standard deviation of ability estimates in the Rasch model.
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) of respondent \\(p\\) is assumed to follow standard normal
             distribution. "),
      "2PL" =
        paste0("The <b>Two Parameter Logistic (2PL) IRT model</b> allows for different difficulty parameters \\(b_i\\),
             which are represented by the locations of the item characteristic curve inflection points,
             and items can also differ in their discrimination parameters \\(a_i\\), represented by the slope in the inflection point.
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) of respondent \\(p\\) is assumed to follow standard normal
             distribution. "),
      "3PL" =
        paste0("The <b>Three Parameter Logistic (3PL) IRT model </b> allows for different item difficulties \\(b_i\\),
             different discriminations of items \\(a_i\\), and also for nonzero left asymptotes, the pseudo-guessing \\(c_i\\).
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) is assumed to  follow standard normal distribution. "),
      "4PL" =
        paste0("The <b>Four Parameter Logistic (4PL) IRT model </b> allows for different item difficulties \\(b_i\\),
             different discriminations of items \\(a_i\\), nonzero left asymptotes, the pseudo-guessing \\(c_i\\),
             and also for upper asymptote lower than one, i.e, inattention parameters \\(d_i\\).
             Model parameters are estimated using a marginal maximum likelihood method.
             The ability \\(\\theta_p\\) is assumed to follow standard normal distribution. ")
    )
  }
  txt
})

output$IRT_binary_summary_model_description <- renderText({
  IRT_binary_summary_model_description()
})

# ** ICC equation ####
IRT_binary_summary_icc_equation <- reactive({
  if (input$IRT_binary_summary_parametrization == "irt") {
    txt1 <- switch(input$IRT_binary_summary_model,
      "Rasch" = "{(\\theta_p - b_i)}",
      "1PL" = "{a(\\theta_p - b_i)}",
      "2PL" = "{a_i(\\theta_p - b_i)}",
      "3PL" = "{a_i(\\theta_p - b_i)}",
      "4PL" = "{a_i(\\theta_p - b_i)}"
    )
  } else {
    txt1 <- switch(input$IRT_binary_summary_model,
      "Rasch" = "{\\beta_{i0} + \\theta_p}",
      "1PL" = "{\\beta_{i0} + \\beta_{1} \\theta_p}",
      "2PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}",
      "3PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}",
      "4PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}"
    )
  }

  txt2 <- switch(input$IRT_binary_summary_model,
    "Rasch" = "",
    "1PL" = "",
    "2PL" = "",
    "3PL" = "c_i + (1 - c_i)",
    "4PL" = "c_i + (d_i - c_i)"
  )

  txt <- paste0(
    "$$\\mathrm{P}(Y_{pi} = 1|\\theta_p) = \\pi_{pi} = ", txt2,
    "\\frac{e^", txt1, "}{1 + e^", txt1, "}$$"
  )
  txt
})

output$IRT_binary_summary_icc_equation <- renderUI({
  withMathJax(IRT_binary_summary_icc_equation())
})

# ** IIC equation ####
IRT_binary_summary_iic_equation <- reactive({
  if (input$IRT_binary_summary_parametrization == "irt") {
    txt1 <- switch(input$IRT_binary_summary_model,
      "Rasch" = "\\pi_{pi} (1 - \\pi_{pi})",
      "1PL" = "a^2 \\pi_{pi} (1 - \\pi_{pi})",
      "2PL" = "a_i^2 \\pi_{pi} (1 - \\pi_{pi})",
      "3PL" = "\\frac{a_i^2 (\\pi_{pi} - c_i)^2 (1 - \\pi_{pi})}{(1 - c_i^2) \\pi_{pi}}",
      "4PL" = "\\frac{a_i^2 (\\pi_{pi} - c_i)^2 (d_i - \\pi_{pi})^2}{(d_i - c_i^2) \\pi_{pi} (1 - \\pi_{pi})}",
    )
  } else {
    txt1 <- switch(input$IRT_binary_summary_model,
      "Rasch" = "\\pi_{pi} (1 - \\pi_{pi})",
      "1PL" = "\\beta_{1}^2 \\pi_{pi} (1 - \\pi_{pi})",
      "2PL" = "\\beta_{i1}^2 \\pi_{pi} (1 - \\pi_{pi})",
      "3PL" = "\\frac{\\beta_{i1}^2 (\\pi_{pi} - c_i)^2 (1 - \\pi_{pi})}{(1 - c_i^2) \\pi_{pi}}",
      "4PL" = "\\frac{\\beta_{i1}^2 (\\pi_{pi} - c_i)^2 (d_i - \\pi_{pi})^2}{(d_i - c_i^2) \\pi_{pi} (1 - \\pi_{pi})}",
    )
  }
  txt <- paste0(
    "$$\\mathrm{I}(Y_{pi}) = ", txt1, "$$"
  )
  txt
})

output$IRT_binary_summary_iic_equation <- renderUI({
  withMathJax(IRT_binary_summary_iic_equation())
})

# ** Equation interpretation ####
IRT_binary_summary_equation_interpretation <- reactive({
  if (input$IRT_binary_summary_parametrization == "irt") {
    txt <- switch(input$IRT_binary_summary_model,
      "Rasch" =
        "Parameter \\(b_i\\) is a location of the inflection points of the characteristic curve
        of item \\(i\\). It also gives the level of ability \\(\\theta_p\\) for which the probability
        of the correct answer is exactly 0.5. ",
      "1PL" =
        "Parameter \\(b_i\\) is a location of the inflection points of the characteristic curve
        of item \\(i\\). It also gives the level of ability \\(\\theta_p\\) for which the probability
        of the correct answer is exactly 0.5. Parameter \\(a\\) is a common discrimination for all items,
        i.e., common slope at infection points \\(b_i\\). ",
      "2PL" =
        "Parameter \\(b_i\\) is a location of the inflection points of the characteristic curve
        of item \\(i\\). It also gives the level of ability \\(\\theta_p\\) for which the probability
        of the correct answer is exactly 0.5. Parameter \\(a_i\\) is a discrimination for item \\(i\\),
        i.e., slope at infection point \\(b_i\\). ",
      "3PL" =
        "Parameter \\(b_i\\) is a location of the inflection point of the characteristic curve
        of item \\(i\\). It also gives the level of ability \\(\\theta_p\\) for which the probability
        of the correct answer is exactly 0.5. Parameter \\(a_i\\) is a discrimination for item \\(i\\),
        i.e., slope at infection point \\(b_i\\). Parameter \\(c_i\\) is a (pseudo)-guessing parameter,
        i.e., it gives the probability of correct answer when trait is not sufficient, represented by
        the lower asymptote of the item characteristic curve. ",
      "4PL" =
        "Parameter \\(b_i\\) is a location of the inflection points of the characteristic curve
        of item \\(i\\). It also gives the level of ability \\(\\theta_p\\) for which the probability
        of the correct answer is exactly 0.5. Parameter \\(a_i\\) is a discrimination for item \\(i\\),
        i.e., slope at infection points \\(b_i\\). Parameter \\(c_i\\) is a (pseudo)-guessing parameter,
        i.e., it gives the probability of correct answer when trait is not sufficient, represented by
        the lower asymptote of the item characteristic curve. Parameter \\(d_i\\) is an inattention
        parameter, i.e., \\(1 - d_i\\) gives the probability of uncorrect answer when trait is high,
        represented by the upper asymptote of the item characteristic curve. ",
    )
  } else {
    txt <- switch(input$IRT_binary_summary_model,
      "Rasch" =
        "Parameter \\(\\beta_{i0}\\) is an intercept and it describes a location of the inflection
        point of the characteristic function of item \\(i\\). ",
      "1PL" =
        "Parameter \\(\\beta_{i0}\\) is an intercept and it describes a location of the inflection
        point of the characteristic function of item \\(i\\). Parameter \\(\\beta_{1}\\) describes
        common slope at all infection points. ",
      "2PL" =
        "Parameter \\(\\beta_{i0}\\) is an intercept and it describes a location of the inflection
        point of the characteristic function of item \\(i\\). Parameter \\(\\beta_{i1}\\) describes
        slope at infection point. ",
      "3PL" = "Parameter \\(\\beta_{i0}\\) is an intercept and it describes a location of the inflection
        point of the characteristic function of item \\(i\\). Parameter \\(\\beta_{i1}\\) describes
        slope at infection point. Parameter \\(c_i\\) is a (pseudo)-guessing parameter, i.e., it gives
        the probability of correct answer when trait is not sufficient, represented by
        the lower asymptote of the item characteristic curve. ",
      "4PL" = "Parameter \\(\\beta_{i0}\\) is an intercept and it describes a location of the inflection
        point of the characteristic function of item \\(i\\). Parameter \\(\\beta_{i1}\\) describes
        slope at infection point. Parameter \\(c_i\\) is a (pseudo)-guessing parameter, i.e., it gives
        the probability of correct answer when trait is not sufficient, represented by
        the lower asymptote of the item characteristic curve. Parameter \\(d_i\\) is an inattention
        parameter, i.e., \\(1 - d_i\\) gives the probability of uncorrect answer when trait is high,
        represented by the upper asymptote of the item characteristic curve. ",
    )
  }
  txt
})

output$IRT_binary_summary_equation_interpretation <- renderUI({
  withMathJax(IRT_binary_summary_equation_interpretation())
})

# ** Check whether model converged ####
output$IRT_binary_summary_model_converged <- renderUI({
  fit <- IRT_binary_model()
  txt <- ifelse(fit@OptimInfo$converged,
    "",
    "<font color = 'orange'> Estimation process terminated without convergence.
    Estimates are not reliable. Try to increase a number of iterations of the EM
    algorithm in Settings. </font>"
  )
  HTML(txt)
})

# ** Plot of ICC ####
IRT_binary_summary_icc <- reactive({
  fit <- IRT_binary_model()

  # names from the model
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
  g
})

output$IRT_binary_summary_icc <- renderPlotly({
  g <- IRT_binary_summary_icc()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of ICC ####
output$IRT_binary_summary_icc_download <- downloadHandler(
  filename = function() {
    "fig_IRT_binary_ICC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_binary_summary_icc() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of IIC ####
IRT_binary_summary_iic <- reactive({
  fit <- IRT_binary_model()

  # names from model
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
  g
})

output$IRT_binary_summary_iic <- renderPlotly({
  g <- IRT_binary_summary_iic()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# *** Download plot of IIC ####
output$IRT_binary_summary_iic_download <- downloadHandler(
  filename = function() {
    "fig_IRT_binary_IIC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_binary_summary_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Plot of TIC ####
IRT_binary_summary_tic <- reactive({
  fit <- IRT_binary_model()

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
  g
})

output$IRT_binary_summary_tic <- renderPlotly({
  g <- IRT_binary_summary_tic()

  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# *** Download plot of TIC ####
output$IRT_binary_summary_tic_download <- downloadHandler(
  filename = function() {
    "fig_IRT_binary_TIC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_binary_summary_tic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of parameters ####
IRT_binary_summary_coef <- reactive({
  fit <- IRT_binary_model()
  n <- length(item_names())

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
})

output$IRT_binary_summary_coef <- renderTable(
  {
    IRT_binary_summary_coef()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Download table ####
output$IRT_binary_summary_coef_download <- downloadHandler(
  filename = function() {
    "tab_IRT_binary_parameters.csv"
  },
  content = function(file) {
    write.csv(IRT_binary_summary_coef(), file)
  }
)

# ** Ability estimates ####
IRT_binary_summary_ability <- reactive({
  fit <- IRT_binary_model()

  score <- as.vector(total_score())
  zscore <- as.vector(z_score())
  fscore <- fscores(fit, full.scores.SE = TRUE)

  tab <- data.frame(score, zscore, fscore)
  colnames(tab) <- c("Total score", "Z-score", "F-score", "SE(F-score)")
  rownames(tab) <- paste("Respondent", 1:nrow(tab))

  tab
})

output$IRT_binary_summary_ability <- renderTable(
  {
    factors <- IRT_binary_summary_ability()
    head(factors, n = 6)
  },
  include.rownames = TRUE
)

# ** Download of ability estimates ####
output$IRT_binary_summary_ability_download <- downloadHandler(
  filename = function() {
    "tab_IRT_binary_abilities.csv"
  },
  content = function(file) {
    write.csv(IRT_binary_summary_ability(), file)
  }
)

# ** Ability estimates correlation ####
IRT_binary_summary_ability_correlation <- reactive({
  fit <- IRT_binary_model()
  fscore <- as.vector(fscores(fit))
  zscore <- z_score()

  cor <- cor(fscore, zscore, use = "pairwise.complete.obs")
  cor
})

output$IRT_binary_summary_ability_correlation_text <- renderText({
  paste0("This scatterplot shows the relationship between the standardized total
         score (Z-score) and the factor score estimated by the IRT model. The
         Pearson correlation coefficient between these two scores is ",
         sprintf("%.3f", IRT_binary_summary_ability_correlation()), ". ")
})

# ** Ability estimates plot ####
IRT_binary_summary_ability_plot <- reactive({
  fit <- IRT_binary_model()
  fscore <- as.vector(fscores(fit))
  zscore <- z_score()

  df <- data.frame(fscore, zscore)

  g <- ggplot(df, aes_string("zscore", "fscore")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
  g
})

output$IRT_binary_summary_ability_plot <- renderPlotly({
  g <- IRT_binary_summary_ability_plot()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("zscore", "Z-score", p$x$data[[1]]$text)
  p$x$data[[1]]$text <- gsub("fscore", "F-score", p$x$data[[1]]$text)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# *** Download factor scores plot ####
output$IRT_binary_summary_ability_plot_download <- downloadHandler(
  filename = function() {
    "fig_IRT_binary_abilities.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_binary_summary_ability_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Wright map ####
# get args used by ggWrightMap() as a list for later use
IRT_binary_summary_wrightmap_args <- reactive({
  fit <- IRT_binary_model()

  fscore <- as.vector(fscores(fit))
  b <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items[, "b"]

  list(fscore, b)
})

output$IRT_binary_summary_wrightmap <- renderPlotly({
  # use internal Wright Map fun, returning separate "facets" in a list
  plts <- ShinyItemAnalysis:::gg_wright_internal(
    theta = IRT_binary_summary_wrightmap_args()[[1]],
    b = IRT_binary_summary_wrightmap_args()[[2]],
    item.names = item_names()
  )

  # plotlify "facets", tweaking the presentation and tooltip of item-side "facet"
  plt_left <- plts[[1]] %>%
    ggplotly()
  txt <- gsub("count", "Count", plt_left$x$data[[1]]$text)
  txt <- sapply(strsplit(txt, "<br />"), "[", 1)
  thetas <- as.numeric(paste(sapply(strsplit(plt_left$x$data[[1]]$text, "theta: "), "[", 2)))
  binwidth <- c(diff(thetas) / 2)[1]
  txt <- paste0(txt, "<br />", thetas - binwidth, "< Theta <", thetas + binwidth)
  plt_left$x$data[[1]]$text <- txt
  plt_right <- (plts[[2]] +
    geom_text(aes(text = paste0(
      "Item: ",
      stringr::str_remove(item, "(\\|\\s)?0*"),
      "\n", "Difficulty: ", round(IRT_binary_summary_wrightmap_args()[[2]], 3)
    )))) %>%
    ggplotly(tooltip = "text") %>%
    style(textposition = "right") %>%
    layout(yaxis = list(side = "right"))

  # merge into one output plotly plot
  subplot(plt_left, plt_right, titleY = TRUE, margin = 0) %>%
    plotly::config(displayModeBar = FALSE)
})

# ** Download Wright map ####
output$IRT_binary_summary_wrightmap_download <- downloadHandler(
  filename = function() {
    "fig_IRT_binary_WrightMap.png"
  },
  content = function(file) {
    ggsave(file,
      plot = do.call(ggWrightMap, IRT_binary_summary_wrightmap_args()), # apply fun args from list
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ####

# ** Model description ####
output$IRT_binary_items_model_description <- renderText({
  IRT_binary_summary_model_description()
})

# ** ICC equation ####
output$IRT_binary_items_icc_equation <- renderUI({
  withMathJax(IRT_binary_summary_icc_equation())
})

# ** IIC equation ####
output$IRT_binary_items_iic_equation <- renderUI({
  withMathJax(IRT_binary_summary_iic_equation())
})

# ** Equation interpretation ####
output$IRT_binary_items_equation_interpretation <- renderUI({
  withMathJax(IRT_binary_summary_equation_interpretation())
})

# ** Check whether model converged ####
output$IRT_binary_items_model_converged <- renderUI({
  fit <- IRT_binary_model()
  txt <- ifelse(fit@OptimInfo$converged,
                "",
                "<font color = 'orange'> Estimation process terminated without convergence.
    Estimates are not reliable. Try to increase a number of iterations of the EM
    algorithm in Settings. </font>"
  )
  HTML(txt)
})

# ** Plot of ICC for selected item ####
IRT_binary_items_icc <- reactive({
  item <- input$IRT_binary_items
  fit <- IRT_binary_model()
  n_items <- fit@Data$nitems
  curve_col <- gg_color_hue(n_items)[item]

  d <- tibble(
    Ability = IRT_thetas_for_plots(), # vector only
    Probability = probtrace(extract.item(fit, item),
                            IRT_thetas_for_plots())[, 2] # ascending probs
  )

  g <- d %>% ggplot(aes(x = Ability, y = Probability)) +
    geom_line(color = curve_col) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[item]) +
    theme_app()
  g
})

output$IRT_binary_items_icc <- renderPlotly({
  g <- IRT_binary_items_icc()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of ICC for selected item ####
output$IRT_binary_items_icc_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_binary_items
    paste0("fig_IRT_binary_ICC_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_binary_items_icc() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of IIC for selected item ####
IRT_binary_items_iic <- reactive({
  item <- input$IRT_binary_items
  fit <- IRT_binary_model()

  n_items <- fit@Data$nitems
  curve_col <- gg_color_hue(n_items)[item]

  d <- tibble(
    Ability = IRT_thetas_for_plots(), # vector only
    Information = iteminfo(extract.item(fit, item),
                           IRT_thetas_for_plots()) # ascending probs
  )

  g <- d %>% ggplot(aes(x = Ability, y = Information)) +
    geom_line(color = curve_col) +
    ggtitle(item_names()[item]) +
    theme_app()
  g
})

output$IRT_binary_items_iic <- renderPlotly({
  g <- IRT_binary_items_iic()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of IIC for selected item ####
output$IRT_binary_items_iic_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_binary_items
    paste0("fig_IRT_binary_IIC_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_binary_items_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of parameters for selected item ####
IRT_binary_items_coef <- reactive({
  item <- input$IRT_binary_items
  tab <- IRT_binary_summary_coef()[item, ]

  tab
})

output$IRT_binary_items_coef <- renderTable(
  {
    IRT_binary_items_coef()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * COMPARISON OF DICHOTOMOUS MODELS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ** Check whether model converged ####
output$IRT_binary_comparison_model_converged <- renderUI({
  fit1PL <- IRT_binary_model_1pl()
  fit2PL <- IRT_binary_model_2pl()
  fit3PL <- IRT_binary_model_3pl()
  fit4PL <- IRT_binary_model_4pl()

  txt1 <- ifelse(fit1PL@OptimInfo$converged,
    "",
    "Estimation process in the <b>1PL IRT model</b> terminated without convergence. <br>"
  )
  txt2 <- ifelse(fit2PL@OptimInfo$converged,
    "",
    "Estimation process in the <b>2PL IRT model</b> terminated without convergence. <br>"
  )
  txt3 <- ifelse(fit3PL@OptimInfo$converged,
    "",
    "Estimation process in the <b>3PL IRT model</b> terminated without convergence. <br>"
  )
  txt4 <- ifelse(fit4PL@OptimInfo$converged,
    "",
    "Estimation process in the <b>4PL IRT model</b> terminated without convergence. <br>"
  )
  txt <- paste0(txt1, txt2, txt3, txt4)
  if (txt != "") {
    txt <- paste0(
    "<font color = 'orange'>", txt, "Estimates are not reliable. Try to increase
    a number of iterations of the EM algorithm in Settings. </font>" )
  }
  HTML(txt)
})

# ** Comparison of binary models ####
IRT_binary_comparison <- reactive({
  fit1PL <- IRT_binary_model_1pl()
  fit2PL <- IRT_binary_model_2pl()
  fit3PL <- IRT_binary_model_3pl()
  fit4PL <- IRT_binary_model_4pl()

  df <- rbind(
    anova(fit1PL, fit2PL, verbose = FALSE),
    anova(fit2PL, fit3PL, verbose = FALSE),
    anova(fit3PL, fit4PL, verbose = FALSE)
  )

  df <- round(df[c(1, 2, 4, 6), ], 3)
  df <- df[, c("AIC", "AICc", "BIC", "SABIC", "logLik")]
  nam <- c("1PL", "2PL", "3PL", "4PL")

  df <- rbind(
    df,
    c(nam[sapply(1:4, function(i) which(df[, i] == min(df[, i], na.rm = TRUE)))], "")
  )

  rownames(df) <- c(nam, "BEST")
  df
})

output$IRT_binary_comparison <- renderTable(
  {
    IRT_binary_comparison()
  },
  include.rownames = TRUE
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BOCK'S NOMINAL MODEL ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Modification of dataset for Bock's model ####
IRT_bock_data <- reactive({
  # this function change nominal values to numbers for nominal dataset and key
  data <- nominal()
  key <- as.factor(key())

  m <- ncol(data)
  levels_data_original <- lapply(1:m, function(i) levels(factor(unlist(data[, i, with = FALSE]))))
  lev <- c(unlist(levels_data_original), levels(key)) # all levels in data and key
  lev <- unique(lev) # all unique levels
  lev_num <- as.numeric(as.factor(lev)) - 1 # change them to numbers

  # new numeric levels for key
  levels_key_num <- sapply(
    1:length(levels(key)),
    function(i) lev_num[levels(key)[i] == lev]
  )
  # new numeric levels for dataset
  levels_data_num <- lapply(1:m, function(i) {
    sapply(
      1:length(levels(factor(unlist(data[, i, with = FALSE])))),
      function(j) lev_num[levels(factor(unlist(data[, i, with = FALSE])))[j] == lev]
    )
  })

  # creating new numeric key
  key_num <- key
  levels(key_num) <- levels_key_num
  key_num <- as.numeric(paste(key_num))

  # creating new numeric dataset
  data_num <- data.frame(data)
  for (i in 1:m) {
    levels(data_num[, i]) <- levels_data_num[[i]]
    data_num[, i] <- as.numeric(paste(data_num[, i]))
  }

  list(
    data = data.table(data_num),
    key = key_num,
    levels_data_numeric = levels_data_num,
    levels_data_original = levels_data_original,
    levels_original = lev
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** UPDATING INPUTS ####

IRT_bock <- reactiveValues(
  parametrization = NULL
)

# ** Updating parametrization ####
observeEvent(input$IRT_bock_summary_parametrization, {
  IRT_bock$parametrization <- input$IRT_bock_summary_parametrization
})
observeEvent(input$IRT_bock_items_parametrization, {
  IRT_bock$parametrization <- input$IRT_bock_items_parametrization
})
observeEvent(IRT_bock$parametrization, {
  if (IRT_bock$parametrization != input$IRT_bock_summary_parametrization) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "IRT_bock_summary_parametrization",
      selected = IRT_bock$parametrization
    )
  }
  if (IRT_bock$parametrization != input$IRT_bock_items_parametrization) {
    updateCheckboxGroupInput(
      session = session,
      inputId = "IRT_bock_items_parametrization",
      selected = IRT_bock$parametrization
    )
  }
})

# ** Updating item slider ####
observe({
  item_count <- ncol(nominal())
  updateSliderInput(
    session = session,
    inputId = "IRT_bock_items",
    max = item_count
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** MODEL ####

# ** Model ####
IRT_bock_model <- reactive({
  data <- IRT_bock_data()$data
  key <- IRT_bock_data()$key
  m <- ncol(data)

  # starting values
  sv <- mirt(data, 1, "nominal", pars = "values", verbose = FALSE, SE = TRUE)

  # starting values of discrimination for distractors need to be lower than
  # for the correct answer (fixed at 0, see below)
  sv$value[grepl("ak", sv$name)] <- -0.5
  sv$est[grepl("ak", sv$name)] <- TRUE

  # we don't want to estimate ak parameter for the correct answer
  # they are fixed at 0, the same for parameters d
  for (i in 1:m) {
    item_name <- gsub(" ", "\\.", item_names()[i])
    tmp <- sv[sv$item == item_name, ]
    tmp$est <- TRUE
    tmp[tmp$name == paste0("ak", key[i]), "value"] <- 0
    tmp[tmp$name == paste0("ak", key[i]), "est"] <- FALSE
    tmp[tmp$name == paste0("d", key[i]), "value"] <- 0
    tmp[tmp$name == paste0("d", key[i]), "est"] <- FALSE
    sv[sv$item == item_name, ] <- tmp
  }

  # we don't want to estimate a1 parameter for any item as it multiplies all
  # category specific slopes
  sv[sv$name == "a1", "value"] <- 1
  sv[sv$name == "a1", "est"] <- FALSE

  # Old specification of starting values, not clear why defined as this:
  # # starting values
  # sv <- mirt(data, 1, "nominal", pars = "values", verbose = FALSE, SE = TRUE)
  #
  # # set all values to 0 and estimated
  # sv$value[grepl("ak", sv$name)] <- 0
  # sv$est[grepl("ak", sv$name)] <- TRUE
  #
  # nms <- colnames(data)
  # for (i in 1:length(nms)) {
  #
  #   # set the highest category based on key fixed to 3
  #   pick <- paste0("ak", key[i] - 1)
  #   index <- sv$item == nms[i] & pick == sv$name
  #   sv[index, "value"] <- 3
  #   sv[index, "est"] <- FALSE
  #
  #   # set arbitrary lowest category fixed at 0
  #   if (pick == "ak0") {
  #     pick2 <- "ak3"
  #   } else {
  #     pick2 <- paste0("ak", key[i] - 2)
  #   }
  #   index2 <- sv$item == nms[i] & pick2 == sv$name
  #   sv[index2, "est"] <- FALSE
  # }

  fit <- mirt(data, model = 1, itemtype = "nominal", pars = sv, SE = TRUE, verbose = FALSE)
  fit
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** SUMMARY ####

# ** ICC equation ####
IRT_bock_summary_icc_equation <- reactive({
  if (input$IRT_bock_summary_parametrization == "irt") {
    txt <- "$$\\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{a_{ik} (\\theta_p - b_{ik})}}{\\sum_l^{K_i} e^{e^{a_{il} (\\theta_p - b_{il})}}}$$"
  } else {
    txt <- "$$\\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{\\beta_{0ik} + \\beta_{1ik}\\theta_p}}{\\sum_l^{K_i} e^{\\beta_{0il} + \\beta_{1il}\\theta_p}}$$"
  }
  txt
})

output$IRT_bock_summary_icc_equation <- renderUI({
  withMathJax(IRT_bock_summary_icc_equation())
})

# ** Plot of ICC ####
IRT_bock_summary_icc <- reactive({
  fit <- IRT_bock_model()
  levels_data_original <- IRT_bock_data()$levels_data_original

  # names from the model
  mod_item_names <- fit@Data$data %>% colnames()
  m <- length(mod_item_names)

  d <- lapply(1:m, function(i) map2_dfr(
    mod_item_names[i],
    item_names()[i], # names from user
    ~ tibble(
      Ability = IRT_thetas_for_plots(), # vector only
      Probability = probtrace(
        extract.item(fit, .x),
        IRT_thetas_for_plots()), # ascending probs
      Item = .y,
    )
  ))
  d <- lapply(1:m, function(i)
    data.frame(d[[i]]$Ability, data.frame(d[[i]]$Probability), d[[i]]$Item)
  )

  d <- lapply(1:m, function(i) {
    tmp <- gather(
      d[[i]],
      key = "Option", value = "Probability",
      paste0("P.", 1:(ncol(d[[i]]) - 2))
    )
    tmp$Option <- as.factor(tmp$Option)
    levels(tmp$Option) <- levels_data_original[[i]]
    tmp
  }
  )

  d <- do.call(rbind, d)
  colnames(d) <- c("Ability", "Item", "Option", "Probability")
  d$Item <- factor(d$Item, levels = item_names())

  g <- d %>%
    ggplot(aes(x = Ability, y = Probability, color = Item,
               linetype = Option)) +
    geom_line() +
    ylab("Probability of correct answer") +
    theme_app()
  g
})

output$IRT_bock_summary_icc <- renderPlotly({
  g <- IRT_bock_summary_icc()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of ICC ####
output$IRT_bock_summary_icc_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_ICC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_summary_icc() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines"),
          legend.box = "horizontal", legend.spacing.x = unit(0, "cm")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of IIC ####
IRT_bock_summary_iic <- reactive({
  fit <- IRT_bock_model()

  # names from model
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
  g
})

output$IRT_bock_summary_iic <- renderPlotly({
  g <- IRT_bock_summary_iic()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of IIC ####
output$IRT_bock_summary_iic_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_IIC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_summary_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of TIC ####
IRT_bock_summary_tic <- reactive({
  fit <- IRT_bock_model()

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
  g
})

output$IRT_bock_summary_tic <- renderPlotly({
  g <- IRT_bock_summary_tic()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of TIC ####
output$IRT_bock_summary_tic_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_TIC.png"
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_summary_tic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of parameters ####
IRT_bock_summary_coef <- reactive({
  fit <- IRT_bock_model()
  levels_original <- IRT_bock_data()$levels_original
  m <- ncol(nominal())

  IRTpars <- input$IRT_bock_summary_parametrization == "irt"

  # estimated parameters with SE
  par_list <- coef(fit, IRTpars = IRTpars, printSE = TRUE)[1:20]
  max_par <- max((sapply(par_list, ncol) - 1) / 2) # max number of parameters

  # extracting estimated parameters
  par_tab <- as.data.frame(rbindlist(
    lapply(par_list, function(x) data.table(t(x[1, ]))), fill = TRUE
  ))
  # extracting SE
  if (dim(fit@vcov)[1] > 1) {
    se_tab <- as.data.frame(rbindlist(
      lapply(par_list, function(x) data.table(t(x[2, ]))), fill = TRUE
    ))
  } else {
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  }

  # ordering of parameters
  par_tab <- cbind(par_tab[, paste0("ak", 0:(max_par - 1))],
                   par_tab[, paste0("d", 0:(max_par - 1))])
  se_tab <- cbind(se_tab[, paste0("ak", 0:(max_par - 1))],
                  se_tab[, paste0("d", 0:(max_par - 1))])
  # merging tables
  tab <- data.frame(par_tab, se_tab)
  tab <- cbind(par_tab, se_tab)[, order(c(1:ncol(par_tab), 1:ncol(se_tab)))]

  # fit statistics
  tab_fit <- itemfit(fit)[, c(2, 3, 5)]
  if (!is.null(tryCatch(round(tab_fit, 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")
  }))) {
    tab <- data.frame(tab, tab_fit)
    colnames(tab)[(ncol(tab) - 2):ncol(tab)] <- c("SX2-value", "df", "p-value")
  } else {
    tab <- data.frame(tab, cbind("-", "-", "-"))
    colnames(tab)[(ncol(tab) - 2):ncol(tab)] <- c("SX2-value", "df", "p-value")
  }

  # renaming columns
  if (IRTpars) {
    colnames(tab)[1:(max_par * 4)] <- paste0(
      c("", "SE("),
      paste0("%%mathit{", rep(paste0(rep(c("a", "c"), each = max_par), "_", 1:max_par), each = 2), "}%%"),
      c("", ")")
    )
  } else {
    # removing not-estimated parameter a1
    colnames(tab)[1:(max_par * 4)] <- paste0(
      c("", "SE("),
      paste0("%%mathit{", rep(paste0(rep(c("\\beta_{0", "\\beta_{1"), each = max_par),
                                     levels_original, "}"), each = 2), "}%%"),
      c("", ")")
    )
  }

  rownames(tab) <- item_names()
  tab
})

output$IRT_bock_summary_coef <- renderTable(
  {
    IRT_bock_summary_coef()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Ability estimates ####
IRT_bock_summary_ability <- reactive({
  fit <- IRT_bock_model()

  score <- as.vector(total_score())
  zscore <- as.vector(z_score())
  fscore <- fscores(fit, full.scores.SE = TRUE)

  tab <- data.frame(score, zscore, fscore)
  colnames(tab) <- c("Total score", "Z-score", "F-score", "SE(F-score)")
  rownames(tab) <- paste("Respondent", 1:nrow(tab))

  tab
})

output$IRT_bock_summary_ability <- renderTable(
  {
    factors <- IRT_bock_summary_ability()
    head(factors, n = 6)
  },
  include.rownames = TRUE
)

# ** Download of ability estimates ####
output$IRT_bock_summary_ability_download <- downloadHandler(
  filename = function() {
    "IRT_bock_abilities.csv"
  },
  content = function(file) {
    write.csv(IRT_bock_summary_ability(), file)
  }
)

# ** Ability estimates correlation ####
IRT_bock_summary_ability_correlation <- reactive({
  fit <- IRT_bock_model()
  fscore <- as.vector(fscores(fit))
  zscore <- z_score()

  cor <- cor(fscore, zscore, use = "pairwise.complete.obs")
  cor
})

output$IRT_bock_summary_ability_correlation_text <- renderText({
  paste0("This scatterplot shows the relationship between the standardized total
         score (Z-score) and the factor score estimated by the IRT model. The
         Pearson correlation coefficient between these two scores is ",
         sprintf("%.3f", IRT_bock_summary_ability_correlation()), ". ")
})

# ** Ability estimates plot ####
IRT_bock_summary_ability_plot <- reactive({
  fit <- IRT_bock_model()
  fscore <- as.vector(fscores(fit))
  zscore <- z_score()

  df <- data.frame(fscore, zscore)

  g <- ggplot(df, aes_string("zscore", "fscore")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
  g
})

output$IRT_bock_summary_ability_plot <- renderPlotly({
  g <- IRT_bock_summary_ability_plot()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("zscore", "Z-score", p$x$data[[1]]$text)
  p$x$data[[1]]$text <- gsub("fscore", "F-score", p$x$data[[1]]$text)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# *** Download factor scores plot ####
output$IRT_bock_summary_ability_plot_download <- downloadHandler(
  filename = function() {
    "fig_IRT_bock_abilities.png"
  },
  content = function(file) {
    ggsave(file,
           plot = IRT_bock_summary_ability_plot() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi
    )
  }
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** ITEMS ####

# ** ICC equation ####
output$IRT_bock_items_icc_equation <- renderUI({
  withMathJax(IRT_bock_summary_icc_equation())
})


# ** Plot of ICC for selected item ####
IRT_bock_items_icc <- reactive({
  item <- input$IRT_bock_items
  levels_data_original <- IRT_bock_data()$levels_data_original[[item]]

  fit <- IRT_bock_model()
  m <- fit@Data$nitems

  curve_col <- gg_color_hue(m)[item]

  d <- tibble(
    Ability = IRT_thetas_for_plots(), # vector only
    Probability = probtrace(extract.item(fit, item),
                            IRT_thetas_for_plots()) # ascending probs
  )
  d <- data.frame(d$Ability, data.frame(d$Probability))
  colnames(d) <- c("Ability", paste0("Probability", 1:(ncol(d) - 1)))

  d <- gather(d, key = "Option", value = "Probability",
              paste0("Probability", 1:(ncol(d) - 1)))
  d$Option <- as.factor(d$Option)
  levels(d$Option) <- levels_data_original
  g <- d %>% ggplot(aes(x = Ability, y = Probability, linetype = Option)) +
    geom_line(color = curve_col) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[item]) +
    theme_app()
  g
})

output$IRT_bock_items_icc <- renderPlotly({
  g <- IRT_bock_items_icc()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of ICC for selected item ####
output$IRT_bock_items_icc_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_bock_items
    paste0("fig_IRT_bock_ICC, ", item_name()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_bock_items_icc() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot of IIC for selected item ####
IRT_bock_items_iic <- reactive({
  item <- input$IRT_bock_items
  fit <- IRT_bock_model()

  n_items <- fit@Data$nitems
  curve_col <- gg_color_hue(n_items)[item]

  d <- tibble(
    Ability = IRT_thetas_for_plots(), # vector only
    Information = iteminfo(extract.item(fit, item),
                           IRT_thetas_for_plots()) # ascending probs
  )

  g <- d %>% ggplot(aes(x = Ability, y = Information)) +
    geom_line(color = curve_col) +
    ggtitle(item_names()[item]) +
    theme_app()
  g
})

output$IRT_bock_items_iic <- renderPlotly({
  g <- IRT_bock_items_iic()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of IIC for selected item ####
output$IRT_bock_items_iic_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_bock_items
    paste0("fig_IRT_bock_IIC_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
           plot = IRT_bock_items_iic() +
             theme(
               text = element_text(size = setting_figures$text_size),
               legend.position = "right", legend.key.size = unit(0.8, "lines")
             ),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi
    )
  }
)

# ** Table of parameters for selected item ####
IRT_bock_items_coef <- reactive({
  item <- input$IRT_bock_items

  tab <- IRT_bock_summary_coef()[item, ]
  tab
})

output$IRT_bock_items_coef <- renderTable(
  {
    IRT_bock_items_coef()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** DICHOTOMOUS MODELS ####

# *** Interpretation ####
output$ccIRT_interpretation <- renderUI({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta0 <- input$ccIRTSlider_theta

  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }
  iicirt <- function(theta, a, b, c, d) {
    pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
    return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
  }

  ICC1 <- ccirt(theta0, a1, b1, c1, d1)
  ICC2 <- ccirt(theta0, a2, b2, c2, d2)
  IIC1 <- iicirt(theta0, a1, b1, c1, d1)
  IIC2 <- iicirt(theta0, a2, b2, c2, d2)

  txt1 <- paste0(
    "The probability of the correct answer with the latent ability ",
    withMathJax(paste0("\\(\\theta= ", theta0, "\\)")),
    " in the <font color='red'>red</font> item with parameters ",
    withMathJax(paste0("\\(a = ", a1, "\\)")), ", ",
    withMathJax(paste0("\\(b = ", b1, "\\)")), ", ",
    withMathJax(paste0("\\(c = ", c1, "\\)")), ", and ",
    withMathJax(paste0("\\(d = ", d1, "\\)")),
    " is equal to <b>", sprintf("%.2f", ICC1), "</b>. "
  )
  txt2 <- paste0(
    "The probability of the correct answer with the latent ability ",
    withMathJax(paste0("\\(\\theta= ", theta0, "\\)")),
    " in the <font color='blue'>blue</font> item with parameters ",
    withMathJax(paste0("\\(a = ", a2, "\\)")), ", ",
    withMathJax(paste0("\\(b = ", b2, "\\)")), ", ",
    withMathJax(paste0("\\(c = ", c2, "\\)")), ", and ",
    withMathJax(paste0("\\(d = ", d2, "\\)")),
    " is equal to <b>", sprintf("%.2f", ICC2), "</b>. "
  )
  txt3 <- paste0(
    "The information for the latent ability ",
    withMathJax(paste0("\\(\\theta= ", theta0, "\\)")),
    " in the <font color='red'>red</font> item ",
    " is equal to <b>", sprintf("%.2f", IIC1), "</b>. "
  )
  txt4 <- paste0(
    "The information for the latent ability ",
    withMathJax(paste0("\\(\\theta= ", theta0, "\\)")),
    " in the <font color='blue'>blue</font> item ",
    " is equal to <b>", sprintf("%.2f", IIC2), "</b>. "
  )
  txt <- paste0("<b>Interpretation: </b>", txt1, txt3, txt2, txt4)
  HTML(txt)
})

# *** ICC ####
ccIRT_plot_Input <- reactive({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta0 <- input$ccIRTSlider_theta

  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }


  df <- data.frame(
    X1 = ccirt(seq(-4, 4, 0.01), a1, b1, c1, d1),
    X2 = ccirt(seq(-4, 4, 0.01), a2, b2, c2, d2),
    theta = seq(-4, 4, 0.01)
  )
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))

  ICC1 <- ccirt(theta0, a = a1, b = b1, c = c1, d = d1)
  ICC2 <- ccirt(theta0, a = a2, b = b2, c = c2, d = d2)
  ICC <- max(ICC1, ICC2)

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    geom_segment(aes(
      y = ICC1, yend = ICC1,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = ICC2, yend = ICC2,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = 0, yend = ICC,
      x = theta0, xend = theta0
    ), color = "gray", linetype = "dashed") +
    xlim(-4, 4) +
    xlab("Ability") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    scale_color_manual(
      name = "",
      values = c("red", "blue"),
      labels = c(
        paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
          collapse = ", "
        ),
        paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
          collapse = ", "
        )
      )
    ) +
    theme_app() +
    ggtitle("Item characteristic curve")
  g
})

output$ccIRT_plot <- renderPlotly({
  g <- ccIRT_plot_Input()

  p <- ggplotly(g)
  theta0 <- input$ccIRTSlider_theta

  # item 1, probabilities
  text <- gsub("~", "", p$x$data[[1]]$text)
  text <- gsub("value", "Probability", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X1", "", text)
  p$x$data[[1]]$text <- text

  # item 2, probabilities
  text <- gsub("~", "", p$x$data[[2]]$text)
  text <- gsub("value", "Probability", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X2", "", text)
  p$x$data[[2]]$text <- text

  # item 1 and selected theta
  text <- gsub("~", "", p$x$data[[3]]$text)
  text <- gsub("ICC1", "Probability", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[3]]$text <- text

  text <- gsub("~", "", p$x$data[[4]]$text)
  text <- gsub("ICC2", "Probability", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Probability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[4]]$text <- text

  text <- gsub("~", "", p$x$data[[5]]$text)
  text <- gsub("ICC", "Probability", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("<br />0: 0", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  text <- gsub(paste0("<br />y: ", theta0), "", text)
  pos <- gregexpr("Ability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[5]]$text <- text

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_ccIRT <- downloadHandler(
  filename = function() {
    paste("fig_CustomItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = ccIRT_plot_Input() +
        theme(
          legend.position = c(0.97, 0.03),
          legend.justification = c(0.97, 0.03)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** IIC ####
iicIRT_plot_Input <- reactive({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta0 <- input$ccIRTSlider_theta

  iicirt <- function(theta, a, b, c, d) {
    pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
    return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
  }

  df <- data.frame(
    X1 = iicirt(seq(-4, 4, 0.01), a1, b1, c1, d1),
    X2 = iicirt(seq(-4, 4, 0.01), a2, b2, c2, d2),
    theta = seq(-4, 4, 0.01)
  )
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))

  IIC1 <- iicirt(theta0, a = a1, b = b1, c = c1, d = d1)
  IIC2 <- iicirt(theta0, a = a2, b = b2, c = c2, d = d2)
  IIC <- max(IIC1, IIC2)

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    geom_segment(aes(
      y = IIC1, yend = IIC1,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = IIC2, yend = IIC2,
      x = -4, xend = theta0
    ), color = "gray", linetype = "dashed") +
    geom_segment(aes(
      y = 0, yend = IIC,
      x = theta0, xend = theta0
    ), color = "gray", linetype = "dashed") +
    xlim(-4, 4) +
    ylim(0, 4) +
    xlab("Ability") +
    ylab("Information") +
    scale_color_manual(
      name = "",
      breaks = c("X1", "X2"),
      values = c("red", "blue"),
      labels = c(
        paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
          collapse = ", "
        ),
        paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
          collapse = ", "
        )
      )
    ) +
    theme_app() +
    ggtitle("Item information function")
  g
})

output$iicIRT_plot <- renderPlotly({
  g <- iicIRT_plot_Input()

  p <- ggplotly(g)
  theta0 <- input$ccIRTSlider_theta

  text <- gsub("~", "", p$x$data[[1]]$text)
  text <- gsub("value", "Information", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X1", "", text)
  p$x$data[[1]]$text <- text

  text <- gsub("~", "", p$x$data[[2]]$text)
  text <- gsub("value", "Information", text)
  text <- gsub("theta", "Ability", text)
  text <- gsub("variable: X2", "", text)
  p$x$data[[2]]$text <- text

  # item 1 and selected theta
  text <- gsub("~", "", p$x$data[[3]]$text)
  text <- gsub("IIC1", "Information", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Information", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[3]]$text <- text

  text <- gsub("~", "", p$x$data[[4]]$text)
  text <- gsub("IIC2", "Information", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Information", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr("Information", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[4]]$text <- text

  text <- gsub("~", "", p$x$data[[5]]$text)
  text <- gsub("IIC", "Information", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("<br />0: 0", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("value", "Information", text)
  text <- gsub("<br />variable: gray", "", text)
  text <- gsub(paste0("<br />y: ", theta0), "", text)
  pos <- gregexpr("Ability", text)[[1]][2]
  text <- substring(text, 1, pos - 1)
  p$x$data[[5]]$text <- text

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_iicIRT <- downloadHandler(
  filename = function() {
    paste("fig_CustomItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = iicIRT_plot_Input() +
        theme(
          legend.position = c(0.97, 0.97),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)
# *** EXERCISES ####
# **** Exercises 1 ####
irt_dich1_answers <- reactive({
  ccirt <- function(theta, a, b, c, d) {
    return(c + (d - c) / (1 + exp(-a * (theta - b))))
  }
  iicirt <- function(theta, a, b, c, d) {
    pi <- c + (d - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
    return(a^2 * (pi - c)^2 * (d - pi)^2 / (pi * (1 - pi) * (d - c)^2))
  }

  a1 <- 2.5
  b1 <- -0.5
  c1 <- 0
  d1 <- 1
  a2 <- 1.5
  b2 <- 0
  c2 <- 0
  d2 <- 1

  par1 <- c(a1, b1, c1, d1)
  par2 <- c(a2, b2, c2, d2)

  theta0 <- c(-2, -1, 0, 1, 2)

  cci1 <- ccirt(theta0, a1, b1, c1, d1)
  cci2 <- ccirt(theta0, a2, b2, c2, d2)

  theta <- (a1 * b1 - a2 * b2) / (a1 - a2)

  iicirt1a <- iicirt(-2, a1, b1, c1, d1)
  iicirt2a <- iicirt(-2, a2, b2, c2, d2)
  iica <- as.numeric(iicirt1a < iicirt2a) + 1
  iicirt1b <- iicirt(0, a1, b1, c1, d1)
  iicirt2b <- iicirt(0, a2, b2, c2, d2)
  iicb <- as.numeric(iicirt1b < iicirt2b) + 1
  iicirt1c <- iicirt(2, a1, b1, c1, d1)
  iicirt2c <- iicirt(2, a2, b2, c2, d2)
  iicc <- as.numeric(iicirt1c < iicirt2c) + 1

  answers <- list(
    par1 = par1,
    par2 = par2,
    cci1 = cci1,
    cci2 = cci2,
    theta = theta,
    iic = c(iica, iicb, iicc)
  )
  answers
})

irt_training_dich1_check <- eventReactive(input$irt_training_dich1_submit, {
  answers <- irt_dich1_answers()

  # answer 1
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  par1 <- answers[[1]]
  par2 <- answers[[2]]

  par1input <- c(a1, b1, c1, d1)
  par2input <- c(a2, b2, c2, d2)

  ans1 <- c(all(abs(par1 - par1input) <= 0.05) & all(abs(par2 - par2input) <= 0.05))

  # answers 2, item 1
  cci1 <- answers[[3]]
  cci1input <- c(
    input$irt_training_dich1_1_2a, input$irt_training_dich1_1_2b, input$irt_training_dich1_1_2c,
    input$irt_training_dich1_1_2d, input$irt_training_dich1_1_2e
  )
  ans2_1 <- c(abs(cci1 - cci1input) <= 0.05)
  # answers 2, item 1
  cci2 <- answers[[4]]
  cci2input <- c(
    input$irt_training_dich1_2_2a, input$irt_training_dich1_2_2b, input$irt_training_dich1_2_2c,
    input$irt_training_dich1_2_2d, input$irt_training_dich1_2_2e
  )
  ans2_2 <- c(abs(cci2 - cci2input) <= 0.05)

  # answer 3
  ans3 <- c(abs(answers[[5]] - input$irt_training_dich1_3) <= 0.05)

  # answer 4
  ans4 <- c(answers[["iic"]] == c(input$irt_training_dich1_4a, input$irt_training_dich1_4b, input$irt_training_dich1_4c))


  ans <- list(
    ans1 = ans1,
    ans2_1 = ans2_1,
    ans2_2 = ans2_2,
    ans3 = ans3,
    ans4 = ans4
  )
  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_dich1_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans1"]])
})

output$irt_training_dich1_2a_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][1])
})

output$irt_training_dich1_2b_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][2])
})

output$irt_training_dich1_2c_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][3])
})

output$irt_training_dich1_2d_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][4])
})

output$irt_training_dich1_2e_1_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_1"]][5])
})

output$irt_training_dich1_2a_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][1])
})

output$irt_training_dich1_2b_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][2])
})

output$irt_training_dich1_2c_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][3])
})

output$irt_training_dich1_2d_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][4])
})

output$irt_training_dich1_2e_2_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans2_2"]][5])
})

output$irt_training_dich1_3_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans3"]])
})

output$irt_training_dich1_4a_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans4"]][1])
})

output$irt_training_dich1_4b_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans4"]][2])
})

output$irt_training_dich1_4c_answer <- renderUI({
  HTML(irt_training_dich1_check()[["ans4"]][3])
})

output$irt_training_dich1_answer <- renderUI({
  res <- irt_training_dich1_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})


# **** Exercises 2 ####
irt_dich2_answers <- reactive({
  a1 <- 1.5
  b1 <- 0
  c1 <- 0
  d1 <- 1
  a2 <- 1.5
  b2 <- 0
  c2 <- 0.2
  d2 <- 1

  ans1 <- c(c1, c2)
  ans2 <- c((1 + c1) / 2, (1 + c2) / 2)
  ans3 <- 1

  answers <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  answers
})

irt_training_dich2_check <- eventReactive(input$irt_training_dich2_submit, {
  answers <- irt_dich2_answers()

  # answer 1
  c1cor <- answers[["ans1"]][1]
  c2cor <- answers[["ans1"]][2]
  c1inp <- input$irt_training_dich2_1a
  c2inp <- input$irt_training_dich2_1b

  ans1 <- c(
    abs(c1cor - c1inp) <= 0.05,
    abs(c2cor - c2inp) <= 0.05
  )

  # answers 2
  p1cor <- answers[["ans2"]][1]
  p2cor <- answers[["ans2"]][2]
  p1inp <- input$irt_training_dich2_2a
  p2inp <- input$irt_training_dich2_2b

  ans2 <- c(
    abs(p1cor - p1inp) <= 0.05,
    abs(p2cor - p2inp) <= 0.05
  )

  # answer 3
  itcor <- answers[["ans3"]]
  itinp <- input$irt_training_dich2_3
  ans3 <- (itcor == itinp)

  ans <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_dich2_1a_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans1"]][1])
})

output$irt_training_dich2_1b_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans1"]][2])
})

output$irt_training_dich2_2a_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans2"]][1])
})

output$irt_training_dich2_2b_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans2"]][2])
})

output$irt_training_dich2_3_answer <- renderUI({
  HTML(irt_training_dich2_check()[["ans3"]])
})

output$irt_training_dich2_answer <- renderUI({
  res <- irt_training_dich2_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})

# **** Exercises 3 ####
irt_dich3_answers <- reactive({
  a1 <- 1.5
  b1 <- 0
  c1 <- 0
  d1 <- 0.9
  a2 <- 1.5
  b2 <- 0
  c2 <- 0
  d2 <- 1

  ans1 <- c(d1, d2)
  ans2 <- c(d1 / 2, d2 / 2)
  ans3 <- 2

  answers <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  answers
})

irt_training_dich3_check <- eventReactive(input$irt_training_dich3_submit, {
  answers <- irt_dich3_answers()

  # answer 1
  d1cor <- answers[["ans1"]][1]
  d2cor <- answers[["ans1"]][2]
  d1inp <- input$irt_training_dich3_1a
  d2inp <- input$irt_training_dich3_1b

  ans1 <- c(
    abs(d1cor - d1inp) <= 0.05,
    abs(d2cor - d2inp) <= 0.05
  )

  # answers 2
  p1cor <- answers[["ans2"]][1]
  p2cor <- answers[["ans2"]][2]
  p1inp <- input$irt_training_dich3_2a
  p2inp <- input$irt_training_dich3_2b

  ans2 <- c(
    abs(p1cor - p1inp) <= 0.05,
    abs(p2cor - p2inp) <= 0.05
  )

  # answer 3
  itcor <- answers[["ans3"]]
  itinp <- input$irt_training_dich3_3

  ans3 <- (itcor == itinp)

  ans <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3
  )
  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_dich3_1a_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans1"]][1])
})

output$irt_training_dich3_1b_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans1"]][2])
})

output$irt_training_dich3_2a_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans2"]][1])
})

output$irt_training_dich3_2b_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans2"]][2])
})

output$irt_training_dich3_3_answer <- renderUI({
  HTML(irt_training_dich3_check()[["ans3"]])
})

output$irt_training_dich3_answer <- renderUI({
  res <- irt_training_dich3_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ** POLYTOMOUS MODELS ####

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *** GRADED RESPONSE MODEL ####

output$irt_training_grm_sliders <- renderUI({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  num <- input$irt_training_grm_numresp

  sliders <- tagList(
    tags$div(
      class = "js-irs-red",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b1",
        label = withMathJax("$b_1$ - difficulty"),
        value = -1.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-yellow",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b2",
        label = withMathJax("$b_2$ - difficulty"),
        value = -1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-green",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b3",
        label = withMathJax("$b_3$ - difficulty"),
        value = -0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-blue",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b4",
        label = withMathJax("$b_4$ - difficulty"),
        value = 0, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-purple",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b5",
        label = withMathJax("$b_5$ - difficulty"),
        value = 0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-orange",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_grm_b6",
        label = withMathJax("$b_6$ - difficulty"),
        value = 1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(2 * num)]

  sliders
})
# *** Cumulative ####
irt_training_grm_plot_cumulative_Input <- reactive({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  input$irt_training_grm_numresp

  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)) {
    b <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
      "2" = b,
      "3" = c(b, input$irt_training_grm_b3),
      "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
      "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
      "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b) {
    return(1 / (1 + exp(-a * (theta - b))))
  }

  df <- data.frame(sapply(1:num, function(i) ccirt(theta, a, b[i])), theta)
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))

  col <- c("red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:num]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Cumulative probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y >= ", 1:length(col), ")")) +
    theme_app() +
    ggtitle("Cumulative probabilities")

  g
})

output$irt_training_grm_plot_cumulative <- renderPlotly({
  g <- irt_training_grm_plot_cumulative_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Cumulative probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i), paste0("P(Y >= ", i, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_grm_plot_cumulative <- downloadHandler(
  filename = function() {
    paste("fig_GRM_cumulative.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_grm_plot_cumulative_Input() +
        theme(
          legend.position = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Category probabilities ####
irt_training_grm_plot_category_Input <- reactive({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp < 6)

  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)) {
    b <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
      "2" = b,
      "3" = c(b, input$irt_training_grm_b3),
      "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
      "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
      "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b) {
    return(1 / (1 + exp(-a * (theta - b))))
  }

  df <- data.frame(X0 = 1, sapply(1:length(b), function(i) ccirt(theta, a, b[i])))
  df <- data.frame(sapply(1:(ncol(df) - 1), function(i) df[, i] - df[, i + 1]),
    X99 = df[, ncol(df)],
    theta
  )
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))

  levels(df$variable) <- paste0("X", 0:(nlevels(df$variable) - 1))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:((length(levels(df$variable)) - 1) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y >= ", 0:(length(col) - 1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_grm_plot_category <- renderPlotly({
  g <- irt_training_grm_plot_category_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i - 1), paste0("P(Y = ", i - 1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_grm_plot_category <- downloadHandler(
  filename = function() {
    paste("fig_GRM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_grm_plot_category_Input() +
        theme(
          legend.position = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Expected item score ####
irt_training_grm_plot_expected_Input <- reactive({
  req(input$irt_training_grm_numresp, input$irt_training_grm_numresp >= 2, input$irt_training_grm_numresp <= 6)

  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)) {
    b <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
      "2" = b,
      "3" = c(b, input$irt_training_grm_b3),
      "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
      "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
      "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b) {
    return(1 / (1 + exp(-a * (theta - b))))
  }

  df <- data.frame(1, sapply(1:length(b), function(i) ccirt(theta, a, b[i])))
  df <- data.frame(
    sapply(1:(ncol(df) - 1), function(i) df[, i] - df[, i + 1]),
    df[, ncol(df)]
  )
  df <- data.frame(exp = as.matrix(df) %*% c(0:(dim(df)[2] - 1)), theta)

  g <- ggplot(data = df, aes(x = theta, y = exp)) +
    geom_line() +
    xlab("Ability") +
    ylab("Expected item score") +
    xlim(-4, 4) +
    ylim(0, num) +
    theme_app() +
    ggtitle("Expected item score")

  g
})

output$irt_training_grm_plot_expected <- renderPlotly({
  g <- irt_training_grm_plot_expected_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("theta", "Ability", text)
    text <- gsub("exp", "Expected score", text)
    text <- paste0(text, "<br />E(Y)")
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_grm_plot_expected <- downloadHandler(
  filename = function() {
    paste("fig_GRM_expected.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_grm_plot_expected_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)
# *** Exercise ###
irt_training_grm_answer <- reactive({
  cdf_k1 <- function(theta, a, b1) {
    return(exp(a * (theta - b1)) / (1 + exp(a * (theta - b1))))
  }
  cdf_k2 <- function(theta, a, b2) {
    return(exp(a * (theta - b2)) / (1 + exp(a * (theta - b2))))
  }
  cdf_k3 <- function(theta, a, b3) {
    return(exp(a * (theta - b3)) / (1 + exp(a * (theta - b3))))
  }

  theta0 <- c(-2, -1, 0, 1, 2)
  a <- 1
  b1 <- -0.5
  b2 <- 1
  b3 <- 1.5

  ck0 <- rep(1, 5)
  ck1 <- cdf_k1(theta0, a, b1)
  ck2 <- cdf_k2(theta0, a, b2)
  ck3 <- cdf_k3(theta0, a, b3)

  prob_k0 <- c(1 - ck1)
  prob_k1 <- c(ck1 - ck2)
  prob_k2 <- c(ck2 - ck3)
  prob_k3 <- as.numeric(apply(as.data.frame(rbind(prob_k0, prob_k1, prob_k2)), 2, function(x) 1 - sum(x)))

  exp_v <- as.numeric(as.matrix(cbind(prob_k0, prob_k1, prob_k2, prob_k3)) %*% 0:3)
  bb <- input$irt_training_grm_numresp

  answers <- list(
    ans1_1 = ck0,
    ans1_2 = ck1,
    ans1_3 = ck2,
    ans1_4 = ck3,
    ans2_1 = prob_k0,
    ans2_2 = prob_k1,
    ans2_3 = prob_k2,
    ans2_4 = prob_k3,
    ans3 = exp_v
  )
  answers
})

irt_training_grm_check <- eventReactive(input$irt_training_grm_1_submit, {
  answers <- irt_training_grm_answer()

  # answ 1_1
  cdf_k1_input <- c(
    input$irt_training_grm_1_1a, input$irt_training_grm_1_1b,
    input$irt_training_grm_1_1c, input$irt_training_grm_1_1d,
    input$irt_training_grm_1_1e
  )
  ans1_1 <- c(abs(answers[[1]] - cdf_k1_input) <= 0.05)


  # answ 1_2
  cdf_k2_input <- c(
    input$irt_training_grm_1_2a, input$irt_training_grm_1_2b,
    input$irt_training_grm_1_2c, input$irt_training_grm_1_2d,
    input$irt_training_grm_1_2e
  )
  ans1_2 <- c(abs(answers[[2]] - cdf_k2_input) <= 0.05)

  # answ 1_3
  cdf_k3_input <- c(
    input$irt_training_grm_1_3a, input$irt_training_grm_1_3b,
    input$irt_training_grm_1_3c, input$irt_training_grm_1_3d,
    input$irt_training_grm_1_3e
  )
  ans1_3 <- c(abs(answers[[3]] - cdf_k3_input) <= 0.05)

  # answ 1_4
  cdf_k4_input <- c(
    input$irt_training_grm_1_4a, input$irt_training_grm_1_4b,
    input$irt_training_grm_1_4c, input$irt_training_grm_1_4d,
    input$irt_training_grm_1_4e
  )
  ans1_4 <- c(abs(answers[[4]] - cdf_k4_input) <= 0.05)

  # answ 2_1
  prob_k0_input <- c(
    input$irt_training_grm_2_1a, input$irt_training_grm_2_1b,
    input$irt_training_grm_2_1c, input$irt_training_grm_2_1d,
    input$irt_training_grm_2_1e
  )
  ans2_1 <- c(abs(answers[[5]] - prob_k0_input) <= 0.05)

  # answ 2_2
  prob_k1_input <- c(
    input$irt_training_grm_2_2a, input$irt_training_grm_2_2b,
    input$irt_training_grm_2_2c, input$irt_training_grm_2_2d,
    input$irt_training_grm_2_2e
  )
  ans2_2 <- c(abs(answers[[6]] - prob_k1_input) <= 0.05)

  # answ 2_3
  prob_k2_input <- c(
    input$irt_training_grm_2_3a, input$irt_training_grm_2_3b,
    input$irt_training_grm_2_3c, input$irt_training_grm_2_3d,
    input$irt_training_grm_2_3e
  )
  ans2_3 <- c(abs(answers[[7]] - prob_k2_input) <= 0.05)

  # answ 2_4
  prob_k3_input <- c(
    input$irt_training_grm_2_4a, input$irt_training_grm_2_4b,
    input$irt_training_grm_2_4c, input$irt_training_grm_2_4d,
    input$irt_training_grm_2_4e
  )
  ans2_4 <- c(abs(answers[[8]] - prob_k3_input) <= 0.05)

  # answ 3
  exp_v_input <- c(
    input$irt_training_grm_3_1a, input$irt_training_grm_3_2a,
    input$irt_training_grm_3_3a, input$irt_training_grm_3_4a,
    input$irt_training_grm_3_5a
  )
  ans3 <- c(abs(answers[[9]] - exp_v_input) <= 0.05)


  ans <- list(
    ans1 = ans1_1,
    ans2 = ans1_2,
    ans3 = ans1_3,
    ans4 = ans1_4,
    ans5 = ans2_1,
    ans6 = ans2_2,
    ans7 = ans2_3,
    ans8 = ans2_4,
    ans9 = ans3
  )

  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_grm_1_1a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][1])
})

output$irt_training_grm_1_1b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][2])
})

output$irt_training_grm_1_1c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][3])
})

output$irt_training_grm_1_1d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][4])
})

output$irt_training_grm_1_1e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans1"]][5])
})

output$irt_training_grm_1_2a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][1])
})

output$irt_training_grm_1_2b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][2])
})

output$irt_training_grm_1_2c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][3])
})

output$irt_training_grm_1_2d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][4])
})

output$irt_training_grm_1_2e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans2"]][5])
})

output$irt_training_grm_1_3a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][1])
})

output$irt_training_grm_1_3b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][2])
})

output$irt_training_grm_1_3c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][3])
})

output$irt_training_grm_1_3d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][4])
})

output$irt_training_grm_1_3e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans3"]][5])
})

output$irt_training_grm_1_4a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][1])
})

output$irt_training_grm_1_4b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][2])
})

output$irt_training_grm_1_4c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][3])
})

output$irt_training_grm_1_4d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][4])
})

output$irt_training_grm_1_4e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans4"]][5])
})

output$irt_training_grm_2_1a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][1])
})

output$irt_training_grm_2_1b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][2])
})

output$irt_training_grm_2_1c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][3])
})

output$irt_training_grm_2_1d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][4])
})

output$irt_training_grm_2_1e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans5"]][5])
})

output$irt_training_grm_2_2a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][1])
})

output$irt_training_grm_2_2b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][2])
})

output$irt_training_grm_2_2c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][3])
})

output$irt_training_grm_2_2d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][4])
})

output$irt_training_grm_2_2e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans6"]][5])
})

output$irt_training_grm_2_3a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][1])
})

output$irt_training_grm_2_3b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][2])
})

output$irt_training_grm_2_3c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][3])
})

output$irt_training_grm_2_3d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][4])
})

output$irt_training_grm_2_3e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans7"]][5])
})

output$irt_training_grm_2_4a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][1])
})

output$irt_training_grm_2_4b_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][2])
})

output$irt_training_grm_2_4c_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][3])
})

output$irt_training_grm_2_4d_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][4])
})

output$irt_training_grm_2_4e_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans8"]][5])
})

output$irt_training_grm_3_1a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][1])
})

output$irt_training_grm_3_2a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][2])
})

output$irt_training_grm_3_3a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][3])
})

output$irt_training_grm_3_4a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][4])
})

output$irt_training_grm_3_5a_answer <- renderUI({
  HTML(irt_training_grm_check()[["ans9"]][5])
})



output$irt_training_grm_answer <- renderUI({
  res <- irt_training_grm_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})


# *** GENERALIZED PARTIAL CREDIT MODEL ####

output$irt_training_gpcm_sliders <- renderUI({
  req(input$irt_training_gpcm_numresp, input$irt_training_gpcm_numresp >= 2, input$irt_training_gpcm_numresp <= 6)

  num <- input$irt_training_gpcm_numresp

  sliders <- tagList(
    tags$div(
      class = "js-irs-red",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d1",
        label = withMathJax("$b_1$ - threshold"),
        value = -1.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-yellow",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d2",
        label = withMathJax("$b_2$ - threshold"),
        value = -1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-green",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d3",
        label = withMathJax("$b_3$ - threshold"),
        value = -0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-blue",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d4",
        label = withMathJax("$b_4$ - threshold"),
        value = 0, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-purple",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d5",
        label = withMathJax("$b_5$ - threshold"),
        value = 0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-orange",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_gpcm_d6",
        label = withMathJax("$b_6$ - threshold"),
        value = 1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(2 * num)]

  sliders
})

# *** Category probabilities ####
irt_training_gpcm_plot_Input <- reactive({
  req(input$irt_training_gpcm_numresp, input$irt_training_gpcm_numresp >= 2, input$irt_training_gpcm_numresp <= 6)

  num <- input$irt_training_gpcm_numresp

  a <- input$irt_training_gpcm_a

  if (is.null(input$irt_training_gpcm_d1)) {
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_gpcm_d1, input$irt_training_gpcm_d2)
    d <- switch(paste(num),
      "2" = d,
      "3" = c(d, input$irt_training_gpcm_d3),
      "4" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4),
      "5" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5),
      "6" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5, input$irt_training_gpcm_d6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d) {
    a * (theta - d)
  }

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))

  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))

  pk <- cbind(0, pk)
  pk <- exp(pk)

  denom <- apply(pk, 1, sum)

  df <- data.frame(apply(pk, 2, function(x) x / denom), theta)
  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:(length(levels(df$variable)) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y = ", 0:(length(col) - 1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_gpcm_plot <- renderPlotly({
  g <- irt_training_gpcm_plot_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i), paste0("P(Y = ", i - 1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_gpcm_plot <- downloadHandler(
  filename = function() {
    paste("fig_GPCM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_gpcm_plot_Input() +
        theme(
          legend.position = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# *** Expected item score ####
irt_training_gpcm_plot_expected_Input <- reactive({
  req(input$irt_training_gpcm_numresp, input$irt_training_gpcm_numresp >= 2, input$irt_training_gpcm_numresp <= 6)

  num <- input$irt_training_gpcm_numresp

  a <- input$irt_training_gpcm_a

  if (is.null(input$irt_training_gpcm_d1)) {
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_gpcm_d1, input$irt_training_gpcm_d2)
    d <- switch(paste(num),
      "2" = d,
      "3" = c(d, input$irt_training_gpcm_d3),
      "4" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4),
      "5" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5),
      "6" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5, input$irt_training_gpcm_d6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d) {
    a * (theta - d)
  }

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))

  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))

  pk <- cbind(0, pk)
  pk <- exp(pk)

  denom <- apply(pk, 1, sum)

  df <- data.frame(apply(pk, 2, function(x) x / denom))
  df <- data.frame(exp = as.matrix(df) %*% c(0:(dim(df)[2] - 1)), theta)

  g <- ggplot(data = df, aes(x = theta, y = exp)) +
    geom_line() +
    xlab("Ability") +
    ylab("Expected item score") +
    xlim(-4, 4) +
    ylim(0, num) +
    theme_app() +
    ggtitle("Expected item score")

  g
})

output$irt_training_gpcm_plot_expected <- renderPlotly({
  g <- irt_training_gpcm_plot_expected_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("theta", "Ability", text)
    text <- gsub("exp", "Expected score", text)
    text <- paste0(text, "<br />E(Y)")
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_gpcm_plot_expected <- downloadHandler(
  filename = function() {
    paste("fig_GPCM_expected.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_gpcm_plot_expected_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# *** Exercise ###
irt_gpcm_answer <- reactive({
  ans1 <- c("No", "No", "No", "Yes", "Yes", "Yes", "No", "No", "No")

  a <- 1
  d <- c(-1, 1)
  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d) {
    a * (theta - d)
  }
  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))
  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))
  pk <- cbind(0, pk)
  pk <- exp(pk)
  denom <- apply(pk, 1, sum)
  df <- apply(pk, 2, function(x) x / denom)

  df1 <- tidyr::pivot_longer(data.frame(df, theta), -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))


  df2 <- data.frame(exp = as.matrix(df) %*% 0:2, theta)

  ans2 <- c(df2$exp[which(theta %in% c(-1.50, 0, 1.50))])
  ans3 <- "Yes"

  a2 <- 2

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a2, d[i]))
  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))
  pk <- cbind(0, pk)
  pk <- exp(pk)
  denom <- apply(pk, 1, sum)
  df <- apply(pk, 2, function(x) x / denom)
  df1 <- tidyr::pivot_longer(data.frame(df, theta), -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))

  df2 <- data.frame(exp = as.matrix(df) %*% 0:2, theta)

  ans4 <- c(df2$exp[which(theta %in% c(-1.50, 0, 1.50))])

  answers <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3,
    ans4 = ans4
  )

  answers
})

irt_gpcm_check <- eventReactive(input$irt_training_gpcm_1_submit, {
  answers <- irt_gpcm_answer()

  # answ 1_1
  idx <- as.integer(input$irt_training_gpcm_1)
  theta_input <- rep("No", 9)
  theta_input[idx] <- "Yes"
  ans1 <- all(theta_input == answers[[1]])

  exp_theta_input_1 <- c(input$irt_training_gpcm_2_1, input$irt_training_gpcm_2_2, input$irt_training_gpcm_2_3)

  ans2 <- c(abs(answers[[2]] - exp_theta_input_1) <= 0.05)

  ans3 <- input$irt_training_gpcm_3 == answers[[3]]

  exp_theta_input_2 <- c(input$irt_training_gpcm_4_1, input$irt_training_gpcm_4_2, input$irt_training_gpcm_4_3)

  ans4 <- c(abs(answers[[4]] - exp_theta_input_2) <= 0.05)

  ans <- list(
    ans1 = ans1,
    ans2 = ans2,
    ans3 = ans3,
    ans4 = ans4
  )

  res <- sum(sapply(ans, sum)) / sum(sapply(ans, length))
  ans <- lapply(ans, function(x) {
    ifelse(is.na(x),
      "<b><font color = 'red'>!</font></b>",
      ifelse(x,
        "<font color='green'>&#10004;</font>",
        "<font color='red'>&#10006;</font>"
      )
    )
  })
  ans[["ans"]] <- res
  ans
})

output$irt_training_gpcm_1_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans1"]][1])
})

output$irt_training_gpcm_2_1_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans2"]][1])
})

output$irt_training_gpcm_2_2_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans2"]][2])
})

output$irt_training_gpcm_2_3_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans2"]][3])
})

output$irt_training_gpcm_3_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans3"]][1])
})

output$irt_training_gpcm_4_1_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans4"]][1])
})

output$irt_training_gpcm_4_2_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans4"]][2])
})

output$irt_training_gpcm_4_3_answer <- renderUI({
  HTML(irt_gpcm_check()[["ans4"]][3])
})


output$irt_training_gpcm_answer <- renderUI({
  res <- irt_gpcm_check()[["ans"]]
  HTML(ifelse(is.na(res),
    "<font color = 'red'>Check the format</font>",
    ifelse(res == 1,
      "<font color='green'>Everything correct! Well done!</font>",
      paste0("<font color='red'>", round(100 * res), "% correct. Try again.</font>")
    )
  ))
})

# *** NOMINAL RESPONSE MODEL ####

output$irt_training_nrm_sliders <- renderUI({
  req(input$irt_training_nrm_numresp, input$irt_training_nrm_numresp >= 2, input$irt_training_nrm_numresp <= 6)

  num <- input$irt_training_nrm_numresp

  sliders <- tagList(
    tags$div(
      class = "js-irs-red",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_a1",
        label = withMathJax("$a_1$ - discrimination"),
        value = 2.5, min = 0, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-red",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_d1",
        label = withMathJax("$b_1$ - threshold"),
        value = -1.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-yellow",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_a2",
        label = withMathJax("$a_2$ - discrimination"),
        value = 2, min = 0, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-yellow",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_d2",
        label = withMathJax("$b_2$ - threshold"),
        value = -1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-green",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_a3",
        label = withMathJax("$a_3$ - discrimination"),
        value = 1, min = 0, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-green",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_d3",
        label = withMathJax("$b_3$ - threshold"),
        value = -0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-blue",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_a4",
        label = withMathJax("$a_4$ - discrimination"),
        value = 1.5, min = 0, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-blue",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_d4",
        label = withMathJax("$b_4$ - threshold"),
        value = 0, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-purple",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_a5",
        label = withMathJax("$a_5$ - discrimination"),
        value = 0.5, min = 0, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-purple",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_d5",
        label = withMathJax("$b_5$ - threshold"),
        value = 0.5, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-orange",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_a6",
        label = withMathJax("$a_6$ - discrimination"),
        value = 1.3, min = 0, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(
      class = "js-irs-orange",
      style = "display: inline-block; vertical-align: middle; width: 18%;",
      sliderInput("irt_training_nrm_d6",
        label = withMathJax("$b_6$ - threshold"),
        value = 1, min = -4, max = 4, step = 0.1
      )
    ),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(4 * num)]
  sliders
})

# *** Category probabilities ####
irt_training_nrm_plot_Input <- reactive({
  req(input$irt_training_nrm_numresp, input$irt_training_nrm_numresp >= 2, input$irt_training_nrm_numresp <= 6)

  num <- input$irt_training_nrm_numresp

  if (is.null(input$irt_training_nrm_a1)) {
    a <- c(2.5, 2, 1, 1.5, 0.5, 1.3)
    a <- a[1:num]
  } else {
    a <- c(input$irt_training_nrm_a1, input$irt_training_nrm_a2)
    a <- switch(paste(num),
      "2" = a,
      "3" = c(a, input$irt_training_nrm_a3),
      "4" = c(a, input$irt_training_nrm_a3, input$irt_training_nrm_a4),
      "5" = c(a, input$irt_training_nrm_a3, input$irt_training_nrm_a4, input$irt_training_nrm_a5),
      "6" = c(a, input$irt_training_nrm_a3, input$irt_training_nrm_a4, input$irt_training_nrm_a5, input$irt_training_nrm_a6)
    )
  }

  if (is.null(input$irt_training_nrm_d1)) {
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_nrm_d1, input$irt_training_nrm_d2)
    d <- switch(paste(num),
      "2" = d,
      "3" = c(d, input$irt_training_nrm_d3),
      "4" = c(d, input$irt_training_nrm_d3, input$irt_training_nrm_d4),
      "5" = c(d, input$irt_training_nrm_d3, input$irt_training_nrm_d4, input$irt_training_nrm_d5),
      "6" = c(d, input$irt_training_nrm_d3, input$irt_training_nrm_d4, input$irt_training_nrm_d5, input$irt_training_nrm_d6)
    )
  }

  theta <- seq(-4, 4, 0.01)

  ccnrm <- function(theta, a, d) {
    exp(a * (theta - d))
  }

  df <- sapply(1:length(d), function(i) ccnrm(theta, a[i], d[i]))
  df <- data.frame(1, df)
  denom <- apply(df, 1, sum)
  df <- apply(df, 2, function(x) x / denom)
  df <- data.frame(df, theta)

  df <- tidyr::pivot_longer(df, -theta, names_to = "variable") %>%
    mutate(variable = as.factor(variable))
  levels(df$variable) <- paste0("X", 0:(length(levels(df$variable)) - 1))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:(length(levels(df$variable)) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y = ", 0:(length(col) - 1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_nrm_plot <- renderPlotly({
  g <- irt_training_nrm_plot_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i - 1), paste0("P(Y = ", i - 1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

output$DB_irt_training_nrm_plot <- downloadHandler(
  filename = function() {
    paste("fig_NRM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = irt_training_nrm_plot_Input() +
        theme(
          legend.position = c(0.97, 0.7),
          legend.justification = c(0.97, 0.97)
        ) +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

observeEvent(!is.na(input$irt_training_grm_numresp) | is.na(input$irt_training_grm_numresp), {
  if (!is.na(input$irt_training_grm_numresp)) {
    if (input$irt_training_grm_numresp < 2) {
      updateNumericInput(session, "irt_training_grm_numresp", value = 2)
    } else if (input$irt_training_grm_numresp > 6) {
      updateNumericInput(session, "irt_training_grm_numresp", value = 6)
    }
  } else if (is.na(input$irt_training_grm_numresp)) {
    updateNumericInput(session, "irt_training_grm_numresp", value = 4)
  }
})

observeEvent(!is.na(input$irt_training_gpcm_numresp) | is.na(input$irt_training_gpcm_numresp), {
  if (!is.na(input$irt_training_gpcm_numresp)) {
    if (input$irt_training_gpcm_numresp < 2) {
      updateNumericInput(session, "irt_training_gpcm_numresp", value = 2)
    } else if (input$irt_training_gpcm_numresp > 6) {
      updateNumericInput(session, "irt_training_gpcm_numresp", value = 6)
    }
  } else if (is.na(input$irt_training_gpcm_numresp)) {
    updateNumericInput(session, "irt_training_gpcm_numresp", value = 4)
  }
})

observeEvent(!is.na(input$irt_training_nrm_numresp) | is.na(input$irt_training_nrm_numresp), {
  if (!is.na(input$irt_training_nrm_numresp)) {
    if (input$irt_training_nrm_numresp < 2) {
      updateNumericInput(session, "irt_training_nrm_numresp", value = 2)
    } else if (input$irt_training_nrm_numresp > 6) {
      updateNumericInput(session, "irt_training_nrm_numresp", value = 6)
    }
  } else if (is.na(input$irt_training_nrm_numresp)) {
    updateNumericInput(session, "irt_training_nrm_numresp", value = 4)
  }
})
