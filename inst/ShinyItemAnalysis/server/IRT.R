# source server logic for polytomous IRT models
source("server/IRT/polytomous.R", local = T, encoding = "UTF-8")
source("server/IRT/training.R", local = T, encoding = "UTF-8")


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
    data,
    model = 1, itemtype = "Rasch",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
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
    data,
    model = model, itemtype = "2PL",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** 2PL model ####
IRT_binary_model_2pl <- reactive({
  data <- binary()
  fit <- mirt(
    data,
    model = 1, itemtype = "2PL",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** 3PL model ####
IRT_binary_model_3pl <- reactive({
  data <- binary()
  fit <- mirt(
    data,
    model = 1, itemtype = "3PL",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** 4PL model ####
IRT_binary_model_4pl <- reactive({
  data <- binary()
  fit <- mirt(
    data,
    model = 1, itemtype = "4PL",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
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
  IRT_binary_summary_icc_equation()
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
  IRT_binary_summary_iic_equation()
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
  IRT_binary_summary_equation_interpretation()
})

# ** Check whether model converged ####
output$IRT_binary_summary_model_converged <- renderUI({
  fit <- IRT_binary_model()
  txt <- ifelse(extract.mirt(fit, "converged"),
    "",
    paste0(
      "<font color = 'orange'> Estimation process terminated without convergence after ",
      extract.mirt(fit, "iterations"), " iterations. Estimates are not reliable.
      Try to increase a number of iterations of the EM algorithm in Settings. </font>"
    )
  )
  HTML(txt)
})

# ** Plot of ICC ####
IRT_binary_summary_icc <- reactive({
  fit <- IRT_binary_model()

  # names from the model
  mod_item_names <- fit@Data$data |> colnames()

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

  g <- d |> ggplot(aes(x = Ability, y = Probability, color = Item)) +
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
  p |> plotly::config(displayModeBar = FALSE)
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
  mod_item_names <- fit@Data$data |> colnames()

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

  g <- d |> ggplot(aes(x = Ability, y = Information, color = Item)) +
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
  p |> plotly::config(displayModeBar = FALSE)
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

  # TODO: use testinfo and 1 / sqrt(info)
  # FIXME: secondary axis in plotly

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
  p |> plotly::config(displayModeBar = FALSE)
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

  item_fit_cols <- c("S_X2", "df.S_X2", "p.S_X2")

  tab_fit <- itemfit(fit, na.rm = TRUE)[, item_fit_cols]

  if (!is.null(tryCatch(round(tab_fit, 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")
  }))) {
    tab <- data.frame(tab, tab_fit)
    colnames(tab)[9:11] <- c("SX2-value", "df", "p-value")
  } else {
    tab <- data.frame(tab, cbind("-", "-", "-"))
    colnames(tab)[9:11] <- c("SX2-value", "df", "p-value")
  }

  if (IRTpars) {
    colnames(tab)[1:8] <- paste0(
      c("", "SE("),
      paste0("\\(\\mathit{", rep(c("a", "b", "c", "d"), each = 2), "}\\)"),
      c("", ")")
    )
  } else {
    colnames(tab)[1:8] <- paste0(
      c("", "SE("),
      paste0("\\(\\mathit{", rep(c("\\beta_{1}", "\\beta_{0}", "c", "d"), each = 2), "}\\)"),
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
    tab <- IRT_binary_summary_coef()

    if (input$IRT_binary_summary_parametrization == "irt") {
      colnames(tab)[1:8] <- paste0(
        c("", "SE("),
        rep(c("a", "b", "c", "d"), each = 2),
        c("", ")")
      )
    } else {
      colnames(tab)[1:8] <- paste0(
        c("", "SE("),
        rep(c("beta_1", "beta_0", "c", "d"), each = 2),
        c("", ")")
      )
    }

    write.csv(tab, file)
  }
)

# ** Ability estimates ####
IRT_binary_summary_ability <- reactive({
  fit <- IRT_binary_model()

  score <- as.vector(total_score())
  zscore <- as.vector(z_score())
  tscore <- as.vector(t_score())
  fscore <- fscores(fit, full.scores.SE = TRUE)

  tab <- data.frame(score, zscore, tscore, fscore)
  colnames(tab) <- c("Total score", "Z-score", "T-score", "F-score", "SE(F-score)")
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
  paste0(
    "This scatterplot shows the relationship between the standardized total
         score (Z-score) and the factor score estimated by the IRT model. The
         Pearson correlation coefficient between these two scores is ",
    sprintf("%.3f", IRT_binary_summary_ability_correlation()), ". "
  )
})

# ** Ability estimates plot ####
IRT_binary_summary_ability_plot <- reactive({
  fit <- IRT_binary_model()
  fscore <- as.vector(fscores(fit))
  zscore <- z_score()

  df <- data.frame(fscore, zscore)

  g <- ggplot(df, aes(.data$zscore, .data$fscore)) +
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
  p |> plotly::config(displayModeBar = FALSE)
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
  item.names <- item_names()

  list(theta = fscore, b = b, item.names = item.names)
})

output$IRT_binary_summary_wrightmap <- renderPlotly({
  # use internal Wright Map fun, returning separate "facets" in a list
  plts <- ShinyItemAnalysis:::gg_wright_internal(
    theta = IRT_binary_summary_wrightmap_args()[[1]],
    b = IRT_binary_summary_wrightmap_args()[[2]],
    item.names = item_names()
  )

  # plotlify "facets", tweaking the presentation and tooltip of item-side "facet"
  plt_left <- plts[[1]] |>
    ggplotly()
  txt <- gsub("count", "Count", plt_left$x$data[[1]]$text)
  txt <- sapply(strsplit(txt, "<br />"), "[", 1)
  thetas <- as.numeric(paste(sapply(strsplit(plt_left$x$data[[1]]$text, "theta: "), "[", 2)))
  binwidth <- c(diff(thetas) / 2)[1]
  txt <- paste0(txt, "<br />", thetas - binwidth, "< Theta <", thetas + binwidth)
  plt_left$x$data[[1]]$text <- txt
  plt_right <- (plts[[2]] +
    suppressWarnings(geom_text(aes(text = paste0(
      "Item: ",
      stringr::str_remove(item, "(\\|\\s)?0*"),
      "\n", "Difficulty: ", round(IRT_binary_summary_wrightmap_args()[[2]], 3)
    ))))) |>
    ggplotly(tooltip = "text") |>
    style(textposition = "right") |>
    layout(yaxis = list(side = "right"))

  # merge into one output plotly plot
  subplot(plt_left, plt_right, titleY = TRUE, margin = 0) |>
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
  IRT_binary_summary_icc_equation()
})

# ** IIC equation ####
output$IRT_binary_items_iic_equation <- renderUI({
  IRT_binary_summary_iic_equation()
})

# ** Equation interpretation ####
output$IRT_binary_items_equation_interpretation <- renderUI({
  IRT_binary_summary_equation_interpretation()
})

# ** Check whether model converged ####
output$IRT_binary_items_model_converged <- renderUI({
  fit <- IRT_binary_model()
  txt <- ifelse(extract.mirt(fit, "converged"),
    "",
    paste0(
      "<font color = 'orange'> Estimation process terminated without convergence after ",
      extract.mirt(fit, "iterations"), " iterations. Estimates are not reliable.
      Try to increase a number of iterations of the EM algorithm in Settings. </font>"
    )
  )
  HTML(txt)
})

# ** Plot of ICC for selected item ####
IRT_binary_items_icc <- reactive({
  item <- input$IRT_binary_items
  fit <- IRT_binary_model()
  n_items <- extract.mirt(fit, "nitems")
  curve_col <- gg_color_hue(n_items)[item]

  d <- tibble(
    Ability = IRT_thetas_for_plots(), # vector only
    Probability = probtrace(
      extract.item(fit, item),
      IRT_thetas_for_plots()
    )[, 2] # ascending probs
  )

  g <- d |> ggplot(aes(x = Ability, y = Probability)) +
    geom_line(color = curve_col) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[item]) +
    ylim(0, 1) +
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
  p |> plotly::config(displayModeBar = FALSE)
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
    Information = iteminfo(
      extract.item(fit, item),
      IRT_thetas_for_plots()
    ) # ascending probs
  )

  g <- d |> ggplot(aes(x = Ability, y = Information)) +
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
  p |> plotly::config(displayModeBar = FALSE)
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

  txt1 <- ifelse(extract.mirt(fit1PL, "converged"),
    "",
    "Estimation process in the <b>1PL IRT model</b> terminated without convergence. <br>"
  )
  txt2 <- ifelse(extract.mirt(fit2PL, "converged"),
    "",
    "Estimation process in the <b>2PL IRT model</b> terminated without convergence. <br>"
  )
  txt3 <- ifelse(extract.mirt(fit3PL, "converged"),
    "",
    "Estimation process in the <b>3PL IRT model</b> terminated without convergence. <br>"
  )
  txt4 <- ifelse(extract.mirt(fit4PL, "converged"),
    "",
    "Estimation process in the <b>4PL IRT model</b> terminated without convergence. <br>"
  )
  txt <- paste0(txt1, txt2, txt3, txt4)
  if (txt != "") {
    txt <- paste0(
      "<font color = 'orange'>", txt, "Estimates are not reliable. Try to increase
    a number of iterations of the EM algorithm in Settings. </font>"
    )
  }
  HTML(txt)
})

# ** Comparison of binary models ####
IRT_binary_comparison <- reactive({
  fit1PL <- IRT_binary_model_1pl()
  fit2PL <- IRT_binary_model_2pl()
  fit3PL <- IRT_binary_model_3pl()
  fit4PL <- IRT_binary_model_4pl()

  df <- anova(fit1PL, fit2PL, fit3PL, fit4PL)

  df <- round(df, 3)

  df <- df[, c("AIC", "BIC", "logLik")]
  nam <- c("1PL", "2PL", "3PL", "4PL")

  df <- rbind(
    df,
    c(nam[sapply(1:2, function(i) which(df[, i] == min(df[, i], na.rm = TRUE)))], "")
  )

  rownames(df) <- c(nam, "BEST")
  df
})

output$IRT_binary_comparison <- renderTable(
  {
    IRT_binary_comparison()
  },
  rownames = TRUE
)
