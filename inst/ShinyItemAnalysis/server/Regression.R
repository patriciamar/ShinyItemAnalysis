# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# REGRESSION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_logistic_item_slider",
    max = item_count
  )
})

# ** Model of logistic regression ####
regression_logistic_model <- reactive({
  item <- input$regression_logistic_item_slider
  data <- binary()
  total_score <- total_score()

  model <- glm(unlist(data[, item, with = FALSE]) ~ total_score, family = binomial)
})

# ** Plot with estimated logistic curve ####
regression_logistic_plot <- reactive({
  total_score <- total_score()
  data <- binary()
  fit <- regression_logistic_model()
  item <- input$regression_logistic_item_slider

  fun <- function(x, b0, b1) {
    exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
  }

  df <- data.table(
    Score = sort(unique(total_score)),
    Probability = tapply(unlist(data[, item, with = FALSE]), total_score, mean),
    Count = as.numeric(table(total_score))
  )

  ggplot(df, aes(x = Score, y = Probability)) +
    geom_point(aes(size = Count),
      color = "darkblue",
      fill = "darkblue",
      shape = 21, alpha = 0.5
    ) +
    stat_function(
      fun = fun, geom = "line",
      args = list(
        b0 = coef(fit)[1],
        b1 = coef(fit)[2]
      ),
      size = 0.8,
      color = "darkblue"
    ) +
    xlab("Total score") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    theme_app() +
    theme(
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1)
    ) +
    ggtitle(item_names()[item])
})

# ** Output estimated logistic curve ####
output$regression_logistic_plot <- renderPlotly({
  g <- regression_logistic_plot()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download estimated logistic curve ####
output$regression_logistic_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_LogisticRegressionCurve_", item_names()[input$regression_logistic_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_logistic_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of estimated parameters of logistic curve ####
output$regression_logistic_coef <- renderTable(
  {
    item <- input$regression_logistic_item_slider
    tab <- summary(regression_logistic_model())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{\\beta}_{", item, "0}%%"),
      paste0("%%mathit{\\beta}_{", item, "1}%%")
    )
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Interpretation of estimated parameters of logistic curve ####
output$regression_logistic_interpretation <- renderUI({
  b1 <- coef(regression_logistic_model())[2]

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <- paste(
    "A one-unit increase in the total
    score is associated with the", txt0, " in the log
    odds of answering the item correctly
    vs. not correctly in the amount of"
  )
  b1 <- sprintf("%.2f", abs(b1))
  txt3 <- paste("<b>", b1, "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$regression_logistic_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC Z ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_logistic_Z_item_slider",
    max = item_count
  )
})

# ** Model of logistic regression on Z-scores ####
regression_logistic_Z_model <- reactive({
  zscore <- z_score()
  item <- input$regression_logistic_Z_item_slider
  data <- binary()
  model <- glm(unlist(data[, item, with = FALSE]) ~ zscore, family = "binomial")
})

# ** Plot of logistic regression on Z-scores ####
regression_logistic_Z_plot <- reactive({
  zscore <- z_score()
  item <- input$regression_logistic_Z_item_slider
  data <- binary()
  fit <- regression_logistic_Z_model()

  fun <- function(x, b0, b1) {
    exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
  }

  df <- data.table(
    Zscore = sort(unique(zscore)),
    Probability = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    Count = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = Zscore, y = Probability)) +
    geom_point(aes(size = Count),
      color = "darkblue",
      fill = "darkblue",
      shape = 21, alpha = 0.5
    ) +
    stat_function(
      fun = fun, geom = "line",
      args = list(
        b0 = coef(fit)[1],
        b1 = coef(fit)[2]
      ),
      size = 0.8,
      color = "darkblue"
    ) +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1)
    ) +
    ggtitle(item_names()[item])
})

# ** Output plot of logistic regression on Z-scores ####
output$regression_logistic_Z_plot <- renderPlotly({
  g <- regression_logistic_Z_plot()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download for plot of logistic regression on Z-scores ####
output$regression_logistic_Z_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_LogisticRegressionCurve_Zscores_", item_names()[input$regression_logistic_Z_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_logistic_Z_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of estimated parameters of logistic regression on Z-scores ####
output$regression_logistic_Z_coef <- renderTable(
  {
    item <- input$regression_logistic_Z_item_slider
    tab <- summary(regression_logistic_Z_model())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{\\beta}_{", item, "0}%%"),
      paste0("%%mathit{\\beta}_{", item, "1}%%")
    )
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Interpretation of estimated parameters of logistic regression on Z-scores ####
output$regression_logistic_Z_interpretation <- renderUI({
  b1 <- summary(regression_logistic_Z_model())$coef[2, 1]

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <-
    paste(
      "A one-unit increase in the Z-score (one SD increase in original
      scores) is associated with the", txt0, " in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
  b1 <- sprintf("%.2f", abs(b1))
  txt3 <- paste("<b>", b1, "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$regression_logistic_Z_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC IRT Z ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_logistic_IRT_item_slider",
    max = item_count
  )
})

# ** Model for logistic regression on Z scores with IRT param. ####
regression_logistic_IRT_model <- reactive({
  zscore <- z_score()
  item <- input$regression_logistic_IRT_item_slider
  data <- binary()

  model <- glm(unlist(data[, item, with = FALSE]) ~ zscore, family = "binomial")
})

# ** Plot with estimated logistic curve on Z scores with IRT param. ####
regression_logistic_IRT_plot <- reactive({
  zscore <- z_score()
  item <- input$regression_logistic_IRT_item_slider
  data <- binary()
  fit <- regression_logistic_IRT_model()

  fun <- function(x, b0, b1) {
    exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
  }

  df <- data.table(
    Zscore = sort(unique(zscore)),
    Probability = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    Count = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = Zscore, y = Probability)) +
    geom_point(aes(size = Count),
      color = "darkblue",
      fill = "darkblue",
      shape = 21, alpha = 0.5
    ) +
    stat_function(
      fun = fun, geom = "line",
      args = list(
        b0 = coef(fit)[1],
        b1 = coef(fit)[2]
      ),
      size = 0.8,
      color = "darkblue"
    ) +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1)
    ) +
    ggtitle(item_names()[item])
})

# ** Output plot with estimated logistic curve on Z scores with IRT param. ####
output$regression_logistic_IRT_plot <- renderPlotly({
  g <- regression_logistic_IRT_plot()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot with estimated logistic curve on Z scores with IRT param. ####
output$regression_logistic_IRT_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_LogisticRegressionCurve_Zscores_IRT_", item_names()[input$regression_logistic_IRT_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_logistic_IRT_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of estimated parameters of logistic curve on Z scores with IRT param. ####
output$regression_logistic_IRT_coef <- renderTable(
  {
    fit <- regression_logistic_IRT_model()
    tab_coef_old <- coef(fit)
    item <- input$regression_logistic_IRT_item_slider

    # delta method
    tab_se <- msm::deltamethod(
      list(~x2, ~ -x1 / x2),
      mean = tab_coef_old,
      cov = vcov(fit),
      ses = TRUE
    )

    tab_coef <- c(tab_coef_old[2], -tab_coef_old[1] / tab_coef_old[2])
    tab <- cbind(tab_coef, tab_se)
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{a}_{", item, "}%%"),
      paste0("%%mathit{b}_{", item, "}%%")
    )
    tab
  },
  include.rownames = TRUE
)

# ** Interpretation of estimated parameters of logistic curve on Z scores with IRT param. ####
output$regression_logistic_IRT_interpretation <- renderUI({
  b1 <- summary(regression_logistic_IRT_model())$coef[2, 1]

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <-
    paste(
      "A one-unit increase in the Z-score (one SD increase in original scores) is associated
      with the", txt0, "in the log odds of answering the item correctly vs. not correctly in
      the amount of"
    )
  b1 <- sprintf("%.2f", abs(b1))
  txt3 <- paste("<b>", b1, "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$regression_logistic_IRT_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 3P IRT Z ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_3pl_item_slider",
    max = item_count
  )
})

# ** Model of nonlinear curve ####
regression_3pl_model <- reactive({
  data <- binary()
  zscore <- z_score()
  item <- input$regression_3pl_item_slider

  if (any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data,
    group = c(rep(0, nrow(data) / 2), rep(1, nrow(data) / 2)),
    model = "3PLcg", parameterization = "classic", simplify = TRUE
  )[, 1:3]

  glr <- function(x, a, b, c) {
    c + (1 - c) / (1 + exp(-a * (x - b)))
  }

  fit <- tryCatch(nls(unlist(data[, item, with = FALSE]) ~ glr(zscore, a, b, c),
    algorithm = "port", start = start[item, ],
    lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, 1)
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "nls",
    HTML(paste0("Error: Method cannot be fitted for item ", item, ". The error message returned: ", fit$message))
  ),
  errorClass = "validation-error")

  fit
})

# ** Plot of estimated nonlinear curve ####
regression_3pl_plot <- reactive({
  zscore <- z_score()
  item <- input$regression_3pl_item_slider
  data <- binary()

  fit <- regression_3pl_model()

  fun <- function(x, a, b, c) {
    c + (1 - c) / (1 + exp(-a * (x - b)))
  }

  df <- data.table(
    Zscore = sort(unique(zscore)),
    Probability = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    Count = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = Zscore, y = Probability)) +
    geom_point(aes(size = Count),
      color = "darkblue",
      fill = "darkblue",
      shape = 21, alpha = 0.5
    ) +
    stat_function(
      fun = fun, geom = "line",
      args = list(
        a = coef(fit)[1],
        b = coef(fit)[2],
        c = coef(fit)[3]
      ),
      size = 0.8,
      color = "darkblue"
    ) +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1)
    ) +
    ggtitle(item_names()[item])
})

# ** Output plot of estimated nonlinear curve ####
output$regression_3pl_plot <- renderPlotly({
  g <- regression_3pl_plot()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of estimated nonlinear curve ####
output$regression_3pl_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_NLR_3P_", item_names()[input$regression_3pl_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_3pl_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# Table of estimated parameters of nonlinear curve ####
output$regression_3pl_coef <- renderTable(
  {
    fit <- regression_3pl_model()
    item <- input$regression_3pl_item_slider

    tab <- summary(fit)$parameters[, 1:2]
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{a}_{", item, "}%%"),
      paste0("%%mathit{b}_{", item, "}%%"),
      paste0("%%mathit{c}_{", item, "}%%")
    )
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Interpretation of estimated parameters of nonlinear curve ####
output$regression_3pl_interpretation <- renderUI({
  fit <- regression_3pl_model()

  a <- round(coef(fit)[1], 2)
  c <- sprintf("%.2f", coef(fit)[3])

  txt0 <- paste0("<b>", "Interpretation:", "</b>")
  txt1 <- paste0(
    "A one-unit increase in the Z-score (one SD increase in original scores) is associated with the ",
    ifelse(a < 0, "decrease", "increase"), " in the log odds of answering the item correctly vs.
    not correctly in the amount of <b>", sprintf("%.2f", abs(a)), "</b>. "
  )
  txt2 <- paste0("Probability of guessing is <b>", c, "</b>. ")

  HTML(paste0(txt0, txt1, txt2))
})

# ** Warning for missing values ####
output$regression_3pl_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 4P IRT Z ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_4pl_item_slider",
    max = item_count
  )
})

# ** Model of nonlinear curve ####
regression_4pl_model <- reactive({
  data <- binary()
  zscore <- z_score()
  item <- input$regression_4pl_item_slider

  if (any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data,
    group = c(rep(0, nrow(data) / 2), rep(1, nrow(data) / 2)),
    model = "4PLcgdg", parameterization = "classic", simplify = TRUE
  )[, 1:4]

  glr <- function(x, a, b, c, d) {
    c + (d - c) / (1 + exp(-a * (x - b)))
  }

  fit <- tryCatch(nls(unlist(data[, item, with = FALSE]) ~ glr(zscore, a, b, c, d),
    algorithm = "port", start = start[item, ],
    lower = c(-Inf, -Inf, 0, 0), upper = c(Inf, Inf, 1, 1)
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "nls",
    HTML(paste0("Error: Method cannot be fitted for item ", item, ". The error message returned: ", fit$message))
  ),
  errorClass = "validation-error")

  fit
})

# ** Plot of estimated nonlinear curve ####
regression_4pl_plot <- reactive({
  zscore <- z_score()
  item <- input$regression_4pl_item_slider
  data <- binary()
  fit <- regression_4pl_model()

  fun <- function(x, a, b, c, d) {
    c + (d - c) / (1 + exp(-a * (x - b)))
  }

  df <- data.table(
    Zscore = sort(unique(zscore)),
    Probability = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    Count = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = Zscore, y = Probability)) +
    geom_point(aes(size = Count),
      color = "darkblue",
      fill = "darkblue",
      shape = 21, alpha = 0.5
    ) +
    stat_function(
      fun = fun, geom = "line",
      args = list(
        a = coef(fit)[1],
        b = coef(fit)[2],
        c = coef(fit)[3],
        d = coef(fit)[4]
      ),
      size = 0.8,
      color = "darkblue"
    ) +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1)
    ) +
    ggtitle(item_names()[item])
})

# ** Output plot of estimated nonlinear curve ####
output$regression_4pl_plot <- renderPlotly({
  g <- regression_4pl_plot()
  p <- ggplotly(g)

  p$elementId <- NULL
  p %>% plotly::config(displayModeBar = FALSE)
})

# ** Download plot of estimated nonlinear curve ####
output$regression_4pl_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_NLR_4P", item_names()[input$regression_4pl_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_4pl_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# Table of estimated parameters of nonlinear curve ####
output$regression_4pl_coef <- renderTable(
  {
    fit <- regression_4pl_model()
    item <- input$regression_4pl_item_slider

    tab <- summary(fit)$parameters[, 1:2]
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{a}_{", item, "}%%"),
      paste0("%%mathit{b}_{", item, "}%%"),
      paste0("%%mathit{c}_{", item, "}%%"),
      paste0("%%mathit{d}_{", item, "}%%")
    )
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Interpretation of estimated parameters of nonlinear curve ####
output$regression_4pl_interpretation <- renderUI({
  fit <- regression_4pl_model()

  a <- coef(fit)[1]
  b <- round(coef(fit)[2], 2)
  c <- sprintf("%.2f", coef(fit)[3])
  d <- sprintf("%.2f", 1 - coef(fit)[4])

  txt0 <- paste0("<b>", "Interpretation: ", "</b>")
  txt1 <- paste0(
    "A one-unit increase in the Z-score (one SD increase in original scores) is associated with the ",
    ifelse(a < 0, "decrease", "increase"), " in the log odds of answering the item correctly
                  vs. not correctly in the amount of <b>", sprintf("%.2f", abs(a)), "</b>. "
  )
  txt2 <- paste0("Probability of guessing is <b>", c, "</b>. ")
  txt3 <- paste0("Probability of inattention is <b>", d, "</b>. ")
  HTML(paste0(txt0, txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$regression_4pl_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Table of information criteria ####
output$regression_comparison_table <- DT::renderDataTable({
  data <- binary()
  zscore <- z_score()

  m <- ncol(data)

  glr <- function(x, a, b, c, d) {
    c + (d - c) / (1 + exp(-a * (x - b)))
  }

  # If zscore has NA values, those need to be ommited.
  # While nls does that automatically, nevertheless the indices of those values
  # would cause unlist(data[, i, with = FALSE]) have higher dimension
  # than output of function glr(zscore, a, b, c = 0, d = 1).
  # Thus the following code is needed:

  if (any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data,
    group = c(rep(0, nrow(data) / 2), rep(1, nrow(data) / 2)),
    model = "4PLcgdg",
    parameterization = "classic",
    simplify = TRUE
  )[, 1:4]


  fit2PL <- lapply(1:m, function(i) {
    tryCatch(nls(unlist(data[, i, with = FALSE]) ~ glr(zscore, a, b, c = 0, d = 1),
      algorithm = "port", start = start[i, 1:2],
      lower = c(-Inf, -Inf),
      upper = c(Inf, Inf)
    ), error = function(e) {
      cat("ERROR : ", conditionMessage(e), "\n")
    })
  })

  fit3PL <- lapply(1:m, function(i) {
    tryCatch(nls(unlist(data[, i, with = FALSE]) ~ glr(zscore, a, b, c, d = 1),
      algorithm = "port", start = start[i, 1:3],
      lower = c(-Inf, -Inf, 0),
      upper = c(Inf, Inf, 1)
    ), error = function(e) {
      cat("ERROR : ", conditionMessage(e), "\n")
    })
  })

  fit4PL <- lapply(1:m, function(i) {
    tryCatch(nls(unlist(data[, i, with = FALSE]) ~ glr(zscore, a, b, c, d),
      algorithm = "port", start = start[i, 1:4],
      lower = c(-Inf, -Inf, 0, 0),
      upper = c(Inf, Inf, 1, 1)
    ), error = function(e) {
      cat("ERROR : ", conditionMessage(e), "\n")
    })
  })

  whok2PL <- !sapply(fit2PL, is.null)
  whok3PL <- !sapply(fit3PL, is.null)
  whok4PL <- !sapply(fit4PL, is.null)
  AIC2PL <- AIC3PL <- AIC4PL <- BIC2PL <- BIC3PL <- BIC4PL <- rep(NA, m)

  AIC2PL[whok2PL] <- sapply(fit2PL[whok2PL], AIC)
  AIC3PL[whok3PL] <- sapply(fit3PL[whok3PL], AIC)
  AIC4PL[whok4PL] <- sapply(fit4PL[whok4PL], AIC)

  BIC2PL[whok2PL] <- sapply(fit2PL[whok2PL], BIC)
  BIC3PL[whok3PL] <- sapply(fit3PL[whok3PL], BIC)
  BIC4PL[whok4PL] <- sapply(fit4PL[whok4PL], BIC)

  bestAIC <- bestBIC <- rep(NA, m)
  dfAIC <- cbind(AIC2PL, AIC3PL, AIC4PL)
  bestAIC <- apply(dfAIC, 1, function(x) which(x == min(x, na.rm = TRUE))[1])
  bestAIC <- ifelse(is.na(bestAIC), bestAIC, paste0(bestAIC + 1, "PL"))

  dfBIC <- cbind(BIC2PL, BIC3PL, BIC4PL)
  bestBIC <- apply(dfBIC, 1, function(x) which(x == min(x, na.rm = TRUE))[1])
  bestBIC <- ifelse(is.na(bestBIC), bestBIC, paste0(bestBIC + 1, "PL"))

  # LRstat23 <- LRpval23 <- rep(NA, m)
  # whok23PL <- c(whok2PL & whok3PL)
  # LRstat23[whok23PL] <- -2 * (sapply(fit2PL[whok23PL], logLik) - sapply(fit3PL[whok23PL], logLik))
  # LRdf <- 1
  # LRpval23[whok23PL] <- 1 - pchisq(LRstat23[whok23PL], LRdf)
  # LRpval23[whok23PL] <- p.adjust(LRpval23[whok23PL], method = input$correction_method_regrmodels)
  #
  # LRstat34 <- LRpval34 <- rep(NA, m)
  # whok34PL <- c(whok3PL & whok4PL)
  # LRstat34[whok34PL] <- -2 * (sapply(fit3PL[whok34PL], logLik) - sapply(fit4PL[whok34PL], logLik))
  # LRdf <- 1
  # LRpval34[whok34PL] <- 1 - pchisq(LRstat34[whok34PL], LRdf)
  # LRpval34[whok34PL] <- p.adjust(LRpval34[whok34PL], method = input$correction_method_regrmodels)
  #
  # bestLR <- rep(NA, m)
  #
  # bestLR[whok34PL] <- ifelse(LRpval23[whok34PL] < 0.05, "4PL", "3PL")
  # bestLR[whok23PL] <- ifelse(LRpval23[whok23PL] != "4PL" & LRpval23[whok23PL] < 0.05, "3PL", "2PL")

  tab <- rbind(
    sprintf("%.2f", round(AIC2PL, 2)),
    sprintf("%.2f", round(AIC3PL, 2)),
    sprintf("%.2f", round(AIC4PL, 2)),
    bestAIC,
    sprintf("%.2f", round(BIC2PL, 2)),
    sprintf("%.2f", round(BIC3PL, 2)),
    sprintf("%.2f", round(BIC4PL, 2)),
    bestBIC
    # sprintf("%.2f", round(LRstat23, 3)),
    # ifelse(round(LRpval23, 3) < 0.001, "<0.001",
    #        sprintf("%.3f", round(LRpval23, 3))),
    # sprintf("%.2f", round(LRstat34, 3)),
    # ifelse(round(LRpval34, 3) < 0.001, "<0.001",
    #        sprintf("%.3f", round(LRpval34, 3))),
    # bestLR
  )

  tab <- as.data.table(tab)
  colnames(tab) <- item_names()
  rownames(tab) <- c(
    "AIC 2PL", "AIC 3PL", "AIC 4PL", "BEST AIC",
    "BIC 2PL", "BIC 3PL", "BIC 4PL", "BEST BIC"
    # "Chisq-value 2PL vs 3PL", "p-value 2PL vs 3PL",
    # "Chisq-value 3PL vs 4PL", "p-value 3PL vs 4PL",
    # "BEST LR"
  )

  tab <- datatable(tab,
    rownames = TRUE,
    style = "bootstrap",
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = "100px", targets = list(0)),
        list(width = "65", targets = "_all")
      ),
      scrollX = TRUE,
      ordering = FALSE,
      fixedColumns = list(leftColumns = 1),
      pageLength = 13,
      dom = "tr"
    )
  ) %>%
    formatStyle(0, target = "row", fontWeight = styleEqual(
      c("BEST AIC", "BEST BIC"), # "BEST LR"),
      c("bold", "bold") # , "bold")
    ))

  tab
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CUMULATIVE LOGIT ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion for cumulative model ####
regression_cumulative_matching <- reactive({
  if (input$regression_cumulative_matching == "total") {
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Updating select input for different parametrization ####
observe({
  if (input$regression_cumulative_parametrization == "irt") {
    updateSelectInput(
      session = session,
      inputId = "regression_cumulative_matching",
      selected = "zscore"
    )
  }
})

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_cumulative_item_slider",
    max = item_count
  )
})

# ** Model for cumulative logit regression ####
regression_cumulative_model <- reactive({
  matching <- regression_cumulative_matching()
  data <- as.data.frame(ordinal())
  categories <- lapply(data, function(x) sort(unique(x)))
  data[] <- sapply(
    1:ncol(data),
    function(i) factor(data[, i], levels = categories[[i]], ordered = TRUE)
  )

  # fit for all items
  fit <- sapply(
    data,
    function(x) {
      VGAM::vglm(x ~ matching,
        family = cumulative(reverse = TRUE, parallel = TRUE)
      )
    }
  )
  fit
})

# ** Plot with cumulative curves ####
regression_cumulative_plot_cumulative <- reactive({
  item <- input$regression_cumulative_item_slider
  fit <- regression_cumulative_model()
  matching.name <- ifelse(
    input$regression_cumulative_matching == "total",
    "Total score",
    "Standardized total score"
  )

  g <- plotCumulative(fit[[item]], type = "cumulative", matching.name = matching.name) +
    ggtitle(item_names()[item])

  g
})

# ** Output plot with estimated curves of cumulative regression ####
output$regression_cumulative_plot_cumulative <- renderPlotly({
  g <- regression_cumulative_plot_cumulative()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  hide_legend(p %>% plotly::config(displayModeBar = FALSE))
})

# ** Download plot with estimated curves of cumulative regression ####
output$regression_cumulative_plot_cumulative_download <- downloadHandler(
  filename = function() {
    paste0("fig_CumulativeRegressionCurve_cumulative_", item_names()[input$regression_cumulative_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_cumulative_plot_cumulative() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Plot with category curves ####
regression_cumulative_plot_category <- reactive({
  item <- input$regression_cumulative_item_slider
  fit.cum <- regression_cumulative_model()
  matching.name <- ifelse(
    input$regression_cumulative_matching == "total",
    "Total score",
    "Standardized total score"
  )

  g <- plotCumulative(fit.cum[[item]], type = "category", matching.name = matching.name) +
    ggtitle(item_names()[item])
  g
})

# ** Output plot with category curves ####
output$regression_cumulative_plot_category <- renderPlotly({
  g <- regression_cumulative_plot_category()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  hide_legend(p %>% plotly::config(displayModeBar = FALSE))
})

# ** Download plot with estimated curves of cumulative regression ####
output$regression_cumulative_plot_category_download <- downloadHandler(
  filename = function() {
    paste0("fig_CumulativeRegressionCurve_category_", item_names()[input$regression_cumulative_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_cumulative_plot_category() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Equation ####
output$regression_cumulative_equation <- renderUI({
  txt1 <- ifelse(input$regression_cumulative_matching == "total", "X_p", "Z_p")

  if (input$regression_cumulative_parametrization == "classic") {
    txt2 <- paste("\\beta_{i0k} + \\beta_{i1}", txt1)
  } else {
    txt2 <- paste("a_i(", txt1, "- b_{ik})")
  }
  txt3 <- paste("$$\\mathrm{P}(Y_{pi} \\geq k|", txt1, ") = \\pi_{pik} = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")
  txt4 <- paste("$$\\mathrm{P}(Y_{pi} = k|", txt1, ") = \\pi_{pik} - \\pi_{pi(k + 1)}$$")

  withMathJax(HTML(paste(txt3, txt4)))
})

# ** Interpretation ####
output$regression_cumulative_interpretation <- renderUI({
  if (input$regression_cumulative_parametrization == "classic") {
    par <- c("\\(\\beta_{i0k}\\)", "\\(\\beta_{i1}\\)")
  } else {
    par <- c("\\(b_{ik}\\)", "\\(a_i\\)")
  }

  item <- input$regression_cumulative_item_slider
  min_cat <- min(as.data.frame(ordinal())[, item], na.rm = TRUE) + 1
  max_cat <- max(as.data.frame(ordinal())[, item], na.rm = TRUE)
  category_range <- unique(c(min_cat, max_cat))
  categories <-
    ifelse(
      length(category_range) == 1,
      sprintf("\\(k = %s\\)", category_range),
      ifelse(
        category_range[2] - category_range[1] == 0,
        sprintf("\\(k = %s, %s\\)", category_range[1], category_range[2]),
        sprintf("\\(k = %s, ..., %s\\)", category_range[1], category_range[2])
      )
    )

  txt <- sprintf(
    "Parameters %s describe the horizontal position of the fitted cumulative curves for item \\(i\\),
               where %s is the number of obtained scores, parameter %s
               describes their common slope. Category probabilities are then calculated as
               differences between the two subsequent cumulative probabilities. ",
    par[1], categories, par[2]
  )
  withMathJax(HTML(txt))
})

# ** Table of estimated parameters ####
regression_cumulative_coef <- reactive({
  fit <- regression_cumulative_model()
  item <- input$regression_cumulative_item_slider
  data <- as.data.frame(ordinal())

  if (input$regression_cumulative_parametrization == "classic") {
    tab_coef <- coef(fit[[item]])
    tab_se <- sqrt(diag(vcov(fit[[item]])))

    tab <- data.frame(Estimate = tab_coef, SE = tab_se)
    rownames(tab) <- c(
      paste0("%%mathit{\\beta}_{", item, "0", sort(unique(data[, item]))[-1], "}%%"),
      paste0("%%mathit{\\beta}_{", item, "1}%%")
    )
  } else {
    tab_coef_old <- coef(fit[[item]])
    num_par <- length(tab_coef_old)

    # delta method
    formula <- append(
      paste0("~ x", num_par),
      as.list(paste0("~ -x", 1:(num_par - 1), "/", "x", num_par))
    )
    formula <- lapply(formula, as.formula)

    tab_se <- msm::deltamethod(
      formula,
      mean = tab_coef_old,
      cov = vcov(fit[[item]]),
      ses = TRUE
    )
    tab_coef <- c(tab_coef_old[num_par], -tab_coef_old[-num_par] / tab_coef_old[num_par])

    tab <- data.frame(Estimate = tab_coef, SE = tab_se)
    rownames(tab) <- c(
      paste0("%%mathit{a}_{", item, "}%%"),
      paste0("%%mathit{b}_{", item, sort(unique(data[, item]))[-1], "}%%")
    )
  }
  tab
})

output$regression_cumulative_coef <- renderTable(
  {
    regression_cumulative_coef()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Warning for missing values ####
output$regression_cumulative_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT CATEGORY LOGIT ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion adjacent category logit regression ####
regression_adjacent_matching <- reactive({
  if (input$regression_adjacent_matching == "total") {
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Updating select input for different parametrization ####
observe({
  if (input$regression_adjacent_parametrization == "irt") {
    updateSelectInput(
      session = session,
      inputId = "regression_adjacent_matching",
      selected = "zscore"
    )
  }
})

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_adjacent_item_slider",
    max = item_count
  )
})

# ** Model for adjacent regression ####
regression_adjacent_model <- reactive({
  matching <- regression_adjacent_matching()
  data <- as.data.frame(ordinal())
  categories <- lapply(data, function(x) sort(unique(x)))
  data[] <- sapply(
    1:ncol(data),
    function(i) factor(data[, i], levels = categories[[i]], ordered = TRUE)
  )

  fit <- sapply(data, function(x) {
    vglm(x ~ matching, family = acat(reverse = FALSE, parallel = TRUE))
  })
  fit
})

# ** Plot with category curves ####
regression_adjacent_plot <- reactive({
  item <- input$regression_adjacent_item_slider
  fit <- regression_adjacent_model()
  matching.name <- ifelse(
    input$regression_adjacent_matching == "total",
    "Total score",
    "Standardized total score"
  )

  g <- plotAdjacent(fit[[item]], matching.name = matching.name) +
    ggtitle(item_names()[item])

  g
})

# ** Output plot with estimated curves of adjacent regression ####
output$regression_adjacent_plot <- renderPlotly({
  g <- regression_adjacent_plot()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  hide_legend(p %>% plotly::config(displayModeBar = FALSE))
})


# ** Download plot with estimated curves of adjacent regression ####
output$regression_adjacent_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_AdjacentRegressionCurve_category_", item_names()[input$regression_adjacent_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_adjacent_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Equation ####
output$regression_adjacent_equation <- renderUI({
  txt1 <- ifelse(input$regression_adjacent_matching == "total", "X_p", "Z_p")

  if (input$regression_adjacent_parametrization == "classic") {
    txt2 <- paste0("\\beta_{i0l} + \\beta_{i1}", txt1)
  } else {
    txt2 <- paste0("a_i(", txt1, " - b_{ik})")
  }

  txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt1, ") = \\frac{e^{\\sum_{l = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K_i}e^{\\sum_{l = 0}^{r}", txt2, "}}$$")

  withMathJax(HTML(txt))
})

# ** Interpretation ####
output$regression_adjacent_interpretation <- renderUI({
  item <- input$regression_adjacent_item_slider
  if (input$regression_adjacent_parametrization == "classic") {
    par <- c("\\(\\beta_{i0k}\\)", "\\(\\beta_{i1}\\)")
  } else {
    par <- c("\\(b_{ik}\\)", "\\(a_i\\)")
  }

  min_cat <- min(as.data.frame(ordinal())[, item], na.rm = TRUE) + 1
  max_cat <- max(as.data.frame(ordinal())[, item], na.rm = TRUE)
  category_range <- unique(c(min_cat, max_cat))
  categories <-
    ifelse(
      length(category_range) == 1,
      sprintf("\\(k = %s\\)", category_range),
      ifelse(
        category_range[2] - category_range[1] == 0,
        sprintf("\\(k = %s, %s\\)", category_range[1], category_range[2]),
        sprintf("\\(k = %s, ..., %s\\)", category_range[1], category_range[2])
      )
    )

  if (input$regression_adjacent_parametrization == "classic") {
    txt <- sprintf(
      "Threshold parameters %s describe the horizontal position of the fitted curves for item \\(i\\),
               where %s is the number of obtained scores, parameter %s describes their common slope. ",
      par[1], categories, par[2]
    )
  } else {
    txt <- sprintf(
      "Threshold parameters %s describe the horizontal position of the fitted curves for item \\(i\\)
              where %s is the number of obtained scores and they indicate intersections of the probability
              curves for two adjacent categories, parameter %s describes their common slope. ",
      par[1], categories, par[2]
    )
  }

  withMathJax(HTML(txt))
})

# ** Table of estimated parameters ####
output$regression_adjacent_coef <- renderTable(
  {
    item <- input$regression_adjacent_item_slider
    fit <- regression_adjacent_model()[[item]]
    data <- as.data.frame(ordinal())

    if (input$regression_adjacent_parametrization == "classic") {
      tab_coef <- coef(fit)
      tab_se <- sqrt(diag(vcov(fit)))

      tab <- data.frame(Estimate = tab_coef, SE = tab_se)
      rownames(tab) <- c(
        paste0("%%mathit{\\beta}_{", item, "0", sort(unique(data[, item]))[-1], "}%%"),
        paste0("%%mathit{\\beta}_{", item, "1}%%")
      )
    } else {
      tab_coef_old <- coef(fit)
      num_par <- length(tab_coef_old)

      # delta method
      formula <- append(
        paste0("~ x", num_par),
        as.list(paste0("~ -x", 1:(num_par - 1), "/", "x", num_par))
      )
      formula <- lapply(formula, as.formula)

      tab_se <- msm::deltamethod(
        formula,
        mean = tab_coef_old,
        cov = vcov(fit),
        ses = TRUE
      )
      tab_coef <- c(tab_coef_old[num_par], -tab_coef_old[-num_par] / tab_coef_old[num_par])

      tab <- data.frame(Estimate = tab_coef, SE = tab_se)
      rownames(tab) <- c(
        paste0("%%mathit{a}_{", item, "}%%"),
        paste0("%%mathit{b}_{", item, sort(unique(data[, item]))[-1], "}%%")
      )
    }

    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Warning for missing values ####
output$regression_adjacent_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion for multinomial regression ####
regression_multinomial_matching <- reactive({
  if (input$regression_multinomial_matching == "total") {
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Updating select input for different parametrization ####
observe({
  if (input$regression_multinomial_parametrization == "irt") {
    updateSelectInput(
      session = session,
      inputId = "regression_multinomial_matching",
      selected = "zscore"
    )
  }
})

# ** Updating item slider ####
observe({
  item_count <- ncol(binary())

  updateSliderInput(
    session = session,
    inputId = "regression_multinomial_item_slider",
    max = item_count
  )
})

# ** Model for multinomial regression ####
regression_multinomial_model <- reactive({
  matching <- regression_multinomial_matching()
  key <- t(as.data.table(key()))
  item <- input$regression_multinomial_item_slider
  data <- nominal()

  dfhw <- data.table(data[, item, with = FALSE], matching)
  dfhw <- dfhw[complete.cases(dfhw), ]

  item_cats <- unlist(dfhw[, 1])
  item_values <- unlist(dfhw[, 2])

  fit <- tryCatch(multinom(relevel(as.factor(item_cats),
    ref = paste(key[item])
  ) ~ item_values,
  trace = FALSE
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "nnet",
    HTML(paste0("Error: Method cannot be fitted for item ", item, ". The error message returned: ", fit$message))
  ),
  errorClass = "validation-error")

  fit
})

# ** Plot with category curves ####
regression_multinomial_plot <- reactive({
  fit <- regression_multinomial_model()

  matching <- regression_multinomial_matching()
  item <- input$regression_multinomial_item_slider
  data <- nominal()

  dfhw <- data.table(data[, item, with = FALSE], matching)
  matching <- unlist(dfhw[complete.cases(dfhw), 2])

  matching_name <- ifelse(
    input$regression_multinomial_matching == "total",
    "Total score",
    "Standardized total score"
  )

  g <- plotMultinomial(fit, matching, matching.name = matching_name) +
    ggtitle(item_names()[item])
  g
})

# ** Output plot with category curves ####
output$regression_multinomial_plot <- renderPlotly({
  g <- regression_multinomial_plot()
  p <- ggplotly(g)

  for (i in 1:length(p$x$data)) {
    text <- p$x$data[[i]]$text
    text <- lapply(strsplit(text, split = "<br />"), unique)
    text <- unlist(lapply(text, paste, collapse = "<br />"))
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL
  hide_legend(p %>% plotly::config(displayModeBar = FALSE))
})

# ** Download plot with estimated curves of multinomial regression ####
output$regression_multinomial_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_MultinomialRegressionCurve_", item_names()[input$regression_multinomial_item_slider], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = regression_multinomial_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Reports: Plot with estimated curves of multinomial regression ####
report_regression_multinomial_plot <- reactive({
  graflist <- list()
  key <- unlist(key())
  data <- nominal()
  matching <- regression_multinomial_matching()
  matching_name <- ifelse(
    input$regression_multinomial_matching == "total",
    "Total score",
    "Standardized total score"
  )

  data <- sapply(1:ncol(data), function(i) as.factor(unlist(data[, i, with = FALSE])))

  error_measages <- c()
  for (item in 1:length(key)) {
    dfhw <- data.table(data[, item], matching)
    dfhw <- dfhw[complete.cases(dfhw), ]
    matching <- unlist(dfhw[, 2])

    fitM <- multinom(relevel(as.factor(unlist(dfhw[, 1])),
      ref = paste(key[item])
    ) ~ unlist(dfhw[, 2]),
    trace = FALSE
    )

    test <- tryCatch(plotMultinomial(fitM, matching, matching.name = matching_name),
      error = function(x) print(x)
    )

    if (mode(test$message) == "character") {
      graflist[[item]] <- NULL
      error_measages[item] <- test$message
    } else {
      g <- plotMultinomial(fitM, matching, matching.name = matching_name)
      g <- g + ggtitle(paste("Multinomial plot for item", item_numbers()[item]))
      graflist[[item]] <- g
    }
  }

  null_idx_true <- which(sapply(graflist, is.null))
  null_idx_false <- which(!sapply(graflist, is.null))

  graflist
})

# ** Reports: Length of legend in multinomial plot ####
report_distractor_plot_legend_length <- reactive({
  data <- nominal()

  legend_length <- max(sapply(data, function(x) length(unique(x))), na.rm = TRUE)
  legend_length
})

# AH: momentalne zakomentovavam, protoze stranka Reports hrozne dlouho nabiha
# # ** Warning for report ####
# output$report_multinomial_report <- renderUI({
#   items <- report_regression_multinomial_plot()[[2]]
#   messages <- unique(report_regression_multinomial_plot()[[3]])
#   txt <- paste0("<font color = 'orange'>
# 				Multinomial plot for items ", items, " is missing.
#         Error message returned: ", strong(messages[2]),".
# 				</font>")
#   HTML(txt)
# })

# ** Equation of multinomial regression ####
output$regression_multinomial_equation <- renderUI({
  req(regression_multinomial_model()[[2]])

  item <- input$regression_multinomial_item_slider
  match <- ifelse(input$regression_multinomial_matching == "total", "X_p", "Z_p")
  correct_option <- key()[item]

  if (input$regression_multinomial_parametrization == "classic") {
    eq1 <- sprintf(
      "$$\\mathrm{P}(Y_{pi} = k|%s) = \\frac{e^{\\left(\\beta_{i0k} + \\beta_{i1k} %s\\right)}}{1 + \\sum_{l} e^{\\left(\\beta_{i0l} + \\beta_{i1l} %s\\right)}},$$",
      match, match, match
    )
    eq2 <- sprintf(
      "$$\\mathrm{P}(Y_{pi} = %s|%s) = \\frac{1}{1 + \\sum_l e^{\\left(\\beta_{i0l} + \\beta_{i1l} %s\\right)}},$$",
      correct_option, match, match
    )
    txt <- sprintf(
      "where \\(k\\) is one of the wrong options (distractors) and \\(%s\\) is the correct one. Parameters \\(\\beta_{%s0k}\\) represent
                    locations of response probability curves for item %s and \\(\\beta_{%s1k}\\) are slopes of these curves. ",
      correct_option, item, item, item
    )
  } else {
    eq1 <- sprintf(
      "$$\\mathrm{P}(Y_{pi} = k|%s) = \\frac{e^{a_{%sk}(%s - b_{%sk})}}{\\sum_{t = 0}^{K_%s} e^{a_{%st}(%s - b_{%st})}},$$",
      match, item, match, item, item, item, match, item
    )
    eq2 <- sprintf(
      "$$\\mathrm{P}(Y_{pi} = %s|%s) = \\frac{1}{\\sum_{t = 0}^{K_%s} e^{a_{%st}(%s - b_{%st})}},$$",
      correct_option, match, item, item, match, item
    )
    txt <- sprintf(
      "where \\(k\\) is one of the wrong options (distractors) and \\(%s\\) is the correct one. Parameters \\(b_{%sk}\\) are
                    locations of response probability curve intersections with the curve of the correct answer for item %s, while \\(a_{%sk}\\)
                    are slopes of these curves. ",
      correct_option, item, item, item
    )
  }
  withMathJax(HTML(paste(eq1, eq2, txt)))
})

# ** Table of estimated parameters of curves of multinomial regression ####
output$regression_multinomial_coef <- renderTable(
  {
    withMathJax()
    fit <- regression_multinomial_model()

    key <- t(as.data.table(key()))
    data <- nominal()
    item <- input$regression_multinomial_item_slider

    if (input$regression_multinomial_parametrization == "classic") {
      dfhw <- na.omit(data.table(data[, item, with = FALSE]))
      temp <- as.factor(unlist(dfhw[, 1]))
      temp <- relevel(temp, ref = paste(key[item]))

      coefs <- as.vector(coef(fit))
      ses <- sqrt(diag(vcov(fit)))
      ses <- c(ses[grepl("Intercept", names(ses))], ses[!grepl("Intercept", names(ses))])
      tab <- cbind(coefs, ses)
      rnam <- rownames(coef(fit))
      if (is.null(dim(coef(fit))[1]) & !(all(levels(temp) %in% c("1", "0")))) {
        rnam <- rev(levels(temp))[1]
      }
      colnames(tab) <- c("Estimate", "SE")
      rownames(tab) <- c(
        paste0("%%mathit{\\beta}_{", item, "0", rnam, "}%%"),
        paste0("%%mathit{\\beta}_{", item, "1", rnam, "}%%")
      )
      tab
    } else {
      subst_vcov <- function(vcov, cat) {
        ind <- grep(cat, colnames(vcov))
        vcov[ind, ind]
      }

      coef_si <- coef(fit)
      varcov <- vcov(fit)

      if (is.null(dim(coef_si))) {
        coef_tab <- matrix(c(-coef_si[1] / coef_si[2], coef_si[2]), nrow = 1)
      } else {
        coef_tab <- cbind(-coef_si[, 1] / coef_si[, 2], coef_si[, 2])
      }

      se_tab <- if (is.null(dim(coef_si))) {
        matrix(msm::deltamethod(
          list(~ -x1 / x2, ~x2),
          mean = unlist(coef_si),
          cov = varcov,
          ses = TRUE
        ), nrow = 1)
      } else {
        t(sapply(
          rownames(coef_si),
          function(.x) {
            vcov_subset <- subst_vcov(varcov, .x)
            msm::deltamethod(
              list(~ -x1 / x2, ~x2),
              mean = coef_si[.x, ],
              cov = vcov_subset,
              ses = TRUE
            )
          }
        ))
      }

      tab <- data.frame(
        b = coef_tab[, 1],
        b_se = se_tab[, 1],
        a = coef_tab[, 2],
        a_se = se_tab[, 2]
      )
      withMathJax()
      colnames(tab) <- c(
        paste0(c("", "SE("), rep(paste0("%%mathit{b}_{", item, "}%%"), 2), c("", ")")),
        paste0(c("", "SE("), rep(paste0("%%mathit{a}_{", item, "}%%"), 2), c("", ")"))
      )
      tab
    }
  },
  rownames = TRUE
)

# ** Interpretation of parameters of curves of multinomial regression ####
output$regression_multinomial_interpretation <- renderUI({
  if (input$regression_multinomial_parametrization == "classic") {
    koef <- summary(regression_multinomial_model())$coefficients
    txt <- c()
    matching_name <- ifelse(input$regression_multinomial_matching == "total",
      "total score",
      "Z-score (one SD increase in original scores)"
    )

    if (is.null(dim(koef))) {
      m <- length(koef)
      txt0 <- ifelse(koef[2] < 0, "decrease", "increase")
      txt <- paste(
        "A one-unit increase in the", matching_name, "is associated with the ",
        txt0, " in the log odds of answering the item ", "<b> 0 </b>", "vs.",
        "<b> 1 </b>", " in the amount of ", "<b>", abs(round(koef[2], 2)), "</b>", "<br/>"
      )
    } else {
      m <- nrow(koef)
      for (i in 1:m) {
        txt0 <- ifelse(koef[i, 2] < 0, "decrease", "increase")
        txt[i] <- paste(
          "A one-unit increase in the", matching_name, "is associated with the ",
          txt0, " in the log odds of answering the item ", "<b>", row.names(koef)[i],
          "</b>", "vs.", "<b>", key()[input$regression_multinomial_item_slider], "</b>", "in the amount of ",
          "<b>", abs(round(koef[i, 2], 2)), "</b>", "<br/>"
        )
      }
    }
    HTML(c("<b>Interpretation</b><br>", txt, "<br>"))
  }
})

# ** Warning for missing values ####
output$regression_multinomial_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})
