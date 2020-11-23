# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# REGRESSION ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of logistic regression ######
logreg_model <- reactive({
  item <- input$logregSlider
  data <- binary()
  total_score <- total_score()

  model <- glm(unlist(data[, item, with = FALSE]) ~ total_score, family = binomial)
})

# ** Plot with estimated logistic curve ######
logreg_plot_Input <- reactive({
  total_score <- total_score()
  data <- binary()
  fit <- logreg_model()
  item <- input$logregSlider

  fun <- function(x, b0, b1) {
    exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
  }

  df <- data.table(
    x = sort(unique(total_score)),
    y = tapply(unlist(data[, item, with = FALSE]), total_score, mean),
    size = as.numeric(table(total_score))
  )

  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
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
      size = 1,
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

# ** Output estimated logistic curve ######
output$logreg_plot <- renderPlot({
  logreg_plot_Input()
})

# ** DB estimated logistic curve ######
output$DB_logreg_plot <- downloadHandler(
  filename = function() {
    paste("fig_LogisticRegressionCurve_", item_names()[input$logregSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = logreg_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of estimated parameters of logistic curve ######
output$coef_logreg_table <- renderTable(
  {
    item <- input$logregSlider
    tab <- summary(logreg_model())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{b}_{", item, "0}%%"),
      paste0("%%mathit{b}_{", item, "1}%%")
    )
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Interpretation of estimated parameters of logistic curve ######
output$logreg_interpretation <- renderUI({
  b1 <- coef(logreg_model())[2]
  b1 <- round(b1, 2)

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <- paste(
    "A one-unit increase in the total
    score is associated with the", txt0, " in the log
    odds of answering the item correctly
    vs. not correctly in the amount of"
  )
  txt3 <- paste("<b>", abs(b1), "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$logreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of logistic regression on Z-scores ######
z_logreg_model <- reactive({
  zscore <- z_score()
  item <- input$zlogregSlider
  data <- binary()
  model <- glm(unlist(data[, item, with = FALSE]) ~ zscore, family = "binomial")
})

# ** Plot of logistic regression on Z-scores ######
z_logreg_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$zlogregSlider
  data <- binary()
  fit <- z_logreg_model()

  fun <- function(x, b0, b1) {
    exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
  }

  df <- data.table(
    x = sort(unique(zscore)),
    y = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    size = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
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
      size = 1,
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

# ** Output plot of logistic regression on Z-scores ######
output$z_logreg_plot <- renderPlot({
  z_logreg_plot_Input()
})

# ** DB for plot of logistic regression on Z-scores ######
output$DB_z_logreg_plot <- downloadHandler(
  filename = function() {
    paste("fig_LogisticRegressionCurve_Zscores_", item_names()[input$zlogregSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = z_logreg_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of estimated parameters of logistic regression on Z-scores ######
output$coef_z_logreg <- renderTable(
  {
    item <- input$zlogregSlider
    tab <- summary(z_logreg_model())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{b}_{", item, "0}%%"),
      paste0("%%mathit{b}_{", item, "1}%%")
    )
    tab
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# * Interpretation of estimated parameters of logistic regression on Z-scores ######
output$z_logreg_interpretation <- renderUI({
  fit <- z_logreg_model()

  b1 <- round(summary(fit)$coef[2, 1], 2)
  b0 <- round(summary(fit)$coef[1, 1], 2)

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <-
    paste(
      "A one-unit increase in the Z-score (one SD increase in original
      scores) is associated with the", txt0, " in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
  txt3 <- paste("<b>", abs(b1), "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$z_logreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC IRT Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for logistic regression on Z scores with IRT param. ######
z_logreg_irt_model <- reactive({
  zscore <- z_score()
  item <- input$zlogreg_irtSlider
  data <- binary()

  model <- glm(unlist(data[, item, with = FALSE]) ~ zscore, family = "binomial")
})

# ** Plot with estimated logistic curve on Z scores with IRT param. ######
z_logreg_irt_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$zlogreg_irtSlider
  data <- binary()
  fit <- z_logreg_irt_model()

  fun <- function(x, b0, b1) {
    exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
  }

  df <- data.table(
    x = sort(unique(zscore)),
    y = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    size = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
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
      size = 1,
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

# ** Output plot with estimated logistic curve on Z scores with IRT param. ######
output$z_logreg_irt_plot <- renderPlot({
  z_logreg_irt_plot_Input()
})

# ** DB plot with estimated logistic curve on Z scores with IRT param. ######
output$DB_z_logreg_irt_plot <- downloadHandler(
  filename = function() {
    paste("fig_LogisticRegressionCurve_Zscores_IRT_", item_names()[input$zlogreg_irtSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = z_logreg_irt_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Table of estimated parameters of logistic curve on Z scores with IRT param. ######
output$coef_z_logreg_irt <- renderTable(
  {
    fit <- z_logreg_irt_model()
    tab_coef_old <- coef(fit)
    item <- input$zlogreg_irtSlider

    # delta method
    g <- list(~x2, ~ -x1 / x2)
    cov <- vcov(fit)
    cov <- as.matrix(cov)
    syms <- paste("x", 1:2, sep = "")
    for (i in 1:2) assign(syms[i], tab_coef_old[i])
    gdashmu <- t(sapply(g, function(form) {
      as.numeric(attr(eval(deriv(form, syms)), "gradient"))
      # in some shiny v. , envir = parent.frame() in eval() needs to be added
    }))
    new.covar <- gdashmu %*% cov %*% t(gdashmu)
    tab_sd <- sqrt(diag(new.covar))

    tab_coef <- c(tab_coef_old[2], -tab_coef_old[1] / tab_coef_old[2])
    tab <- cbind(tab_coef, tab_sd)
    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{a}_{", item, "}%%"),
      paste0("%%mathit{b}_{", item, "}%%")
    )
    tab
  },
  include.rownames = TRUE
)

# ** Interpretation of estimated parameters of logistic curve on Z scores with IRT param. ######
output$z_logreg_irt_interpretation <- renderUI({
  fit <- z_logreg_irt_model()

  b1 <- round(summary(fit)$coef[2, 1], 2)
  b0 <- round(summary(fit)$coef[1, 1], 2)

  txt1 <- paste("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <-
    paste(
      "A one-unit increase in the Z-score (one SD increase in original
      scores) is associated with the", txt0, " in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
  txt3 <- paste("<b>", abs(b1), "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$z_logreg_irt_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 3P IRT Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of nonlinear curve ######
nlr_3P_model <- reactive({
  data <- binary()
  zscore <- z_score()
  item <- input$slider_nlr_3P_item

  if (any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data,
    group = c(rep(0, nrow(data) / 2), rep(1, nrow(data) / 2)),
    model = "3PLcg", parameterization = "classic", simplify = T
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
  ))

  fit
})

# ** Plot of estimated nonlinear curve ######
nlr_3P_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$slider_nlr_3P_item
  data <- binary()

  fit <- nlr_3P_model()

  fun <- function(x, a, b, c) {
    c + (1 - c) / (1 + exp(-a * (x - b)))
  }

  df <- data.table(
    x = sort(unique(zscore)),
    y = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    size = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
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
      size = 1,
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

# ** Output plot of estimated nonlinear curve ######
output$nlr_3P_plot <- renderPlot({
  nlr_3P_plot_Input()
})

# ** DB plot of estimated nonlinear curve ######
output$DB_nlr_3P_plot <- downloadHandler(
  filename = function() {
    paste("fig_NLR_3P_", item_names()[input$slider_nlr_3P_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = nlr_3P_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# Table of estimated parameters of nonlinear curve ######
output$coef_nlr_3P <- renderTable(
  {
    fit <- nlr_3P_model()
    item <- input$slider_nlr_3P_item

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

# ** Interpretation of estimated parameters of nonlinear curve ######
output$nlr_3P_interpretation <- renderUI({
  fit <- nlr_3P_model()

  a <- round(coef(fit)[1], 2)
  b <- round(coef(fit)[2], 2)
  c <- round(coef(fit)[3], 2)

  txt0 <- paste0("<b>", "Interpretation:", "</b>")
  txt1 <- paste0(
    "A one-unit increase in the Z-score (one SD increase in original scores) is associated with the ",
    ifelse(a < 0, "decrease", "increase"), " in the log odds of answering the item correctly vs.
                 not correctly in the amount of <b>", abs(a), "</b>. "
  )
  txt2 <- paste0("Probability of guessing is <b>", c, "</b>. ")

  HTML(paste0(txt0, txt1, txt2))
})

# ** Warning for missing values ####
output$nlr_3P_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 4P IRT Z ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of nonlinear curve ######
nlr_4P_model <- reactive({
  data <- binary()
  zscore <- z_score()
  item <- input$slider_nlr_4P_item

  if (any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data,
    group = c(rep(0, nrow(data) / 2), rep(1, nrow(data) / 2)),
    model = "4PLcgdg", parameterization = "classic", simplify = T
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
  ))

  fit
})

# ** Plot of estimated nonlinear curve ######
nlr_4P_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$slider_nlr_4P_item
  data <- binary()
  fit <- nlr_4P_model()

  fun <- function(x, a, b, c, d) {
    c + (d - c) / (1 + exp(-a * (x - b)))
  }

  df <- data.table(
    x = sort(unique(zscore)),
    y = tapply(unlist(data[, item, with = FALSE]), zscore, mean),
    size = as.numeric(table(zscore))
  )
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
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
      size = 1,
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

# ** Output plot of estimated nonlinear curve ######
output$nlr_4P_plot <- renderPlot({
  nlr_4P_plot_Input()
})

# ** DB plot of estimated nonlinear curve ######
output$DB_nlr_4P_plot <- downloadHandler(
  filename = function() {
    paste0("fig_NLR_4P", item_names()[input$slider_nlr_4P_item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = nlr_4P_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# Table of estimated parameters of nonlinear curve ######
output$coef_nlr_4P <- renderTable(
  {
    fit <- nlr_4P_model()
    item <- input$slider_nlr_4P_item

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

# ** Interpretation of estimated parameters of nonlinear curve ######
output$nlr_4P_interpretation <- renderUI({
  fit <- nlr_4P_model()

  a <- round(coef(fit)[1], 2)
  b <- round(coef(fit)[2], 2)
  c <- round(coef(fit)[3], 2)
  d <- round(coef(fit)[4], 2)

  txt0 <- paste0("<b>", "Interpretation: ", "</b>")
  txt1 <- paste0("A one-unit increase in the Z-score (one SD increase in original scores) is associated
                  with the ", ifelse(a < 0, "decrease", "increase"), " in the log odds of answering the item correctly vs. not correctly
                  in the amount of <b>", abs(a), "</b>. ")
  txt2 <- paste0("Probability of guessing is <b>", c, "</b>. ")
  txt3 <- paste0("Probability of inattention is <b>", round(1 - d, 2), "</b>. ")
  HTML(paste0(txt0, txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$nlr_4P_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output$regr_comp_table <- DT::renderDataTable({
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
# * CUMULATIVE LOGISTIC ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion for cumulative model ######
cumreg_matching <- reactive({
  if (input$cumreg_matching == "total") {
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Update select input for different parametrization ######
observe({
  if (input$cumreg_parametrization == "irt") {
    updateSelectInput(
      session = session,
      inputId = "cumreg_matching",
      selected = "zscore"
    )
  }
})

# ** Model for cumulative logistic regression ######
cumreg_model <- reactive({
  matching <- cumreg_matching()
  data <- as.data.frame(ordinal())
  maxval <- sapply(data, function(x) max(x, na.rm = TRUE))

  for (i in 1:ncol(data)) {
    data[, i] <- factor(data[, i], levels = 0:maxval[i])
  }

  fit.cum <- apply(
    data, 2,
    function(x) {
      VGAM::vglm(x ~ matching,
        family = cumulative(reverse = TRUE, parallel = TRUE)
      )
    }
  )
  fit.cum
})

# ** Plot with cumulative curves ######
cumreg_plot_cum_Input <- reactive({
  item <- input$cumreg_slider_item
  fit.cum <- cumreg_model()
  matching.name <- ifelse(input$cumreg_matching == "total", "Total score", "Standardized total score")

  g <- plotCumulative(fit.cum[[item]], type = "cumulative", matching.name = matching.name) +
    ggtitle(item_names()[item])

  g
})

# ** Output plot with estimated curves of cumulative regression ######
output$cumreg_plot_cum <- renderPlot({
  cumreg_plot_cum_Input()
})

# ** DB plot with estimated curves of cumulative regression ######
output$DB_cumreg_plot_cum <- downloadHandler(
  filename = function() {
    paste("fig_CumulativeRegressionCurve_cumulative_", item_names()[input$cumreg_slider_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = cumreg_plot_cum_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Calculation of empirical cumulative probabilities ######
cumreg_plot_cat_Input <- reactive({
  item <- input$cumreg_slider_item
  fit.cum <- cumreg_model()
  matching.name <- ifelse(input$cumreg_matching == "total", "Total score", "Standardized total score")

  g <- plotCumulative(fit.cum[[item]], type = "category", matching.name = matching.name) +
    ggtitle(item_names()[item])
  g
})

# ** Output plot with estimated curves of cumulative regression ######
output$cumreg_plot_cat <- renderPlot({
  cumreg_plot_cat_Input()
})

# ** DB plot with estimated curves of cumulative regression ######
output$DB_cumreg_plot_cat <- downloadHandler(
  filename = function() {
    paste("fig_CumulativeRegressionCurve_category_", item_names()[input$cumreg_slider_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = cumreg_plot_cat_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Equation ######
output$cumreg_equation <- renderUI({
  txt1 <- ifelse(input$cumreg_matching == "total", "X_p", "Z_p")

  if (input$cumreg_parametrization == "classic") {
    txt2 <- paste("b_{i0k} + b_{i1}", txt1)
  } else {
    txt2 <- paste("a_i(", txt1, "- d_{i0k})")
  }

  txt <- paste("$$\\mathrm{P}(Y_{pi} \\geq k|", txt1, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")

  withMathJax(HTML(txt))
})

# ** Interpretation ######
output$cumreg_interpretation <- renderUI({
  if (input$cumreg_parametrization == "classic") {
    par <- c("\\(b_{i0k}\\)", "\\(b_{i1}\\)")
  } else {
    par <- c("\\(d_{i0k}\\)", "\\(a_i\\)")
  }

  txt <- paste(
    "Parameters", par[1], "describe horizontal position of the fitted curves for item \\(i\\),
               where \\(k = 0, 1, 2, ...\\) is a number of obtained scores, parameter", par[2],
    "describes their common slope. Category probabilities are then calculated as
               differences of two subsequent cumulative probabilities. "
  )
  withMathJax(HTML(txt))
})

# ** Table of parameters ######
cumreg_coef_tab_Input <- reactive({
  fit.cum <- cumreg_model()
  item <- input$cumreg_slider_item
  data <- as.data.frame(ordinal())

  if (input$cumreg_parametrization == "classic") {
    tab.coef <- coef(fit.cum[[item]])
    tab.se <- sqrt(diag(vcov(fit.cum[[item]])))

    tab <- data.frame(Estimate = tab.coef, SE = tab.se)
    rownames(tab) <- c(
      paste0("%%mathit{b}_{", item, "0", sort(unique(data[, item]))[-1], "}%%"),
      paste0("%%mathit{b}_{", item, "1}%%")
    )
  } else {
    tab.coef.tmp <- coef(fit.cum[[item]])
    c <- length(tab.coef.tmp)

    tab.coef <- c(-tab.coef.tmp[-c] / tab.coef.tmp[c], tab.coef.tmp[c])

    Sigma <- vcov(fit.cum[[item]])
    c <- length(tab.coef)
    D <- matrix(0, nrow = c, ncol = c)
    diag(D)[-c] <- -1 / tab.coef.tmp[c]
    D[c, ] <- c(tab.coef.tmp[1:(c - 1)] / (tab.coef.tmp[c])^2, 1)

    Sigma.new <- t(D) %*% Sigma %*% D

    tab <- data.frame(Estimate = tab.coef, SE = sqrt(diag(Sigma.new)))
    rownames(tab) <- c(
      paste0("%%mathit{d}_{", item, "0", sort(unique(data[, item]))[-1], "}%%"),
      paste0("%%mathit{a}_{", item, "}%%")
    )
  }

  tab
})

output$cumreg_coef_tab <- renderTable(
  {
    cumreg_coef_tab_Input()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Warning for missing values ####
output$cumreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT LOGISTIC ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion adjacent logistic regression ######
adjreg_matching <- reactive({
  if (input$adjreg_matching == "total") {
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Update select input for different parametrization ######
observe({
  if (input$adjreg_parametrization == "irt") {
    updateSelectInput(
      session = session,
      inputId = "adjreg_matching",
      selected = "zscore"
    )
  }
})

# ** Model for adjacent regression ######
adjreg_model <- reactive({
  matching <- adjreg_matching()
  data <- as.data.frame(ordinal())
  maxval <- sapply(data, function(x) max(x, na.rm = TRUE))

  for (i in 1:ncol(data)) {
    data[, i] <- factor(data[, i], levels = 0:maxval[i])
  }

  fit.adj <- apply(data, 2, function(x) {
    vglm(x ~ matching, family = acat(reverse = FALSE, parallel = TRUE))
  })

  fit.adj
})

# ** Plot with category curves ######
adjreg_plot_cat_Input <- reactive({
  item <- input$adjreg_slider_item
  fit.adj <- adjreg_model()
  matching.name <- ifelse(input$adjreg_matching == "total", "Total score", "Standardized total score")

  g <- plotAdjacent(fit.adj[[item]], matching.name = matching.name) +
    ggtitle(item_names()[item])

  g
})

# ** Output plot with estimated curves of adjacent regression ######
output$adjreg_plot_cat <- renderPlot({
  adjreg_plot_cat_Input()
})

# ** DB plot with estimated curves of adjacent regression ######
output$DB_adjreg_plot_cat <- downloadHandler(
  filename = function() {
    paste0("fig_AdjacentRegressionCurve_category_", item_names()[input$adjreg_slider_item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = adjreg_plot_cat_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Equation ######
output$adjreg_equation <- renderUI({
  txt1 <- ifelse(input$adjreg_matching == "total", "X_p", "Z_p")

  if (input$adjreg_parametrization == "classic") {
    txt2 <- paste0("b_{i0t} + b_{i1}", txt1)
  } else {
    txt2 <- paste0("a_i(", txt1, " - d_{i0k})")
  }

  txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt1, ") = \\frac{e^{\\sum_{t = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K_i}e^{\\sum_{t = 0}^{r}", txt2, "}}$$")

  withMathJax(HTML(txt))
})

# ** Interpretation ######
output$adjreg_interpretation <- renderUI({
  if (input$adjreg_parametrization == "classic") {
    par <- c("\\(b_{i0k}\\)", "\\(b_{i1}\\)")
  } else {
    par <- c("\\(d_{i0k}\\)", "\\(a_i\\)")
  }

  txt <- paste(
    "Parameters", par[1], "describe horizontal position of the fitted curves for item \\(i\\),
               where \\(k = 0, 1, 2, ...\\) is a number of obtained scores, parameter", par[2],
    "describes their common slope. "
  )

  withMathJax(HTML(txt))
})

# ** Table of parameters ######
adjreg_coef_tab_Input <- reactive({
  item <- input$adjreg_slider_item
  fit.adj <- adjreg_model()[[item]]
  data <- as.data.frame(ordinal())

  if (input$adjreg_parametrization == "classic") {
    tab.coef <- coef(fit.adj)
    tab.se <- sqrt(diag(vcov(fit.adj)))

    tab <- data.frame(Estimate = tab.coef, SE = tab.se)
    rownames(tab) <- c(
      paste0("%%mathit{b}_{", item, "0", sort(unique(data[, item]))[-1], "}%%"),
      paste0("%%mathit{b}_{", item, "1}%%")
    )
  } else {
    tab.coef.tmp <- coef(fit.adj)
    c <- length(tab.coef.tmp)

    tab.coef <- c(-tab.coef.tmp[-c] / tab.coef.tmp[c], tab.coef.tmp[c])

    Sigma <- vcov(fit.adj)
    c <- length(tab.coef)
    D <- matrix(0, nrow = c, ncol = c)
    diag(D)[-c] <- -1 / tab.coef.tmp[c]
    D[c, ] <- c(tab.coef.tmp[1:(c - 1)] / (tab.coef.tmp[c])^2, 1)

    Sigma.new <- t(D) %*% Sigma %*% D

    tab <- data.frame(Estimate = tab.coef, SE = sqrt(diag(Sigma.new)))
    rownames(tab) <- c(
      paste0("%%mathit{d}_{", item, "0", sort(unique(data[, item]))[-1], "}%%"),
      paste0("%%mathit{a}_{", item, "}%%")
    )
  }

  tab
})

output$adjreg_coef_tab <- renderTable(
  {
    adjreg_coef_tab_Input()
  },
  include.rownames = TRUE,
  include.colnames = TRUE
)

# ** Warning for missing values ####
output$adjreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion for multinomial regression ######
multi_matching <- reactive({
  if (input$multi_matching == "total") {
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Model for multinomial regression ######
multi_model <- reactive({
  matching <- multi_matching()
  key <- t(as.data.table(key()))
  item <- input$multi_slider_item
  data <- nominal()

  dfhw <- data.table(data[, item, with = FALSE], matching)
  dfhw <- dfhw[complete.cases(dfhw), ]

  fit <- tryCatch(multinom(relevel(as.factor(unlist(dfhw[, 1])),
    ref = paste(key[item])
  ) ~ unlist(dfhw[, 2]),
  trace = F
  ),
  error = function(e) e
  )

  validate(need(
    class(fit) == "nnet",
    HTML(paste0("Error: Method cannot be fitted for item ", item, ". The error message returned: ", fit$message))
  ))

  fit
})

# ** Plot with estimated curves of multinomial regression ######
multi_plot_Input <- reactive({
  fit <- multi_model()

  matching <- multi_matching()
  item <- input$multi_slider_item
  data <- nominal()

  dfhw <- data.table(data[, item, with = FALSE], matching)
  matching <- unlist(dfhw[complete.cases(dfhw), 2])

  matching_name <- ifelse(input$multi_matching == "total", "Total score", "Standardized total score")

  g <- plotMultinomial(fit, matching, matching.name = matching_name) +
    ggtitle(item_names()[item])
  g
})

# ** Output plot with estimated curves of multinomial regression ######
output$multi_plot <- renderPlot({
  multi_plot_Input()
})

# ** DB plot with estimated curves of multinomial regression ######
output$DB_multi_plot <- downloadHandler(
  filename = function() {
    paste("fig_MultinomialRegressionCurve_", item_names()[input$multi_slider_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file,
      plot = multi_plot_Input() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Reports: Plot with estimated curves of multinomial regression ######
multiplotReportInput <- reactive({
  graflist <- list()
  key <- unlist(key())
  data <- nominal()
  matching <- multi_matching()
  matching_name <- ifelse(input$multi_matching == "total", "Total score", "Standardized total score")

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
      # g <- ggplotGrob(g)
      graflist[[item]] <- g
    }
  }

  null_idx_true <- which(sapply(graflist, is.null))
  null_idx_false <- which(!sapply(graflist, is.null))

  graflist
})

# ** Reports: Length of legend in multinomial plot ######
report_distractor_plot_legend_length <- reactive({
  data <- nominal()

  legend_length <- max(sapply(data, function(x) length(unique(x))), na.rm = TRUE)
  legend_length
})

# AH: momentalne zakomentovavam, protoze stranka Reports hrozne dlouho nabiha
# # ** Warning for report ######
# output$report_multinomial_report <- renderUI({
#   items <- multiplotReportInput()[[2]]
#   messages <- unique(multiplotReportInput()[[3]])
#   txt <- paste0("<font color = 'orange'>
# 				Multinomial plot for items ", items, " is missing.
#         Error message returned: ", strong(messages[2]),".
# 				</font>")
#   HTML(txt)
# })

# ** Equation of multinomial regression ######
output$multi_equation <- renderUI({
  req(multi_model()[[2]])

  txt1 <- ifelse(input$multi_matching == "total", "X_p", "Z_p")
  cor_option <- key()[input$multi_slider_item]
  withMathJax(
    sprintf(
      "$$\\mathrm{P}(Y_{pi} = k|%s) = \\frac{e^{\\left(b_{i0k} + b_{i1k} %s\\right)}}{1 + \\sum_{l} e^{\\left(b_{i0l} + b_{i1l} %s\\right)}}, \\\\
      \\mathrm{P}(Y_{pi} = %s|%s) = \\frac{1}{1 + \\sum_l e^{\\left(b_{i0l} + b_{i1l} %s\\right)}}, \\\\
      \\text{where } k \\text{ is one of the wrong options (distractors) and } %s \\text{ is the correct one. }$$",
      txt1, txt1, txt1, cor_option, txt1, txt1, cor_option
    )
  )
})

# ** Table of parameters of curves of multinomial regression ######
output$coef_multi <- renderTable(
  {
    fit <- multi_model()

    key <- t(as.data.table(key()))
    data <- nominal()
    item <- input$multi_slider_item

    dfhw <- na.omit(data.table(data[, item, with = FALSE]))
    temp <- as.factor(unlist(dfhw[, 1]))
    temp <- relevel(temp, ref = paste(key[item]))

    koef <- as.vector(coef(fit))
    std <- as.vector(sqrt(diag(vcov(fit))))
    tab <- cbind(koef, std)
    rnam <- rownames(coef(fit))
    if (is.null(dim(coef(fit))[1]) & !(all(levels(temp) %in% c("1", "0")))) {
      rnam <- rev(levels(temp))[1]
    }

    colnames(tab) <- c("Estimate", "SE")
    rownames(tab) <- c(
      paste0("%%mathit{b}_{", item, rnam, "0}%%"),
      paste0("%%mathit{b}_{", item, rnam, "1}%%")
    )
    tab
  },
  include.rownames = TRUE
)

# ** Interpretation of parameters of curves of multinomial regression ######
output$multi_interpretation <- renderUI({
  koef <- summary(multi_model())$coefficients
  txt <- c()
  matching_name <- ifelse(input$multi_matching == "total",
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
        "</b>", "vs.", "<b>", key()[input$multi_slider_item], "</b>", "in the amount of ",
        "<b>", abs(round(koef[i, 2], 2)), "</b>", "<br/>"
      )
    }
  }
  HTML(paste(txt))
})

# ** Warning for missing values ####
output$multi_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})
