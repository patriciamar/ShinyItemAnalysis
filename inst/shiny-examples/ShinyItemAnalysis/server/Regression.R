#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# REGRESSION ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of logistic regression ######
logreg_model <- reactive ({
  item <- input$logregSlider
  data <- binary()
  total_score <- total_score()

  model <- glm(unlist(data[, item, with = F]) ~ total_score, family = binomial)
})

# ** Plot with estimated logistic curve ######
logreg_plot_Input <- reactive({
  total_score <- total_score()
  data <- binary()
  fit <- logreg_model()
  item <- input$logregSlider

  fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

  df <- data.table(x = sort(unique(total_score)),
                   y = tapply(unlist(data[, item, with = F]), total_score, mean),
                   size = as.numeric(table(total_score)))

  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
               color = "darkblue",
               fill = "darkblue",
               shape = 21, alpha = 0.5) +
    stat_function(fun = fun, geom = "line",
                  args = list(b0 = coef(fit)[1],
                              b1 = coef(fit)[2]),
                  size = 1,
                  color = "darkblue") +
    xlab("Total score") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    theme_app() +
    theme(legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1)) +
    ggtitle(item_names()[item])
})

# ** Output estimated logistic curve ######
output$logreg_plot <- renderPlot({
  logreg_plot_Input()
})

# ** DB estimated logistic curve ######
output$DB_logreg_plot <- downloadHandler(
  filename =  function() {
    paste("fig_LogisticRegressionCurve_", item_names()[input$logregSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = logreg_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of estimated parameters of logistic curve ######
output$coef_logreg_table <- renderTable({

  tab <- summary(logreg_model())$coef[1:2, 1:2]
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c("%%mathit{b}_{0}%%", "%%mathit{b}_{1}%%")
  tab
},
include.rownames = T,
include.colnames = T)



# ** Interpretation of estimated parameters of logistic curve ######
output$logreg_interpretation <- renderUI({
  b1 <- coef(logreg_model())[2]
  b1 <- round(b1, 2)

  txt1 <- paste ("<b>", "Interpretation:","</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <- paste (
    "A one-unit increase in the total
    score is associated with the", txt0, " in the log
    odds of answering the item correctly
    vs. not correctly in the amount of"
  )
  txt3 <- paste ("<b>", abs(b1), "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$logreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of logistic regression on Z-scores ######
z_logreg_model <- reactive({
  zscore <- z_score()
  item <- input$zlogregSlider
  data <- binary()
  model <- glm(unlist(data[, item, with = F]) ~ zscore, family = "binomial")
})

# ** Plot of logistic regression on Z-scores ######
z_logreg_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$zlogregSlider
  data <- binary()
  fit <- z_logreg_model()

  fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

  df <- data.table(x = sort(unique(zscore)),
                   y = tapply(unlist(data[, item, with = F]), zscore, mean),
                   size = as.numeric(table(zscore)))
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
               color = "darkblue",
               fill = "darkblue",
               shape = 21, alpha = 0.5) +
    stat_function(fun = fun, geom = "line",
                  args = list(b0 = coef(fit)[1],
                              b1 = coef(fit)[2]),
                  size = 1,
                  color = "darkblue") +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1)) +
    ggtitle(item_names()[item])
})

# ** Output plot of logistic regression on Z-scores ######
output$z_logreg_plot <- renderPlot({
  z_logreg_plot_Input()
})

# ** DB for plot of logistic regression on Z-scores ######
output$DB_z_logreg_plot <- downloadHandler(
  filename =  function() {
    paste("fig_LogisticRegressionCurve_Zscores_", item_names()[input$zlogregSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = z_logreg_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of estimated parameters of logistic regression on Z-scores ######
output$coef_z_logreg <- renderTable({

  tab <- summary(z_logreg_model())$coef[1:2, 1:2]
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c("%%mathit{b}_{0}%%", "%%mathit{b}_{1}%%")
  tab
},
include.rownames = T,
include.colnames = T)



# * Interpretation of estimated parameters of logistic regression on Z-scores ######
output$z_logreg_interpretation <- renderUI({
  fit <- z_logreg_model()

  b1 <- round(summary(fit)$coef[2, 1], 2)
  b0 <- round(summary(fit)$coef[1, 1], 2)

  txt1 <- paste ("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <-
    paste (
      "A one-unit increase in the Z-score (one SD increase in original
      scores) is associated with the", txt0, " in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
  txt3 <- paste ("<b>", abs(b1), "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$z_logreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * LOGISTIC IRT Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for logistic regression on Z scores with IRT param. ######
z_logreg_irt_model <- reactive({
  zscore <- z_score()
  item <- input$zlogreg_irtSlider
  data <- binary()

  model <- glm(unlist(data[, item, with = F]) ~ zscore, family = "binomial")
})

# ** Plot with estimated logistic curve on Z scores with IRT param. ######
z_logreg_irt_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$zlogreg_irtSlider
  data <- binary()
  fit <- z_logreg_irt_model()

  fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

  df <- data.table(x = sort(unique(zscore)),
                   y = tapply(unlist(data[, item, with = F]), zscore, mean),
                   size = as.numeric(table(zscore)))
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
               color = "darkblue",
               fill = "darkblue",
               shape = 21, alpha = 0.5) +
    stat_function(fun = fun, geom = "line",
                  args = list(b0 = coef(fit)[1],
                              b1 = coef(fit)[2]),
                  size = 1,
                  color = "darkblue") +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1)) +
    ggtitle(item_names()[item])
})

# ** Output plot with estimated logistic curve on Z scores with IRT param. ######
output$z_logreg_irt_plot <- renderPlot({
  z_logreg_irt_plot_Input()
})

# ** DB plot with estimated logistic curve on Z scores with IRT param. ######
output$DB_z_logreg_irt_plot <- downloadHandler(
  filename =  function() {
    paste("fig_LogisticRegressionCurve_Zscores_IRT_", item_names()[input$zlogreg_irtSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = z_logreg_irt_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Table of estimated parameters of logistic curve on Z scores with IRT param. ######
output$coef_z_logreg_irt <- renderTable({
  fit <- z_logreg_irt_model()
  tab_coef_old <- coef(fit)

  # delta method
  g <- list( ~ x2,  ~ -x1/x2)
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

  tab_coef <- c(tab_coef_old[2], -tab_coef_old[1]/tab_coef_old[2])
  tab <- cbind(tab_coef, tab_sd)
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c("%%mathit{a}%%", "%%mathit{b}%%")
  tab
},
include.rownames = T)



# ** Interpretation of estimated parameters of logistic curve on Z scores with IRT param. ######
output$z_logreg_irt_interpretation <- renderUI({
  fit <- z_logreg_irt_model()

  b1 <- round(summary(fit)$coef[2, 1], 2)
  b0 <- round(summary(fit)$coef[1, 1], 2)

  txt1 <- paste ("<b>", "Interpretation:", "</b>")
  txt0 <- ifelse(b1 < 0, "decrease", "increase")
  txt2 <-
    paste (
      "A one-unit increase in the Z-score (one SD increase in original
      scores) is associated with the", txt0, " in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
  txt3 <- paste ("<b>", abs(b1), "</b>")
  HTML(paste(txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$z_logreg_irt_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 3P IRT Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of nonlinear curve ######
nlr_3P_model <- reactive({
  data <- binary()
  zscore <- z_score()
  i <- input$slider_nlr_3P_item

  if(any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }


  start <- startNLR(data, group = c(rep(0, nrow(data)/2), rep(1, nrow(data)/2)),
                    model = "3PLcg", parameterization = "classic", simplify = T)[, 1:3]

  glr <- function(x, a, b, c){c + (1 - c) / (1 + exp(-a * (x - b)))}

  fit <- nls(unlist(data[, i, with = F]) ~ glr(zscore, a, b, c),
             algorithm = "port", start = start[i, ],
             lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, 1))
  fit
})

# ** Plot of estimated nonlinear curve ######
nlr_3P_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$slider_nlr_3P_item
  data <- binary()
  fit <- nlr_3P_model()

  fun <- function(x, a, b, c){c + (1 - c) / (1 + exp(-a * (x - b)))}

  df <- data.table(x = sort(unique(zscore)),
                   y = tapply(unlist(data[, item, with = F]), zscore, mean),
                   size = as.numeric(table(zscore)))
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
               color = "darkblue",
               fill = "darkblue",
               shape = 21, alpha = 0.5) +
    stat_function(fun = fun, geom = "line",
                  args = list(a = coef(fit)[1],
                              b = coef(fit)[2],
                              c = coef(fit)[3]),
                  size = 1,
                  color = "darkblue") +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1)) +
    ggtitle(item_names()[item])
})

# ** Output plot of estimated nonlinear curve ######
output$nlr_3P_plot <- renderPlot({
  nlr_3P_plot_Input()
})

# ** DB plot of estimated nonlinear curve ######
output$DB_nlr_3P_plot <- downloadHandler(
  filename =  function() {
    paste("fig_NLR_3P_", item_names()[input$slider_nlr_3P_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = nlr_3P_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# Table of estimated parameters of nonlinear curve ######
output$coef_nlr_3P <- renderTable({
  fit <- nlr_3P_model()

  tab <- summary(fit)$parameters[, 1:2]
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c("%%mathit{a}%%","%%mathit{b}%%","%%mathit{c}%%")
  tab
},
include.rownames = T,
include.colnames = T
)


# ** Interpretation of estimated parameters of nonlinear curve ######
output$nlr_3P_interpretation <- renderUI({
  fit <- nlr_3P_model()

  a <- round(coef(fit)[1], 2)
  b <- round(coef(fit)[2], 2)
  c <- round(coef(fit)[3], 2)

  txt0 <- paste0("<b>", "Interpretation:", "</b>")
  txt1 <- paste0("A one-unit increase in the Z-score (one SD increase in original scores) is associated with the ",
                 ifelse(a < 0, "decrease", "increase"), " in the log odds of answering the item correctly vs.
                 not correctly in the amount of <b>", abs(a), "</b>. ")
  txt2 <- paste0("Probability of guessing is <b>", c, "</b>. ")

  HTML(paste0(txt0, txt1, txt2))
})

# ** Warning for missing values ####
output$nlr_3P_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 4P IRT Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of nonlinear curve ######
nlr_4P_model <- reactive({
  data <- binary()
  zscore <- z_score()
  i <- input$slider_nlr_4P_item

  if(any(is.na(zscore))) {
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data, group = c(rep(0, nrow(data)/2), rep(1, nrow(data)/2)),
                    model = "4PLcgdg", parameterization = "classic", simplify = T)[, 1:4]

  glr <- function(x, a, b, c, d){c + (d - c) / (1 + exp(-a * (x - b)))}

  fit <- nls(unlist(data[, i, with = F]) ~ glr(zscore, a, b, c, d),
             algorithm = "port", start = start[i, ],
             lower = c(-Inf, -Inf, 0, 0), upper = c(Inf, Inf, 1, 1))
  fit
})

# ** Plot of estimated nonlinear curve ######
nlr_4P_plot_Input <- reactive({
  zscore <- z_score()
  item <- input$slider_nlr_4P_item
  data <- binary()
  fit <- nlr_4P_model()

  fun <- function(x, a, b, c, d){c + (d - c) / (1 + exp(-a * (x - b)))}

  df <- data.table(x = sort(unique(zscore)),
                   y = tapply(unlist(data[, item, with = F]), zscore, mean),
                   size = as.numeric(table(zscore)))
  ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = size),
               color = "darkblue",
               fill = "darkblue",
               shape = 21, alpha = 0.5) +
    stat_function(fun = fun, geom = "line",
                  args = list(a = coef(fit)[1],
                              b = coef(fit)[2],
                              c = coef(fit)[3],
                              d = coef(fit)[4]),
                  size = 1,
                  color = "darkblue") +
    xlab("Standardized total score (Z-score)") +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1)) +
    ggtitle(item_names()[item])
})

# ** Output plot of estimated nonlinear curve ######
output$nlr_4P_plot <- renderPlot({
  nlr_4P_plot_Input()
})

# ** DB plot of estimated nonlinear curve ######
output$DB_nlr_4P_plot <- downloadHandler(
  filename =  function() {
    paste0("fig_NLR_4P", item_names()[input$slider_nlr_4P_item], ".png")
  },
  content = function(file) {
    ggsave(file, plot = nlr_4P_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# Table of estimated parameters of nonlinear curve ######
output$coef_nlr_4P <- renderTable({
  fit <- nlr_4P_model()
  tab <- summary(fit)$parameters[, 1:2]
  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c("%%mathit{a}%%","%%mathit{b}%%","%%mathit{c}%%","%%mathit{d}%%")
  tab
},
include.rownames = T,
include.colnames = T
)



# ** Interpretation of estimated parameters of nonlinear curve ######
output$nlr_4P_interpretation <- renderUI({
  fit <- nlr_4P_model()

  a <- round(coef(fit)[1], 2)
  b <- round(coef(fit)[2], 2)
  c <- round(coef(fit)[3], 2)
  d <- round(coef(fit)[4], 2)

  txt0 <- paste0("<b>", "Interpretation: ", "</b>")
  txt1 <- paste0( "A one-unit increase in the Z-score (one SD increase in original scores) is associated
                  with the ", ifelse(a < 0, "decrease", "increase"), " in the log odds of answering the item correctly vs. not correctly
                  in the amount of <b>", abs(a), "</b>. ")
  txt2 <- paste0("Probability of guessing is <b>", c, "</b>. ")
  txt3 <- paste0("Probability of inattention is <b>", round(1-d,2), "</b>. ")
  HTML(paste0(txt0, txt1, txt2, txt3))
})

# ** Warning for missing values ####
output$nlr_4P_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output$regr_comp_table <- DT::renderDataTable({
  data <- binary()
  zscore <- z_score()

  m <- ncol(data)

  glr <- function(x, a, b, c, d){c + (d - c) / (1 + exp(-a * (x - b)))}

  # If zscore has NA values, those need to be ommited.
  # While nls does that automatically, nevertheless the indices of those values
  # would cause unlist(data[, i, with = F]) have higher dimension
  # than output of function glr(zscore, a, b, c = 0, d = 1).
  # Thus the following code is needed:

  if(any(is.na(zscore))){
    idx_NA <- which(is.na(zscore))
    data <- data[-idx_NA, ]
    zscore <- na.omit(zscore)
  }

  start <- startNLR(data, group = c(rep(0, nrow(data)/2), rep(1, nrow(data)/2)),
                    model = "4PLcgdg",
                    parameterization = "classic",
                    simplify = T)[, 1:4]


  fit2PL <- lapply(1:m, function(i) tryCatch(nls(unlist(data[, i, with = F]) ~  glr(zscore, a, b, c = 0, d = 1),
                                                 algorithm = "port", start = start[i, 1:2],
                                                 lower = c(-Inf, -Inf),
                                                 upper = c(Inf, Inf)), error = function(e) {
                                                   cat("ERROR : ", conditionMessage(e), "\n")}))

  fit3PL <- lapply(1:m, function(i) tryCatch(nls(unlist(data[, i, with = F]) ~  glr(zscore, a, b, c, d = 1),
                                                 algorithm = "port", start = start[i, 1:3],
                                                 lower = c(-Inf, -Inf, 0),
                                                 upper = c(Inf, Inf, 1)), error = function(e) {
                                                   cat("ERROR : ", conditionMessage(e), "\n")}))

  fit4PL <- lapply(1:m, function(i) tryCatch(nls(unlist(data[, i, with = F]) ~  glr(zscore, a, b, c, d),
                                                 algorithm = "port", start = start[i, 1:4],
                                                 lower = c(-Inf, -Inf, 0, 0),
                                                 upper = c(Inf, Inf, 1, 1)), error = function(e) {
                                                   cat("ERROR : ", conditionMessage(e), "\n")}))


  whok <- !c(sapply(fit2PL, is.null) | sapply(fit3PL, is.null) | sapply(fit4PL, is.null))
  AIC2PL <- AIC3PL <- AIC4PL <- BIC2PL <- BIC3PL <- BIC4PL <- rep(NA, m)

  AIC2PL[whok] <- sapply(fit2PL[whok], AIC)
  AIC3PL[whok] <- sapply(fit3PL[whok], AIC)
  AIC4PL[whok] <- sapply(fit4PL[whok], AIC)

  BIC2PL[whok] <- sapply(fit2PL[whok], BIC)
  BIC3PL[whok] <- sapply(fit3PL[whok], BIC)
  BIC4PL[whok] <- sapply(fit4PL[whok], BIC)

  bestAIC <- bestBIC <- rep(NA, m)
  dfAIC <- cbind(AIC2PL[whok], AIC3PL[whok], AIC4PL[whok])
  bestAIC[whok] <- paste0(apply(dfAIC, 1, function(x) which(x == min(x))[1]) + 1, "PL")

  dfBIC <- cbind(BIC2PL[whok], BIC3PL[whok], BIC4PL[whok])
  bestBIC[whok] <- paste0(apply(dfBIC, 1, function(x) which(x == min(x))[1]) + 1, "PL")

  LRstat23 <- LRpval23 <- rep(NA, m)
  LRstat23[whok] <- -2 * (sapply(fit2PL[whok], logLik) - sapply(fit3PL[whok], logLik))
  LRdf <- 1
  LRpval23[whok] <- 1 - pchisq(LRstat23[whok], LRdf)
  LRpval23 <- p.adjust(LRpval23, method = input$correction_method_regrmodels)

  LRstat34 <- LRpval34 <- rep(NA, m)
  LRstat34[whok] <- -2 * (sapply(fit3PL[whok], logLik) - sapply(fit4PL[whok], logLik))
  LRdf <- 1
  LRpval34[whok] <- 1 - pchisq(LRstat34[whok], LRdf)
  LRpval34 <- p.adjust(LRpval34, method = input$correction_method_regrmodels)

  bestLR <- ifelse(LRpval34 < 0.05, "4PL",
                   ifelse(LRpval23 < 0.05, "3PL", "2PL"))

  tab <- rbind(sprintf("%.2f", round(AIC2PL, 2)),
               sprintf("%.2f", round(AIC3PL, 2)),
               sprintf("%.2f", round(AIC4PL, 2)),
               bestAIC,
               sprintf("%.2f", round(BIC2PL, 2)),
               sprintf("%.2f", round(BIC3PL, 2)),
               sprintf("%.2f", round(BIC4PL, 2)),
               bestBIC,
               sprintf("%.2f", round(LRstat23, 3)),
               ifelse(round(LRpval23, 3) < 0.001, "<0.001",
                      sprintf("%.3f", round(LRpval23, 3))),
               sprintf("%.2f", round(LRstat34, 3)),
               ifelse(round(LRpval34, 3) < 0.001, "<0.001",
                      sprintf("%.3f", round(LRpval34, 3))),
               bestLR)

  tab <- as.data.table(tab)
  colnames(tab) <- item_names()
  rownames(tab) <- c("AIC 2PL", "AIC 3PL", "AIC 4PL", "BEST AIC",
                     "BIC 2PL", "BIC 3PL", "BIC 4PL", "BEST BIC",
                     "Chisq-value 2PL vs 3PL", "p-value 2PL vs 3PL",
                     "Chisq-value 3PL vs 4PL", "p-value 3PL vs 4PL",
                     "BEST LR")

  tab <- datatable(tab, rownames = T,
                   options = list(autoWidth = T,
                                  columnDefs = list(list(width = '140px', targets = list(0)),
                                                    list(width = '100px', targets = list(1:ncol(tab))),
                                                    list(targets = "_all")),
                                  scrollX = T,
                                  pageLength = 13,
                                  dom = 'tipr')) %>%
    formatStyle(0, target = 'row', fontWeight = styleEqual(c('BEST AIC', 'BEST BIC', 'BEST LR'),
                                                           c('bold', 'bold', 'bold')))
  tab

})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CUMULATIVE LOGISTIC ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Matching criterion for cumulative model ######
cumreg_matching <- reactive({
  if (input$cumreg_matching == "total"){
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Update select input for different parametrization ######
observe({
  if (input$cumreg_parametrization == "irt")
    updateSelectInput(session = session,
                      inputId = "cumreg_matching",
                      selected = "zscore")
})

# ** Model for cumulative logistic regression ######
cumreg_model <- reactive({
  matching <- cumreg_matching()
  data <- as.data.frame(ordinal())
  maxval <- sapply(data, function(x) max(x, na.rm = TRUE))

  for (i in 1:ncol(data)){
    data[, i] <- factor(data[, i], levels = 0:maxval[i])
  }

  fit.cum <- apply(data, 2,
                   function(x) VGAM::vglm(x ~ matching,
                                          family = cumulative(reverse = TRUE, parallel = TRUE)))
  fit.cum
})


# ** Calculation of sizes ######
cumreg_calculation_sizes <- reactive({
  data <- as.data.frame(ordinal())
  maxval <- sapply(data, function(x) max(x, na.rm = TRUE))
  matching <- cumreg_matching()

  # relevel data
  for (i in 1:ncol(data)){
    data[, i] <- factor(data[, i], levels = 0:maxval[i])
  }

  # calculation of number of observations wrt matching
  sizes <- list()
  for (i in 1:ncol(data)) {
    sizes[[i]] <- ftable(xtabs(~ data[, i] + matching))
  }

  sizes
})

# ** Calculation of empirical cumulative probabilities ######
cumreg_calculation_cumprob_points <- reactive({
  matching <- cumreg_matching()
  sizes <- cumreg_calculation_sizes()

  # calculation of cumulative probabilities wrt matching
  dfs <- list()
  for(i in 1:length(sizes)) {
    # categories
    K <- as.numeric(attributes(sizes[[i]])$row.vars[[1]]) # categories
    # seK <- 0:max(K)
    # seK <- seK[(which(seK != 0))] # excluding 0

    seK <- K[(which(K != 0))]

    # creating matrix
    rows <- length(sort(unique(matching)))
    cols <- 2 * length(seK) + 1
    mtrx <- matrix(rep(0, rows * cols), nrow = rows, ncol = cols)

    idx <- if(any(K == 0)) seq(2, max(K) + 1) else seq(1, length(K))
    df <- as.data.frame(mtrx)
    df[, 1] <- sort(unique(matching))

    # number of observations per category wrt matching
    for (j in seK) {
      df[, j + 1] <- if(j != length(seK)) colSums(sizes[[i]][idx[j]:max(idx), ]) else sizes[[i]][max(idx), ]
    }

    colNam1 <- paste0("k", seK)

    # empirical probabilities per category wrt matching
    for (l in seK) {
      df[, length(seK) + 1 + l] <- df[, (l + 1)]/(colSums(sizes[[i]]))
    }

    colNam2 <- paste0("P", seK)
    colNams <- append(colNam1, colNam2)
    colnames(df) <- c("x", colNams)

    dfs[[i]] <- df
  }
  dfs
})

# ** Calculation of empirical cumulative probabilities ######
cumreg_calculation_cumprob_lines <- reactive({
  item <- input$cumreg_slider_item
  matching <- cumreg_matching()
  dfs <- cumreg_calculation_cumprob_points()
  fit.cum <- cumreg_model()
  data <- as.data.frame(ordinal())

  # function for plot
  cum.prob <- function(x, b0, b1){(exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)))}

  # index
  c <- length(coef(fit.cum[[item]])) - 1

  # dataset for lines
  x <- seq(min(matching, na.rm = T), max(matching, na.rm = T), 0.01)
  df.lines.tmp <- data.frame(x = x,
                             y = sapply(1:c, function(k) {cum.prob(x = x,
                                                                   b0 = coef(fit.cum[[item]])[k],
                                                                   b1 = coef(fit.cum[[item]])[c + 1])}))

  colnames(df.lines.tmp) <- c("x", paste0("k", sort(unique(data[, item]))[-1]))
  df.lines.tmp
})

# ** Plot with cumulative curves ######
cumreg_plot_cum_Input <- reactive({
  item <- input$cumreg_slider_item
  matching <- cumreg_matching()
  dfs <- cumreg_calculation_cumprob_points()
  fit.cum <- cumreg_model()
  df.lines.tmp <- cumreg_calculation_cumprob_lines()

  # index
  c <- (ncol(dfs[[item]]) - 1)/2

  # dataset for points
  df.points <- melt(dfs[[item]][, 1:(c + 1)], id.vars = "x",
                    value.name = "size", variable.name = "category")
  df.points <- data.frame(df.points,
                          melt(dfs[[item]][, c(1, (c + 2):ncol(dfs[[item]]))], id.vars = "x",
                               value.name = "prob", variable.name = "category"))
  df.points <- df.points[, c(1, 2, 3, 6)]

  # dataset for lines
  df.lines <- melt(df.lines.tmp, id.vars = "x", value.name = "prob", variable.name = "category")

  # changing labels for plot
  levels(df.points$category) <- gsub("k", "P >= ", levels(df.points$category))
  levels(df.lines$category) <- gsub("k", "P >= ", levels(df.lines$category))


  # plot
  ### points
  g <- ggplot() +
    geom_point(data = df.points, aes(x = x, y = prob, group = category,
                                     size = size,
                                     col = category,
                                     fill = category),
               shape = 21, alpha = 0.5)
  ### lines
  g <- g +
    geom_line(data = df.lines, aes(x = x, y = prob, group = category,
                                   col = category)) +
    xlab(ifelse(input$cumreg_matching == "total", "Total score",
                "Standardized total score (Z-score)")) +
    ylab("Cumulative probability") +
    ylim(0, 1) +
    xlim(min(matching), max(matching)) +
    ggtitle(item_names()[item]) +
    theme_app() +
    theme(legend.position = c(0.97, 0.03),
          legend.justification = c(0.97, 0.03),
          legend.box = "horizontal")
  g
})

# ** Output plot with estimated curves of cumulative regression ######
output$cumreg_plot_cum <- renderPlot({
  cumreg_plot_cum_Input()
})

# ** DB plot with estimated curves of cumulative regression ######
output$DB_cumreg_plot_cum <- downloadHandler(
  filename =  function() {
    paste("fig_CumulativeRegressionCurve_cumulative_", item_names()[input$cumreg_slider_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = cumreg_plot_cum_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)


# ** Calculation of sizes and empirical category probabilities ######
cumreg_calculation_catprob <- reactive({
  matching <- cumreg_matching()
  sizes <- cumreg_calculation_sizes()

  dfs.cat <- list()
  for(i in 1:length(sizes)) {
    K <- as.numeric(attributes(sizes[[i]])$row.vars[[1]])

    # seK <- 0:max(K)
    seK <- K

    rows <- length(sort(unique(matching)))
    cols <- 2 * length(seK) + 1
    mtrx <- matrix(rep(0, rows * cols), nrow = rows, ncol = cols)
    idx <- seq(1, length(K))
    df <- as.data.frame(mtrx)
    df[, 1] <- sort(unique(matching))

    for (j in 1:length(seK)) {
      df[, j + 1] <- if(j != length(seK)) sizes[[i]][idx[j], ] else sizes[[i]][max(idx), ]
    }

    colNam1 <- paste0("k", seK)

    for (l in 1:length(seK)) {
      df[, length(seK) + 1 + l] <- df[, (l + 1)]/(colSums(sizes[[i]]))
    }

    colNam2 <- paste0("P", seK)
    colNams <- append(colNam1, colNam2)
    colnames(df) <- c('x', colNams)

    dfs.cat[[i]] <- df
  }

  dfs.cat
})

# ** Calculation of empirical cumulative probabilities ######
cumreg_calculation_cumprob_lines <- reactive({
  item <- input$cumreg_slider_item
  matching <- cumreg_matching()
  dfs <- cumreg_calculation_cumprob_points()
  fit.cum <- cumreg_model()
  data <- as.data.frame(ordinal())

  # function for plot
  cum.prob <- function(x, b0, b1){(exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)))}

  # index
  c <- length(coef(fit.cum[[item]])) - 1

  # dataset for lines
  x <- seq(min(matching, na.rm = T), max(matching, na.rm = T), 0.01)
  df.lines.tmp <- data.frame(x = x,
                             y = sapply(1:c, function(k) {cum.prob(x = x,
                                                                   b0 = coef(fit.cum[[item]])[k],
                                                                   b1 = coef(fit.cum[[item]])[c + 1])}))
  colnames(df.lines.tmp) <- c("x", paste0("k", sort(unique(data[, item]))[-1]))
  df.lines.tmp
})

# ** Plot with category curves ######
cumreg_plot_cat_Input <- reactive({
  item <- input$cumreg_slider_item
  matching <- cumreg_matching()
  dfs.cat <- cumreg_calculation_catprob()
  fit.cum <- cumreg_model()
  df.lines.tmp <- cumreg_calculation_cumprob_lines()

  # index (it is + 1 compare to cumulative as it includes 0)
  c <- (ncol(dfs.cat[[item]]) - 1)/2

  # dataset for points
  df.points.cat <- melt(dfs.cat[[item]][, 1:(c + 1)], id.vars = "x",
                        value.name = "size", variable.name = "category")
  df.points.cat <- data.frame(df.points.cat[, c("x", "size")],
                              melt(dfs.cat[[item]][, c(1, (c + 2):ncol(dfs.cat[[item]]))], id.vars = "x",
                                   value.name = "prob", variable.name = "category"))
  df.points.cat <- df.points.cat[, c(1, 2, 4, 5)]

  # dataset for lines
  df.lines.cat <- as.data.frame(matrix(1, nrow = nrow(df.lines.tmp), ncol = c))
  colnames(df.lines.cat) <- paste0("k", 0:(c - 1))
  df.lines.cat[, intersect(colnames(df.lines.cat), colnames(df.lines.tmp))] <- df.lines.tmp[, intersect(colnames(df.lines.cat), colnames(df.lines.tmp))]

  check.probs <- which(sapply(1:(c - 1), function(i) any(df.lines.cat[, i] < df.lines.cat[, i + 1])))
  df.lines.cat[, check.probs + 1] <- df.lines.cat[, check.probs]

  df.lines.cat <- data.frame(x = df.lines.tmp$x,
                             sapply(1:(c - 1), function(k) {df.lines.cat[, k] - df.lines.cat[, k + 1]}),
                             df.lines.cat[, c])
  colnames(df.lines.cat) <- c("x", paste0("k", 0:(c - 1)))
  head(df.lines.cat)
  df.lines.cat <- melt(df.lines.cat, id.vars = "x", value.name = "prob", variable.name = "category")

  # changing labels for plot
  levels(df.points.cat$category) <- gsub("P", "P = ", levels(df.points.cat$category))
  levels(df.lines.cat$category) <- gsub("k", "P = ", levels(df.lines.cat$category))

  # plot
  # get baseline colour palette
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  cols <- c("black", gg_color_hue(c - 1))

  ### points
  g <- ggplot() +
    geom_point(data = df.points.cat, aes(x = x, y = prob, group = category,
                                         size = size,
                                         col = category,
                                         fill = category),
               shape = 21, alpha = 0.5)
  ### lines
  g <- g +
    geom_line(data = df.lines.cat, aes(x = x, y = prob, group = category,
                                       col = category)) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    xlab(ifelse(input$cumreg_matching == "total", "Total score",
                "Standardized total score (Z-score)")) +
    ylab("Category probability") +
    ylim(0, 1) +
    xlim(min(matching), max(matching)) +
    ggtitle(item_names()[item]) +
    theme_app() +
    theme(legend.position = c(0.97, 0.03),
          legend.justification = c(0.97, 0.03),
          legend.box = "horizontal")

  g
})

# ** Output plot with estimated curves of cumulative regression ######
output$cumreg_plot_cat <- renderPlot({
  cumreg_plot_cat_Input()
})

# ** DB plot with estimated curves of cumulative regression ######
output$DB_cumreg_plot_cat <- downloadHandler(
  filename =  function() {
    paste("fig_CumulativeRegressionCurve_category_", item_names()[input$cumreg_slider_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = cumreg_plot_cat_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Equation ######
output$cumreg_equation <- renderUI({
  txt1 <- ifelse(input$cumreg_matching == "total", "X", "Z")

  if (input$cumreg_parametrization == "classic"){
    txt2 <- paste("b_{0k} + b_1", txt1)
    txt3 <- paste(txt1, ", b_{0k},  b_1")
  } else {
    txt2 <- paste("a(", txt1, "- d_{0k})")
    txt3 <- paste(txt1, ", a,  d_{0k}")
  }

  txt <- paste("$$P(Y \\geq k|", txt3, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")

  withMathJax(HTML(txt))
})

# ** Interpretation ######
output$cumreg_interpretation <- renderUI({
  if (input$cumreg_parametrization == "classic"){
    par <- c("\\(b_{0k}\\)", "\\(b_1\\)")
  } else {
    par <- c("\\(d_{0k}\\)", "\\(a\\)")
  }

  txt <- paste("Parameters", par[1], "describe horizontal position of the fitted curves,
               where \\(k = 0, 1, 2, ...\\) is number of obtained scores, parameter", par[2],
               "describes their common slope. Category probabilities are then calculated as
               differences of two subsequent cumulative probabilities. ")
  withMathJax(HTML(txt))
})

# ** Table of parameters ######
cumreg_coef_tab_Input <- reactive({
  fit.cum <- cumreg_model()
  item <- input$cumreg_slider_item
  data <- as.data.frame(ordinal())

  if (input$cumreg_parametrization == "classic"){
    tab.coef <- coef(fit.cum[[item]])
    tab.se <- sqrt(diag(vcov(fit.cum[[item]])))

    tab <- data.frame(Estimate = tab.coef, SE = tab.se)
    rownames(tab) <- c(paste0("%%mathit{b}_{0", sort(unique(data[, item]))[-1], "}%%"), "%%mathit{b}_{1}%%")
  } else {
    tab.coef.tmp <- coef(fit.cum[[item]])
    c <- length(tab.coef.tmp)

    tab.coef <- c(-tab.coef.tmp[-c]/tab.coef.tmp[c], tab.coef.tmp[c])

    Sigma <- vcov(fit.cum[[item]])
    c <- length(tab.coef)
    D <- matrix(0, nrow = c, ncol = c)
    diag(D)[-c] <- -1/tab.coef.tmp[c]
    D[c, ] <- c(tab.coef.tmp[1:(c - 1)]/(tab.coef.tmp[c])^2, 1)

    Sigma.new <- t(D) %*% Sigma %*% D

    tab <- data.frame(Estimate = tab.coef, SE = sqrt(diag(Sigma.new)))
    rownames(tab) <- c(paste0("%%mathit{d}_{0", sort(unique(data[, item]))[-1], "}%%"), "%%mathit{a}%%")
  }

  tab
})

output$cumreg_coef_tab <- renderTable({
  cumreg_coef_tab_Input()
},
include.rownames = T,
include.colnames = T)

# ** Warning for missing values ####
output$cumreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ADJACENT LOGISTIC ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for adjacent logistic regression ######

adjreg_matching <- reactive({
  if (input$adjreg_matching == "total"){
    matching <- total_score()
  } else {
    matching <- z_score()
  }
  matching
})

# ** Update select input for different parametrization ######
observe({
  if (input$adjreg_parametrization == "irt")
    updateSelectInput(session = session,
                      inputId = "adjreg_matching",
                      selected = "zscore")
})

# ** Model for adjacent regression ######
adjreg_model <- reactive({
  matching <- adjreg_matching()
  data <- as.data.frame(ordinal())
  maxval <- sapply(data, function(x) max(x, na.rm = TRUE))

  for (i in 1:ncol(data)){
    data[, i] <- factor(data[, i], levels = 0:maxval[i])
  }

  fit.adj <- apply(data, 2, function(x)
    vglm(x ~ matching, family = acat(reverse = FALSE, parallel = TRUE)))
  fit.adj
})

# ** Calculation of sizes ######
adjreg_calculation_sizes <- reactive({
  data <- as.data.frame(ordinal())
  maxval <- sapply(data, function(x) max(x, na.rm = TRUE))
  matching <- adjreg_matching()

  # relevel data
  for (i in 1:ncol(data)){
    data[, i] <- factor(data[, i], levels = 0:maxval[i])
  }

  # calculation of number of observations wrt matching
  sizes <- list()
  for (i in 1:ncol(data)) {
    sizes[[i]] <- ftable(xtabs(~ data[, i] + matching))
  }

  sizes
})


# ** Calculation of sizes and empirical category probabilities ######
adjreg_calculation_catprob_points <- reactive({
  matching <- adjreg_matching()
  sizes <- adjreg_calculation_sizes()

  dfs.cat <- list()
  for(i in 1:length(sizes)) {
    K <- as.numeric(attributes(sizes[[i]])$row.vars[[1]])

    # seK <- 0:max(K)
    seK <- K

    rows <- length(sort(unique(matching)))
    cols <- 2 * length(seK) + 1
    mtrx <- matrix(rep(0, rows * cols), nrow = rows, ncol = cols)
    idx <- seq(1, length(K))
    df <- as.data.frame(mtrx)
    df[, 1] <- sort(unique(matching))

    for (j in 1:length(seK)) {
      df[, j + 1] <- if(j != length(seK)) sizes[[i]][idx[j], ] else sizes[[i]][max(idx), ]
    }

    colNam1 <- paste0("k", seK)

    for (l in 1:length(seK)) {
      df[, length(seK) + 1 + l] <- df[, (l + 1)]/(colSums(sizes[[i]]))
    }

    colNam2 <- paste0("P", seK)
    colNams <- append(colNam1, colNam2)
    colnames(df) <- c('x', colNams)

    dfs.cat[[i]] <- df
  }

  dfs.cat
})


# ** Category lines ** #
adjreg_calculation_catprob_lines <- reactive({
  item <- input$adjreg_slider_item
  matching <- adjreg_matching()
  dfs <- adjreg_calculation_catprob_points()
  fit.adj <- adjreg_model()
  data <- as.data.frame(ordinal())

  coefs <- lapply(fit.adj, function(x) coef(x))
  # take a length of coeficient for item i
  coefs_no <- length(coefs[[item]])
  # take all intercepts, except score
  coefs_b0 <- coefs[[item]][-coefs_no]
  # take score
  coefs_b1 <- coefs[[item]][coefs_no]

  # function for plot
  cat.prob <- function(x, b0, b1){
    k <- length(b0)

    # term b0 + b1X
    mat <- data.frame(0, sapply(1:k, function(i) b0[i] + b1 * x))
    # cumulative sum
    mat <- t(apply(mat, 1, cumsum))
    # exponential
    mat <- exp(mat)
    # norming
    norm <- apply(mat, 1, sum)
    mat <- mat/norm

    mat
  }

  # dataset for lines
  x <- seq(min(matching, na.rm = T), max(matching, na.rm = T), 0.01)
  df.lines.tmp <- data.frame(x = x,
                             y = cat.prob(x = x, b0 = coefs_b0, b1 = coefs_b1))
  colnames(df.lines.tmp) <- c("x", paste0("k", sort(unique(data[, item]))))
  df.lines.tmp
})



# ** Plot with category curves ######
adjreg_plot_cat_Input <- reactive({
  item <- input$adjreg_slider_item
  matching <- adjreg_matching()
  dfs.cat <- adjreg_calculation_catprob_points()
  fit.adj <- adjreg_model()
  df.lines.tmp <- adjreg_calculation_catprob_lines()

  # index (it is + 1 compare to cumulative as it includes 0)
  c <- (ncol(dfs.cat[[item]]) - 1)/2

  # dataset for points
  df.points.cat <- melt(dfs.cat[[item]][, 1:(c + 1)], id.vars = "x",
                        value.name = "size", variable.name = "category")
  df.points.cat <- data.frame(df.points.cat[, c("x", "size")],
                              melt(dfs.cat[[item]][, c(1, (c + 2):ncol(dfs.cat[[item]]))], id.vars = "x",
                                   value.name = "prob", variable.name = "category"))
  df.points.cat <- df.points.cat[, c(1, 2, 4, 5)]

  # dataset for lines
  # df.lines.cat <- as.data.frame(matrix(1, nrow = nrow(df.lines.tmp), ncol = c))
  # colnames(df.lines.cat) <- paste0("k", 0:(c - 1))
  # df.lines.cat[, intersect(colnames(df.lines.cat), colnames(df.lines.tmp))] <- df.lines.tmp[, intersect(colnames(df.lines.cat), colnames(df.lines.tmp))]
  # df.lines.cat <- data.frame(x = df.lines.tmp$x,
  #                            df.lines.cat)

  df.lines.cat <- melt(df.lines.tmp, id.vars = "x", value.name = "prob", variable.name = "category")

  # changing labels for plot
  levels(df.points.cat$category) <- gsub("P", "P = ", levels(df.points.cat$category))
  levels(df.lines.cat$category) <- gsub("k", "P = ", levels(df.lines.cat$category))

  # plot
  # get baseline colour palette
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  cols <- c("black", gg_color_hue(c - 1))

  ### points
  g <- ggplot() +
    geom_point(data = df.points.cat, aes(x = x, y = prob, group = category,
                                         size = size,
                                         col = category,
                                         fill = category),
               shape = 21, alpha = 0.5)
  ### lines
  g <- g +
    geom_line(data = df.lines.cat, aes(x = x, y = prob, group = category,
                                       col = category)) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    xlab(ifelse(input$adjreg_matching == "total", "Total score",
                "Standardized total score (Z-score)")) +
    ylab("Category probability") +
    ylim(0, 1) +
    xlim(min(matching), max(matching)) +
    ggtitle(item_names()[item]) +
    theme_app() +
    theme(legend.position = c(0.97, 0.03),
          legend.justification = c(0.97, 0.03),
          legend.box = "horizontal")

  g
})

# ** Output plot with estimated curves of adjacent regression ######
output$adjreg_plot_cat <- renderPlot({
  adjreg_plot_cat_Input()
})

# ** DB plot with estimated curves of adjacent regression ######
output$DB_adjreg_plot_cat <- downloadHandler(
  filename =  function() {
    paste("fig_AdjacentRegressionCurve_category_", item_names()[input$adjreg_slider_item], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = adjreg_plot_cat_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Equation ######
output$adjreg_equation <- renderUI({
  txt1 <- ifelse(input$adjreg_matching == "total", "X", "Z")

  if (input$adjreg_parametrization == "classic"){
    txt2 <- paste("b_{0t} + b_1", txt1)
    txt3 <- paste(txt1, ", b_1, b_{01}, ..., b_{0k}")
  } else {
    txt2 <- paste("a(", txt1, "- d_{0k})")
    txt3 <- paste(txt1, ", a, d_{01}, ..., d_{0k}")
  }

  txt <- paste("$$P(Y = k|", txt3, ") = \\frac{e^{\\sum_{t = 0}^{k}", txt2, "}}{\\sum_{r = 0}^{K}e^{\\sum_{t = 0}^{r}", txt2, "}}$$")

  withMathJax(HTML(txt))
})

# ** Interpretation ######
output$adjreg_interpretation <- renderUI({
  if (input$cumreg_parametrization == "classic"){
    par <- c("\\(b_{0k}\\)", "\\(b_1\\)")
  } else {
    par <- c("\\(d_{0k}\\)", "\\(a\\)")
  }

  txt <- paste("Parameters", par[1], "describe horizontal position of the fitted curves,
               where \\(k = 0, 1, 2, ...\\) is number of obtained scores, parameter", par[2],
               "describes their common slope. Category probabilities are then calculated as
               differences of two subsequent cumulative probabilities. ")
  withMathJax(HTML(txt))
})

# ** Table of parameters ######
adjreg_coef_tab_Input <- reactive({
  fit.cum <- cumreg_model()
  item <- input$cumreg_slider_item
  data <- as.data.frame(ordinal())

  if (input$cumreg_parametrization == "classic"){
    tab.coef <- coef(fit.cum[[item]])
    tab.se <- sqrt(diag(vcov(fit.cum[[item]])))

    tab <- data.frame(Estimate = tab.coef, SE = tab.se)
    rownames(tab) <- c(paste0("%%mathit{b}_{", sort(unique(data[, item]))[-1], "}%%"), "%%a%%")
  } else {
    tab.coef.tmp <- coef(fit.cum[[item]])
    c <- length(tab.coef.tmp)

    tab.coef <- c(-tab.coef.tmp[-c]/tab.coef.tmp[c], tab.coef.tmp[c])

    Sigma <- vcov(fit.cum[[item]])
    c <- length(tab.coef)
    D <- matrix(0, nrow = c, ncol = c)
    diag(D)[-c] <- -1/tab.coef.tmp[c]
    D[c, ] <- c(tab.coef.tmp[1:(c - 1)]/(tab.coef.tmp[c])^2, 1)

    Sigma.new <- t(D) %*% Sigma %*% D

    tab <- data.frame(Estimate = tab.coef, SE = sqrt(diag(Sigma.new)))
    rownames(tab) <- c(paste0("%%mathit{d}_{0", sort(unique(data[, item]))[-1], "}%%"), "%%mathit{a}%%")
  }

  tab
})

output$adjreg_coef_tab <- renderTable({
  cumreg_coef_tab_Input()
},
include.rownames = T,
include.colnames = T)

# ** Warning for missing values ####
output$adjreg_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MULTINOMIAL ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model for multinomial regression ######
multi_model <- reactive({
  zscore <- z_score()
  key <- t(as.data.table(key()))
  item <- input$multiSlider
  data <- nominal()

  dfhw <- data.table(data[, item, with = F], zscore)
  dfhw <- dfhw[complete.cases(dfhw), ]

  fitM <- multinom(relevel(as.factor(unlist(dfhw[, 1])),
                           ref = paste(key[item])) ~ unlist(dfhw[, 2]),
                   trace = F)
  fitM
})

# ** Plot with estimated curves of multinomial regression ######
multi_plot_Input <- reactive({
  key <- t(as.data.table(key()))
  data <- nominal()
  zscore <- z_score()
  item <- input$multiSlider

  fitM <- multi_model()

  data <- sapply(1:ncol(data), function(i) as.factor(unlist(data[, i, with = F])))

  f <- function(Z, b_i0, b_i1) {
    coefs <- as.vector(coef(fitM))
    j <- length(coefs)/2

    idx1 <- 1:j
    idx2 <- (j+1):length(coefs)

    num <- exp(b_i0 + b_i1 * Z)
    den <- sapply(1:j, function(x) exp(coefs[idx1[x]] + coefs[idx2[x]] * Z))
    den <- apply(den, 1, sum)

    num/(1 + den)
  }

  f2 <- function(Z) {
    coefs <- as.vector(coef(fitM))
    j <- length(coefs)/2

    idx1 <- 1:j
    idx2 <- (j+1):length(coefs)

    den <- sapply(1:j, function(x) exp(coefs[idx1[x]] + coefs[idx2[x]] * Z))
    den <- apply(den, 1, sum)

    1/(1 + den)
  }

  # take x axis
  x <- seq(min(zscore, na.rm = TRUE), max(zscore, na.rm = TRUE), 0.01)
  # take number of categories (except the correct one) - but those are in coef(fitM)
  no_cat <- length(coef(fitM))/2
  df.probs <- matrix(c(rep(0, no_cat * length(x))), nrow = length(x), ncol = no_cat + 1)

  # take coefs
  coefs <- coef(fitM)
  # b_io
  idx1 <- 1:no_cat
  # b_i1
  idx2 <- (no_cat + 1):length(coef(fitM))

  for (i in 1:(length(coef(fitM))/2)) {
    df.probs[, i+1] <- f(x, coefs[idx1[i]], coefs[idx2[i]])
  }

  df.probs[, 1] <- f2(x)

  dfhw <- data.table(data[, item], zscore)
  dfhw <- dfhw[complete.cases(dfhw), ]

  # pp <- fitted(fitM)

  temp <- as.factor(unlist(dfhw[, 1]))
  temp <- relevel(temp, ref = paste(key[item]))

  colnames(df.probs) <- levels(temp)

  if(ncol(df.probs) == 1){
    df_test <- cbind(df_test, 1 - df_test)
    colnames(df_test) <- rev(levels(df_test))
  }

  # stotals <- rep(unlist(dfhw[, 2]),
  #                length(levels(temp)))

  # df <- cbind(melt(pp), stotals)

  df <- cbind(melt(df.probs), x)
  df$Var2 <- relevel(as.factor(df$Var2), ref = paste(key[item]))


  df2 <- data.table(table(data[, item], zscore),
                    y = data.table(prop.table(table(data[, item], zscore), 2))[, 3])
  df2$zscore <- as.numeric(paste(df2$zscore))
  df2$Var2 <- relevel(factor(df2$V2), ref = paste(key[item]))


  ggplot() +
    geom_line(data = df,
              aes(x = x , y = value,
                  colour = Var2, linetype = Var2), size = 1) +
    geom_point(data = df2,
               aes(x = zscore, y = y.N,
                   colour = Var2, fill = Var2,
                   size = N),
               alpha = 0.5, shape = 21) +

    ylim(0, 1) +
    labs(x = "Standardized total score",
         y = "Probability of answer") +
    theme_app() +
    theme(legend.box = "horizontal",
          legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1)) +
    ggtitle(item_names()[item])
})

# ** Reports: Plot with estimated curves of multinomial regression ######
multiplotReportInput <- reactive({
  graflist <- list()
  key <- unlist(key())
  data <- nominal()
  zscore <- z_score()

  data <- sapply(1:ncol(data), function(i) as.factor(unlist(data[, i, with = F])))

  for (item in 1:length(key)) {
    dfhw <- data.table(data[, item], zscore)
    dfhw <- dfhw[complete.cases(dfhw), ]

    fitM <- multinom(relevel(as.factor(unlist(dfhw[, 1])),
                             ref = paste(key[item])) ~ unlist(dfhw[, 2]),
                     trace = F)


	f <- function(Z, b_i0, b_i1) {
    coefs <- as.vector(coef(fitM))
    j <- length(coefs)/2

    idx1 <- 1:j
    idx2 <- (j+1):length(coefs)

    num <- exp(b_i0 + b_i1 * Z)
    den <- sapply(1:j, function(x) exp(coefs[idx1[x]] + coefs[idx2[x]] * Z))
    den <- apply(den, 1, sum)

    num/(1 + den)
  }

  f2 <- function(Z) {
    coefs <- as.vector(coef(fitM))
    j <- length(coefs)/2

    idx1 <- 1:j
    idx2 <- (j+1):length(coefs)

    den <- sapply(1:j, function(x) exp(coefs[idx1[x]] + coefs[idx2[x]] * Z))
    den <- apply(den, 1, sum)

    1/(1 + den)
  }

  x <- seq(min(zscore, na.rm = TRUE), max(zscore, na.rm = TRUE), 0.01)
  # take number of categories (except the correct one) - but those are in coef(fitM)
  no_cat <- length(coef(fitM))/2
  df.probs <- matrix(c(rep(0, no_cat * length(x))), nrow = length(x), ncol = no_cat + 1)

  # take coefs
  coefs <- coef(fitM)
  # b_io
  idx1 <- 1:no_cat
  # b_i1
  idx2 <- (no_cat + 1):length(coef(fitM))

  for (i in 1:(length(coef(fitM))/2)) {
    df.probs[, i+1] <- f(x, coefs[idx1[i]], coefs[idx2[i]])
  }

  df.probs[, 1] <- f2(x)



  #pp <- fitted(fitM)
  temp <- as.factor(unlist(dfhw[, 1]))
  temp <- relevel(temp, ref = paste(key[item]))

  if(ncol(df.probs) == 1){
    df_test <- cbind(df_test, 1 - df_test)
    colnames(df_test) <- rev(levels(df_test))
  }

    #stotals <- rep(unlist(dfhw[, 2]), length(levels(as.factor(unlist(dfhw[, 1, with = F])))))
    df <- cbind(melt(df.probs), x)
    df$Var2 <- relevel(as.factor(df$Var2), ref = paste(key[item]))
    df2 <- data.table(table(data[, item], zscore),
                      y = data.table(prop.table(table(data[, item], zscore), 2))[, 3])
    df2$zscore <- as.numeric(paste(df2$zscore))
    df2$Var2 <- relevel(factor(df2$V2), ref = paste(key[item]))

    g <-  ggplot() +
      geom_line(data = df,
                aes(x = x , y = value,
                    colour = Var2, linetype = Var2), size = 1) +
      geom_point(data = df2,
                 aes(x = zscore, y = y.N,
                     colour = Var2, fill = Var2,
                     size = N),
                 alpha = 0.5, shape = 21) +
      ylim(0, 1) +
      labs(x = "Standardized total score",
           y = "Probability of answer") +
      guides(colour = guide_legend(order = 1),
             linetype = guide_legend(order = 1),
             fill = guide_legend(order = 1),
             size = guide_legend(order = 2)) +
      theme_app() +
      theme(legend.box = "horizontal",
            legend.position = c(0.01, 0.98),
            legend.justification = c(0, 1))
    g = g +
      ggtitle(paste("Multinomial plot for item", item_numbers()[item]))
    g = ggplotGrob(g)
    graflist[[item]] = g
  }
  graflist
})

# ** Output plot with estimated curves of multinomial regression ######
output$multi_plot <- renderPlot({
  multi_plot_Input()
})

# ** DB plot with estimated curves of multinomial regression ######
output$DB_multi_plot <- downloadHandler(
  filename =  function() {
    paste("fig_MultinomialRegressionCurve_", item_names()[input$multiSlider], ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = multi_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# ** Equation of multinomial regression ######
output$multi_equation <- renderUI ({
  cor_option <- key()[input$multiSlider]
  withMathJax(
    sprintf(
      '$$\\mathrm{P}(Y = i|Z, b_{i0}, b_{i1}) = \\frac{e^{\\left( b_{i0} + b_{i1} Z\\right)}}{1 + \\sum_j e^{\\left( b_{j0} + b_{j1} Z\\right)}}, \\\\
      \\mathrm{P}(Y = %s|Z, b_{i0}, b_{i1}) = \\frac{1}{1 + \\sum_j e^{\\left( b_{j0} + b_{j1} Z\\right)}}, \\\\
      \\text{where } i \\text{ is one of the wrong options and } %s \\text{ is the correct one.}$$',
      cor_option, cor_option, cor_option, cor_option
    )
  )
})

# ** Table of parameters of curves of multinomial regression ######
output$coef_multi <- renderTable({
  fit <- multi_model()

  key <- t(as.data.table(key()))
  data <- nominal()
  item <- input$multiSlider

  dfhw <- na.omit(data.table(data[, item, with = FALSE]))
  temp <- as.factor(unlist(dfhw[, 1]))
  temp <- relevel(temp, ref = paste(key[item]))

  koef <- as.vector(coef(fit))
  std  <- as.vector(sqrt(diag(vcov(fit))))
  tab  <- cbind(koef, std)
  rnam <- rownames(coef(fit))
  if (is.null(dim(coef(fit))[1]) & !(all(levels(temp) %in% c("1", "0")))){
    rnam <- rev(levels(temp))[1]
  }

  colnames(tab) <- c("Estimate", "SE")
  rownames(tab) <- c(paste("%%mathit{b}_{", rnam, "0}%%", sep = ""),
                     paste("%%mathit{b}_{", rnam, "1}%%", sep = ""))
  tab
},
include.rownames = T)


# ** Interpretation of parameters of curves of multinomial regression ######
output$multi_interpretation <- renderUI({

  koef <- summary(multi_model())$coefficients
  txt  <- c()

  if(is.null(dim(koef))){
    m <- length(koef)
    txt0 <- ifelse(koef[2] < 0, "decrease", "increase")
    txt <-  paste (
      "A one-unit increase in the Z-score (one SD increase in original
      scores)  is associated with the ", txt0, " in the log odds of
      answering the item "
      ,"<b> 0 </b>", "vs.", "<b> 1 </b>", " in the amount of ",
      "<b>", abs(round(koef[2], 2)), "</b>", '<br/>')
  } else {
    m <- nrow(koef)
    for (i in 1:m){
      txt0 <- ifelse(koef[i, 2] < 0, "decrease", "increase")
      txt[i] <- paste (
        "A one-unit increase in the Z-score (one SD increase in original
        scores)  is associated with the ", txt0, " in the log odds of
        answering the item "
        ,"<b>", row.names(koef)[i], "</b>", "vs.", "<b>",

        key()[input$multiSlider],

        "</b>","in the amount of ",
        "<b>", abs(round(koef[i, 2], 2)), "</b>", '<br/>')
    }
  }
  HTML(paste(txt))
})

# ** Warning for missing values ####
output$multi_na_alert <- renderUI({
  txt <- na_score()
  HTML(txt)
})
