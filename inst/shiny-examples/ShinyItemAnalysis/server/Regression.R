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
  colnames(tab) <- c("Estimate", "SD")
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
  colnames(tab) <- c("Estimate", "SD")
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
  colnames(tab) <- c("Estimate", "SD")
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 3P IRT Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of nonlinear curve ######
nlr_3P_model <- reactive({
  data <- binary()
  zscore <- z_score()
  i <- input$slider_nlr_3P_item

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
  colnames(tab) <- c("Estimate", "SD")
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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * NONLINEAR 4P IRT Z ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model of nonlinear curve ######
nlr_4P_model <- reactive({
  data <- binary()
  zscore <- z_score()
  i <- input$slider_nlr_4P_item

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
  colnames(tab) <- c("Estimate", "SD")
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output$regr_comp_table <- DT::renderDataTable({
  data <- binary()
  zscore <- z_score()

  m <- ncol(data)

  glr <- function(x, a, b, c, d){c + (d - c) / (1 + exp(-a * (x - b)))}

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

  dfhw <- data.table(data[, item], zscore)
  dfhw <- dfhw[complete.cases(dfhw), ]

  pp <- fitted(fitM)

  temp <- as.factor(unlist(dfhw[, 1]))
  temp <- relevel(temp, ref = paste(key[item]))

  if(ncol(pp) == 1){
    pp <- cbind(pp, 1 - pp)
    colnames(pp) <- rev(levels(temp))
  }

  stotals <- rep(unlist(dfhw[, 2]),
                 length(levels(temp)))

  df <- cbind(melt(pp), stotals)
  df$Var2 <- relevel(as.factor(df$Var2), ref = paste(key[item]))

  df2 <- data.table(table(data[, item], zscore),
                    y = data.table(prop.table(table(data[, item], zscore), 2))[, 3])
  df2$zscore <- as.numeric(paste(df2$zscore))
  df2$Var2 <- relevel(factor(df2$V2), ref = paste(key[item]))


  ggplot() +
    geom_line(data = df,
              aes(x = stotals , y = value,
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

    pp <- fitted(fitM)
    temp <- as.factor(unlist(dfhw[, 1]))
    temp <- relevel(temp, ref = paste(key[item]))

    if(ncol(pp) == 1){
      pp <- cbind(pp, 1 - pp)
      colnames(pp) <- rev(levels(temp))
    }

    stotals <- rep(unlist(dfhw[, 2]), length(levels(as.factor(unlist(dfhw[, 1, with = F])))))
    df <- cbind(melt(pp), stotals)
    df$Var2 <- relevel(as.factor(df$Var2), ref = paste(key[item]))
    df2 <- data.table(table(data[, item], zscore),
                      y = data.table(prop.table(table(data[, item], zscore), 2))[, 3])
    df2$zscore <- as.numeric(paste(df2$zscore))
    df2$Var2 <- relevel(factor(df2$V2), ref = paste(key[item]))

    g <-  ggplot() +
      geom_line(data = df,
                aes(x = stotals , y = value,
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
  
  colnames(tab) <- c("Estimate", "SD")
  rownames(tab) <- c(paste("%%mathit{b", rnam, "}_{0}%%", sep = ""),
                     paste("%%mathit{b", rnam, "}_{1}%%", sep = ""))
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
