#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# IRT MODELS WITH MIRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * RASCH ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rasch_model_mirt <- reactive({
  fitRasch <- mirt(binary(), model = 1, itemtype = "Rasch",
                   SE = T, verbose = F)
})

# ** UPDATING MAXIMUM VALUE IN INPUT SLIDER, IN ITEMS TAB ** #
observe({
  updateSliderInput(session, 'rachSliderChar', max = length(item_names()))
})

# *** CC ######
raschInput_mirt <- reactive({
  plt <- plot(rasch_model_mirt(), type = 'trace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }
  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c("Ability", "Probability", "Item")

  g <- ggplot(data = df, aes(x = Ability, y = Probability, color = Item)) +
    geom_line() +
    ylab("Probability of correct answer") +
    theme_app()
})

output$rasch_mirt <- renderPlotly({
  g <- raschInput_mirt()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_rasch_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** CC items ######
raschInput_mirt_tab <- reactive({
  plt <- plot(rasch_model_mirt(), type = 'trace', which.item = input$rachSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c("Ability", "Probability")

  g <- ggplot(data = df, aes(x = Ability, y = Probability)) +
    geom_line(color = cols[input$rachSliderChar]) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[input$rachSliderChar]) +
    theme_app()
})

output$rasch_mirt_tab <- renderPlotly({
  g <- raschInput_mirt_tab()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_rasch_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_RaschItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC ######
raschiicInput_mirt <- reactive({
  plt <- plot(rasch_model_mirt(), type = 'infotrace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c("Ability", "Information", "Item")

  g <- ggplot(data = df, aes(x = Ability, y = Information, color = Item )) +
    geom_line() +
    theme_app()
})


output$raschiic_mirt <- renderPlotly({
  g <- raschiicInput_mirt()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_raschiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschiicInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** ICC items ######
raschiicInput_mirt_tab <- reactive({
  plt <- plot(rasch_model_mirt(), type = 'infotrace', which.item = input$rachSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y

  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  colnames(df) <- c("Ability", "Information")

  g <- ggplot(data = df, aes(x = Ability, y = Information)) +
    geom_line(color = cols[input$rachSliderChar]) +
    ggtitle(item_names()[input$rachSliderChar]) +
    theme_app()
})


output$raschiic_mirt_tab <- renderPlotly({
  g <- raschiicInput_mirt_tab()
  p <- ggplotly(g)

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_raschiic_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_RaschItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschiicInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** TIF ######
raschtifInput_mirt <- reactive({
  plt <- plot(rasch_model_mirt(), type = "infoSE")

  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  df <- data.frame(cbind(Ability = x, Information = y))

  df$SE <- 1/sqrt(df$Information)

  ggplot(data = df, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information",
                       sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = 'pink')) +
    theme_app()

})

output$raschtif_mirt <- renderPlotly({
  g <- raschtifInput_mirt()

  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_raschtif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschtifInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Table of parameters ######
raschcoefInput_mirt <- reactive({
  fit <- rasch_model_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
  se_list <- coef(fit, printSE = T)
  se_tab <- sapply(1:length(par_tab), function(i) se_list[[i]]["SE", "d"])

  tab <- cbind(par_tab, se_tab)

  tab <- round(tab, 3)

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("b", "SE(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("b", "SE(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp
})

output$raschcoef_mirt <- renderTable({
  raschcoefInput_mirt()
},
include.rownames = T)

raschcoefInput_mirt_tab <- reactive({
  fit <- rasch_model_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
  se_list <- coef(fit, printSE = T)
  se_tab <- sapply(1:length(par_tab), function(i) se_list[[i]]["SE", "d"])

  tab <- cbind(par_tab, se_tab)

  tab <- round(tab, 3)

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("b", "SE(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("b", "SE(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp[input$rachSliderChar,]
})

output$raschcoef_mirt_tab <- renderTable({
  raschcoefInput_mirt_tab()
},
include.rownames = T)



# ** Download table ######
output$download_Rasch_table <- downloadHandler(
  filename = function() {
    paste("Rasch_table",".csv",sep = "")
  },
  content = function(file) {
    data <- raschcoefInput_mirt()
    write.csv(data,file)
  }
)

# * Abilities estimates parameters * #
raschAbilities <- reactive({
  fit <- rasch_model_mirt()

  ts <- as.vector(total_score())
  sts <- as.vector(z_score())
  fs <- as.vector(fscores(fit))
  fs.Err <- as.vector(fscores(fit, full.scores.SE = TRUE)[, 2])

  tab <- data.frame(cbind(ts, sts, fs, fs.Err))
  colnames(tab) <- c('Total scores', 'Z-score', 'F-scores', 'SE of F-score')
  n <- nrow(tab)
  nam <- vector('character', length = n)
  for (i in 1:n) {
    nam[i] <- paste('Respondent', i)
  }
  rownames(tab) <- c(nam)
  tab
})

output$raschcoef_abilities <- renderTable({

  head(raschAbilities(), 6)

}, include.rownames = TRUE)

# ** Download abilities ######
output$download_Rasch_abilities <- downloadHandler(
  filename = function() {
    paste("Rasch_abilities",".csv",sep = "")
  },
  content = function(file) {
    data <- raschAbilities()
    write.csv(data,file, col.names = TRUE)
  }
)


# *** Factor scores correlation ######
raschFactorCorInput_mirt <- reactive({
  fs <- as.vector(fscores(rasch_model_mirt()))
  sts <- z_score()

  whok <- !(is.na(fs) | is.na(sts))

  cor <- cor(fs[whok], sts[whok])
  cor
})
output$raschFactorCor_mirt <- renderText({
  paste("The Pearson correlation coefficient between standardized total score (Z-score)
        and factor score estimated by IRT model is", round(raschFactorCorInput_mirt(), 3))
})

# *** Factor scores plot ######
raschFactorInput_mirt <- reactive({

  fs <- as.vector(fscores(rasch_model_mirt()))
  sts <- z_score()

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app() +
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box = "vertical",
          legend.key.size = unit(1, "lines"),
          legend.text.align = 0,
          legend.title.align = 0)
})

output$raschFactor_mirt <- renderPlot({
  raschFactorInput_mirt()
})

output$DP_raschFactor_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschFactorVsStandardized.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschFactorInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Wright Map ######
raschWrightMapInput_mirt <- reactive({
  fit <- rasch_model_mirt()
  fs <- as.vector(fscores(fit))

  b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
  names(b) <- item_names()

  ggWrightMap(fs, b)

})

output$raschWrightMap_mirt<- renderPlot({
  raschWrightMapInput_mirt()
})

output$DP_raschWM_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschWrightMap.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = raschWrightMapInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 1PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

one_param_irt_mirt <- reactive({
  data <- binary()
  s <- paste("F = 1-", ncol(data), "\n",
             "CONSTRAIN = (1-", ncol(data), ", a1)")
  model <- mirt.model(s)
  fit1PL <- mirt(data, model = model, itemtype = "2PL", SE = T, verbose = F,
                 technical = list(NCYCLES = input$ncycles))
})

# Updating number of items in slider input, in ITEMS tab #
observe({
  updateSliderInput(session, 'onePLSliderChar', max = length(item_names()))
})


# *** CC ####
oneparamirtInput_mirt <- reactive({
  plt <- plot(one_param_irt_mirt(), type = 'trace',facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }
  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Probability', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Probability, color = Item )) +
    geom_line() +
    ylab("Probability of correct answer") +
    theme_app()
})

output$oneparamirt_mirt <- renderPlotly({
  p <- ggplotly(oneparamirtInput_mirt())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)

})

output$DP_oneparamirt_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLItemCharactersticCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = setting_figures$height,
        width = setting_figures$width,
        units = "in",
        res = setting_figures$dpi,
        pointsize = setting_figures$dpi/72)
    print(oneparamirtInput_mirt())
    dev.off()
  }
)
# *** CC items ####
oneparamirtInput_mirt_tab <- reactive({
  plt <- plot(one_param_irt_mirt(), type = 'trace', which.item = input$onePLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c("Ability", "Probability")

  g <- ggplot(data = df, aes(x = Ability, y = Probability)) +
    geom_line(color = cols[input$onePLSliderChar]) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[input$onePLSliderChar]) +
    theme_app()
})

output$oneparamirt_mirt_tab <- renderPlotly({

  p <- ggplotly(oneparamirtInput_mirt_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_oneparamirt_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_1PLItemCharactersticCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = oneparamirtInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC ######
oneparamirtiicInput_mirt <- reactive({
  plt <- plot(one_param_irt_mirt(), type = 'infotrace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Information', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Information, color = Item )) +
    geom_line() +
    ylab('Information') +
    theme_app()
})

output$oneparamirtiic_mirt <- renderPlotly({
  p <- ggplotly(oneparamirtiicInput_mirt())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ","",p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_oneparamirtiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = setting_figures$height,
        width = setting_figures$width,
        units = "in",
        res = setting_figures$dpi,
        pointsize = setting_figures$dpi/72)
    print(oneparamirtiicInput_mirt())
    dev.off()
  }
)
# *** IIC items ####
oneparamirtiicInput_mirt_tab <- reactive({
  plt <- plot(one_param_irt_mirt(), type = 'infotrace', which.item = input$onePLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c('Ability', 'Information')

  g <- ggplot(data = df, aes(x = Ability, y = Information)) +
    geom_line(color = cols[input$onePLSliderChar]) +
    ylab("Information") +
    ggtitle(item_names()[input$onePLSliderChar]) +
    theme_app()
})


output$oneparamirtiic_mirt_tab <- renderPlotly({
  p <- ggplotly(oneparamirtiicInput_mirt_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_oneparamirtiic_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_1PLItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = oneparamirtiicInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** TIF ######
oneparamirttifInput_mirt <- reactive({
  plt <- plot(one_param_irt_mirt(), type = "infoSE")

  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  df <- data.frame(cbind(Ability = x, Information = y))

  df$SE <- 1/sqrt(df$Information)

  ggplot(data = df, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information",
                       sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = 'pink')) +
    theme_app()

})

output$oneparamirttif_mirt <- renderPlotly({
  g <- oneparamirttifInput_mirt()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})


output$DP_oneparamirttif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = oneparamirttifInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Table of parameters ######
oneparamirtcoefInput_mirt <- reactive({
  fit <- one_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  se_tab <- c()
  for (item in 1:nrow(par_tab)){
    pick <- c(1, item + 1)
    ad <- parvec[pick]
    v <- vcov[pick, pick]

    SEs <- deltamethod(list(~ x1, ~ -x2/x1), ad, v)
    names(SEs) <- c('a', 'b')
    se_tab <- rbind(se_tab, SEs)
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 3, 2, 4)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp
})

output$oneparamirtcoef_mirt <- renderTable({
  oneparamirtcoefInput_mirt()
},
include.rownames = T)

oneparamirtcoefInput_mirt_tab <- reactive({
  fit <- one_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  se_tab <- c()
  for (item in 1:nrow(par_tab)){
    pick <- c(1, item + 1)
    ad <- parvec[pick]
    v <- vcov[pick, pick]

    SEs <- deltamethod(list(~ x1, ~ -x2/x1), ad, v)
    names(SEs) <- c('a', 'b')
    se_tab <- rbind(se_tab, SEs)
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 3, 2, 4)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp[input$onePLSliderChar,]
})

output$oneparamirtcoef_mirt_tab <- renderTable({
  oneparamirtcoefInput_mirt_tab()
},
include.rownames = T)

# ** Download table ######
output$download_1pl_table <- downloadHandler(
  filename = function() {
    paste("1PL_table",".csv",sep = "")
  },
  content = function(file) {
    data <- oneparamirtcoefInput_mirt()
    write.csv(data,file)
  }
)

# * Abilities estimates parameters * #
onePlAbilities <- reactive({

  ts <- as.vector(total_score())
  sts <- as.vector(z_score())
  fs <- as.vector(fscores(one_param_irt_mirt()))
  fs.Err <- as.vector(fscores(one_param_irt_mirt(), full.scores.SE = TRUE)[,2])
  tab <- data.frame(cbind(ts,sts,fs,fs.Err))
  colnames(tab) <- c('Total scores', 'Z-score', 'F-scores', 'SE of F-score')
  n <- nrow(tab)
  nam <- vector('character', length = n)
  for (i in 1:n) {

    nam[i] <- paste('Respondent',i)

  }
  rownames(tab) <- c(nam)
  tab
})

output$one_PL_abilities <- renderTable({

  head(onePlAbilities(),6)

}, include.rownames = TRUE)

# ** Download abilities ######
output$download_onePL_abilities <- downloadHandler(
  filename = function() {
    paste("1PL_abilities",".csv",sep = "")
  },
  content = function(file) {
    data <- onePlAbilities()
    write.csv(data,file, col.names = TRUE)
  }
)

# *** Factor scores correlation ######
oneparamirtFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(one_param_irt_mirt()))
  sts <- z_score()

  whok <- !(is.na(fs) | is.na(sts))

  cor <- cor(fs[whok], sts[whok])
  cor
})
output$oneparamirtFactorCor_mirt <- renderText({
  paste("The Pearson correlation coefficient between standardized total score (Z-score)
        and factor score estimated by IRT model is", round(oneparamirtFactorCorInput_mirt(), 3))
})
# *** Factor scores plot ######
oneparamirtFactorInput_mirt <- reactive({

  fs <- as.vector(fscores(one_param_irt_mirt()))
  sts <- z_score()

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app() +
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box = "vertical",
          legend.key.size = unit(1, "lines"),
          legend.text.align = 0,
          legend.title.align = 0)
})

output$oneparamirtFactor_mirt <- renderPlot({
  oneparamirtFactorInput_mirt()
})

output$DP_oneparamirtFactor_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLFactorVsStandardized.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = oneparamirtFactorInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Wright Map ######
oneparamirtWrightMapInput_mirt <- reactive({
  fit <- one_param_irt_mirt()
  fs <- as.vector(fscores(fit))

  b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
  names(b) <- item_names()

  ggWrightMap(fs, b)
})


oneparamirtWrightMapReportInput_mirt <- reactive({
  if (input$irt_type_report != "none") {
    fit <- one_param_irt_mirt()
    fs <- as.vector(fscores(fit))

    b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
    names(b) <- item_names()

    list <- list()
    list$fs <- fs
    list$b <- b

    list
  } else {
    list <- ""
    list
  }

})

output$oneparamirtWrightMap_mirt<- renderPlot({
  oneparamirtWrightMapInput_mirt()
})

output$DP_oneparamirtWM_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLWrightMap.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = oneparamirtWrightMapInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 2PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
two_param_irt_mirt <- reactive({
  data <- binary()
  fit2PL <- mirt(data, model = 1, itemtype = "2PL",
                 constrain = NULL,
                 SE = T, verbose = F,
                 technical = list(NCYCLES = input$ncycles))
})

# Updating number of items in slider input, in ITEMS tab #
observe({
  updateSliderInput(session, 'twoPLSliderChar', max = length(item_names()))
})

# *** CC ######
twoparamirtInput_mirt <- reactive({
  plt <- plot(two_param_irt_mirt(), type = 'trace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Probability', 'Item')

  g <- ggplot(data= df, aes(x = Ability, y = Probability, color = Item )) +
    geom_line() +
    ylab('Probability of correct answer') +
    theme_app()
})

output$twoparamirt_mirt <- renderPlotly({
  p <- ggplotly(twoparamirtInput_mirt())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_twoparamirt_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = twoparamirtInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** CC items ######
twoparamirtInput_mirt_tab <- reactive({
  plt <- plot(two_param_irt_mirt(), type = 'trace', which.item = input$twoPLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c("Ability", "Probability")

  g <- ggplot(data = df, aes(x = Ability, y = Probability)) +
    geom_line(color = cols[input$twoPLSliderChar]) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[input$twoPLSliderChar]) +
    theme_app()
})

output$twoparamirt_mirt_tab <- renderPlotly({
  p <- ggplotly(twoparamirtInput_mirt_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ","", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }


  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_twoparamirt_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_2PLItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = twoparamirtInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC ######
twoparamirtiicInput_mirt <- reactive({
  plt <- plot(two_param_irt_mirt(), type = 'infotrace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Information', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Information, color = Item)) +
    geom_line() +
    ylab('Information') +
    theme_app()
})

output$twoparamirtiic_mirt <- renderPlotly({
  p <- ggplotly(twoparamirtiicInput_mirt())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_twoparamirtiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = twoparamirtiicInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** IIC items ####
twoparamirtiicInput_mirt_tab <- reactive({
  plt <- plot(two_param_irt_mirt(), type = 'infotrace', which.item = input$twoPLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c('Ability', 'Information')

  g <- ggplot(data = df, aes(x = Ability, y = Information)) +
    geom_line(color = cols[input$twoPLSliderChar]) +
    ylab("Information") +
    ggtitle(item_names()[input$twoPLSliderChar]) +
    theme_app()
})

output$twoparamirtiic_mirt_tab <- renderPlotly({
  p <- ggplotly(twoparamirtiicInput_mirt_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_twoparamirtiic_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_2PLItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = twoparamirtiicInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** TIF ######
twoparamirttifInput_mirt <- reactive({
  plt <- plot(two_param_irt_mirt(), type = "infoSE")

  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  df <- data.frame(cbind(Ability = x, Information = y))

  df$SE <- 1/sqrt(df$Information)

  ggplot(data = df, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information",
                       sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = 'pink')) +
    theme_app()

})

output$twoparamirttif_mirt <- renderPlotly({
  g <- twoparamirttifInput_mirt()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_twoparamirttif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = twoparamirttifInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Table of parameters ######
twoparamirtcoefInput_mirt <- reactive({
  fit <- two_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  se_tab <- c()
  for (item in seq(1, 2*nrow(par_tab), 2)){
    pick <- c(item, item + 1)
    ad <- parvec[pick]
    v <- vcov[pick, pick]

    SEs <- deltamethod(list(~ x1, ~ -x2/x1), ad, v)
    names(SEs) <- c('a', 'b')
    se_tab <- rbind(se_tab, SEs)
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 3, 2, 4)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SEd)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp
})

output$twoparamirtcoef_mirt <- renderTable({
  twoparamirtcoefInput_mirt()
},
include.rownames = T)

twoparamirtcoefInput_mirt_tab <- reactive({
  fit <- two_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  se_tab <- c()
  for (item in seq(1, 2*nrow(par_tab), 2)){
    pick <- c(item, item + 1)
    ad <- parvec[pick]
    v <- vcov[pick, pick]

    SEs <- deltamethod(list(~ x1, ~ -x2/x1), ad, v)
    names(SEs) <- c('a', 'b')
    se_tab <- rbind(se_tab, SEs)
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 3, 2, 4)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SEd)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp[input$twoPLSliderChar,]
})

output$twoparamirtcoef_mirt_tab <- renderTable({
  twoparamirtcoefInput_mirt_tab()
},
include.rownames = T)

# ** Download table ######
output$download_2pl_table <- downloadHandler(
  filename = function() {
    paste("2PL_table",".csv",sep = "")
  },
  content = function(file) {
    data <- twoparamirtcoefInput_mirt()
    write.csv(data,file)
  }
)

# * Abilities estimates parameters * #
twoPlAbilities <- reactive({

  ts <- as.vector(total_score())
  sts <- as.vector(z_score())
  fs <- as.vector(fscores(two_param_irt_mirt()))
  fs.Err <- as.vector(fscores(two_param_irt_mirt(), full.scores.SE = TRUE)[, 2])
  tab <- data.frame(cbind(ts, sts, fs, fs.Err))
  colnames(tab) <- c('Total scores', 'Z-score', 'F-scores', 'SE of F-score')
  n <- nrow(tab)
  nam <- vector('character', length = n)
  for (i in 1:n) {
    nam[i] <- paste('Respondent', i)
  }
  rownames(tab) <- c(nam)
  tab
})

output$two_PL_abilities <- renderTable({
  head(twoPlAbilities(), 6)
}, include.rownames = TRUE)

# ** Download abilities ######
output$download_twoPL_abilities <- downloadHandler(
  filename = function() {
    paste("2PL_abilities", ".csv", sep = "")
  },
  content = function(file) {
    data <- twoPlAbilities()
    write.csv(data, file, col.names = TRUE)
  }
)

# *** Factor scores correlation ######
twoparamirtFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(two_param_irt_mirt()))
  sts <- z_score()

  whok <- !(is.na(fs) | is.na(sts))

  cor <- cor(fs[whok], sts[whok])
  cor
})

output$twoparamirtFactorCor_mirt <- renderText({
  paste("The Pearson correlation coefficient between standardized total score (Z-score)
        and factor score estimated by IRT model is", round(twoparamirtFactorCorInput_mirt(), 3))
})
# *** Factor scores plot ######
twoparamirtFactorInput_mirt <- reactive({

  fs <- as.vector(fscores(two_param_irt_mirt()))
  sts <- z_score()

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app() +
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box = "vertical",
          legend.key.size = unit(1, "lines"),
          legend.text.align = 0,
          legend.title.align = 0)
})

output$twoparamirtFactor_mirt <- renderPlot({
  twoparamirtFactorInput_mirt()
})



output$DP_twoparamirtFactor_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLFactorVsStandardized.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = twoparamirtFactorInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 3PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

three_param_irt_mirt <- reactive({
  data <- binary()
  fit3PL <- mirt(data, model = 1, itemtype = "3PL",
                 constrain = NULL,
                 SE = T, technical = list(NCYCLES = input$ncycles),
                 verbose = F)
})

# Updating number of items in slider input, in ITEMS tab #
observe({
  updateSliderInput(session, 'threePLSliderChar', max = length(item_names()))
})

output$irt_3PL_model_converged <- renderUI({
  fit <- three_param_irt_mirt()
  txt <- ifelse(fit@OptimInfo$converged,
                "",
                "<font color = 'orange'>
                Estimation process terminated without convergence.
                Estimates are not reliable.
                </font>")
  HTML(txt)
})

# *** CC ######
threeparamirtInput_mirt <- reactive({
  plt <- plot(three_param_irt_mirt(), type = 'trace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Probability', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Probability, color = Item )) +
    geom_line() +
    ylab('Probability of correct answer') +
    theme_app()
})

output$threeparamirt_mirt <- renderPlotly({
  p <- ggplotly(threeparamirtInput_mirt())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_threeparamirt_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = threeparamirtInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** CC items ####
threeparamirtInput_mirt_tab <- reactive({
  plt <- plot(three_param_irt_mirt(), type = 'trace', which.item = input$threePLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c("Ability", "Probability")

  g <- ggplot(data = df, aes(x = Ability, y = Probability)) +
    geom_line(color = cols[input$threePLSliderChar]) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[input$threePLSliderChar]) +
    theme_app()
})

output$threeparamirt_mirt_tab <- renderPlotly({
  p <- ggplotly(threeparamirtInput_mirt_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_threeparamirt_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_3PLItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = threeparamirtInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC ######
threeparamirtiicInput_mirt <- reactive({
  plt <- plot(three_param_irt_mirt(), type = 'infotrace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Information', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Information, color = Item )) +
    geom_line() +
    ylab('Information') +
    theme_app()
})

output$threeparamirtiic_mirt <- renderPlotly({
  p <- ggplotly(threeparamirtiicInput_mirt())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_threeparamirtiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = threeparamirtiicInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** IIC items ######
threeparamirtiicInput_mirt_tab <- reactive({
  plt <- plot(three_param_irt_mirt(), type = 'infotrace', which.item = input$threePLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c('Ability', 'Information')

  g <- ggplot(data = df, aes(x = Ability, y = Information)) +
    geom_line(color = cols[input$threePLSliderChar]) +
    ylab("Information") +
    ggtitle(item_names()[input$threePLSliderChar]) +
    theme_app()
})

output$threeparamirtiic_mirt_tab <- renderPlotly({
  p <- ggplotly(threeparamirtiicInput_mirt_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_threeparamirtiic_mirt_tab <- downloadHandler(
  filename =  function() {
    paste("fig_3PLItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = threeparamirtiicInput_mirt_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** TIF ######
threeparamirttifInput_mirt <- reactive({
  plt <- plot(three_param_irt_mirt(), type = "infoSE")

  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  df <- data.frame(cbind(Ability = x, Information = y))

  df$SE <- 1/sqrt(df$Information)

  ggplot(data = df, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information",
                       sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = 'pink')) +
    theme_app()

})

output$threeparamirttif_mirt <- renderPlotly({
  g <- threeparamirttifInput_mirt()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_threeparamirttif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = threeparamirttifInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)


# *** Table of parameters ######
threeparamirtcoefInput_mirt <- reactive({
  fit <- three_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b", "g")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  if (all(is.na(vcov))){
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  } else {
    se_tab <- c()
    for (item in seq(1, 3*nrow(par_tab), 3)){
      pick <- c(item, item + 1, item + 2)
      ad <- parvec[pick]
      v <- vcov[pick, pick]

      SEs <- deltamethod(list(~ x1, ~ -x2/x1, ~ x3), ad, v)
      names(SEs) <- c('a', 'b', 'c')
      se_tab <- rbind(se_tab, SEs)
    }
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 4, 2, 5, 3, 6)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp
})

output$threeparamirtcoef_mirt <- renderTable({
  threeparamirtcoefInput_mirt()
},
include.rownames = T)

threeparamirtcoefInput_mirt_tab <- reactive({
  fit <- three_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b", "g")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  if (all(is.na(vcov))){
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  } else {
    se_tab <- c()
    for (item in seq(1, 3*nrow(par_tab), 3)){
      pick <- c(item, item + 1, item + 2)
      ad <- parvec[pick]
      v <- vcov[pick, pick]

      SEs <- deltamethod(list(~ x1, ~ -x2/x1, ~ x3), ad, v)
      names(SEs) <- c('a', 'b', 'c')
      se_tab <- rbind(se_tab, SEs)
    }
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 4, 2, 5, 3, 6)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp[input$threePLSliderChar,]
})

output$threeparamirtcoef_mirt_tab <- renderTable({
  threeparamirtcoefInput_mirt_tab()
},
include.rownames = T)

# ** Download table ######
output$download_3pl_table <- downloadHandler(
  filename = function() {
    paste("3PL_table",".csv",sep = "")
  },
  content = function(file) {
    data <- threeparamirtcoefInput_mirt()
    write.csv(data,file)
  }
)

# * Abilities estimates parameters * #
threePlAbilities <- reactive({

  ts <- as.vector(total_score())
  sts <- as.vector(z_score())
  fs <- as.vector(fscores(three_param_irt_mirt()))
  fs.Err <- as.vector(fscores(three_param_irt_mirt(), full.scores.SE = TRUE)[, 2])
  tab <- data.frame(cbind(ts, sts, fs, fs.Err))
  colnames(tab) <- c('Total scores', 'Z-score', 'F-scores', 'SE of F-score')
  n <- nrow(tab)
  nam <- vector('character', length = n)
  for (i in 1:n) {
    nam[i] <- paste('Respondent', i)
  }
  rownames(tab) <- c(nam)

  tab
})

output$three_PL_abilities <- renderTable({
  head(threePlAbilities(), 6)
}, include.rownames = TRUE)

# ** Download abilities ######
output$download_threePL_abilities <- downloadHandler(
  filename = function() {
    paste("3PL_abilities",".csv",sep = "")
  },
  content = function(file) {
    data <- threePlAbilities()
    write.csv(data,file, col.names = TRUE)
  }
)

# *** Factor scores plot ######
threeparamirtFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(three_param_irt_mirt()))
  sts <- z_score()

  whok <- !(is.na(fs) | is.na(sts))

  cor <- cor(fs[whok], sts[whok])
  cor
})
output$threeparamirtFactorCor_mirt <- renderText({
  paste("The Pearson correlation coefficient between standardized total score (Z-score)
        and factor score estimated by IRT model is", round(threeparamirtFactorCorInput_mirt(), 3))
})
# *** Factor scores plot ####
threeparamirtFactorInput_mirt <- reactive({

  fs <- as.vector(fscores(three_param_irt_mirt()))
  sts <- z_score()

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app() +
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box = "vertical",
          legend.key.size = unit(1, "lines"),
          legend.text.align = 0,
          legend.title.align = 0)
})

output$threeparamirtFactor_mirt <- renderPlot({
  threeparamirtFactorInput_mirt()
})

output$DP_threeparamirtFactor_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLFactorVsStandardized.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = threeparamirtFactorInput_mirt() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 4PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

irt_4PL_model <- reactive({
  data <- binary()
  fit <- mirt(data, model = 1, itemtype = "4PL",
              constrain = NULL,
              SE = T, technical = list(NCYCLES = input$ncycles),
              verbose = F)
})

# Updating number of items in slider input, in ITEMS tab #
observe({
  updateSliderInput(session, 'fourPLSliderChar', max = length(item_names()))
})

output$irt_4PL_model_converged <- renderUI({
  fit <- irt_4PL_model()
  txt <- ifelse(fit@OptimInfo$converged,
                "",
                "<font color = 'orange'>
                Estimation process terminated without convergence.
                Estimates are not reliable.
                </font>")
  HTML(txt)
})

# *** ICC ######
irt_4PL_icc_Input <- reactive({
  plt <- plot(irt_4PL_model(), type = 'trace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Probability', 'Item')

  g <- ggplot(data= df, aes(x = Ability, y = Probability, color = Item )) +
    geom_line() +
    ylab('Probability of correct answer') +
    theme_app()
})

output$irt_4PL_icc <- renderPlotly({
  p <- ggplotly(irt_4PL_icc_Input())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DB_irt_4PL_icc <- downloadHandler(
  filename =  function() {
    paste("fig_IRT_4PL_icc.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_4PL_icc_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** CC items ####
irt_4PL_icc_Input_tab <- reactive({
  plt <- plot(irt_4PL_model(), type = 'trace', which.item = input$fourPLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c("Ability", "Probability")

  g <- ggplot(data = df, aes(x = Ability, y = Probability)) +
    geom_line(color = cols[input$fourPLSliderChar]) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[input$fourPLSliderChar]) +
    theme_app()
})

output$irt_4PL_icc_item_tab <- renderPlotly({
  p <- ggplotly(irt_4PL_icc_Input_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DB_irt_4PL_icc_tab <- downloadHandler(
  filename =  function() {
    paste("fig_IRT_4PL_icc.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_4PL_icc_Input_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC ######
irt_4PL_iic_Input <- reactive({
  plt <- plot(irt_4PL_model(), type = 'infotrace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))
  colnames(df) <- c('Ability', 'Information', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Information, color = Item )) +
    geom_line() +
    ylab('Information') +
    theme_app()
})

output$irt_4PL_iic <- renderPlotly({
  p <- ggplotly(irt_4PL_iic_Input())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DB_irt_4PL_iic <- downloadHandler(
  filename =  function() {
    paste("fig_IRT_4PL_iic.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_4PL_iic_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
    print(irt_4PL_iic_Input())
    dev.off()
  }
)
# *** IIC items ####
irt_4PL_iic_Input_tab <- reactive({
  plt <- plot(irt_4PL_model(), type = 'infotrace', which.item = input$fourPLSliderChar, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(n)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  colnames(df) <- c('Ability', 'Information')

  g <- ggplot(data = df, aes(x = Ability, y = Information)) +
    geom_line(color = cols[input$fourPLSliderChar]) +
    ylab("Information") +
    ggtitle(item_names()[input$fourPLSliderChar]) +
    theme_app()
})

output$irt_4PL_iic_item_tab <- renderPlotly({
  p <- ggplotly(irt_4PL_iic_Input_tab())

  for (j in 1:length(p$x$data)) {
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DB_irt_4PL_iic_tab <- downloadHandler(
  filename =  function() {
    paste("fig_IRT_4PL_iic.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_4PL_iic_Input_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** TIF ######
irt_4PL_tif_Input <- reactive({
  plt <- plot(irt_4PL_model(), type = "infoSE")

  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  df <- data.frame(cbind(Ability = x, Information = y))

  df$SE <- 1/sqrt(df$Information)

  ggplot(data = df, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information",
                       sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = 'pink')) +
    theme_app()
})

output$irt_4PL_tif <- renderPlotly({
  g <- irt_4PL_tif_Input()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DB_irt_4PL_tif <- downloadHandler(
  filename =  function() {
    paste("fig_IRT_4PL_tif.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_4PL_tif_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Table of parameters ######
irt_4PL_coef_Input <- reactive({
  fit <- irt_4PL_model()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b", "g", "u")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  if (all(is.na(vcov))){
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  } else {
    se_tab <- c()
    for (item in seq(1, 4*nrow(par_tab), 4)){
      pick <- c(item, item + 1, item + 2, item + 3)
      ad <- parvec[pick]
      v <- vcov[pick, pick]

      SEs <- deltamethod(list(~ x1, ~ -x2/x1, ~ x3, ~ x4), ad, v)
      names(SEs) <- c('a', 'b', 'c', 'd')
      se_tab <- rbind(se_tab, SEs)
    }
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 5, 2, 6, 3, 7, 4, 8)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR: ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp
})

output$irt_4PL_coef <- renderTable({
  irt_4PL_coef_Input()
},
include.rownames = T)

irt_4PL_coef_Input_tab <- reactive({
  fit <- irt_4PL_model()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b", "g", "u")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  if (all(is.na(vcov))){
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  } else {
    se_tab <- c()
    for (item in seq(1, 4*nrow(par_tab), 4)){
      pick <- c(item, item + 1, item + 2, item + 3)
      ad <- parvec[pick]
      v <- vcov[pick, pick]

      SEs <- deltamethod(list(~ x1, ~ -x2/x1, ~ x3, ~ x4), ad, v)
      names(SEs) <- c('a', 'b', 'c', 'd')
      se_tab <- rbind(se_tab, SEs)
    }
  }


  tab <- cbind(par_tab, se_tab)[, c(1, 5, 2, 6, 3, 7, 4, 8)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR: ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SE(a)", "b", "SE(b)", "c", "SE(c)", "d", "SE(d)",
                          "SX2-value", "df", "p-value")
  rownames(tab.comp) <- item_names()

  tab <- round(tab, 3)
  tab.comp[, colnames(tab.comp) %in% colnames(tab)] <- tab

  tab.comp[input$fourPLSliderChar,]
})

output$irt_4PL_coef_tab <- renderTable({
  irt_4PL_coef_Input_tab()
},
include.rownames = T)

# ** Download table ######
output$download_4pl_table <- downloadHandler(
  filename = function() {
    paste("4PL_table",".csv",sep = "")
  },
  content = function(file) {
    data <- irt_4PL_coef_Input()
    write.csv(data,file)
  }
)

# * Abilities estimates parameters * #
fourPlAbilities <- reactive({

  ts <- as.vector(total_score())
  sts <- as.vector(z_score())
  fs <- as.vector(fscores(irt_4PL_model()))
  fs.Err <- as.vector(fscores(irt_4PL_model(), full.scores.SE = TRUE)[, 2])
  tab <- data.frame(cbind(ts,sts,fs,fs.Err))
  colnames(tab) <- c('Total scores', 'Z-score', 'F-scores', 'SE of F-score')
  n <- nrow(tab)
  nam <- vector('character', length = n)
  for (i in 1:n) {
    nam[i] <- paste('Respondent', i)
  }
  rownames(tab) <- c(nam)

  tab
})

output$four_PL_abilities <- renderTable({
  head(fourPlAbilities(), 6)
}, include.rownames = TRUE)

# ** Download abilities ######
output$download_fourPL_abilities <- downloadHandler(
  filename = function() {
    paste("4PL_abilities", ".csv", sep = "")
  },
  content = function(file) {
    data <- fourPlAbilities()
    write.csv(data, file, col.names = TRUE)
  }
)

# *** Factor scores plot ######
irt_4PL_factorscores_correlation_Input <- reactive({

  fs <- as.vector(fscores(irt_4PL_model()))
  sts <- z_score()

  whok <- !(is.na(fs) | is.na(sts))

  cor <- cor(fs[whok], sts[whok])
  cor
})

output$irt_4PL_factorscores_correlation <- renderText({
  paste("The Pearson correlation coefficient between standardized total score (Z-score)
        and factor score estimated by IRT model is",
        round(irt_4PL_factorscores_correlation_Input(), 3))
})
# *** Factor scores plot ####
irt_4PL_factorscores_plot_Input <- reactive({

  fs <- as.vector(fscores(irt_4PL_model()))
  sts <- z_score()

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app() +
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box = "vertical",
          legend.key.size = unit(1, "lines"),
          legend.text.align = 0,
          legend.title.align = 0)
})

output$irt_4PL_factorscores_plot <- renderPlot({
  irt_4PL_factorscores_plot_Input()
})

output$DB_irt_4PL_factorscores_plot <- downloadHandler(
  filename =  function() {
    paste("fig_IRT_4PL_factorscores.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_4PL_factorscores_plot_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT COMPARISON ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

irtcomparisonInput <- reactive({
  fit1PL <- one_param_irt_mirt()
  fit2PL <- two_param_irt_mirt()
  fit3PL <- three_param_irt_mirt()
  fit4PL <- irt_4PL_model()

  models <- list(fit1PL = fit1PL,
                 fit2PL = fit2PL,
                 fit3PL = fit3PL,
                 fit4PL = fit4PL)

  df <- rbind(anova(models[[1]], models[[2]], verbose = F),
              anova(models[[2]], models[[3]], verbose = F),
              anova(models[[3]], models[[4]], verbose = F))

  df <- round(df[c(1, 2, 4, 6), ], 3)
  df <- df[, c("AIC", "AICc", "BIC", "SABIC", "logLik", "X2", "df", "p")]
  nam <- c("1PL", "2PL", "3PL", "4PL")

  if (all(df[, "p"] > 0.05)){
    hv <- "1PL"
  } else {
    p <- which(df[, "p"] < 0.05)
    hv <- nam[p[length(p)]]
  }

  df <- rbind(df,
              c(nam[sapply(1:4, function(i) which(df[, i] == min(df[, i], na.rm = T)))],
                rep("", 3),
                hv))

  rownames(df) <- c(nam, "BEST")
  df
})

output$irtcomparison <- renderTable({
  irtcomparisonInput()
},
include.rownames = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BOCKS NOMINAL MODEL ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** UPDATING MAXIMUM VALUE IN INPUT SLIDER, IN ITEMS TAB ** #
observe({
  updateSliderInput(session, 'bockSlider', max = length(item_names()))
})

adj_data_bock <- reactive({
  a <- nominal()
  k <- as.factor(key())

  m <- ncol(a)
  lev <- unlist(lapply(1:m, function(i) levels(factor(unlist(a[, i, with = F])))))
  lev <- c(lev, levels(k))
  lev <- unique(lev)
  lev_num <- as.numeric(as.factor(lev))

  lev_k_num <- sapply(1:length(levels(k)),
                      function(i) lev_num[levels(k)[i] == lev])

  lev_a_num <- lapply(1:m, function(i)
    sapply(1:length(levels(factor(unlist(a[, i, with = F])))),
           function(j) lev_num[levels(factor(unlist(a[, i, with = F])))[j] == lev]))

  levels(k) <- lev_k_num
  k <- as.numeric(paste(k))


  a <- data.frame(a)
  for (i in 1:m){
    levels(a[, i]) <- lev_a_num[[i]]
    a[, i] <- as.numeric(paste(unlist(a[, i])))
  }

  list(data = data.table(a), key = k,
       lev_a_num = lev_a_num)
})


bock_irt_mirt <- reactive({
  data <- adj_data_bock()$data
  key <- adj_data_bock()$key

  sv <- mirt(data, 1, 'nominal', pars = 'values', verbose = F, SE = T)

  # set all values to 0 and estimated
  sv$value[grepl('ak', sv$name)] <- 0
  sv$est[grepl('ak', sv$name)] <- TRUE

  nms <- colnames(data)
  for(i in 1:length(nms)){

    #set highest category based on key fixed to 3
    pick <- paste0('ak', key[i] - 1)
    index <- sv$item == nms[i] & pick == sv$name
    sv[index, 'value'] <- 3
    sv[index, 'est'] <- FALSE

    # set arbitrary lowest category fixed at 0
    if(pick == 'ak0') pick2 <- 'ak3'
    else pick2 <- paste0('ak', key[i] - 2)
    index2 <- sv$item == nms[i] & pick2 == sv$name
    sv[index2, 'est'] <- FALSE
  }

  fit <- mirt(data, 1, 'nominal', pars = sv, SE = T, verbose = F)
  fit
})

# *** CC ######
bock_CC_Input <- reactive({
  plt <- plot(bock_irt_mirt(), type = 'trace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)

  df$names <- factor(names, levels = paste("Item", 1:m))

  df <- df[order(df$x), ]
  # 200 je delka sekvence -6 az 6
  df$line <- paste0("line", (rep(1:(n/200), 200)))

  colnames(df) <- c('Ability', 'Probability', 'Item', 'line')

  g <- ggplot(data = df, aes(x = Ability, y = Probability, color = Item, group = line)) +
    geom_line() +
    ylab('Probability of answer') +
    theme_app()

})
output$bock_CC <- renderPlotly({
  p <- ggplotly(bock_CC_Input())

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})
output$DP_bock_CC <- downloadHandler(
  filename =  function() {
    paste("fig_BockItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = bock_CC_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** CC items ######
bock_CC_Input_tab <- reactive({
  plt <- plot(bock_irt_mirt(), type = 'trace', which.item = input$bockSlider)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  k <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(k)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  df <- df[order(df$x), ]
  colnames(df) <- c("Ability", "Probability")

  # 200 je delka sekvence -6 az 6
  df$Option <- as.factor(paste0("line", (rep(1:(n/200), 200))))
  levels(df$Option) <- levels(as.data.frame(nominal())[, input$bockSlider])

  g <- ggplot(data = df, aes(x = Ability, y = Probability, group = Option, linetype = Option)) +
    geom_line(color = cols[input$bockSlider]) +
    ylab("Probability of answer") +
    ggtitle(item_names()[input$bockSlider]) +
    theme_app()
})

output$bock_CC_tab <- renderPlotly({
  p <- ggplotly(bock_CC_Input_tab())

  # changing plotly description
  for (j in 1:length(p$x$data)){
    txt0 <- unique(sub("^.+<br />", "", p$x$data[[j]]$text))
    pos <- gregexpr(txt0,  p$x$data[[j]]$text)[[1]][2] - 6
    text <- substring(p$x$data[[j]]$text, 1, pos - 1)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})
output$DP_bock_CC_tab <- downloadHandler(
  filename =  function() {
    paste("fig_BockItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = bock_CC_Input_tab() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC ######
bock_IIC_Input <- reactive({
  plt <- plot(bock_irt_mirt(), type = 'infotrace', facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  n <- length(vals[[1]]$subscripts)
  m <- length(item_names())
  k <- n/m
  names <- list()

  for(j in 1:m){
    names[[j]] <- rep(paste('Item', j), k)
  }

  names <- unlist(names)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <-as.numeric(df$y)
  df$names <- factor(names, levels = paste("Item", 1:m))


  df <- df[order(df$x), ]

  colnames(df) <- c('Ability', 'Information', 'Item')

  g <- ggplot(data = df, aes(x = Ability, y = Information , color = Item)) +
    geom_line() +
    ylab('Information') +
    theme_app()
})
output$bock_IIC <- renderPlotly({
  p <- ggplotly(bock_IIC_Input())

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})
output$DP_bock_IIC <- downloadHandler(
  filename =  function() {
    paste("fig_BockItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = bock_IIC_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** IIC items ####
bock_IIC_Input_tab <- reactive({
  plt <- plot(bock_irt_mirt(), type = 'infotrace', which.item = input$bockSlider, facet_items = F)
  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  k <- ncol(binary())

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- gg_color_hue(k)

  df <- data.frame(cbind(x, y))
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  colnames(df) <- c('Ability', 'Information')

  g <- ggplot(data = df, aes(x = Ability, y = Information)) +
    geom_line(color = cols[input$bockSlider]) +
    ylab("Information") +
    ggtitle(item_names()[input$bockSlider]) +
    theme_app()
})
output$bock_IIC_tab <- renderPlotly({
  p <- ggplotly(bock_IIC_Input_tab())

  # changing plotly description
  for (j in 1:length(p$x$data)){
    text <- gsub("Item: ", "", p$x$data[[j]]$text)
    p$x$data[[j]]$text <- text
  }

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})
output$DP_bock_IIC_tab <- downloadHandler(
  filename =  function() {
    paste("fig_BockItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = bock_IIC_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** TIF ######
bock_TIF_Input <- reactive({
  plt <- plot(bock_irt_mirt(), type = "infoSE")

  vals <- plt$panel.args
  x <- vals[[1]]$x
  y <- vals[[1]]$y
  df <- data.frame(cbind(Ability = x, Information = y))

  df$SE <- 1/sqrt(df$Information)

  ggplot(data = df, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information",
                       sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = 'pink')) +
    theme_app()
})

output$bock_TIF <- renderPlotly({
  g <- bock_TIF_Input()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL

  p %>% plotly::config(displayModeBar = F)
})

output$DP_bock_TIF <- downloadHandler(
  filename =  function() {
    paste("fig_BockTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = bock_TIF_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Table of parameters ######
output$bock_coef_warning <- renderText({
  fit <- bock_irt_mirt()

  coeftab <- coef(fit, printSE = T)
  m <- length(coeftab) - 1

  dims <- sapply(coeftab, dim)[, -(m+1)]
  if (length(unique(dims[2, ])) == 1){
    hide("bock_coef_warning")
  } else {
    show("bock_coef_warning")
  }
  paste("Sorry, for this dataset table is not available!")

})

bock_coef_Input <- reactive({
  fit <- bock_irt_mirt()

  coeftab <- coef(fit, printSE = T)
  m <- length(coeftab) - 1

  dims <- sapply(coeftab, dim)[, -(m+1)]
  if (length(unique(dims[2, ])) == 1){
    partab <- t(sapply(1:m, function(i) coeftab[[i]][1, ]))
    if (unique(dims[1, ]) == 1){
      setab <- matrix(NA, nrow = m, ncol = ncol(partab))
    } else {
      setab <- t(sapply(1:m, function(i) coeftab[[i]][2, ]))
    }

    n <- ncol(partab)
    tab <- c()
    for (i in 1:n){
      tab <- cbind(tab, partab[, i], setab[, i])
    }
    namPAR <- colnames(partab)
    namSE <- paste("SE(", colnames(partab), ")", sep = "")

    colnames(tab) <- c(sapply(1:n, function(i) c(namPAR[i], namSE[i])))
    rownames(tab) <- item_names()
  } else {
    tab <- NULL
  }

  tab
})

output$bock_coef <- renderTable({
  bock_coef_Input()
},
include.rownames = T,
include.colnames = T)

bock_coef_Input_tab <- reactive({
  fit <- bock_irt_mirt()

  coeftab <- coef(fit, printSE = T)
  m <- length(coeftab) - 1

  dims <- sapply(coeftab, dim)[, -(m+1)]
  if (length(unique(dims[2, ])) == 1){
    partab <- t(sapply(1:m, function(i) coeftab[[i]][1, ]))
    if (unique(dims[1, ]) == 1){
      setab <- matrix(NA, nrow = m, ncol = ncol(partab))
    } else {
      setab <- t(sapply(1:m, function(i) coeftab[[i]][2, ]))
    }

    n <- ncol(partab)
    tab <- c()
    for (i in 1:n){
      tab <- cbind(tab, partab[, i], setab[, i])
    }
    namPAR <- colnames(partab)
    namSE <- paste("SE(", colnames(partab), ")", sep = "")

    colnames(tab) <- c(sapply(1:n, function(i) c(namPAR[i], namSE[i])))
    rownames(tab) <- item_names()
  } else {
    tab <- NULL
  }

  tab[input$bockSlider,]
})

output$bock_coef_tab <- renderTable({
  t(bock_coef_Input_tab())
},
include.rownames = T,
include.colnames = T)

# *** Factor scores plot ######
bock_factor_Input <- reactive({

  fs <- as.vector(fscores(bock_irt_mirt()))
  sts <- z_score()

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app() +
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box = "vertical",
          legend.key.size = unit(1, "lines"),
          legend.text.align = 0,
          legend.title.align = 0)
})
output$bock_factor <- renderPlot({
  bock_factor_Input()
})
output$DP_bock_factor <- downloadHandler(
  filename =  function() {
    paste("fig_BockFactorVsStandardized.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = bock_factor_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Factor scores correlation ######
bockFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(bock_irt_mirt()))
  sts <- z_score()

  whok <- !(is.na(fs) | is.na(sts))

  cor <- cor(fs[whok], sts[whok])
  cor
})

output$bockFactorCorInput_mirt <- renderText({
  paste("The Pearson correlation coefficient between standardized total score (Z-score)
        and factor score estimated by Bock's nominal IRT model is", round(bockFactorCorInput_mirt(), 3))
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TRAINING ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** DICHOTOMOUS MODELS ######
# *** CC ######
output$ccIRT_interpretation <- renderUI({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta <- input$ccIRTSlider_theta

  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }

  prob1 <- ccirt(theta, a1, b1, c1, d1)
  prob2 <- ccirt(theta, a2, b2, c2, d2)

  txt1 <- paste("The probability of correct answer with latent ability &#952; = ", theta,
                " in <font color='red'>red</font> item with parameters ",
                paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)), collapse = ", "),
                " is equall to <b>", sprintf("%.2f", prob1), "</b>. ", sep = "")
  txt2 <- paste("The probability of correct answer with latent ability &#952; = ", theta,
                " in <font color='blue'>blue</font> item with parameters ",
                paste(paste(letters[1:4], "=", c(a2, b2, c2, d2)), collapse = ", "),
                " is equall to <b>", sprintf("%.2f", prob2), "</b>. ", sep = "")
  txt <- paste("<b>Interpretation: </b>", txt1, txt2)
  HTML(txt)
})



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

  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }

  df <- data.frame(X1 = ccirt(seq(-4, 4, 0.01), a1, b1, c1, d1),
                   X2 = ccirt(seq(-4, 4, 0.01), a2, b2, c2, d2),
                   theta = seq(-4, 4, 0.01))
  df <- melt(df, id.vars = "theta")

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    geom_segment(aes(y = ccirt(theta0, a = a1, b = b1, c = c1, d = d1),
                     yend = ccirt(theta0, a = a1, b = b1, c = c1, d = d1),
                     x = -4,
                     xend = theta0), color = "gray", linetype = "dashed") +
    geom_segment(aes(y = ccirt(theta0, a = a2, b = b2, c = c2, d = d2),
                     yend = ccirt(theta0, a = a2, b = b2, c = c2, d = d2),
                     x = -4,
                     xend = theta0), color = "gray", linetype = "dashed") +
    geom_segment(aes(y = 0,
                     yend = max(ccirt(theta0, a = a1, b = b1, c = c1, d = d1),
                                ccirt(theta0, a = a2, b = b2, c = c2, d = d2)),
                     x = theta0,
                     xend = theta0), color = "gray", linetype = "dashed") +
    xlim(-4, 4) +
    xlab("Ability") +
    ylab("Probability of correct answer") +
    ylim(0, 1) +
    scale_color_manual(name = "",
                       values = c("red", "blue"),
                       labels = c(paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
                                        collapse = ", "),
                                  paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
                                        collapse = ", "))) +
    theme_app() +
    ggtitle("Item characteristic curve")
  g

})

output$ccIRT_plot <- renderPlotly({
  g <- ccIRT_plot_Input()

  p <- ggplotly(g)

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
  text <- gsub("ccirt\\(theta0, a = a1, b = b1, c = c1, d = d1\\)", "Probability", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr('Probability', text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr('Probability', text)[[1]][2]
  text <- substring(text, 1, pos-1)
  p$x$data[[3]]$text <- text

  text <- gsub("~", "", p$x$data[[4]]$text)
  text <- gsub("ccirt\\(theta0, a = a2, b = b2, c = c2, d = d2\\)", "Probability", text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("theta: -4<br />", "", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  pos <- gregexpr('Probability', text)[[1]][2]
  text <- substring(text, pos)
  pos <- gregexpr('Probability', text)[[1]][2]
  text <- substring(text, 1, pos-1)
  p$x$data[[4]]$text <- text

  text <- gsub("~", "", p$x$data[[5]]$text)
  text <- gsub("max\\(ccirt\\(theta0, a = a1, b = b1, c = c1, d = d1\\), ccirt\\(theta0, \n    a = a2, b = b2, c = c2, d = d2\\)\\)", "Probability", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("<br />0: 0", "", text)
  text <- gsub("theta0", "Ability", text)
  text <- gsub("value", "Probability", text)
  text <- gsub("<br />variable: gray", "", text)
  text <- gsub(paste0("<br />y: ", theta0), "", text)
  pos <- gregexpr('Ability', text)[[1]][2]
  text <- substring(text, 1, pos-1)
  p$x$data[[5]]$text <- text

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_ccIRT <- downloadHandler(
  filename =  function() {
    paste("fig_CustomItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = ccIRT_plot_Input() +
             theme(legend.position = c(0.97, 0.03),
                   legend.justification = c(0.97, 0.03)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** ICC ######
iccIRT_plot_Input <- reactive({
  a1 <- input$ccIRTSlider_a1
  b1 <- input$ccIRTSlider_b1
  c1 <- input$ccIRTSlider_c1
  d1 <- input$ccIRTSlider_d1

  a2 <- input$ccIRTSlider_a2
  b2 <- input$ccIRTSlider_b2
  c2 <- input$ccIRTSlider_c2
  d2 <- input$ccIRTSlider_d2

  theta <- input$ccIRTSlider_theta

  iccirt <- function(theta, a, b, c, d){
    return((d - c)*a^2*exp(a*(theta - b))/(1 + exp(a*(theta - b)))^2)
  }

  df <- data.frame(X1 = iccirt(seq(-4, 4, 0.01), a1, b1, c1, d1),
                   X2 = iccirt(seq(-4, 4, 0.01), a2, b2, c2, d2),
                   theta = seq(-4, 4, 0.01))
  df <- melt(df, id.vars = "theta")

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlim(-4, 4) +
    ylim(0, 4) +
    xlab("Ability") +
    ylab("Information") +
    scale_color_manual(name = "",
                       breaks = c("X1", "X2"),
                       values = c("red", "blue"),
                       labels = c(paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
                                        collapse = ", "),
                                  paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
                                        collapse = ", "))) +
    theme_app() +
    ggtitle("Item information function")
  g
})

output$iccIRT_plot <- renderPlotly({
  g <- iccIRT_plot_Input()

  p <- ggplotly(g)

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

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_iccIRT <- downloadHandler(
  filename =  function() {
    paste("fig_CustomItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = iccIRT_plot_Input() +
             theme(legend.position = c(0.97, 0.97),
                   legend.justification = c(0.97, 0.97)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** EXERCISES ####
# **** Exercises 1 ####
irt_dich1_answers <- reactive({
  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }
  iccirt <- function(theta, a, b, c, d){
    return((d - c)*a^2*exp(a*(theta - b))/(1 + exp(a*(theta - b)))^2)
  }

  a1 <- 2.5; b1 <- -0.5; c1 <- 0; d1 <- 1
  a2 <- 1.5; b2 <- 0; c2 <- 0; d2 <- 1

  par1 <- c(a1, b1, c1, d1)
  par2 <- c(a2, b2, c2, d2)

  theta0 <- c(-2, -1, 0, 1, 2)

  cci1 <- ccirt(theta0, a1, b1, c1, d1)
  cci2 <- ccirt(theta0, a2, b2, c2, d2)

  theta <- (a1*b1 - a2*b2)/(a1 - a2)

  iccirt1a <- iccirt(-2, a1, b1, c1, d1)
  iccirt2a <- iccirt(-2, a2, b2, c2, d2)
  icca <- as.numeric(iccirt1a < iccirt2a) + 1
  iccirt1b <- iccirt( 0, a1, b1, c1, d1)
  iccirt2b <- iccirt( 0, a2, b2, c2, d2)
  iccb <- as.numeric(iccirt1b < iccirt2b) + 1
  iccirt1c <- iccirt( 2, a1, b1, c1, d1)
  iccirt2c <- iccirt( 2, a2, b2, c2, d2)
  iccc <- as.numeric(iccirt1c < iccirt2c) + 1

  answers <- list(par1 = par1,
                  par2 = par2,
                  cci1 = cci1,
                  cci2 = cci2,
                  theta = theta,
                  icc = c(icca, iccb, iccc))
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

  ans1 <- c(all(abs(par1 - par1input) <= 0.05) &  all(abs(par2 - par2input) <= 0.05))

  # answers 2, item 1
  cci1 <- answers[[3]]
  cci1input <- c(input$irt_training_dich1_1_2a, input$irt_training_dich1_1_2b, input$irt_training_dich1_1_2c,
                 input$irt_training_dich1_1_2d, input$irt_training_dich1_1_2e)
  ans2_1 <- c(abs(cci1 - cci1input) <= 0.05)
  # answers 2, item 1
  cci2 <- answers[[4]]
  cci2input <- c(input$irt_training_dich1_2_2a, input$irt_training_dich1_2_2b, input$irt_training_dich1_2_2c,
                 input$irt_training_dich1_2_2d, input$irt_training_dich1_2_2e)
  ans2_2 <- c(abs(cci2 - cci2input) <= 0.05)

  # answer 3
  ans3 <- c(abs(answers[[5]] - input$irt_training_dich1_3) <= 0.05)

  # answer 4,
  ans4 <- c(answers[["icc"]] == c(input$irt_training_dich1_4a, input$irt_training_dich1_4b, input$irt_training_dich1_4c))


  ans <- list(ans1 = ans1,
              ans2_1 = ans2_1,
              ans2_2 = ans2_2,
              ans3 = ans3,
              ans4 = ans4)
  res <- sum(sapply(ans, sum))/sum(sapply(ans, length))
  ans <- lapply(ans, function(x) ifelse(x,
                                        "<font color='green'>&#10004;</font>",
                                        "<font color='red'>&#10006;</font>"))
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
  HTML(ifelse(res == 1,
              "<font color='green'>Everything correct! Well done!</font>",
              paste0("<font color='red'>", round(100*res), "% correct. Try again.</font>")))
})


# **** Exercises 2 ####
irt_dich2_answers <- reactive({
  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }
  iccirt <- function(theta, a, b, c, d){
    return((d - c)*a^2*exp(a*(theta - b))/(1 + exp(a*(theta - b)))^2)
  }

  a1 <- 1.5; b1 <- 0; c1 <- 0; d1 <- 1
  a2 <- 1.5; b2 <- 0; c2 <- 0.2; d2 <- 1

  ans1 <- c(c1, c2)
  ans2 <- c((1 + c1)/2, (1 + c2)/2)
  ans3 <- 1

  answers <- list(ans1 = ans1,
                  ans2 = ans2,
                  ans3 = ans3)
  answers
})

irt_training_dich2_check <- eventReactive(input$irt_training_dich2_submit, {
  answers <- irt_dich2_answers()

  # answer 1
  c1cor <- answers[["ans1"]][1]
  c2cor <- answers[["ans1"]][2]
  c1inp <- input$irt_training_dich2_1a
  c2inp <- input$irt_training_dich2_1b

  ans1 <- c(abs(c1cor - c1inp) <= 0.05,
            abs(c2cor - c2inp) <= 0.05)

  # answers 2
  p1cor <- answers[["ans2"]][1]
  p2cor <- answers[["ans2"]][2]
  p1inp <- input$irt_training_dich2_2a
  p2inp <- input$irt_training_dich2_2b

  ans2 <- c(abs(p1cor - p1inp) <= 0.05,
            abs(p2cor - p2inp) <= 0.05)

  # answer 3
  itcor <- answers[["ans3"]]
  itinp <- input$irt_training_dich2_3
  ans3 <- (itcor == itinp)

  ans <- list(ans1 = ans1,
              ans2 = ans2,
              ans3 = ans3)
  res <- sum(sapply(ans, sum))/sum(sapply(ans, length))
  ans <- lapply(ans, function(x) ifelse(x,
                                        "<font color='green'>&#10004;</font>",
                                        "<font color='red'>&#10006;</font>"))
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
  HTML(ifelse(res == 1,
              "<font color='green'>Everything correct! Well done!</font>",
              paste0("<font color='red'>", round(100*res), "% correct. Try again.</font>")))
})

# **** Exercises 3 ####
irt_dich3_answers <- reactive({
  a1 <- 1.5; b1 <- 0; c1 <- 0; d1 <- 0.9
  a2 <- 1.5; b2 <- 0; c2 <- 0; d2 <- 1

  ans1 <- c(d1, d2)
  ans2 <- c(d1/2, d2/2)
  ans3 <- 2

  answers <- list(ans1 = ans1,
                  ans2 = ans2,
                  ans3 = ans3)
  answers
})

irt_training_dich3_check <- eventReactive(input$irt_training_dich3_submit, {
  answers <- irt_dich3_answers()

  # answer 1
  d1cor <- answers[["ans1"]][1]
  d2cor <- answers[["ans1"]][2]
  d1inp <- input$irt_training_dich3_1a
  d2inp <- input$irt_training_dich3_1b

  ans1 <- c(abs(d1cor - d1inp) <= 0.05,
            abs(d2cor - d2inp) <= 0.05)

  # answers 2
  p1cor <- answers[["ans2"]][1]
  p2cor <- answers[["ans2"]][2]
  p1inp <- input$irt_training_dich3_2a
  p2inp <- input$irt_training_dich3_2b

  ans2 <- c(abs(p1cor - p1inp) <= 0.05,
            abs(p2cor - p2inp) <= 0.05)

  # answer 3
  itcor <- answers[["ans3"]]
  itinp <- input$irt_training_dich3_3

  ans3 <- (itcor == itinp)

  ans <- list(ans1 = ans1,
              ans2 = ans2,
              ans3 = ans3)
  res <- sum(sapply(ans, sum))/sum(sapply(ans, length))
  ans <- lapply(ans, function(x) ifelse(x,
                                        "<font color='green'>&#10004;</font>",
                                        "<font color='red'>&#10006;</font>"))
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
  HTML(ifelse(res == 1,
              "<font color='green'>Everything correct! Well done!</font>",
              paste0("<font color='red'>", round(100*res), "% correct. Try again.</font>")))
})

# ** POLYTOMOUS MODELS ####
# *** GRADED RESPONSE MODEL ####

output$irt_training_grm_sliders <- renderUI({
  num <- input$irt_training_grm_numresp

  sliders <- tagList(
    tags$div(class = "js-irs-red",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_grm_b1", "b1 - difficulty",
                         value = -1.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-yellow",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_grm_b2", "b2 - difficulty",
                         value = -1, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-green",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_grm_b3", "b3 - difficulty",
                         value = -0.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-blue",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_grm_b4", "b4 - difficulty",
                         value = 0, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-purple",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_grm_b5", "b5 - difficulty",
                         value = 0.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-orange",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_grm_b6", "b6 - difficulty",
                         value = 1, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(2*num)]

  sliders
})
# *** Cummulative ######
irt_training_grm_plot_cummulative_Input <- reactive({
  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)){
    b <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
                "2" = b,
                "3" = c(b, input$irt_training_grm_b3),
                "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
                "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
                "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6))
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b){ return(1/(1 + exp(-a*(theta - b)))) }

  df <- data.frame(sapply(1:num, function(i) ccirt(theta, a, b[i])), theta)
  df <- melt(df, id.vars = "theta")

  col <- c("red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:num]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Cummulative probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y >= ", 1:length(col), ")")) +
    theme_app() +
    ggtitle("Cummulative probabilities")

  g
})

output$irt_training_grm_plot_cummulative <- renderPlotly({
  g <- irt_training_grm_plot_cummulative_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)){
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Cummulative probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i), paste0("P(Y >= ", i, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_irt_training_grm_plot_cummulative <- downloadHandler(
  filename =  function() {
    paste("fig_GRM_cummulative.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_training_grm_plot_cummulative_Input() +
             theme(legend.position = c(0.97, 0.7),
                   legend.justification = c(0.97, 0.97)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Category probabilities ######
irt_training_grm_plot_category_Input <- reactive({
  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)){
    b <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
                "2" = b,
                "3" = c(b, input$irt_training_grm_b3),
                "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
                "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
                "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6))
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b){ return(1/(1 + exp(-a*(theta - b)))) }

  df <- data.frame(1, sapply(1:length(b), function(i) ccirt(theta, a, b[i])))
  df <- data.frame(sapply(1:(ncol(df)-1), function(i) df[, i] - df[, i+1]),
                   df[, ncol(df)],
                   theta)
  df <- melt(df, id.vars = "theta")
  levels(df$variable) <- paste0("X", 0:(length(levels(df$variable))-1))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:((length(levels(df$variable))-1) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y >= ", 0:(length(col)-1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_grm_plot_category <- renderPlotly({
  g <- irt_training_grm_plot_category_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)){
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability",text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i-1), paste0("P(Y = ", i-1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_irt_training_grm_plot_category <- downloadHandler(
  filename =  function() {
    paste("fig_GRM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_training_grm_plot_category_Input() +
             theme(legend.position = c(0.97, 0.7),
                   legend.justification = c(0.97, 0.97)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

# *** Expected item score ######
irt_training_grm_plot_expected_Input <- reactive({
  num <- input$irt_training_grm_numresp

  a <- input$irt_training_grm_a

  if (is.null(input$irt_training_grm_b1)){
    b <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    b <- b[1:num]
  } else {
    b <- c(input$irt_training_grm_b1, input$irt_training_grm_b2)
    b <- switch(paste(num),
                "2" = b,
                "3" = c(b, input$irt_training_grm_b3),
                "4" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4),
                "5" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5),
                "6" = c(b, input$irt_training_grm_b3, input$irt_training_grm_b4, input$irt_training_grm_b5, input$irt_training_grm_b6))
  }

  theta <- seq(-4, 4, 0.01)

  ccirt <- function(theta, a, b){ return(1/(1 + exp(-a*(theta - b)))) }

  df <- data.frame(1, sapply(1:length(b), function(i) ccirt(theta, a, b[i])))
  df <- data.frame(sapply(1:(ncol(df)-1), function(i) df[, i] - df[, i+1]),
                   df[, ncol(df)])
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

  for (i in 1:length(p$x$data)){
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("theta", "Ability", text)
    text <- gsub("exp", "Expected score", text)
    text <- paste0(text, "<br />E(Y)")
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_irt_training_grm_plot_expected <- downloadHandler(
  filename =  function() {
    paste("fig_GRM_expected.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_training_grm_plot_expected_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** GENERALIZED PARTIAL CREDIT MODEL ####

output$irt_training_gpcm_sliders <- renderUI({
  num <- input$irt_training_gpcm_numresp

  sliders <- tagList(
    tags$div(class = "js-irs-red",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_gpcm_d1", "d1 - threshold",
                         value = -1.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-yellow",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_gpcm_d2", "d2 - threshold",
                         value = -1, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-green",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_gpcm_d3", "d3 - threshold",
                         value = -0.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-blue",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_gpcm_d4", "d4 - threshold",
                         value = 0, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-purple",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_gpcm_d5", "d5 - threshold",
                         value = 0.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-orange",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_gpcm_d6", "d6 - threshold",
                         value = 1, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(2*num)]

  sliders
})

# *** Category probabilities ######
irt_training_gpcm_plot_Input <- reactive({
  num <- input$irt_training_gpcm_numresp

  a <- input$irt_training_gpcm_a

  if (is.null(input$irt_training_gpcm_d1)){
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_gpcm_d1, input$irt_training_gpcm_d2)
    d <- switch(paste(num),
                "2" = d,
                "3" = c(d, input$irt_training_gpcm_d3),
                "4" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4),
                "5" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5),
                "6" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5, input$irt_training_gpcm_d6))
  }

  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d){ a*(theta - d) }

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))

  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))

  pk <- cbind(0, pk)
  pk <- exp(pk)

  denom <- apply(pk, 1, sum)

  df <- data.frame(apply(pk, 2, function(x) x/denom), theta)
  df <- melt(df, id.vars = "theta")

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:(length(levels(df$variable)) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y = ", 0:(length(col)-1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_gpcm_plot <- renderPlotly({
  g <- irt_training_gpcm_plot_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)){
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i), paste0("P(Y = ", i-1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_irt_training_gpcm_plot <- downloadHandler(
  filename =  function() {
    paste("fig_GPCM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_training_gpcm_plot_Input() +
             theme(legend.position = c(0.97, 0.7),
                   legend.justification = c(0.97, 0.97)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)


# *** Expected item score ######
irt_training_gpcm_plot_expected_Input <- reactive({
  num <- input$irt_training_gpcm_numresp

  a <- input$irt_training_gpcm_a

  if (is.null(input$irt_training_gpcm_d1)){
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_gpcm_d1, input$irt_training_gpcm_d2)
    d <- switch(paste(num),
                "2" = d,
                "3" = c(d, input$irt_training_gpcm_d3),
                "4" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4),
                "5" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5),
                "6" = c(d, input$irt_training_gpcm_d3, input$irt_training_gpcm_d4, input$irt_training_gpcm_d5, input$irt_training_gpcm_d6))
  }

  theta <- seq(-4, 4, 0.01)

  ccgpcm <- function(theta, a, d){ a*(theta - d) }

  df <- sapply(1:length(d), function(i) ccgpcm(theta, a, d[i]))

  pk <- sapply(1:ncol(df), function(k) apply(as.data.frame(df[, 1:k]), 1, sum))

  pk <- cbind(0, pk)
  pk <- exp(pk)

  denom <- apply(pk, 1, sum)

  df <- data.frame(apply(pk, 2, function(x) x/denom))
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

  for (i in 1:length(p$x$data)){
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("theta", "Ability", text)
    text <- gsub("exp", "Expected score", text)
    text <- paste0(text, "<br />E(Y)")
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_irt_training_gpcm_plot_expected <- downloadHandler(
  filename =  function() {
    paste("fig_GPCM_expected.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_training_gpcm_plot_expected_Input() +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)
# *** NOMINAL RESPONSE MODEL ####

output$irt_training_nrm_sliders <- renderUI({
  num <- input$irt_training_nrm_numresp

  sliders <- tagList(
    tags$div(class = "js-irs-red",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_a1", "a1 - discrimination",
                         value = 2.5, min = 0, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-red",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_d1", "d1 - threshold",
                         value = -1.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-yellow",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_a2", "a2 - discrimination",
                         value = 2, min = 0, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-yellow",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_d2", "d2 - threshold",
                         value = -1, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-green",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_a3", "a3 - discrimination",
                         value = 1, min = 0, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-green",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_d3", "d3 - threshold",
                         value = -0.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-blue",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_a4", "a4 - discrimination",
                         value = 1.5, min = 0, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-blue",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_d4", "d4 - threshold",
                         value = 0, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-purple",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_a5", "a5 - discrimination",
                         value = 0.5, min = 0, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-purple",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_d5", "d5 - threshold",
                         value = 0.5, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-orange",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_a6", "a6 - discrimination",
                         value = 1.3, min = 0, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", ""),
    tags$div(class = "js-irs-orange",
             style = "display: inline-block; vertical-align: middle; width: 18%;",
             sliderInput("irt_training_nrm_d6", "d6 - threshold",
                         value = 1, min = -4, max = 4, step = 0.1)),
    div(style = "display: inline-block; vertical-align: middle; width: 2.6%;", "")
  )

  sliders <- sliders[1:(4*num)]

  sliders
})

# *** Category probabilities ######
irt_training_nrm_plot_Input <- reactive({
  num <- input$irt_training_nrm_numresp

  if (is.null(input$irt_training_nrm_a1)){
    a <- c(2.5, 2, 1, 1.5, 0.5, 1.3)
    a <- a[1:num]
  } else {
    a <- c(input$irt_training_nrm_a1, input$irt_training_nrm_a2)
    a <- switch(paste(num),
                "2" = a,
                "3" = c(a, input$irt_training_nrm_a3),
                "4" = c(a, input$irt_training_nrm_a3, input$irt_training_nrm_a4),
                "5" = c(a, input$irt_training_nrm_a3, input$irt_training_nrm_a4, input$irt_training_nrm_a5),
                "6" = c(a, input$irt_training_nrm_a3, input$irt_training_nrm_a4, input$irt_training_nrm_a5, input$irt_training_nrm_a6))
  }

  if (is.null(input$irt_training_nrm_d1)){
    d <- c(-1.5, -1, -0.5, 0, 0.5, 1)
    d <- d[1:num]
  } else {
    d <- c(input$irt_training_nrm_d1, input$irt_training_nrm_d2)
    d <- switch(paste(num),
                "2" = d,
                "3" = c(d, input$irt_training_nrm_d3),
                "4" = c(d, input$irt_training_nrm_d3, input$irt_training_nrm_d4),
                "5" = c(d, input$irt_training_nrm_d3, input$irt_training_nrm_d4, input$irt_training_nrm_d5),
                "6" = c(d, input$irt_training_nrm_d3, input$irt_training_nrm_d4, input$irt_training_nrm_d5, input$irt_training_nrm_d6))
  }

  theta <- seq(-4, 4, 0.01)

  ccnrm <- function(theta, a, d){ exp(d + a*theta) }

  df <- sapply(1:length(d), function(i) ccnrm(theta, a[i], d[i]))
  df <- data.frame(1, df)
  denom <- apply(df, 1, sum)
  df <- apply(df, 2, function(x) x/denom)
  df <- data.frame(df, theta)

  df <- melt(df, id.vars = "theta")
  levels(df$variable) <- paste0("X", 0:(length(levels(df$variable))-1))

  col <- c("black", "red", "#e6b800", "#00b300", "blue", "#990099", "#ff6600")
  col <- col[1:(length(levels(df$variable)) + 1)]

  g <- ggplot(data = df, aes(x = theta, y = value, col = variable)) +
    geom_line() +
    xlab("Ability") +
    ylab("Category probability") +
    xlim(-4, 4) +
    ylim(0, 1) +
    scale_color_manual("", values = col, labels = paste0("P(Y = ", 0:(length(col)-1), ")")) +
    theme_app() +
    ggtitle("Category probabilities")

  g
})

output$irt_training_nrm_plot <- renderPlotly({
  g <- irt_training_nrm_plot_Input()

  p <- ggplotly(g)

  for (i in 1:length(p$x$data)){
    text <- gsub("~", "", p$x$data[[i]]$text)
    text <- gsub("value", "Category probability", text)
    text <- gsub("theta", "Ability", text)
    text <- gsub(paste0("variable: X", i-1), paste0("P(Y = ", i-1, ")"), text)
    p$x$data[[i]]$text <- text
  }

  p$elementId <- NULL

  p %>%  plotly::config(displayModeBar = F)
})

output$DB_irt_training_nrm_plot <- downloadHandler(
  filename =  function() {
    paste("fig_NRM_category.png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = irt_training_nrm_plot_Input() +
             theme(legend.position = c(0.97, 0.7),
                   legend.justification = c(0.97, 0.97)) +
             theme(text = element_text(size = setting_figures$text_size)),
           device = "png",
           height = setting_figures$height, width = setting_figures$width,
           dpi = setting_figures$dpi)
  }
)

