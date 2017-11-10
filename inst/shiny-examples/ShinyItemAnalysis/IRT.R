#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# IRT MODELS WITH MIRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * RASCH ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rasch_model_mirt <- reactive({
  fitRasch <- mirt(correct_answ(), model = 1, itemtype = "Rasch",
                   SE = T, verbose = F)
})

# *** CC ######
raschInput_mirt <- reactive({
  g <- plot(rasch_model_mirt(), type = "trace", facet_items = F)
  g
})

output$rasch_mirt <- renderPlot({
  raschInput_mirt()
})

output$DP_rasch_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(raschInput_mirt())
    dev.off()
  }
)

# *** IIC ######
raschiicInput_mirt <- reactive({
  g <- plot(rasch_model_mirt(), type = "infotrace", facet_items = F)
  g
})

output$raschiic_mirt <- renderPlot({
  raschiicInput_mirt()
})

output$DP_raschiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(raschiicInput_mirt())
    dev.off()
  }
)

# *** TIF ######
raschtifInput_mirt <- reactive({
  g <- plot(rasch_model_mirt(), type = "infoSE")
  g
})

output$raschtif_mirt <- renderPlot({
  raschtifInput_mirt()
})

output$DP_raschtif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(raschtifInput_mirt())
    dev.off()
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
    colnames(tab) <- c("b", "SD(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("b", "SD(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)", "d", "SD(d)",
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

# *** Factor scores correlation ######
raschFactorCorInput_mirt <- reactive({
  fs <- as.vector(fscores(rasch_model_mirt()))
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

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
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(face = "bold", vjust = 1.5),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
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
    ggsave(file, plot = raschFactorInput_mirt(), device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

# *** Wright Map ######
raschWrightMapInput_mirt <- reactive({
  fit <- rasch_model_mirt()
  fs <- as.vector(fscores(fit))

  b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
  names(b) <- item_names()

  wrightMap(fs, b, item.side = itemClassic)

})

output$raschWrightMap_mirt<- renderPlot({
  raschWrightMapInput_mirt()
})

output$DP_raschWM_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_RaschWrightMap.png", sep = "")
  },
  content = function(file) {
    fs <- as.vector(fscores(rasch_model_mirt()))
    fit <- rasch_model_mirt()

    b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
    names(b) <- item_names()

    png(file, height = 800, width = 1200, res = 100)
    wrightMap(fs, b, item.side = itemClassic)
    dev.off()
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 1PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

one_param_irt_mirt <- reactive({
  data <- correct_answ()
  fit1PL <- mirt(data, model = 1, itemtype = "2PL",
                 constrain = list((1:ncol(data)) + seq(0, (ncol(data) - 1)*3, 3)),
                 SE = T, verbose = F)
})

# *** CC ####
oneparamirtInput_mirt <- reactive({
  g <- plot(one_param_irt_mirt(), type = "trace", facet_items = F)
  g
})

output$oneparamirt_mirt <- renderPlot({
  oneparamirtInput_mirt()
})

output$DP_oneparamirt_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLItemCharactersticCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(oneparamirtInput_mirt())
    dev.off()
  }
)

# *** IIC ######
oneparamirtiicInput_mirt <- reactive({
  plot(one_param_irt_mirt(), type = "infotrace", facet_items = F)
})

output$oneparamirtiic_mirt <- renderPlot({
  oneparamirtiicInput_mirt()
})

output$DP_oneparamirtiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(oneparamirtiicInput_mirt())
    dev.off()
  }
)

# *** TIF ######
oneparamirttifInput_mirt <- reactive({
  plot(one_param_irt_mirt(), type = "infoSE")
})

output$oneparamirttif_mirt <- renderPlot({
  oneparamirttifInput_mirt()
})

output$DP_oneparamirttif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_1PLTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(oneparamirttifInput_mirt())
    dev.off()
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
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)", "d", "SD(d)",
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


# *** Factor scores correlation ######
oneparamirtFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(one_param_irt_mirt()))
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

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
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(face = "bold", vjust = 1.5),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
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
    ggsave(file, plot = oneparamirtFactorInput_mirt(), device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

# *** Wright Map ######
oneparamirtWrightMapInput_mirt <- reactive({
  fit <- one_param_irt_mirt()
  fs <- as.vector(fscores(fit))

  b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
  names(b) <- item_names()

  wrightMap(fs, b, item.side = itemClassic)
})


oneparamirtWrightMapReportInput_mirt <- reactive({
  if (input$irt_type_report!="none") {
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
    fit <- one_param_irt_mirt()
    fs <- as.vector(fscores(fit))

    b <- coef(fit, IRTpars = T, simplify = T)$items[, "b"]
    names(b) <- item_names()

    png(file, height = 800, width = 1200, res = 100)
    wrightMap(fs, b, item.side = itemClassic)
    dev.off()
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 2PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
two_param_irt_mirt <- reactive({
  data <- correct_answ()
  fit2PL <- mirt(data, model = 1, itemtype = "2PL",
                 constrain = NULL,
                 SE = T, verbose = F)
})

# *** CC ######
twoparamirtInput_mirt <- reactive({
  plot(two_param_irt_mirt(), type = "trace", facet_items = F)
})

output$twoparamirt_mirt <- renderPlot({
  twoparamirtInput_mirt()
})

output$DP_twoparamirt_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(twoparamirtInput_mirt())
    dev.off()
  }
)

# *** IIC ######
twoparamirtiicInput_mirt <- reactive({
  plot(two_param_irt_mirt(), type = "infotrace", facet_items = F)
})

output$twoparamirtiic_mirt <- renderPlot({
  twoparamirtiicInput_mirt()
})

output$DP_twoparamirtiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(twoparamirtiicInput_mirt())
    dev.off()
  }
)

# *** TIF ######
twoparamirttifInput_mirt <- reactive({
  plot(two_param_irt_mirt(), type = "infoSE")
})

output$twoparamirttif_mirt <- renderPlot({
  twoparamirttifInput_mirt()
})

output$DP_twoparamirttif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_2PLTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(twoparamirttifInput_mirt())
    dev.off()
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
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)", "d", "SD(d)",
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
# *** Factor scores correlation ######
twoparamirtFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(two_param_irt_mirt()))
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

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
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(face = "bold", vjust = 1.5),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
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
    ggsave(file, plot = twoparamirtFactorInput_mirt(), device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * 3PL IRT ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

three_param_irt_mirt <- reactive({
  data <- correct_answ()
  fit3PL <- mirt(data, model = 1, itemtype = "3PL",
                 constrain = NULL,
                 SE = T, technical = list(NCYCLES = 2000),
                 verbose = F)
})

# *** CC ######
threeparamirtInput_mirt <- reactive({
  plot(three_param_irt_mirt(), type = "trace", facet_items = F)
})

output$threeparamirt_mirt <- renderPlot({
  threeparamirtInput_mirt()
})

output$DP_threeparamirt_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(threeparamirtInput_mirt())
    dev.off()
  }
)

# *** IIC ######
threeparamirtiicInput_mirt <- reactive({
  plot(three_param_irt_mirt(), type = "infotrace", facet_items = F)
})

output$threeparamirtiic_mirt <- renderPlot({
  threeparamirtiicInput_mirt()
})

output$DP_threeparamirtiic_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(threeparamirtiicInput_mirt())
    dev.off()
  }
)

# *** TIF ######
threeparamirttifInput_mirt <- reactive({
  plot(three_param_irt_mirt(), type = "infoSE")
})

output$threeparamirttif_mirt <- renderPlot({
  threeparamirttifInput_mirt()
})

output$DP_threeparamirttif_mirt <- downloadHandler(
  filename =  function() {
    paste("fig_3PLTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(threeparamirttifInput_mirt())
    dev.off()
  }
)


# *** Table of parameters ######
threeparamirtcoefInput_mirt <- reactive({
  fit <- three_param_irt_mirt()

  par_tab <- coef(fit, IRTpars = T, simplify = T)$items[, c("a", "b", "g")]

  parvec <- extract.mirt(fit, 'parvec')
  vcov <- vcov(fit)

  se_tab <- c()
  for (item in seq(1, 3*nrow(par_tab), 3)){
    pick <- c(item, item + 1, item + 2)
    ad <- parvec[pick]
    v <- vcov[pick, pick]

    SEs <- deltamethod(list(~ x1, ~ -x2/x1, ~ x3), ad, v)
    names(SEs) <- c('a', 'b', 'c')
    se_tab <- rbind(se_tab, SEs)
  }

  tab <- cbind(par_tab, se_tab)[, c(1, 4, 2, 5, 3, 6)]

  if(!is.null(tryCatch(round(itemfit(fit)[, 2:4], 3), error = function(e) {
    cat("ERROR : ", conditionMessage(e), "\n")}, finally = ""))){
    tab <- data.frame(tab, round(itemfit(fit)[, 2:4], 3))
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)", "SX2-value", "df", "p-value")
  } else {
    colnames(tab) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)")
  }
  rownames(tab) <- item_names()

  n <- length(item_names())
  tab.comp <- data.frame(rep(1, n), "-", 0, "-", 0, "-", 1, "-", "-", "-", "-")
  colnames(tab.comp) <- c("a", "SD(a)", "b", "SD(b)", "c", "SD(c)", "d", "SD(d)",
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

# *** Factor scores plot ######
threeparamirtFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(three_param_irt_mirt()))
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

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
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(face = "bold", vjust = 1.5),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
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
    ggsave(file, plot = threeparamirtFactorInput_mirt(), device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT COMPARISON ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

irtcomparisonInput <- reactive({
  fit1PL <- one_param_irt_mirt()
  fit2PL <- two_param_irt_mirt()
  fit3PL <- three_param_irt_mirt()

  models <- list(fit1PL = fit1PL,
                 fit2PL = fit2PL,
                 fit3PL = fit3PL)

  df <- data.frame(anova(models[[1]], models[[2]], verbose = F))
  df <- rbind(df,
              data.frame(anova(models[[2]], models[[3]], verbose = F)))
  df <- round(df[c(1, 2, 4), ], 3)
  nam <- c("1PL", "2PL", "3PL")

  if (all(df[, 8] > 0.05)){
    hv <- "1PL"
  } else {
    p <- which(df[, 8] < 0.05)
    hv <- nam[p[length(p)]]
  }

  df <- rbind(df,
              c(nam[sapply(1:4, function(i) which(df[, i] == min(df[, i], na.rm = T)))],
                rep("", 3),
                hv))


  rownames(df) <- c(nam, "BEST")
  df <- df[, c(1, 2, 4, 3, 5:8)]
  df
})

output$irtcomparison <- renderTable({
  irtcomparisonInput()
},
include.rownames = T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * BOCKS NOMINAL MODEL ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adj_data_bock <- reactive({
  a <- test_answers()
  k <- as.factor(test_key())

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

  list(data = data.table(a), key = k)
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
  plot(bock_irt_mirt(), type = "trace", facet_items = F)
})
output$bock_CC <- renderPlot({
  bock_CC_Input()
})
output$DP_bock_CC <- downloadHandler(
  filename =  function() {
    paste("fig_BockItemCharacteristicCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(bock_CC_Input())
    dev.off()
  }
)

# *** IIC ######
bock_IIC_Input <- reactive({
  plot(bock_irt_mirt(), type = "infotrace", facet_items = F)
})
output$bock_IIC <- renderPlot({
  bock_IIC_Input()
})
output$DP_bock_IIC <- downloadHandler(
  filename =  function() {
    paste("fig_BockItemInformationCurves.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(bock_IIC_Input())
    dev.off()
  }
)

# *** TIF ######
bock_TIF_Input <- reactive({
  plot(bock_irt_mirt(), type = "infoSE")
})
output$bock_TIF <- renderPlot({
  bock_TIF_Input()
})
output$DP_bock_TIF <- downloadHandler(
  filename =  function() {
    paste("fig_BockTestInformationFunction.png", sep = "")
  },
  content = function(file) {
    png(file, height = 800, width = 1200, res = 100)
    print(bock_TIF_Input())
    dev.off()
  }
)

# *** Table of parameters ######
output$bock_coef_warning <- renderText({
  fit <- bock_irt_mirt()

  coeftab <- coef(fit, printSE = T)
  m <- length(coeftab) - 1

  dims <- sapply(coeftab, dim)[, -(m+1)]
  print(length(unique(dims[2, ])))
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

  print(coeftab)
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

# *** Factor scores plot ######
bock_factor_Input <- reactive({

  fs <- as.vector(fscores(bock_irt_mirt()))
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

  df <- data.frame(fs, sts)

  ggplot(df, aes_string("sts", "fs")) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(face = "bold", vjust = 1.5),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
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
    ggsave(file, plot = bock_factor_Input(), device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

# *** Factor scores correlation ######
bockFactorCorInput_mirt <- reactive({

  fs <- as.vector(fscores(bock_irt_mirt()))
  sts <- as.vector(scale(apply(correct_answ(), 1, sum)))

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

# ** CC ######
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

  theta <- input$ccIRTSlider_theta

  ccirt <- function(theta, a, b, c, d){
    return(c + (d - c)/(1 + exp(-a*(theta - b))))
  }

  g <- ggplot(data = data.frame(x = -4:4),
              mapping = aes(x = x)) +
    stat_function(fun = ccirt, args = list(a = a1, b = b1, c = c1, d = d1),
                  aes(color = "b", linetype = "b")) +
    stat_function(fun = ccirt, args = list(a = a2, b = b2, c = c2, d = d2),
                  aes(color = "c", linetype = "c")) +
    geom_segment(aes(y = ccirt(theta, a = a1, b = b1, c = c1, d = d1),
                     yend = ccirt(theta, a = a1, b = b1, c = c1, d = d1),
                     x = -4,
                     xend = theta, color = "a", linetype = "a")) +
    geom_segment(aes(y = ccirt(theta, a = a2, b = b2, c = c2, d = d2),
                     yend = ccirt(theta, a = a2, b = b2, c = c2, d = d2),
                     x = -4,
                     xend = theta, color = "a", linetype = "a")) +
    geom_segment(aes(y = 0,
                     yend = max(ccirt(theta, a = a1, b = b1, c = c1, d = d1),
                                ccirt(theta, a = a2, b = b2, c = c2, d = d2)),
                     x = theta,
                     xend = theta, color = "a", linetype = "a")) +
    xlim(-4, 4) +
    xlab("Ability") +
    ylab("Probability of correct answer") +
    theme_bw() +
    ylim(0, 1) +
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 14, face = "bold", vjust = 1.5),
          axis.line  = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "none") +
    scale_color_manual(name = "",
                       # breaks = c("blue", "gray", "red"),
                       # values = c("blue", "gray", "red"),
                       breaks = c("a", "b", "c"),
                       values = c("gray", "red", "blue"),
                       labels = c(paste(expression(theta), "=", theta),
                                  paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
                                        collapse = ", "),
                                  paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
                                        collapse = ", "))) +
    scale_linetype_manual(name = "",
                          # breaks = c("blue", "gray", "red"),
                          # values = c("solid", "dashed", "solid"),
                          breaks = c("a", "b", "c"),
                          values = c("dashed", "solid", "solid"),
                          labels = c(paste(expression(theta), "=", theta),
                                     paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
                                           collapse = ", "),
                                     paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
                                           collapse = ", "))) +
    ggtitle("Item characteristic curve")
  g

})

output$ccIRT_plot <- renderPlotly({
  g <- ccIRT_plot_Input()

  p <- ggplotly(g)

  text <- gsub("y", "Probability", p$x$data[[1]]$text)
  text <- gsub("x", "Ability", text)
  text <- gsub("b: b<br />b: b<br />", "", text)
  p$x$data[[1]]$text <- text

  text <- gsub("y", "Probability", p$x$data[[2]]$text)
  text <- gsub("x", "Ability", text)
  text <- gsub("c: c<br />c: c<br />", "", text)
  p$x$data[[2]]$text <- text

  text <- gsub("ccirt\\(theta, a = a1, b = b1, c = c1, d = d1\\)", "Probability", p$x$data[[3]]$text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("<br />a: a<br />a: a<br />x: -4", "", text)
  pos <- gregexpr('Probability', text)[[1]][2]
  text <- substring(text, pos)
  p$x$data[[3]]$text <- text

  text <- gsub("ccirt\\(theta, a = a2, b = b2, c = c2, d = d2\\)", "Probability", p$x$data[[4]]$text)
  text <- gsub("-4: -4<br />", "", text)
  text <- gsub("<br />a: a<br />a: a<br />x: -4", "", text)
  pos <- gregexpr('Probability', text)[[1]][2]
  text <- substring(text, pos)
  p$x$data[[4]]$text <- text

  text <- gsub("max\\(ccirt\\(theta, a = a1, b = b1, c = c1, d = d1\\), ccirt\\(theta, a = a2, b = b2, c = c2, d = d2\\)\\)", "Probability", p$x$data[[5]]$text)
  text <- gsub("a: a<br />", "", text)
  pos <- regexpr('Probability', text)
  text <- substring(text, pos)
  pos <- regexpr("<br />x:", text)
  text <- substring(text, 1, pos-1)
  p$x$data[[5]]$text <- text

  p %>% config(displayModeBar = F)
})

output$DB_ccIRT <- downloadHandler(
  filename =  function() {
    paste("fig_CustomItemCharacteristicCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
           plot = ccIRT_plot_Input() +
             theme(legend.position = c(0.97, 0.03),
                   legend.justification = c(0.97, 0.03)),
           device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

# ** ICC ######
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

  g <- ggplot(data = data.frame(x = -4:4), mapping = aes(x = x)) +
    stat_function(fun = iccirt, args = list(a = a1, b = b1, c = c1, d = d1),
                  aes(color = "b")) +
    stat_function(fun = iccirt, args = list(a = a2, b = b2, c = c2, d = d2),
                  aes(color = "c")) +
    xlim(-4, 4) +
    ylim(0, 4) +
    xlab("Ability") +
    ylab("Information") +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 14, face = "bold", vjust = 1.5),
          axis.line  = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "none") +
    scale_color_manual(name = "",
                       breaks = c("b", "c"),
                       values = c("red", "blue"),
                       labels = c(paste(paste(letters[1:4], "=", c(a1, b1, c1, d1)),
                                        collapse = ", "),
                                  paste(paste(paste(letters[1:4], "=", c(a2, b2, c2, d2))),
                                        collapse = ", "))) +
    ggtitle("Item information function")
  g
})

output$iccIRT_plot <- renderPlotly({
  g <- iccIRT_plot_Input()

  p <- ggplotly(g)

  text <- gsub("y", "Information", p$x$data[[1]]$text)
  text <- gsub("x", "Ability", text)
  text <- gsub("b: b<br />", "", text)
  p$x$data[[1]]$text <- text

  text <- gsub("y", "Information", p$x$data[[2]]$text)
  text <- gsub("x", "Ability", text)
  text <- gsub("c: c<br />", "", text)
  p$x$data[[2]]$text <- text

  p %>% config(displayModeBar = F)
})

output$DB_iccIRT <- downloadHandler(
  filename =  function() {
    paste("fig_CustomItemInformationCurve.png", sep = "")
  },
  content = function(file) {
    ggsave(file,
           plot = iccIRT_plot_Input() +
             theme(legend.position = c(0.97, 0.97),
                   legend.justification = c(0.97, 0.97)),
           device = "png",
           height = 3, width = 9, dpi = 300)
  }
)

# *** Sliders and text input updates ######
# 1a
observe({
  val <- input$ccIRTSlider_a1
  updateTextInput(session, "ccIRTtext_a1", value = val)
})
observe({
  val <- input$ccIRTtext_a1
  updateSliderInput(session, "ccIRTSlider_a1", value = val)
})
# 1b
observe({
  val <- input$ccIRTSlider_b1
  updateTextInput(session, "ccIRTtext_b1", value = val)
})
observe({
  val <- input$ccIRTtext_b1
  updateSliderInput(session, "ccIRTSlider_b1", value = val)
})
# 1c
observe({
  val <- input$ccIRTSlider_c1
  updateTextInput(session, "ccIRTtext_c1", value = val)
})
observe({
  val <- input$ccIRTtext_c1
  updateSliderInput(session, "ccIRTSlider_c1", value = val)
})
# 1d
observe({
  val <- input$ccIRTSlider_d1
  updateTextInput(session, "ccIRTtext_d1", value = val)
})
observe({
  val <- input$ccIRTtext_d1
  updateSliderInput(session, "ccIRTSlider_d1", value = val)
})
# 2a
observe({
  val <- input$ccIRTSlider_a2
  updateTextInput(session, "ccIRTtext_a2", value = val)
})
observe({
  val <- input$ccIRTtext_a2
  updateSliderInput(session, "ccIRTSlider_a2", value = val)
})
# 2b
observe({
  val <- input$ccIRTSlider_b2
  updateTextInput(session, "ccIRTtext_b2", value = val)
})
observe({
  val <- input$ccIRTtext_b2
  updateSliderInput(session, "ccIRTSlider_b2", value = val)
})
# 2c
observe({
  val <- input$ccIRTSlider_c2
  updateTextInput(session, "ccIRTtext_c2", value = val)
})
observe({
  val <- input$ccIRTtext_c2
  updateSliderInput(session, "ccIRTSlider_c2", value = val)
})
# 2d
observe({
  val <- input$ccIRTSlider_d2
  updateTextInput(session, "ccIRTtext_d2", value = val)
})
observe({
  val <- input$ccIRTtext_d2
  updateSliderInput(session, "ccIRTSlider_d2", value = val)
})

# theta
observe({
  val <- input$ccIRTSlider_theta
  updateTextInput(session, "ccIRTtext_theta", value = val)
})
observe({
  val <- input$ccIRTtext_theta
  updateSliderInput(session, "ccIRTSlider_theta", value = val)
})
