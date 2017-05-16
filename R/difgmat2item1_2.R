dataOptions<-c("GMAT" = "GMAT_difNLR",
               "GMAT2" = "GMAT2_difNLR",
               "Medical 20 DIF" = "difMedical_difNLR",
               "Medical 100" = "dataMedical_ShinyItemAnalysis"
)

a=dataOptions[2] #NUTNO ZVOLIT

pos=regexpr("_", a)[1]
datasetName=str_sub(a, 1,pos-1)
packageName=str_sub(a, pos+1)

do.call(data, args=list(paste0(datasetName,"test"), package=packageName))
test=get(paste0(datasetName,"test"))

do.call(data, args=list(paste0(datasetName,"key"), package=packageName))
key=get(paste0(datasetName,"key"))

test_answers = test[,1:length(key)]

pos=regexpr("_", a)[1]
datasetName=str_sub(a, 1,pos-1)
packageName=str_sub(a, pos+1)

do.call(data, args=list(paste0(datasetName,"key"), package=packageName))
test_key=get(paste0(datasetName,"key"))

sc <- score(test_answers, test_key)$score

correct<-score(test_answers, test_key, output.scored = TRUE)$scored

group <- test[, ncol(test)] #NUTNO POHLÍDAT
DIF_groups<-group






type_plot_DIF_logistic<-c("H0: Any DIF vs. H1: No DIF" = 'both',
                          "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                          "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
)

correction_method_logItems<-c("BH" = "BH",
                              "Holm" = "holm",
                              "Hochberg" = "hochberg",
                              "Hommel" = "hommel",
                              "BY" = "BY",
                              "FDR" = "fdr",
                              "none" = "none")

type_plot_DIF_logistic<-type_plot_DIF_logistic[1] #NUTNO ZVOLIT
correction_method_logItems<-correction_method_logItems[1] #NUTNO ZVOLIT
item = 10 #NUTNO ZVOLIT

group <- DIF_groups
data <- correct

type <- type_plot_DIF_logistic


plotDIFLogistic<-function(data, group, type = "both", item, IRT = F, p.adjust.method = "BH"){
  if (IRT){
    match <- c(scale(apply(data, 1, sum)))
  } else {
    match <- "score"
  }
  fit <- difR::difLogistic(Data = data, group = group, focal.name = 1, type = type,
                           match = match, p.adjust.method = p.adjust.method)

  LR_plot <- function(x, group, beta0, beta1, beta2, beta3){
    return(1/(1 + exp(-(beta0 + beta1*x + beta2*group + beta3*x*group))))
  }

  ### data
  if (IRT){
    score_R <- scale(apply(data[group == 0, ], 1, sum))
    score_F <- scale(apply(data[group == 1, ], 1, sum))
  } else {
    score_R <- apply(data[group == 0, ], 1, sum)
    score_F <- apply(data[group == 1, ], 1, sum)
  }


  max_score <- max(score_R, score_F)
  min_score <- min(score_R, score_F)

  col   <- c("dodgerblue2", "goldenrod2")
  alpha <- .5
  shape <-  21
  size  <- .8
  linetype <- c(2, 1)
  if (IRT){
    xlab <- "Standardized total score (Z-score)"
  } else {
    xlab <- "Total score"
  }


  hv_R <- data.frame(X1 = as.numeric(levels(as.factor(score_R))),
                     X2 = tapply(data[group == 0, item], as.factor(score_R), mean))
  hv_F <- data.frame(X1 = as.numeric(levels(as.factor(score_F))),
                     X2 = tapply(data[group == 1, item], as.factor(score_F), mean))
  hv   <- data.frame(rbind(cbind(hv_R, Group = "Reference"), cbind(hv_F, Group = "Focal")))
  rownames(hv) <- 1:dim(hv)[1]
  hv$size <- c(table(score_R), table(score_F))

  coef <- fit$logitPar[item, ]

  plot_CC <- ggplot(hv, aes_string("X1", "X2")) +
    ### points
    geom_point(aes_string(colour = "Group", fill = "Group",
                          size = "size"),
               alpha = alpha, shape = shape) +
    ### lines
    stat_function(aes(colour = "Reference", linetype = "Reference"),
                  fun = LR_plot,
                  args = list(group = 0,
                              beta0 = coef[1],
                              beta1 = coef[2],
                              beta2 = coef[3],
                              beta3 = coef[4]),
                  size = size, geom = "line") +
    stat_function(aes(colour = "Focal", linetype = "Focal"),
                  fun = LR_plot,
                  args = list(group = 1,
                              beta0 = coef[1],
                              beta1 = coef[2],
                              beta2 = coef[3],
                              beta3 = coef[4]),
                  size = size, geom = "line")  +
    ### style
    scale_size_continuous(name = "Counts")  +
    scale_colour_manual(name = "Group",
                        breaks = hv$Group,
                        values = col) +
    scale_fill_manual(values = col) +
    scale_linetype_manual(name = "Group",
                          breaks = hv$Group,
                          values = linetype) +
    ### theme
    xlab(xlab) +
    ylab("Probability of correct answer") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme_bw() +
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 14, face = "bold", vjust = 1.5),
          axis.line  = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA)) +
    ### legend
    theme(legend.box.just = "left",
          legend.justification = c(1, 0),
          legend.position = c(0.97, 0.03),
          # legend.margin = unit(0, "lines"),
          legend.box = "vertical",
          legend.key.size = unit(0.9, "cm"),
          legend.key.height = unit(0.8, "line"),
          legend.text.align = 0,
          legend.title.align = 0,
          legend.key = element_rect(colour = "white"),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    ggtitle(paste("Item", item))

  plot_CC

}



plot_DIF_logisticITEM2<-plotDIFLogistic(data, group,
                                   type = type_plot_DIF_logistic,
                                   item =  2,
                                   IRT = F,
                                   p.adjust.method = correction_method_logItems
)


ggsave(filename = "plot_DIF_logsticITEM2.png", plot = plot_DIF_logisticITEM2, device = "png", height=3, width=9, dpi=300)




plot_DIF_logisticITEM1<-plotDIFLogistic(data, group,
                                        type = type_plot_DIF_logistic,
                                        item =  1,
                                        IRT = F,
                                        p.adjust.method = correction_method_logItems
)


ggsave(filename = "plot_DIF_logsticITEM1.png", plot = plot_DIF_logisticITEM1, device = "png", height=3, width=9, dpi=300)


