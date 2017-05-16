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



data <- correct
fit3PL <- mirt(data, model = 1, itemtype = "3PL",
               constrain = NULL,
               SE = T, technical = list(NCYCLES = 2000),
               verbose = F)


three_param_irt_mirt<-fit3PL


fs <- as.vector(fscores(three_param_irt_mirt))
sts <- as.vector(scale(apply(correct, 1, sum)))

df <- data.frame(fs, sts)

threeparamirtFactorInput_mirt<-ggplot(df, aes_string("sts", "fs")) +
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


ggsave(filename="3PLfactor.png", plot = threeparamirtFactorInput_mirt, device = "png",
       height = 3, width = 9, dpi = 160)
