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



fit3PL <- tpm(correct, IRT.param = TRUE)

png(filename = "ltm3PLTIF.png", height=800, width=1200, res=100)
plot(fit3PL, items = 0, type = "IIC")
dev.off()
