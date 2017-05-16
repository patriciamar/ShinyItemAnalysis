##### Data #####
require(corrplot)
require(CTT)
require(deltaPlotR)
require(DT)
require(difNLR)
require(difR)
require(ggplot2)
require(grid)
require(gridExtra)
require(latticeExtra)
require(ltm)
require(mirt)
require(moments)
require(msm)
require(nnet)
require(psych)
require(psychometric)
require(reshape2)
require(rmarkdown)
require(ShinyItemAnalysis)
require(shinyjs)
require(stringr)
require(WrightMap)


# WrightMap
source("wrightMap.R")
source("itemClassic.R")
source("personHist.R")

data('GMAT', package = 'difNLR')
data('GMATtest', package = 'difNLR')
data('GMATkey', package = 'difNLR')
test <- get("GMATtest")
key <- get("GMATkey")

dataOptions<-c("GMAT" = "GMAT_difNLR",
  "GMAT2" = "GMAT2_difNLR",
  "Medical 20 DIF" = "difMedical_difNLR",
  "Medical 100" = "dataMedical_ShinyItemAnalysis"
)

a=dataOptions[4] #NUTNO ZVOLIT

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

##### Hisogram TotalScores #####

a <- test_answers
k <- test_key
sc <- sc

bin <- median(sc) #NUTNO ZVOLIT

df <- data.frame(sc,
                 gr = cut(sc,
                          breaks = unique(c(0, bin - 1, bin, ncol(a))),
                          include.lowest = T))

if (bin < min(sc)){
  col <- "blue"
} else {
  if (bin == min(sc)){
    col <- c("grey", "blue")
  } else {
    col <- c("red", "grey", "blue")
  }
}

histogram_totalscores<- ggplot(df, aes(x = sc)) +
                          geom_histogram(aes(fill = gr), binwidth = 1, color = "black") +
                          scale_fill_manual("", breaks = df$gr, values = col) +
                          labs(x = "Total score",
                               y = "Number of students") +
                          scale_y_continuous(expand = c(0, 0),
                                             limits = c(0, max(table(sc)) + 0.01 * nrow(a))) +
                          scale_x_continuous(limits = c(-0.5, ncol(a) + 0.5)) +
                          theme_bw() +
                          theme(legend.title = element_blank(),
                                legend.position = "none",
                                axis.line  = element_line(colour = "black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                text = element_text(size = 14),
                                plot.title = element_text(face = "bold", hjust = 0.5))
histogram_totalscores
ggsave(filename="histogram_totalscores.png", plot = histogram_totalscores, device = "png", height=3, width=9, dpi=300)



##### DIFPLOT #####

DDplot<-function(data){
  if (is.matrix(data) | is.data.frame(data)) {
    if (!all(apply(data, 2, function(i) {
      length(levels(as.factor(i))) == 2
    })))
      stop("'data' must be data frame or matrix of binary vectors",
           call. = FALSE)
  }
  else {
    stop("'data' must be data frame or matrix of binary vectors",
         call. = FALSE)
  }

  # difficulty and discrimination
  difc <- psychometric::item.exam(data, discr = T)[, "Difficulty"]
  disc <- psychometric::item.exam(data, discr = T)[, "Discrimination"]
  value <- c(rbind(difc, disc)[, order(difc)])
  parameter <- rep(c("Difficulty", "Discrimination"), ncol(data))
  # ordered by difficulty
  item <- factor(rep(order(difc), rep(2, ncol(data))),
                 levels = factor(order(difc)))
  df <- data.frame(item, parameter, value)

  # plot
  col <- c("red", "darkblue")
  ggplot(df,
         aes_string(x = "item", y = "value", fill = "parameter", color = "parameter")) +
    stat_summary(fun.y = mean,
                 position = position_dodge(),
                 geom = "bar",
                 alpha = 0.7,
                 width = 0.8) +
    geom_hline(yintercept = 0.2) +
    xlab("Item number (ordered by difficulty)") +
    ylab("Difficulty/Discrimination") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual(breaks = parameter,
                      values = col) +
    scale_colour_manual(breaks = parameter,
                        values = col) +
    theme_bw() +
    theme(axis.line  = element_line(colour = "black"),
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key = element_rect(colour = "white"),
          plot.title = element_text(face = "bold", hjust = 0.5))
}

DDplot(correct)

ggsave(filename = "difplot.png", plot = DDplot(correct), device = "png", height=3, width=9, dpi=300)



##### Graf (Distractor Analysis plot) #####

a <- test_answers
k <- test_key

multiple.answers <- FALSE #NUTNO ZVOLIT
gr = 3 #NUTNO ZVOLIT
item = 10 #NUTNO ZVOLIT


plotDistractorAnalysis<-function (data, key, num.groups = 3, item = 1, multiple.answers = TRUE)
{
  key <- unlist(key)
  # distractor analysis
  tabDA <- DistractorAnalysis(data = data, key = key, p.table = TRUE, num.groups = num.groups)
  x <- tabDA[[item]]
  # only rows where is possitive proportion of correct answers
  if (dim(x)[2] != 1){
    x <- x[!(apply(x, 1, function(y) all(y == 0))), ]
  }

  x <- melt(x, id = "response")
  x$response <- as.factor(x$response)
  levels(x$response)[which(levels(x$response) == "")] <- "NaN"
  x$response <- relevel(x$response, as.character(key[item]))

  if (multiple.answers){
    # all combinations
    df <- x

    CA <- CAall <- as.character(key[item])
    col <- rainbow(n = length(levels(df$response)))
    names(col) <- levels(df$response)
  } else {
    # only distractors and correct combination
    # split combinations to possible choices (i.e. AB to A and B)
    levels(x$response)[which(levels(x$response) == "NaN")] <- "x"
    y <- x[rep(1:nrow(x), nchar(as.character(x$response))), ]
    y$response <- as.factor(unlist(strsplit(as.character(x$response), "")))
    # sum over choices
    df <- aggregate(value ~ response + score.level, data = y, sum)
    # adding correct combination
    CAdf <- x[x$response == as.character(key[item]), ]
    CAdf$response <- paste(key[item], "-correct", sep = "")
    df <- rbind(df, CAdf)
    CA <-  unique(CAdf$response)

    levels(df$response)[which(levels(df$response) == "x")] <- "NaN"

    # plot settings
    col <- rainbow(n = (length(levels(df$response)) + 1))
    names(col) <- levels(df$response)
    col[CA] <- "black"

    df$response <- relevel(df$response, CA)
    CAall <- c(CA, unlist(strsplit(as.character(key[item]), "")))
  }

  # plot settings
  linetype <- rep(2, length(levels(df$response)))
  shape <- rep(1, length(levels(df$response)))
  names(linetype) <- names(shape) <- levels(df$response)
  linetype[CAall] <- 1
  shape[CAall] <- 19

  # plot
  ggplot(df, aes_string(x = "score.level",
                        y = "value",
                        group = "response",
                        colour = "response",
                        linetype = "response",
                        shape = "response"),
         size = 1) +
    geom_line() +
    geom_point(size = 3) +
    xlab("Group by total score") +
    ylab("Option selection percentage") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = 1:num.groups, expand = c(0, 0.2)) +
    scale_linetype_manual(values = linetype) +
    scale_shape_manual(values = shape) +
    scale_color_manual(values = col) +
    theme_bw() +
    theme(axis.line  = element_line(colour = "black"),
          text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key = element_rect(colour = "white"),
          legend.key.width = unit(1, "cm"),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    ggtitle(paste("Item", item))

}


graf<-plotDistractorAnalysis(data = a, key = k, num.group = gr, item = item,
                       multiple.answers = multiple.answers)

plotDistractorAnalysis(data = a, key = k, num.group = 5, item = 13,
                       multiple.answers = multiple.answers)

plotDistractorAnalysis(data = a, key = k, num.group = 5, item = 1,
                       multiple.answers = multiple.answers)

ggsave(filename = "graf.png", plot = graf, device = "png", height=3, width=9, dpi=300)


##### LOGREG #####

sc <- sc
correct <- correct

item=13 #NUTNO ZVOLIT

fun <- function(x, b0, b1) {exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))}

df <- data.frame(x = sort(unique(sc)),
                 y = tapply(correct[, item], sc, mean),
                 size = as.numeric(table(sc)))

logistic_reg<-glm(correct[, item] ~ sc, family = binomial)

logreg<-ggplot(df, aes(x = x, y = y)) +
          geom_point(aes(size = size),
                     color = "darkblue",
                     fill = "darkblue",
                     shape = 21, alpha = 0.5) +
          stat_function(fun = fun, geom = "line",
                        args = list(b0 = coef(logistic_reg)[1],
                                    b1 = coef(logistic_reg)[2]),
                        size = 1,
                        color = "darkblue") +
          xlab("Total score") +
          ylab("Probability of correct answer") +
          scale_y_continuous(limits = c(0, 1)) +
          theme_bw() +
          theme(axis.line  = element_line(colour = "black"),
                text = element_text(size = 14),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.title = element_blank(),
                legend.position = c(0, 1),
                legend.justification = c(0, 1),
                legend.background = element_blank(),
                legend.key = element_rect(colour = "white"),
                plot.title = element_text(face = "bold", hjust = 0.5)) +
          ggtitle(paste("Item", item))

logreg

ggsave(filename="logreg.png", plot = logreg, device = "png", height=3, width=9, dpi=300)




##### NLSPLOT ####

scaledsc = scale(sc)

item=22 #NUTNO ZVOLIT

regFce_noDIF <- deriv3(
  ~ c + (1 - c) / (1 + exp(-a * (x - b))),
  namevec = c("a", "b", "c"),
  function.arg = function(x, a, b, c) {}
)
regFce_klasik <- deriv3(
  ~ c + (1 - c) / (1 + exp(-(b0 + b1 * x))),
  namevec = c("b0", "b1", "c"),
  function.arg = function(x, b0, b1, c) {}
)

scaledsc <- scale(sc)

Q3 <- cut(scaledsc, quantile(scaledsc, (0:3) / 3),
          c("I", "II", "III"),
          include.lowest = TRUE)

x <- cbind(mean(scaledsc[Q3 == "I"]),
           apply(correct[Q3 == "I",], 2, mean))
y <- cbind(mean(scaledsc[Q3 == "III"]),
           apply(correct[Q3 == "III",], 2, mean))
u1 <- y[, 1] - x[, 1]
u2 <- y[, 2] - x[, 2]
### intercept of line
c <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1
### slope of line
t <- u2 / u1
g <- apply(cbind(0, t * (-4) + c), 1, max)

b <- ((1 + g) / 2 - c) / t

alpha <- 4 * t / (1 - g)

discr <- alpha
diffi <- b
guess <- g
i <- item

start <- cbind(discr, diffi, guess)
colnames(start) <- c("a", "b", "c")

estim_klasik1 <-
  nls(
    correct[, i] ~ regFce_noDIF(scaledsc, a, b, c),
    algorithm = "port", start = start[i, ],
    lower = c(-20, -20, 0), upper = c(20, 20, 1)
  )

nls_model<-estim_klasik1

fun <- function(x, a, b, c){c + (1 - c) / (1 + exp(-a * (x - b)))}
df <- data.frame(x = sort(unique(scaledsc)),
                 y = tapply(correct[, item], scaledsc, mean),
                 size = as.numeric(table(scaledsc)))
nlsplot<-ggplot(df, aes(x = x, y = y)) +
            geom_point(aes(size = size),
                       color = "darkblue",
                       fill = "darkblue",
                       shape = 21, alpha = 0.5) +
            stat_function(fun = fun, geom = "line",
                          args = list(a = coef(nls_model)[1],
                                      b = coef(nls_model)[2],
                                      c = coef(nls_model)[3]),
                          size = 1,
                          color = "darkblue") +
            xlab("Standardized total score (Z-score)") +
            ylab("Probability of correct answer") +
            scale_y_continuous(limits = c(0, 1)) +
            theme_bw() +
            theme(axis.line  = element_line(colour = "black"),
                  text = element_text(size = 14),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.title = element_blank(),
                  legend.position = c(0, 1),
                  legend.justification = c(0, 1),
                  legend.background = element_blank(),
                  legend.key = element_rect(colour = "white"),
                  plot.title = element_text(face = "bold", hjust = 0.5)) +
            ggtitle(paste("Item", item))

nlsplot

ggsave(filename="nlsplot.png", plot = nlsplot, device = "png", height=3, width=9, dpi=300)




##### MULTIPLOT ######

item = 13 #NUTNO ZVOLIT

k <- t(as.data.frame(test_key))
data <- test_answers
stotal <- c(scale(sc))
i <- item

data <- sapply(1:ncol(data), function(i) as.factor(data[, i]))

fitM <- multinom(relevel(as.factor(data[, i]),
                         ref = paste(k[i])) ~ stotal,
                 trace = F)

pp <- fitted(fitM)
if(ncol(pp) == 1){
  pp <- cbind(pp, 1 - pp)
  colnames(pp) <- c("0", "1")
}
stotals <- rep(stotal, length(levels(relevel(as.factor(data[, i]),
                                             ref = paste(k[i])))))
df <- cbind(melt(pp), stotals)
df$Var2 <- relevel(as.factor(df$Var2), ref = paste(k[i]))
df2 <- data.frame(table(data[, i], stotal),
                  y = data.frame(prop.table(table(data[, i], stotal), 2))[, 3])
df2$stotal <- as.numeric(levels(df2$stotal))[df2$stotal]
df2$Var2 <- relevel(df2$Var1, ref = paste(k[i]))


multiplot<-ggplot() +
              geom_line(data = df,
                        aes(x = stotals , y = value,
                            colour = Var2, linetype = Var2), size = 1) +
              geom_point(data = df2,
                         aes(x = stotal, y = y,
                             colour = Var2, fill = Var2,
                             size = Freq),
                         alpha = 0.5, shape = 21) +

              ylim(0, 1) +
              labs(title = paste("Item", i),
                   x = "Standardized total score (Z-score)",
                   y = "Probability of answer") +
              theme_bw() +
              theme(axis.line  = element_line(colour = "black"),
                    text = element_text(size = 14),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    legend.title = element_blank(),
                    legend.position = "none",
                    legend.justification = c(0, 1),
                    legend.background = element_blank(),
                    legend.key = element_rect(colour = "white"),
                    plot.title = element_text(face = "bold", hjust = 0.5),
                    legend.key.width = unit(1, "cm"))

multiplot
ggsave(file="multiplot.png", plot = multiplot, device = "png", height=3, width=9, dpi=300)



##### raschFactor #####

rasch_model=rasch(correct)

fit1 <- rasch_model
df1  <- ltm::factor.scores(fit1, return.MIvalues = T)$score.dat
FS   <- as.vector(df1[, "z1"])
df2  <- df1
df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
STS <- as.vector(scale(apply(df2, 1, sum)))
df  <- data.frame(FS, STS)


raschFactor<-ggplot(df, aes_string("STS", "FS")) +
                geom_point(size = 3) +
                labs(x = "Standardized total score", y = "Factor score") +
                theme_bw() +
                theme(text = element_text(size = 14),
                      plot.title = element_text(face = "bold", vjust = 1.5),
                      axis.line  = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank()) +
                theme(legend.box.just = "left",
                      legend.justification = c(1, 0),
                      legend.position = c(1, 0),
                      # legend.margin = unit(0, "lines"),
                      legend.box = "vertical",
                      legend.key.size = unit(1, "lines"),
                      legend.text.align = 0,
                      legend.title.align = 0)
raschFactor
ggsave(filename="raschFactor.png", plot = raschFactor, device = "png", height=3, width=9, dpi=300)




##### wrightMap #####

rasch_model_mirt<-mirt(correct, model = 1, itemtype = "Rasch",
                       SE = T, verbose = F)

fs <- as.vector(fscores(rasch_model_mirt))

fit <- rasch_model_mirt
coeftab <- coef(fit)
b <- sapply(1:(length(coeftab) - 1), function(i) coeftab[[i]][1, "d"])
names(b) <- paste("Item", 1:(length(coeftab) - 1))

png(filename="wrightMap.png", height = 1600, width = 2700, res = 300)
wrightMap(fs, b, item.side = itemClassic)
dev.off()



##### threeparam ####

fit3PL <- tpm(correct, IRT.param = TRUE)

three_param_irt <- fit3PL

png(filename = "threeparam.png", height=1600, width=2700, res=300)
plot(three_param_irt)
dev.off()


##### threeparam_iic #####

fit3PL <- tpm(correct, IRT.param = TRUE)

three_param_irt <- fit3PL

png(filename = "threeparam_iic.png", height=1600, width=2700, res=300)
plot(three_param_irt, type = "IIC")
dev.off()


##### bock_TIF #####

a <- test_answers
k <- as.factor(key)

m <- ncol(a)
lev <- unlist(lapply(1:m, function(i) levels(a[, i])))
lev <- c(lev, levels(k))
lev <- unique(lev)
lev_num <- as.numeric(as.factor(lev))


lev_k_num <- sapply(1:length(levels(k)),
                    function(i) lev_num[levels(k)[i] == lev])

lev_a_num <- lapply(1:m, function(i)
  sapply(1:length(levels(a[, i])),
         function(j) lev_num[levels(a[, i])[j] == lev]))

levels(k) <- lev_k_num
k <- as.numeric(paste(k))


for (i in 1:m){
  levels(a[, i]) <- lev_a_num[[i]]
  a[, i] <- as.numeric(paste(a[, i]))
}

adj_data_bock<-list(data = a, key = k)

data <- adj_data_bock$data
key <- adj_data_bock$key


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
bock_irt_mirt<-fit

bock_TIF_Input<-plot(bock_irt_mirt, type = "infoSE")


png(filename = "bock_TIF.png", height = 900, width = 2700, res = 300)
print(bock_TIF_Input)
dev.off()




##### plot_DIF_logistic ######



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



plot_DIF_logistic<-plotDIFLogistic(data, group,
                                    type = type_plot_DIF_logistic,
                                    item =  item,
                                    IRT = F,
                                    p.adjust.method = correction_method_logItems
                    )


ggsave(filename = "plot_DIF_logstic.png", plot = plot_DIF_logistic, device = "png", height=3, width=9, dpi=300)








