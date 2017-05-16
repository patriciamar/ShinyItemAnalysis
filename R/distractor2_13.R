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



a <- test_answers
k <- test_key

multiple.answers <- FALSE #NUTNO ZVOLIT
gr = 3 #NUTNO ZVOLIT



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




distractorItem2<-plotDistractorAnalysis(data = a, key = k, num.group = 5, item = 2,
                       multiple.answers = multiple.answers)

distractorItem13<-plotDistractorAnalysis(data = a, key = k, num.group = 5, item = 13,
                       multiple.answers = multiple.answers)

ggsave(filename = "distractorItem2.png", plot = distractorItem2, device = "png", height=3, width=9, dpi=300)
ggsave(filename = "distractorItem13.png", plot = distractorItem13, device = "png", height=3, width=9, dpi=300)

