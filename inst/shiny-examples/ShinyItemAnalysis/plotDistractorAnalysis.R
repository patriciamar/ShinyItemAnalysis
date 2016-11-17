# data The unscored item response from a multiple-choice test
# key The answer key for the items
# num.groups The number of groups for distractor analysis
# item item indicator
# multiple.answers AB BC etc combinations

plotDistractorAnalysis <-  function (data, key, num.groups = 3, item = 1, multiple.answers = T)
{
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
  x$response <- relevel(x$response, key[item])

  if (multiple.answers){
    # all combinations
    df <- x

    CA <- CAall <- key[item]
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
    CAdf <- x[x$response == key[item], ]
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
  ggplot(df, aes(x = score.level,
                 y = value,
                 group = response,
                 colour = response,
                 linetype = response,
                 shape = response),
         size = 1) +
    geom_line() +
    geom_point(size = 3) +
    xlab("Group by Total Score") +
    ylab("Option Selection Percentage") +
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
          plot.title = element_text(face = "bold")) +
    ggtitle(paste("Item", item))

}
