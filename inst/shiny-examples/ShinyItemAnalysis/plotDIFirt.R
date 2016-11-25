#' Function for characteristic curve of DIF IRT model
#'
#' @aliases plotDIFirt
#'
#' @description Plots characteristic curve of IRT model.
#'
#' @param parameters numeric: data matrix or data frame. See \strong{Details}.
#' @param test character: type of statistic to be shown.
#' @param item numeric: number of item to be plotted.
#'
#' @usage plotDIFirt(parameters, test = "Lord", item = 1)
#'
#' @details
#' This function plots characteristic curve of DIF IRT model.
#'
#'
#' @author
#' Adela Drabinova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' Faculty of Mathematics and Physics, Charles University \cr
#' adela.drabinova@gmail.com \cr
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science, The Czech Academy of Sciences \cr
#' martinkova@cs.cas.cz \cr
#'
#' @examples
#' \dontrun{
#' # loading libraries
#' library(difNLR, difR)
#'
#'  # loading data based on GMAT
#' data(GMAT, package = "difNLR")
#' data  <- GMAT[, colnames(GMAT) != "group"]
#' group <- GMAT[, "group"]
#'
#' # Estimation of 2PL IRT model
#' fitR <- itemParEst(data[group == 0, ], model = "2PL")
#' fitF <- itemParEst(data[group == 1, ], model = "2PL")
#'
#' # Estimated parameters with sd for item 1
#' est <- c(fitR[1, 1:2], fitF[1, 1:2])[c(1, 3, 2, 4)]
#' sd <- c(fitR[1, 3:4], fitF[1, 3:4])[c(1, 3, 2, 4)]
#' parameters <- data.frame(est, sd)
#'
#' # Characteristic curve for item 1
#' plotDIFirt(parameters, item = 1)
#'
#' # Characteristic curve for item 1 with highlighted area between curves
#' plotDIFirt(parameters, item = 1, test = "Raju")
#' }
#'
#'
#' @export


plotDIFirt <- function(parameters, test = "Lord", item = 1){

  coefR <- switch(as.character(nrow(parameters)),
                  "2" = c(1, parameters[1, 1], 0),
                  "4" = c(parameters[c(1, 3), 1], 0),
                  "5" = parameters[c(1, 3, 5), 1])

  coefF <- switch(as.character(nrow(parameters)),
                  "2" = c(1, parameters[2, 1], 0),
                  "4" = c(parameters[c(2, 4), 1], 0),
                  "5" = parameters[c(2, 4, 5), 1])


  CC_plot <- function(x, a, b, c){
    return(c + (1 - c)/(1 + exp(-(a*(x - b)))))
  }

  col   <- c("dodgerblue2", "goldenrod2")
  alpha <- .5
  shape <-  21
  size  <- .8
  linetype <- c(2, 1)

  df <- data.frame(x = c(-3, 3), y = c(0, 1))

  gg <- ggplot(df, aes_string("x", "y")) +
    xlim(-3, 3)  +
    ### lines
    stat_function(aes(colour = "Reference", linetype = "Reference"),
                  fun = CC_plot,
                  args = list(a = coefR[1],
                              b = coefR[2],
                              c = coefR[3]),
                  size = size, geom = "line") +
    stat_function(aes(colour = "Focal", linetype = "Focal"),
                  fun = CC_plot,
                  args = list(a = coefF[1],
                              b = coefF[2],
                              c = coefF[3]),
                  size = size, geom = "line") +
    ### style
    scale_colour_manual(name = "Group",
                        breaks = c("Reference", "Focal"),
                        values = col) +
    scale_fill_manual(values = col) +
    scale_linetype_manual(name = "Group",
                          breaks = c("Reference", "Focal"),
                          values = linetype) +
    ### theme
    xlab("Ability") +
    ylab("Probability of Correct Answer") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))  +
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
          legend.margin = unit(0, "lines"),
          legend.box = "vertical",
          legend.key.size = unit(0.9, "cm"),
          legend.key.height = unit(0.8, "line"),
          legend.text.align = 0,
          legend.title.align = 0,
          legend.key = element_rect(colour = "white")) +
    ggtitle(paste("Item", item))

  if (test != "Lord"){
    gg1 <- ggplot_build(gg)

    # extract data for the loess lines from the 'data' slot
    df2 <- data.frame(x = gg1$data[[1]]$x,
                      ymin = gg1$data[[1]]$y,
                      ymax = gg1$data[[2]]$y)

    # use the loess data to add the 'ribbon' to plot
    gg <- gg + geom_ribbon(data = df2,
                                aes_string(x = "x",
                                    ymin = "ymin",
                                    ymax = "ymax"),
                                fill = "grey",
                                alpha = 0.4,
                                inherit.aes = FALSE)

  }
    gg
}


