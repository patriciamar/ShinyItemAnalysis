DDplot <- function(data){
  # difficulty and discrimination
  difc <- item.exam(data, discr = T)[, "Difficulty"]
  disc <- item.exam(data, discr = T)[, "Discrimination"]
  value <- c(rbind(difc, disc)[, order(difc)])
  parameter <- rep(c("Difficulty", "Discrimination"), ncol(data))
  # ordered by difficulty
  item <- factor(rep(order(difc), rep(2, ncol(data))),
                 levels = factor(order(difc)))
  df <- data.frame(item, parameter, value)

  # plot
  col <- c("red", "darkblue")
  ggplot(df,
         aes(x = item, y = value, fill = parameter, color = parameter)) +
    stat_summary(fun.y = mean,
                 position = position_dodge(),
                 geom = "bar",
                 alpha = 0.7,
                 width = 0.8) +
    geom_hline(yintercept = 0.2) +
    xlab("Item Number (Ordered by Difficulty)") +
    ylab("Difficulty/Discrimination") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual(breaks = parameter,
                      values = col) +
    scale_colour_manual(breaks = parameter,
                        values = col) +
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
          legend.key = element_rect(colour = "white"))
}

