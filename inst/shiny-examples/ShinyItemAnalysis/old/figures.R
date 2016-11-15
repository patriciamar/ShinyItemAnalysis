library(ggplot2)
regFce_nonUDIF <- function(STS, group, a, b, c, aDif, bDif){
  return(c + (1 - c)/(1 + exp(-(a + aDif*group)*(STS - (b + bDif*group)))))
}

a <- 0.56
b <- 0
c <- 0.2
aDif <- 1.23
bDif <- 0.5

a <- 2
b <- 0
c <- 0.2
aDif <- 0
bDif <- 1

### slope calculations
inter_diff <- (1 + c)/2 - (a + aDif) * (1 - c) * (b + bDif) / 4
slope_diff <- (a + aDif) * (1 - c) / 4
inter_notdiff <- (1 + c)/2 - (a) * (1 - c) * b / 4
slope_notdiff <- a * (1 - c) / 4

col <- c("gold", "red")
linetype <- c(2, 1)

plotCC <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  ### lines
  stat_function(aes(colour = "Reference", linetype = "Reference"),
                fun = regFce_nonUDIF,
                args = list(group = 0, a, b, c, aDif, bDif),
                size = size, 
                geom = "line") +
  stat_function(aes(colour = "Focal", linetype = "Focal"),
                fun = regFce_nonUDIF,
                args = list(group = 1, a, b, c, aDif, bDif),
                size = size,
                geom = "line") +
  ### style
  scale_colour_manual(breaks = c("Reference", "Focal"), 
                      values = col, 
                      name = "Group", 
                      labels = c("Reference", "Focal")) +
  scale_linetype_manual(breaks = c("Reference", "Focal"), 
                        values = linetype, 
                        name = "Group",
                        labels = c("Reference", "Focal")) +
  ### theme
  labs(x = "Ability", 
       y = "Probability of Correct Answer") +
  ylim(c(0, 1)) +
  xlim(c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 11),
        plot.title = element_text(size = 11, face = "bold", vjust = 1.5),
        axis.line  = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  ### legend
  theme(legend.box.just = "left",
        legend.justification = c(1, 0),
        legend.position = c(0.03, 0.97),
        legend.margin = unit(0, "lines"),
        legend.box = "vertical",
        legend.key.size = unit(1, "lines"),
        legend.text.align = 0,
        legend.title.align = 0,
        legend.key.width = unit(1, "cm"),
        legend.key = element_rect(colour = "white")) +
 theme(text = element_text(size = 14),
          legend.position = c(0.55, 0.51))

plotCC <- plotCC +  theme(text = element_text(size = 14),
                legend.position = c(0.32, 0.82)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-3, 3))

# setwd("C:/Users/drabinova/Dropbox/Work/CRAN packages/ShinyItemAnalysis v 1.0.0/ShinyItemAnalysis/inst/shiny-examples/ShinyItemAnalysis/www")
# 
# 
# ggsave("irtnudif.png", width = 5, height = 5, units = "in")


lt <- "dotdash"
co <- "black"
size_l <- 1

p1 <- plotCC + 
  # coordinates
  geom_segment(aes(x = -3,
                   y = (1 + c)/2,
                   xend = b,
                   yend = (1 + c)/2),
               colour = co, linetype = lt, size = size_l,
               show.legend = F) +
  geom_segment(aes(x = b,
                   y = (1 + c)/2,
                   xend = b,
                   yend = 0),
               colour = co, linetype = lt, size = size_l,
               show.legend = F) +
  
  geom_segment(aes(x = -3, y = (1 + c)/2,
                   xend = b + bDif,
                   yend = (1 + c)/2),
               colour = co, linetype = lt, size = size_l,
               show.legend = F) +
  geom_segment(aes(x = b + bDif,
                   y = (1 + c)/2,
                   xend = b + bDif,
                   yend = 0),
               colour = co, linetype = lt, size = size_l,
               show.legend = F) +
  # slope
  geom_segment(aes(x = b + bDif -  0.4,
                   y = inter_diff + (b + bDif  -  0.4) * slope_diff,
                   xend = b + bDif +  0.4,
                   yend = inter_diff + (b + bDif  + 0.4) * slope_diff),
               colour = co, linetype = lt, size = 1.1,
               show.legend = F) +
  geom_segment(aes(x = b  - 0.4,
                   y = inter_notdiff + (b  - 0.4) * slope_notdiff,
                   xend = b  + 0.4,
                   yend = inter_notdiff + (b  +  0.4) * slope_notdiff),
               colour = co, linetype = lt, size = 1.1,
               show.legend = F) 

p1
ggsave("lord_udif.png", width = 5, height = 5, units = "in")
 


gg1 <- ggplot_build(plotCC)

# extract data for the loess lines from the 'data' slot
df2 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 

# use the loess data to add the 'ribbon' to plot 
p2 <- plotCC +  geom_ribbon(data = df2, 
                            aes(x = x, 
                                ymin = ymin, 
                                ymax = ymax),
                            fill = "grey", 
                            alpha = 0.4, 
                            inherit.aes = FALSE)

p2
ggsave("raju_udif.png", width = 5, height = 5, units = "in")

 


gt1 <- ggplotGrob(p1 + th4) 
gt2 <- ggplotGrob(p2 + th5)

newWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])

gt1$widths[2:3] = as.list(newWidth)
gt2$widths[2:3] = as.list(newWidth)

prow <- plot_grid(gt1, gt2, ncol = 2, labels = c("A", "B"))
prow
# setwd("C:/Users/drabinova/Dropbox/Work/papers/HCI/figures")
# ggsave("Figure 4.png", width = 8, height = 3.4, units = "in")
# ggsave("Figure 4.eps", width = 8, height = 3.4, units = "in")
# 
# tiff(file = "Figure 4.tiff", width = 2400, height = 1020, units = "px", res = 300)
# prow
# dev.off()

