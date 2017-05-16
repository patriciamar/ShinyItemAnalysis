personHist <- function (thetas, yRange = NULL, breaks = "FD", dim.lab.cex = 1.2,
          dim.lab.side = 3, dim.lab.adj = 0.5, dim.names = NULL, dim.color = "darkblue",
          person.points = NULL, person.range = NULL, p.point.col = "darkblue",
          p.range.col = "darkblue", axis.persons = "Respondents",
          oma = c(0, 0, 0, 0), axis.logits = "Logits", show.axis.logits = TRUE,
          p.font.lab = 1,
          ...)
{
  densExt <- function(densElem) {
    bin.size <- abs(densElem$breaks[1] - densElem$breaks[2])
    thetaHist <- data.frame(xleft = densElem$mids - (bin.size/2),
                            ybottom = densElem$mids * 0, xright = densElem$mids +
                              (bin.size/2), ytop = densElem$counts)
    return(thetaHist)
  }
  theta.dens <- function(thetas, breaks) {
    densList <- apply(thetas, 2, hist, plot = FALSE)
    distInfo <- lapply(densList, densExt)
    return(distInfo)
  }
  person.plot <- function(distInfo, yRange, p.points, p.range,
                          p.col, r.col, dim.lab.side, dim.lab.cex, dim.lab.adj,
                          p.cex.lab, p.font.lab, p.lwd) {
    par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))
    plot(c(min(distInfo[, 1]), max(distInfo[, 3])),
         c(min(distInfo[, 2]), max(distInfo[, 4])),
         ylim = yRange,
         xlim = c(max(distInfo[, 4]), 0),
         type = "n",
         ylab = "",
         xlab = "",
         axes = FALSE,
         cex.lab = 1.2,
         font.lab = p.font.lab, lwd = p.lwd,
         col = "darkblue")
    if (screen() == first)
      mtext("Student ability", side = 2, line = 1, cex = 1.2,
            font = 1)
    draw.range <- function(upper, lower, col, distInfo) {
      in.range <- distInfo[, 1] < upper & distInfo[, 3] >
        lower
      range.info <- distInfo[in.range, ]
      rect(range.info[, 4], range.info[, 1], range.info[,
                                                        2], range.info[, 3], col = col)
    }
    draw.point <- function(pt, col, distInfo) {
      has.point <- distInfo[, 1] - pt <= 0 & distInfo[,
                                                      3] - pt > 0
      point.info <- distInfo[has.point, ]
      rect(point.info[, 4], point.info[, 1], point.info[,
                                                        2], point.info[, 3], col = col)
    }
    rect(distInfo[, 4], distInfo[, 1], distInfo[, 2], distInfo[,
                                                               3], col = attr(distInfo, "dim.color"))
    if (!is.null(p.range)) {
      p.range <- matrix(p.range, nrow = 2)
      lower <- p.range[1, ]
      upper <- p.range[2, ]
      mapply(draw.range, upper, lower, r.col, list(distInfo))
    }
    if (!is.null(p.points)) {
      mapply(draw.point, p.points, p.col, list(distInfo))
    }
    # mtext(attr(distInfo, "dim.name"), side = dim.lab.side,
    #       line = -1, cex = dim.lab.cex, font = 1, adj = dim.lab.adj)
    box(bty = "c")
    if (screen() != max(split.screen()))
      screen(screen() + 1)
  }
  thetas <- as.matrix(thetas)
  nD <- ncol(thetas)
  if (is.null(yRange))
    yRange <- c(min(thetas), max(thetas))
  distInfo <- theta.dens(thetas, breaks)
  if (is.null(dim.names)) {
    if (!is.null(names(thetas))) {
      dim.names <- names(thetas)
    }
    else dim.names <- c(paste("Dim", seq(1:nD), sep = ""))
  }
  if (ncol(thetas) > 1 & length(dim.color) == 1) {
    dim.color <- rep(dim.color, ncol(thetas))
  }
  for (i in 1:nD) {
    attr(distInfo[[i]], "dim.name") <- dim.names[i]
    attr(distInfo[[i]], "dim.color") <- dim.color[i]
  }
  old.screens <- split.screen()
  split.screen(c(1, nD))
  first <- screen()
  op <- par("mar", "oma")
  par(oma = oma)
  lapply(distInfo, FUN = person.plot, yRange = yRange, p.points = person.points,
         p.range = person.range, p.col = p.point.col, r.col = p.range.col,
         dim.lab.cex = dim.lab.cex, dim.lab.side = dim.lab.side,
         dim.lab.adj = dim.lab.adj, p.cex.lab = 1.2, p.font.lab = 1,
         p.lwd = 2)
  if (show.axis.logits) {
    axis(4, las = 1, cex.axis = 1, font.axis = 1)
  }
  # mtext(axis.logits, side = 4, line = 1.5, cex = 1, font = 1)
  curr.screens <- split.screen()
  new.screens <- curr.screens[!(curr.screens %in% old.screens)]
  close.screen(new.screens)
}
