
itemClassic <- function (thr,
                         yRange = NULL,
                         axis.items = "Items",
                         axis.logits = "Logits",
                         show.axis.logits = "R",
                         oma = c(0, 0, 0, 0),
                         cutpoints = NULL,
                         ...)
{
  Nbins <- function(thr, itemRange) {
    breaks <- seq(from = itemRange[1], to = itemRange[2],
                  length.out = 25)
    if (min(thr, na.rm = TRUE) < min(breaks))
      breaks <- c(min(thr, na.rm = TRUE), breaks)
    if (max(thr, na.rm = TRUE) > max(breaks))
      breaks <- c(breaks, max(thr, na.rm = TRUE))
    return(breaks)
  }
  binItems <- function(level, labelMat, cutMat) {
    paste(sort(labelMat[cutMat == level]), collapse = " | ")
  }
  thr <- as.matrix(thr)
  nI <- dim(thr)[1]
  nL <- dim(thr)[2]
  if (is.null(yRange)) {
    yRange <- c(min(thr, na.rm = TRUE), max(thr, na.rm = TRUE))
    yA <- (yRange[2] - yRange[1]) * 0.1
    yRange <- yRange + c(-yA, yA)
  }
  par(oma = oma)
  par(mgp = c(1, 0.2, 0))
  plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = "",
       ylab = "", ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = 1.2,
       font.lab = 1)
  box(bty = "o")
  usr <- par("usr")
  par(mgp = c(3, 1, 0))
  if (show.axis.logits == "R" | show.axis.logits == TRUE) {
    axis(4, las = 1, cex.axis = 1, font.axis = 1)
    mtext("Item difficulty", side = 4, line = 3, cex = 1.2, font = 1)
  }
  else if (show.axis.logits == "L") {
    axis(2, las = 1, cex.axis = 1, font.axis = 1)
    # mtext("Ability", side = 2, line = 1.5, cex = 1, font = 1)
  }
  if (!is.null(cutpoints)) {
    cutLines(cutpoints, ...)
  }
  item.hist <- hist(thr, plot = FALSE, breaks = Nbins(thr,
                                                      yRange))
  itemBinLocations <- item.hist$mids
  bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
  item.hist <- data.frame(xleft = item.hist$mids - (bin.size/2),
                          ybottom = item.hist$mids * 0, xright = item.hist$mids +
                            (bin.size/2), ytop = item.hist$counts)
  # item.labels <- matrix(rep(formatC(1:nI, digits = 1, format = "d",
  #                                   flag = "0"), nL), ncol = nL)
  # item.labels <- t(apply(item.labels, 1, paste, c(1:nL), sep = "."))
  lab <- c(na.omit(as.numeric(unlist(strsplit(rownames(thr), "[^0-9]+")))))
  lab <- as.character(lab)
  item.labels <- ifelse(nchar(lab) == 1, paste("0", lab, sep = ""), lab)
  binnedItems <- matrix(cut(thr, breaks = Nbins(thr, yRange),
                            labels = c(1:length(item.hist[, 1] + 1))), ncol = nL)
  binnedList <- unlist(lapply(1:length(itemBinLocations), binItems,
                              item.labels, binnedItems))
  text(cbind(0, itemBinLocations), labels = binnedList, pos = 4,
       offset = 1 * 15/nI, cex = 1)
}
