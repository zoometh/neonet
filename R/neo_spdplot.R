# adapted from the function `plot.stackCalSPD()` {rcarbon} R package. This function is triggered by `neo_spd()`
neo_spdplot <- function (x, 
                         type = "stacked", 
                         calendar = "BP", 
                         spdnormalised = FALSE, 
                         rescale = FALSE, 
                         runm = NA, 
                         xlim = NA, ylim = NA,
                         xaxt = "s", yaxt = "s",
                         gapFactor = 0.2,
                         col.fill = NA, col.line = NA, 
                         lwd.obs = 1, lty.obs = 1, 
                         cex.lab = 1, cex.axis = cex.lab, cex.main = 1,
                         legend = TRUE, legend.arg = NULL, legend.pos = "topleft",
                         ylab = NA, ymargin = 1.1, 
                         spd.title = "",
                         periods.colors = c("#0000CF", "#1D1DFF", "#3737FF", "#FF1B1B", "#FF8D1B", "#FFC04D", "#808080"),
                         weighted.median = NA,
                         x.intercept = NA,
                         # ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                         # shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
                         # colpal = c("#0000CF", "#1D1DFF", "#3737FF", "#FF1B1B",
                         #            "#FF8D1B", "#FFC04D", "#808080"), 
                         ...) 
{
  if (!"stackCalSPD" %in% class(x)) {
    stop("The argument x should be a 'stackCalSPD' class object")
  }
  if (!type %in% c("multipanel", "lines", "stacked", 
                   "proportion")) {
    stop("The argument 'type' should be one between 'multipanel', 'lines', 'stacked'  or 'proportion'.")
  }
  if (!calendar %in% c("BP", "BCAD")) {
    stop("The calendar you have chosen is not currently an option.")
  }
  if (calendar == "BP") {
    plotyears <- x$metadata$timeRange[1]:x$metadata$timeRange[2]
    xlabel <- "Years cal BP"
    if (any(is.na(xlim))) {
      xlim <- c(max(plotyears), min(plotyears))
    }
  }
  else if (calendar == "BCAD") {
    plotyears <- rcarbon::BPtoBCAD(x$metadata$timeRange[1]:x$metadata$timeRange[2])
    xlabel <- "Years BC/AD"
    if (all(range(plotyears) < 0)) {
      xlabel <- "Years BC"
    }
    if (all(range(plotyears) > 0)) {
      xlabel <- "Years AD"
    }
    if (any(is.na(xlim))) {
      xlim <- c(min(plotyears), max(plotyears))
    }
  }
  else {
    stop("Unknown calendar type")
  }
  if (xaxt == "n") {
    xlabel <- ""
  }
  if (yaxt == "n") {
    ylabel <- ""
  }
  else {
    ylabel <- ylab
  }
  nsets = length(x$spds)
  if (nsets == 1 & type == "multipanel") {
    type = "stacked"
  }
  PrDens = matrix(NA, ncol = nsets, nrow = length(plotyears))
  for (i in 1:nsets) {
    PrDens[, i] = x$spds[[i]][[2]][[2]]
    if (!is.na(runm)) {
      PrDens[, i] <- runMean(PrDens[, i], runm, edge = "fill")
    }
  }
  if (spdnormalised) {
    PrDens = apply(PrDens, 2, function(x) {
      x/sum(x)
    })
  }
  if (rescale) {
    PrDens = apply(PrDens, 2, reScale)
  }
  if (any(is.na(col.fill))) {
    col.line <- col.fill <- periods.colors
    # periods.colors <- read.csv(ref.period, sep = "\t")
    # periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
    # col.fill <- periods.colors.selected[periods.colors.selected$period %in% periods, "color"]
    # col.line <- periods.colors.selected[periods.colors.selected$period %in% periods, "color"]
    # colpal <- periods.colors$color
    # col.fill <- colpal[1:nsets]
    # if (nsets <= 8) {
    #   col.fill = colpal[1:nsets]
    # }
    # if (nsets > 8) {
    #   col.fill = sample(colors(), size = nsets, replace = TRUE)
    #   warning("Color sequence randomised due to a large number of SPDs (>8). Consider selecting an appropriate color sequence manually")
    # }
  }
  # if (any(is.na(col.line))) {
  #   if (nsets <= 8) {
  #     col.line = colpal[1:nsets]
  #   }
  #   if (nsets > 8) {
  #     col.line = sample(colors(), size = nsets, replace = TRUE)
  #     warning("Color sequence randomised due to a large number of SPDs (>8). Consider selecting an appropriate color sequence manually")
  #   }
  # }
  if (any(is.na(lwd.obs))) {
    lwd.obs = rep(1, nsets)
  }
  if (any(is.na(lty.obs))) {
    lty.obs = rep(1, nsets)
  }
  if (length(lwd.obs) == 1) {
    lwd.obs = rep(lwd.obs, nsets)
  }
  if (length(lty.obs) == 1) {
    lty.obs = rep(lty.obs, nsets)
  }
  if (length(col.line) == 1) {
    col.line = rep(col.line, nsets)
  }
  if (length(col.fill) == 1) {
    col.fill = rep(col.fill, nsets)
  }
  if (type == "lines") {
    if (any(is.na(ylim))) {
      ylim <- c(0, max(PrDens) * ymargin)
    }
    if (is.na(ylab)) {
      ylab = "Summed Probability"
    }
    plot(0, 0, xlim = xlim, ylim = ylim, type = "l", 
         ylab = ylab, xlab = xlabel, xaxt = "n", yaxt = yaxt, 
         cex.axis = cex.axis, cex.lab = cex.lab)
    for (i in 1:nsets) {
      lines(plotyears, PrDens[, i], col = col.line[i], 
            lty = lty.obs[i], lwd = lwd.obs[i])
    }
    if (legend) {
      if (is.null(legend.arg)) {
        legend(legend.pos, legend = names(x$spds), 
               col = col.line, lty = lty.obs, lwd = lwd.obs)
      }
      else {
        args.legend1 <- list(legend.pos, legend = names(x$spds), 
                             col = col.line, lty = lty.obs, lwd = lwd.obs)
        args.legend1[names(legend.arg)] <- legend.arg
        do.call("legend", args = args.legend1)
      }
    }
  }
  if (type == "stacked") {
    if (nsets > 1) {
      PrDens = t(apply(PrDens, 1, cumsum))
    }
    PrDens = cbind(0, PrDens)
    if (any(is.na(ylim))) {
      ylim <- c(0, max(PrDens) * ymargin)
    }
    if (is.na(ylab)) {
      ylab = "Summed Probability"
    }

    print(" ------------->  HERE")
    print(xlim)
    plot(0, 0, 
         xlim = xlim, ylim = ylim, type = "l", 
         ylab = ylab, xlab = xlabel, xaxt = "n", yaxt = yaxt, 
         cex.axis = cex.axis, cex.lab = cex.lab, 
         cex.main = cex.main,
         main = spd.title)
    for (i in 2:(nsets + 1)) {
      polygon(c(plotyears, rev(plotyears)), c(PrDens[, 
                                                     i], rev(PrDens[, i - 1])), col = col.fill[i - 
                                                                                                 1], lwd = lwd.obs[i - 1], border = col.line[i - 
                                                                                                                                               1], lty = lty.obs[i - 1])
    }
    if(!is.na(weighted.median)){
      # print(" ------------->  THERE")
      # print(xlim)
      # add the date(s) weighted median
      # print(" ------------->  THERE")
      weighted.medianBC <- weighted.median - 1950
      print(weighted.medianBC)
      abline(v = -weighted.medianBC, col = "black", lwd = 2)
    }
    if(!is.na(x.intercept)){
      x.interceptBC <- x.intercept - 1950
      print(x.interceptBC)
      abline(v = x.intercept, col = "black", lwd = 2)
    }
    if (legend) {
      if (is.null(legend.arg)) {
        legend(legend.pos, legend = names(x$spds), 
               fill = col.fill)
      }
      else {
        args.legend1 <- list(legend.pos, legend = names(x$spds), 
                             fill = col.fill)
        args.legend1[names(legend.arg)] <- legend.arg
        do.call("legend", args = args.legend1)
      }
    }
  }
  if (type == "proportion") {
    PrDens = prop.table(PrDens, 1)
    NAyears = numeric(length = 0)
    if (any(is.nan(apply(PrDens, 1, sum)))) {
      NAyears = which(is.nan(apply(PrDens, 1, sum)))
      PrDens[NAyears, ] = 0
    }
    if (nsets > 1) {
      PrDens = t(apply(PrDens, 1, cumsum))
    }
    PrDens = cbind(0, PrDens)
    if (is.na(ylab)) {
      ylab = "Relative Proportion"
    }
    plot(0, 0, xlim = xlim, ylim = c(0, 1), type = "l", 
         ylab = ylab, xlab = xlabel, xaxt = "n", yaxt = yaxt, 
         cex.axis = cex.axis, cex.lab = cex.lab)
    for (i in 2:(nsets + 1)) {
      polygon(c(plotyears, rev(plotyears)), c(PrDens[, 
                                                     i], rev(PrDens[, i - 1])), col = col.fill[i - 
                                                                                                 1], lwd = lwd.obs[i - 1], lty = lty.obs[i - 1], 
              border = col.fill[i - 1])
    }
    if (length(NAyears > 0)) {
      ii = which(diff(NAyears) > 1)
      ii = c(0, ii)
      for (k in 1:c(length(ii) - 1)) {
        index = (ii[k] + 1):ii[k + 1]
        polygon(c(plotyears[NAyears[index]], rev(plotyears[NAyears[index]])), 
                c(rep(-0.02, length(index)), rep(1.02, length(index))), 
                col = "white", border = "white")
      }
    }
    if (legend) {
      if (is.null(legend.arg)) {
        legend(legend.pos, legend = names(x$spds), 
               fill = col.fill)
      }
      else {
        args.legend1 <- list(legend.pos, legend = names(x$spds), 
                             fill = col.fill)
        args.legend1[names(legend.arg)] <- legend.arg
        do.call("legend", args.legend1)
      }
    }
  }
  if (type == "multipanel") {
    if (any(is.na(ylim))) {
      ylim <- c(0, max(PrDens) * ymargin)
    }
    if (is.na(ylab)) {
      ylab = "Summed Probability"
    }
    gap = abs(diff(ylim)) * gapFactor
    YLIMs = vector("list", length = nsets)
    YLIMs[[1]] = ylim
    for (i in 2:nsets) {
      YLIMs[[i]] = c(YLIMs[[i - 1]][2] + gap, YLIMs[[i - 
                                                       1]][2] + gap + abs(diff(ylim)))
    }
    plot(0, 0, xlim = xlim, ylim = c(min(unlist(YLIMs)), 
                                     max(unlist(YLIMs)) + gap), 
         type = "l", ylab = ylab, 
         xlab = xlabel, axes = F, cex.lab = cex.lab, main = spd.title)
    for (i in 1:nsets) {
      tmpYlim = YLIMs[[i]]
      axis(2, at = c(tmpYlim[1], median(tmpYlim), max(tmpYlim)), 
           labels = round(c(min(ylim), median(ylim), max(ylim)), 
                          2), las = 2, cex.axis = cex.axis)
      polygon(c(plotyears, rev(plotyears)), c(PrDens[, 
                                                     i] + tmpYlim[1], rep(0, length(plotyears)) + 
                                                tmpYlim[1]), col = col.fill[i], lwd = lwd.obs[i], 
              border = col.line[i], lty = lty.obs[i])
    }
    if (legend) {
      if (is.null(legend.arg)) {
        legend(legend.pos, legend = names(x$spds), 
               fill = col.fill)
      }
      else {
        args.legend1 <- list(legend.pos, legend = names(x$spds), 
                             fill = col.fill)
        args.legend1[names(legend.arg)] <- legend.arg
        do.call("legend", args.legend1)
      }
    }
  }
  if (calendar == "BP" & xaxt != "n") {
    rr <- range(pretty(plotyears))
    axis(side = 1, at = seq(rr[2], rr[1], -100), labels = NA, 
         tck = -0.01, cex.axis = cex.axis)
    axis(side = 1, at = pretty(plotyears), labels = abs(pretty(plotyears)), 
         cex.axis = cex.axis)
  }
  else if (calendar == "BCAD" & xaxt != "n") {
    yy <- plotyears
    rr <- range(pretty(yy))
    prettyTicks <- seq(rr[1], rr[2], +100)
    prettyTicks[which(prettyTicks >= 0)] <- prettyTicks[which(prettyTicks >= 
                                                                0)] - 1
    axis(side = 1, at = prettyTicks, labels = NA, tck = -0.01, 
         cex.axis = cex.axis)
    py <- pretty(yy)
    pyShown <- py
    if (any(pyShown == 0)) {
      pyShown[which(pyShown == 0)] = 1
    }
    py[which(py > 1)] <- py[which(py > 1)] - 1
    axis(side = 1, at = py, labels = abs(pyShown), cex.axis = cex.axis)
  }
}