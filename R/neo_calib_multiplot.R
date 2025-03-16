neo_calib_multiplot <- function(x, 
                                type = "d", 
                                calendar = "BP", 
                                wmedian = TRUE,
                                HPD = FALSE, 
                                credMass = 0.95, 
                                decreasing = NULL, 
                                label = TRUE, 
                                label.pos = 0.5, 
                                label.offset = 0, 
                                label.text = NA,
                                label.wmedian = TRUE,
                                xlim = NULL, 
                                xlab = NA, 
                                ylab = NA, 
                                col.fill = "grey50", 
                                col.fill2 = "grey82", 
                                col.line = "black", 
                                col.wmedian = "blue",
                                lwd = 1, 
                                cex.id = 1, 
                                cex.wmedian = 1,
                                cex.lab = 1, 
                                cex.axis = 1, 
                                ydisp = FALSE, 
                                gapFactor = 0.2, 
                                rescale = FALSE) 
{
  ########### adapted from rcarbon package by E. Crema ####################
  if (length(lwd) == 1) {
    lwd = rep(lwd, length(x))
  }
  if (length(col.line) == 1) {
    col.line = rep(col.line, length(x))
  }
  if (length(col.fill) == 1) {
    col.fill = rep(col.fill, length(x))
  }
  if (length(col.fill2) == 1) {
    col.fill2 = rep(col.fill2, length(x))
  }
  if (!is.null(decreasing)) {
    ord = order(rcarbon::medCal(x), decreasing = decreasing)
    x = x[ord]
    col.line = col.line[ord]
    col.fill = col.fill[ord]
    col.fill2 = col.fill2[ord]
  }
  date.pos = rcarbon::qCal(x, p = label.pos) + label.offset
  calendars <- c("BP", "BCAD")
  if (!calendar %in% calendars) {
    stop("The calendar you have chosen is not currently an option.")
  }
  if (is.null(xlim)) {
    if (anyNA(x$grids)) {
      tmp = apply(x$calmatrix, 1, sum)
      st = as.numeric(names(tmp[which(tmp > 0)[1] - 1]))
      en = as.numeric(names(tmp[which(tmp > 0)[length(which(tmp > 
                                                              0))] + 1]))
    }
    else {
      st = max(unlist(lapply(x$grids, function(x) {
        max(x$calBP)
      })))
      en = min(unlist(lapply(x$grids, function(x) {
        min(x$calBP)
      })))
    }
    edge = 0.1 * abs(st - en)
    xlim = c(st + edge, en - edge)
  }
  yearsBP = xlim[1]:xlim[2]
  if (calendar == "BP") {
    plotyears <- yearsBP
    xvals <- c(plotyears[1], plotyears, plotyears[length(plotyears)], 
               plotyears[1])
    if (is.na(xlab)) {
      xlabel <- "Years cal BP"
    }
    else {
      xlabel <- xlab
    }
  }
  else if (calendar == "BCAD") {
    plotyears <- rcarbon::BPtoBCAD(yearsBP)
    xlim <- rcarbon::BPtoBCAD(xlim)
    xvals <- c(plotyears[1], plotyears, plotyears[length(plotyears)], 
               plotyears[1])
    if (is.na(xlab)) {
      xlabel <- "Years BC/AD"
      if (all(range(plotyears) < 0)) {
        xlabel <- "Years BC"
      }
      if (all(range(plotyears) > 0)) {
        xlabel <- "Years AD"
      }
    }
    else {
      xlabel <- xlab
    }
  }
  else {
    stop("Unknown calendar type")
  }
  if (type == "b") {
    bse = rcarbon::hpdi(x, credMass = credMass)
    plot(0, 0, xlim = xlim, ylim = c(0, length(bse) + 1), 
         axes = F, xlab = xlabel, ylab = "", type = "n")
    for (i in 1:length(bse)) {
      tmp = matrix(bse[[i]][, -3], ncol = 2)
      if (calendar == "BP") {
        apply(tmp, 1, function(x, y, lwd, col) {
          lines(c(x), c(y, y), lwd = lwd, col = col)
        }, y = i, lwd = lwd[i], col = col.line[i])
        if (label) {
          text(x = date.pos[i], y = i + gapFactor, label = x$metadata$DateID[i], 
               cex = cex.id)
        }
      }
      if (calendar == "BCAD") {
        apply(tmp, 1, function(x, y, lwd, col) {
          lines(rcarbon::BPtoBCAD(c(x)), c(y, y), lwd = lwd, 
                col = col)
        }, y = i, lwd = lwd[i], col = col.line[i])
        if (label) {
          text(x = rcarbon::BPtoBCAD(date.pos[i]), 
               y = i + gapFactor, 
               label = x$metadata$DateID[i], 
               cex = cex.id)
        }
      }
    }
  }
  if (type == "d") {
    if (anyNA(x$grids)) {
      if (rescale) {
        x$calmatrix = apply(x$calmatrix, 2, rcarbon::reScale)
      }
      tmp = apply(x$calmatrix, 1, max)
      ylim = as.numeric(c(0, tmp[which.max(tmp)]))
    }
    else {
      if (rescale) {
        for (i in 1:length(x)) {
          x$grids[[i]]$PrDens = rcarbon::reScale(x$grids[[i]]$PrDens)
        }
      }
      ylim = c(0, max(unlist(lapply(x$grids, function(x) {
        max(x$PrDens)
      }))))
    }
    gap = abs(diff(ylim)) * gapFactor
    YLIMs = vector("list", length = length(x))
    YLIMs[[1]] = ylim
    for (i in 2:length(x)) {
      YLIMs[[i]] = c(YLIMs[[i - 1]][2] + gap, YLIMs[[i - 
                                                       1]][2] + gap + abs(diff(ylim)))
    }
    if(label.wmedian){
      # add a buffer on the left to display the label
      xlim[1] <- xlim[1] - 200
    }
    plot(0, 0, 
         xlim = xlim, 
         ylim = c(min(unlist(YLIMs)), 
                  max(unlist(YLIMs)) + gap), type = "n", ylab = ylab, 
         xlab = xlabel, axes = F, cex.lab = cex.lab)
    for (i in 1:length(x)) {
      tmpYlim = YLIMs[[i]]
      if (ydisp) {
        axis(2, at = c(tmpYlim[1], median(tmpYlim), 
                       max(tmpYlim)), labels = round(c(min(ylim), 
                                                       median(ylim), max(ylim)), 2), las = 2, cex.axis = cex.axis)
      }
      if (anyNA(x$grid)) {
        years = as.numeric(rownames(x$calmatrix))
        PrDens = x$calmatrix[, i]
        ii = which(PrDens > 0)[1] - 1
        jj = max(which(PrDens > 0)) + 1
        years = years[ii:jj]
        PrDens = PrDens[ii:jj]
      }
      else {
        years = x$grid[[i]]$calBP
        PrDens = x$grid[[i]]$PrDens
      }
      if (calendar == "BCAD") {
        years = rcarbon::BPtoBCAD(years)
      }
      xvals = c(years, rev(years))
      yvals = c(PrDens + tmpYlim[1], rep(0, length(years)) + 
                  tmpYlim[1])
      if (!HPD) {
        polygon(xvals, yvals, col = col.fill[i], border = col.fill[i])
      }
      else {
        polygon(xvals, yvals, col = col.fill2[i], border = col.fill2[i])
        hdres <- rcarbon::hpdi(x, credMass = credMass)[[i]]
        for (j in 1:nrow(hdres)) {
          if (calendar == "BCAD") {
            index <- which(xvals %in% rcarbon::BPtoBCAD(hdres[j, 
                                                              1]:hdres[j, 2]))
          }
          else {
            index <- which(xvals %in% hdres[j, 1]:hdres[j, 
                                                        2])
          }
          polygon(c(xvals[index], xvals[index[length(index)]], 
                    xvals[index[1]]), c(yvals[index], min(tmpYlim), 
                                        min(tmpYlim)), col = col.fill[i], border = col.fill[i])
        }
      }
      if (label) {
        # print("XXXXXXXX")
        xx = date.pos[i]
        if (calendar == "BCAD") {
          xx = rcarbon::BPtoBCAD(xx)
        }
        ylabel = ifelse(rescale, max(yvals) - 0.5, max(yvals) + 
                          gap/2)
        if(is.na(label.text)){
          # print(min(unlist(YLIMs)))
          label.anchor <- xlim[1] # 00
          new.label <- paste0(x$metadata$DateID[i], " - ", x$metadata$SiteName[i])
          text(x = label.anchor, 
               y = ylabel, 
               labels = new.label, 
               cex = cex.id,
               pos = 4)
        }
      }
      if(wmedian){
        wmed <- x$metadata$wmedian[i]
        # print(wmed)
        # abline(v = rcarbon::BPtoBCAD(x$metadata$wmedian[i]), col = "red", lty = 2)  # Dashed red line
        # abline(v = wmed, col = "red")
        segments(x0 = wmed, y0 = ylabel-.5, 
                 x1 = wmed, y1 = ylabel+.5,
                 col = col.wmedian)
        if(label.wmedian){
          text(x = wmed,
               y = ylabel,
               labels = wmed,
               col = col.wmedian,
               cex = cex.wmedian,
               # adj = .5,
               pos = 4)
          # legend(x = wmed,
          #        y = ylabel, 
          #        legend = wmed, 
          #        col = col.wmedian,
          #        cex = cex.wmedian,
          #        xjust = .5,
          #        box.col = "white",
          #        bg = "white", 
          #        adj = 0)
          
        }
      }
    }
  }
  if (calendar == "BP") {
    rr <- range(pretty(plotyears))
    axis(side = 1, at = seq(rr[2], rr[1], -100), labels = NA, 
         tck = -0.01, cex.axis = cex.axis)
    axis(side = 1, at = pretty(plotyears), labels = abs(pretty(plotyears)), 
         cex.axis = cex.axis)
  }
  else if (calendar == "BCAD") {
    yy <- plotyears
    rr <- range(pretty(yy))
    prettyTicks <- seq(rr[1], rr[2], 100)
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
