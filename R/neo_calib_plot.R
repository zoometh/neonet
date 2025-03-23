#' @name neo_calib_plot
#'
#' @description Plot a calibrate date, or a dataframe of radiocarbon dates, and add the weighted median (wmedian)
#'
#' @param df.c14 A tuple (Age, Delta) or a dataframe
#' @param lbl.median Label the wmedian? Default: TRUE.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return
#'
#' @examples
#'
#' # one date (example: Otzi death)
#' df <- neo_calib_plot(df.c14 = c(4550, 19))
#'
#' # a dataframe (after running a selection with `neo_ischr()`)
#' df.c14 <- isochr$data.raw
#' neo_calib_plot(df.c14)
#' 
#' @export
neo_calib_plot <- function(df.c14 = NA, 
                           intCal = 'intcal20',
                           present = 1950,
                           col.wmedian = "blue",
                           lbl.median = TRUE,
                           cex.wmedian = 1,
                           cex.id = 1,
                           cex.lab = 1,
                           cex.axis = 1,
                           verbose = TRUE){
  if(inherits(df.c14, "numeric")){
    if(verbose){
      print(paste0("Display one date with wmedian"))
    }
    x <- rcarbon::calibrate(x = df.c14[1], 
                            errors = df.c14[2]) 
    weighted.median <- matrixStats::weightedMedian(x = x$grids$`1`$calBP, w = x$grids$`1`$PrDens)
    wmedian <- -(weighted.median - present)
    df.c14 <- c(df.c14, wmedian)
    plot(x, calendar = 'BCAD')
    abline(v = wmedian, 
           col = col.wmedian)
    if(lbl.median){
      text(x = wmedian, 
           y = mean(c(par("usr")[3], par("usr")[4])), 
           labels = paste("wmedian:", abs(round(wmedian, 0)), "BC"), 
           col = col.wmedian, 
           cex = 0.8, 
           pos = 3)  # Position above the point
    }
    return(df.c14)
  }
  if(inherits(df.c14, "data.frame")){
    if(verbose){
      print(paste0("Display seriated dates with wmedians"))
    }
    # df <- head(df.c14)
    x <- rcarbon::calibrate(x = df.c14$C14Age, 
                            errors = df.c14$C14SD)
    # ensure idfs are OK
    x$metadata$DateID <- df.c14$idf
    # add site names to metadata
    x$metadata$SiteName <- df.c14$site
    # add LabCode to metadata
    x$metadata$LabCode <- df.c14$labcode
    ######## calculate medians ############
    wmedians <- c()
    for(i in seq(1, length(x$grids))){
      weighted.median <- matrixStats::weightedMedian(x = x$grids[[i]]$calBP, w = x$grids[[i]]$PrDens)
      wmedians <- c(wmedians, weighted.median)
    }
    wmedians <- round(wmedians, 0)
    wmedians <- -(wmedians - present)
    x$metadata$wmedian <- wmedians
    source("R/neo_calib_plot_multi.R")
    neo_calib_plot_multi(x,
                         calendar = "BCAD",
                         decreasing=TRUE,
                         rescale=TRUE,
                         HPD=TRUE,
                         col.wmedian = col.wmedian,
                         cex.wmedian = cex.wmedian,
                         cex.id = cex.id,
                         cex.lab = cex.lab,
                         cex.axis = cex.axis,
                         label.pos = 0.9,
                         label.offset=-200)
  }
}

# source("R/neo_calib_plot.R")
# df <- neo_calib_plot(df.c14 = c(4550, 27))
#     # max and mins for the plot
#     maxs <- mins <- c()
#     for(m in seq(1, length(x$grids))){
#       maxs <- c(maxs, max(x$grids[[m]]$calBP))
#       mins <- c(mins, min(x$grids[[m]]$calBP))
#     }
#     min.bp <- min(mins)
#     max.bp <- max(maxs)
#     # bins
#     bins <- rcarbon::binPrep(sites=df.c14$Site, 
#                              ages=df.c14$C14Age,
#                              h=50)
#     res <- rcarbon::spd(
#       x,
#       timeRange=c(max.bp + 100, min.bp - 100),
#       bins = bins,
#       datenormalised = FALSE,
#       spdnormalised = FALSE,
#       runm = NA,
#       verbose = TRUE,
#       edgeSize = 500
#     )
#     rcarbon::multiplot(x,
#                        calendar = "BCAD",
#                        decreasing=TRUE,
#                        rescale=TRUE,
#                        HPD=TRUE,
#                        cex.id=.5,
#                        label.pos=0.9,
#                        label.offset=-200)
#     # plot(res, 
#     #      # type = 'multipanel',
#     #      legend.cex = 0.75,
#     #      cex.lab = 0.75,
#     #      fill.p = "grey",
#     #      calendar = "BCAD")
#   }
# }
# 
# source("R/neo_isochr.R")
# isochr <- neo_isochr(df.c14 = df_filtered, 
#                      isochr.subset =  "None", #my_list[[i]][[2]], #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
#                      selected.per = my_list[[i]][[5]],
#                      # largest.isochr = TRUE,
#                      where = my_list[[i]][[3]], #Italia.where, # where.roi,
#                      buff = 0,
#                      kcc.file = kcc.file.path, # NA, 
#                      is.other.geotiff = FALSE,
#                      create.legend = TRUE,
#                      isochr.line.color = NA, # "black", # NA to get colored isochrones (red, blue)
#                      isochr.line.size = .5,
#                      isochr.txt.size = 0,
#                      calibrate = FALSE,
#                      shw.dates = TRUE,
#                      # show.all.dates = FALSE,
#                      size.date = 1,
#                      # color.dates = "darkgrey",
#                      alpha.dates = 1,
#                      lbl.dates = TRUE,
#                      lbl.dates.interval = lbl.dates.interval, # subset dates to be labeled
#                      # lbl.all.dates = FALSE,
#                      # lbl.date.field = "median",
#                      lbl.dates.size = 4,
#                      lbl.time.interv = TRUE)
# df.c14 <- isochr$data.raw

