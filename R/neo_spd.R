#' @name neo_spd
#' 
#' @description SPD on dataset
#'
#' @param df.c14 the original XLSX with neonet columns (SiteName, Period, etc.)
#' @param df.url the URL of a TSV dataset
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Plot
#'
#' @examples
#'
#'
#' # Plot the DOI
#' library(rcarbon)
#' source("R/neo_spdplot.R") # adapted from rcarbon::plot.stackCalSPD.R to fetch the selected colors
#' neo_spd()
#'
#' # plot a dataframe
#' neo_spd(df.c14 = df.c14)
#'
#' @export

neo_spd <- function(df.c14 = NA,
                    df.url = 'https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/140_140_id00140_doc_elencoc14.tsv',
                    ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                    shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
                    ref.c14age = c(9500, 5000),
                    export = FALSE,
                    verbose = TRUE){
  # c14.db.url <- 'http://mappaproject.arch.unipi.it/mod/files/140_id00140_doc_elencoc14.tsv'
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(is.na(df.c14)){
    if(verbose){print("Read data from URL")}
    c14 <- read.table(df.url, sep = "\t", header = TRUE, stringsAsFactors = F, quote="")
  } else {
    c14 <- df.c14
  }
  # periods
  if(verbose){print("Read period colors")}
  periods.colors <- read.csv(ref.period, sep = "\t")
  periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
  periods.colors.selected <- rbind(periods.colors.selected, c("others", "#808080"))
  # fetch not listed periods (others)
  unshown.per <- unique(c14[!(c14$Period %in% shown.per), "Period"])
  if(verbose){
    print("These periods will be gathered into 'other'\n")
    cat(unshown.per, sep = ", ")
  }
  c14 <- within(c14, Period[Period %in% unshown.per] <- 'others')
  unique(c14$Period)
  c14 <- merge(c14, periods.colors.selected, by.x = "Period", by.y = "period", all.x = T)
  c14$colors <- NULL # rm previous colors
  # reference colors in order
  # periods.colors.plotted <- periods.colors.selected[periods.colors.selected$period %in% unique(c14$Period), "period"]
  # periods.colors.plotted.all <- periods.colors[periods.colors$period %in% periods.colors.plotted, c("period", "color")]
  # colpal <- periods.colors.plotted.all$color
  colpal <- periods.colors.selected$color
  if(verbose){
    print("These periods will be plotted with these colors\n")
    cat(periods.colors.plotted, sep = ", ")
    cat(colpal, sep = ", ")
  }
  c14 <- c14 %>% dplyr::arrange(factor(Period, levels = periods.colors.selected$period))
  
  
  # c14 <- c14[match(periods.colors.selected$period, c14$Period),]
  
  # df.c14.others <- df.c14.others[match(df.c14.others$Period, periods.colors.plotted),]
  # names(periods.colors.selected)[names(periods.colors.selected) == 'Period'] <- 'period'
  # xxx <- merge(c14, periods.colors.selected, by = "Period", all.x = TRUE)
  # df.c14.others <- dplyr::left_join(data.frame(Period = periods.colors.selected), c14, by = "Period")
  if(verbose){
    print("Calibration")
  }
  bins <- rcarbon::binPrep(c14$SiteName,
                           c14$C14Age,
                           h = 50)
  x <- rcarbon::calibrate(c14$C14Age,
                          c14$C14SD,
                          normalised = FALSE)
  spd.c14 <- rcarbon::stackspd(x = x,
                               group = c14$Period,
                               timeRange = c(ref.c14age[1], ref.c14age[2]),
                               bins = bins,
                               runm = 50)
  if(export){
    png('SPDneonet.png', height = 11, width = 17, units="cm", res = 600)
  }
  if(verbose){
    print("Plot")
  }
  neo_spdplot(spd.c14,
              type = 'stacked',
              calendar = "BCAD",
              cex.lab = .7,
              cex.axis = .7,
              legend.arg = list(cex = .7,
                                pt.cex = .7,
                                title = 'Periods'),
              colpal = colpal,
              verbose = FALSE
  )
  if(export){
    dev.off()
  }
}

# library(rcarbon)
# source("R/plot_stackSPD.R") # adapted from rcarbon::plot.stackCalSPD.R to fetch the selected colors
# neo_spd()

# df.url = "C:/Rprojects/neonet/R/app-dev/c14_dataset_med_x_atl.tsv")

