#' @name neo_spd
#' 
#' @description SPD on a dataset colored by periods (default) or KCC.
#'
#' @param df.c14 the dataset with NeoNet columns (SiteName, Period, etc.). Can be: a TSV file, or a data.frame, or a GeoJSON, or an sf object. Default: NeoNet med dataset
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param shown.per .
#' @param col.kcc KCC column names. Only useful if `color.on = "kcc"`.
#' @param time.span The min and max of the dates to plot (x limit interval of the ggplot) in cal BP. If `NA` (default), will be calculated on the fly, using `time.round` to round.
#' @param time.round Interval to round-up the axis of the plot. Only used if `time.span` is `NA`.
#' @param calendar a `plot.stackCalSPD()` category: `BP` or `BCAD` (default).
#' @param title The title of the plot description.
#' @param export if TRUE export (by default), if FALSE display,
#' @param color.on The field name to color on (`"Period"` default) or KCC (`"kcc"`).
#' @param fileOut The name of the output file.
#' @param dirOut name of the output folder. Only useful when `export.plot` is TRUE
#' @param width,height dimension of the output map, if exported.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Plot
#'
#' @examples
#' 
#' library(rcarbon)
#' source("R/neo_spdplot.R") # adapted from rcarbon::plot.stackCalSPD.R to fetch the selected colors
#'
#' # Plot NeoNet Med dataset
#' neo_spd()
#'
#' # Plot a dataframe
#' neo_spd(df.c14 = df.c14)
#'
#' Plot a GeoJSON downloaded from the NeoNet app
#' neo_spd(df.c14 = "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-23.geojson")
#' 
#' @export

neo_spd <- function(df.c14 = "https://digitallib.unipi.it/fedora/objects/mag:2627/datastreams/MM54ff3698c0ea78b77469ce6462c2ca36/content",
                    # df.url = 'https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/140_140_id00140_doc_elencoc14.tsv',
                    ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                    col.kcc = c("koppen_6k", "koppen_7k", "koppen_8k", "koppen_9k", "koppen_10k", "koppen_11k"),
                    shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
                    time.span = NA, #c(10000, 5000),
                    time.round = 1000,
                    calendar = "BCAD",
                    color.on = "Period",
                    title = "SPD",
                    plotname = "spd",
                    export = TRUE,
                    outFile = NA,
                    outDir = "C:/Rprojects/neonet/results/",
                    width = 9, height = 11,
                    verbose = TRUE){
  source("R/config.R")
  # c14.db.url <- 'http://mappaproject.arch.unipi.it/mod/files/140_id00140_doc_elencoc14.tsv'
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  # df.c14 <- df_cc
  if(is.data.frame(df.c14)){
    if(verbose){
      print("Read a data.frame")
    }
    c14 <- df.c14
  } 
  if(inherits(df.c14, "sf")){
    if(verbose){
      print("Read an sf object")
    }
    c14 <- sf::st_set_geometry(df.c14, NULL)
  } else if(!is.na(DescTools::SplitPath(df.c14)$extension)){
    if(DescTools::SplitPath(df.c14)$extension == "geojson"){
      if(verbose){
        print("Read an a GeoJSON file")
      }
      c14 <- sf::st_read(df.c14, quiet = T)
    }
    if(DescTools::SplitPath(df.c14)$extension == "tsv"){
      if(verbose){
        print("Read an a TSV file")
      }
      c14 <- read.table(df.c14, sep = "\t", header = TRUE, stringsAsFactors = F, quote = "")
    } 
  }
  # if(is.na(DescTools::SplitPath(df.c14)$extension)){
  #   if(verbose){
  #     print("Read a ?? ")
  #   }
  #   c14 <- data.table::fread(df.c14, verbose = F)
  # }
  ## colores
  if(color.on == "Period"){
    # periods
    if(verbose){print("Read period colors")}
    periods.colors <- read.csv(ref.period, sep = "\t")
    periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
    periods.colors.selected <- rbind(periods.colors.selected, c("others", "#808080"))
    # fetch not listed periods (others)
    # unshown.per <- as.character(unlist(unique(c14[!(c14$Period %in% shown.per), "Period"])))
    unshown.per <- as.character(unlist(unique(c14[!(c14[[color.on]] %in% shown.per), color.on])))
    if(verbose & length(unshown.per) > 0){
      print("These periods will be gathered into 'other':")
      cat(unshown.per, sep = ", ")
    }
    c14 <- within(c14, Period[Period %in% unshown.per] <- 'others')
    c14 <- merge(c14, periods.colors.selected, by.x = color.on, by.y = "period", all.x = T)
    
    # c14$colors <- NULL # rm previous colors
    # unique(c14$Period)
    # unique(c14$color)
    # reference colors in order
    periods.colors.plotted <- periods.colors.selected[periods.colors.selected$period %in% unique(c14[[color.on]]), "period"]
    # periods.colors.plotted.all <- periods.colors[periods.colors$period %in% periods.colors.plotted, c("period", "color")]
    # colpal <- periods.colors.plotted.all$color
    colpal <- periods.colors.selected[periods.colors.selected$period %in% periods.colors.plotted, "color"]
    # colpal <- periods.colors.selected$color
    if(verbose){
      print("These periods will be plotted with these colors")
      print(paste0(periods.colors.plotted, collapse = ", "))
      print(paste0(colpal,  collapse = ", "))
    }
    c14 <- c14 %>% 
      dplyr::arrange(factor(Period, levels = periods.colors.selected$period))
    # c14$Period <- factor(c14$Period, levels = periods.colors.selected$period)
    c14[[color.on]] <- factor(c14[[color.on]], levels = periods.colors.plotted)
    periods <- unique(c14.cal[[color.on]])
    var.colors <- periods.colors.selected[periods.colors.selected$period %in% periods, "color"]
  }
  if(color.on == "kcc"){
    # Koppen classes
    # /!\ it is assumed that a date has only one KCC
    # fill a new column with its KCC
    if(verbose){print("Read KCC colors")}
    c14$kcc <- apply(c14[ , col.kcc], 1, function(x) x[!is.na(x)][1])
    c14$kcc[is.na(c14$kcc)] <- "None"
    # c14 <- c14[!is.na(c14$kcc), ] # rm NA
    # selected.kcc <- na.omit(unique(unlist(c14[, col.req]))) 
    # selected.kcc <- factor(selected.kcc, levels = c(unique(selected.kcc)))
    kcc_colors <- c(kcc_colors, "None" = "#808080")
    kcc_colors_selected <- kcc_colors[names(kcc_colors) %in% c14$kcc]
    c14[[color.on]] <- factor(c14[[color.on]], levels = names(kcc_colors_selected))
    var.colors <- as.character(kcc_colors_selected)
  }
  
  c14$C14Age <- as.numeric(c14$C14Age)
  c14$C14SD <- as.numeric(c14$C14SD)
  # needed for radiocarbon (to check)
  # c14$Period <- as.character(c14$Period)
  c14$C14Age <- as.integer(c14$C14Age)
  c14$C14SD <- as.integer(c14$C14SD)
  
  # c14 <- c14[match(periods.colors.selected$period, c14$Period),]
  
  # df.c14.others <- df.c14.others[match(df.c14.others$Period, periods.colors.plotted),]
  # names(periods.colors.selected)[names(periods.colors.selected) == 'Period'] <- 'period'
  # xxx <- merge(c14, periods.colors.selected, by = "Period", all.x = TRUE)
  # df.c14.others <- dplyr::left_join(data.frame(Period = periods.colors.selected), c14, by = "Period")
  if(verbose){
    print("Calibration")
  }
  # c14.cal <- head(c14, 30)
  c14.cal <- c14
  if(!is.numeric(time.span)){
    # create temporal bound on +/- time span
    time.span <- c()
    starts <- max(c14.cal$C14Age)
    time.span[1] <- starts + (time.round - (starts %% time.round))
    finishes <- min(c14.cal$C14Age)
    time.span[2] <- finishes - (finishes %% time.round)
  }
  bins <- rcarbon::binPrep(sites = c14.cal$SiteName,
                           ages = c14.cal$C14Age,
                           h = 50)
  x <- rcarbon::calibrate(c14.cal$C14Age,
                          c14.cal$C14SD,
                          normalised = FALSE)
  spd.c14 <- rcarbon::stackspd(x = x,
                               group = c14.cal[[color.on]],
                               # timeRange = c(starts.rounded, finishes.rounded),
                               timeRange = c(time.span[1], time.span[2]),
                               bins = bins,
                               runm = 50)
  if(export){
    if(is.na(outFile)){
      # plotname <- DescTools::SplitPath(df.c14)$filename
      outFile <- "spd"
    }
    plotfile <- paste0(outFile, "-spd.png")
    outFile <- paste0(outDir, plotfile)
    png(outFile, height = height, width = width, units = "cm", res = 300)
  }
  if(verbose){
    print("Plot")
  }
  tit <- paste0(title, " (", nrow(c14.cal), " dates used)")
  neo_spdplot(spd.c14,
              type = 'stacked',
              calendar = calendar,
              cex.lab = .7,
              cex.axis = .7,
              legend.arg = list(cex = .7,
                                pt.cex = .7,
                                title = color.on),
              spd.title = tit,
              # colpal = colpal,
              # periods = periods,
              # ref.period = ref.period,
              periods.colors = var.colors, # TODO: change fieldname in neo_spdsubplot() to entail KCC
              # shown.per = shown.per,
              verbose = FALSE)
  if(export){
    dev.off()
    if(verbose){
      print(paste0("The SPD plot '", plotfile, "' has been exported to: '", outDir, "'"))
    }
  }
}

# library(rcarbon)
# source("R/plot_stackSPD.R") # adapted from rcarbon::plot.stackCalSPD.R to fetch the selected colors
# neo_spd()

# df.url = "C:/Rprojects/neonet/R/app-dev/c14_dataset_med_x_atl.tsv")

# neo_spd(df.c14 = "C:/Users/Thomas Huet/Downloads/id00164_doc_elencoc14 (5).tsv")
# neo_spd(df.c14 = "https://digitallib.unipi.it/fedora/objects/mag:2627/datastreams/MM54ff3698c0ea78b77469ce6462c2ca36/content", export = F)

# # c14dates <- data.table::fread("https://digitallib.unipi.it/fedora/objects/mag:2627/datastreams/MM54ff3698c0ea78b77469ce6462c2ca36/content")
# 
# spd.c14.ordered <- spd.c14
# 
# spd.c14.ordered$spds <-  spd.c14.ordered$spds[order(match(spd.c14.ordered$spds, periods))]
# 
# x[order(match(x,y))]


