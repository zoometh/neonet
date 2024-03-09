#' @name neo_spd
#' 
#' @description SPD on a dataset
#'
#' @param df.c14 the dataset with NeoNet columns (SiteName, Period, etc.). Can be: a TSV file, or a data.frame, or a GeoJSON, or an sf object. Default: NeoNet med dataset
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param export.plot if TRUE export (by default), if FALSE display
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
                    shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
                    ref.c14age = c(10000, 5000),
                    plotname = NA,
                    export = TRUE,
                    outDir = "C:/Rprojects/neonet/results/",
                    width = 9,
                    height = 11,
                    verbose = TRUE){
  # c14.db.url <- 'http://mappaproject.arch.unipi.it/mod/files/140_id00140_doc_elencoc14.tsv'
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  
  
  # data <- df.c14 %>%
  #   mutate(category = case_when(
  #     is.data.frame()           ~ "dataframe",
  #     inherits("sf")   ~ "sf",
  #     TRUE                   ~ "Low" # Default case
  #   ))
  # 
  
  
  if(is.data.frame(df.c14)){
    if(verbose){
      print("Read a dataframe")
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
        print("Read an a GeoJson file")
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
  # periods
  if(verbose){print("Read period colors")}
  periods.colors <- read.csv(ref.period, sep = "\t")
  periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
  periods.colors.selected <- rbind(periods.colors.selected, c("others", "#808080"))
  # fetch not listed periods (others)
  unshown.per <- as.character(unlist(unique(c14[!(c14$Period %in% shown.per), "Period"])))
  if(verbose & length(unshown.per) > 0){
    print("These periods will be gathered into 'other':")
    cat(unshown.per, sep = ", ")
  }
  c14 <- within(c14, Period[Period %in% unshown.per] <- 'others')
  c14 <- merge(c14, periods.colors.selected, by.x = "Period", by.y = "period", all.x = T)
  
  # c14$colors <- NULL # rm previous colors
  # unique(c14$Period)
  # unique(c14$color)
  # reference colors in order
  periods.colors.plotted <- periods.colors.selected[periods.colors.selected$period %in% unique(c14$Period), "period"]
  # periods.colors.plotted.all <- periods.colors[periods.colors$period %in% periods.colors.plotted, c("period", "color")]
  # colpal <- periods.colors.plotted.all$color
  colpal <- periods.colors.selected$color
  if(verbose){
    print("These periods will be plotted with these colors\n")
    cat(periods.colors.plotted, sep = ", ")
    cat(colpal, sep = ", ")
  }
  c14 <- c14 %>% dplyr::arrange(factor(Period, levels = periods.colors.selected$period))
  c14$Period <- factor(c14$Period, levels = periods.colors.selected$period)
  c14$C14Age <- as.numeric(c14$C14Age)
  c14$C14SD <- as.numeric(c14$C14SD)
  
  # c14 <- c14[match(periods.colors.selected$period, c14$Period),]
  
  # df.c14.others <- df.c14.others[match(df.c14.others$Period, periods.colors.plotted),]
  # names(periods.colors.selected)[names(periods.colors.selected) == 'Period'] <- 'period'
  # xxx <- merge(c14, periods.colors.selected, by = "Period", all.x = TRUE)
  # df.c14.others <- dplyr::left_join(data.frame(Period = periods.colors.selected), c14, by = "Period")
  if(verbose){
    print("Calibration")
  }
  
  c14.cal <- head(c14, 30)
  
  bins <- rcarbon::binPrep(c14.cal$SiteName,
                           c14.cal$C14Age,
                           h = 50)
  x <- rcarbon::calibrate(c14.cal$C14Age,
                          c14.cal$C14SD,
                          normalised = FALSE)
  spd.c14 <- rcarbon::stackspd(x = x,
                               group = c14.cal$Period,
                               timeRange = c(ref.c14age[1], ref.c14age[2]),
                               bins = bins,
                               runm = 50)
  # data(emedyd)
  # emedyd.samp <- head(emedyd, 100)
  # x = calibrate(x=emedyd.samp$CRA, errors=emedyd.samp$Error,normalised=FALSE)
  # bins = binPrep(sites=emedyd.samp$SiteName, ages=emedyd.samp$CRA,h=50)
  # res = stackspd(x=x,timeRange=c(16000,8000),bins=bins,group=emedyd.samp$Region)
  
  
  if(export){
    if(is.na(plotname)){
      # plotname <- DescTools::SplitPath(df.c14)$filename
      plotname <- "spd"
    }
    plotfile <- paste0(plotname, "-spd.png")
    outFile <- paste0(outDir, plotfile)
    png(outFile, height = height, width = width, units = "cm", res = 300)
  }
  if(verbose){
    print("Plot")
  }
  periods <- unique(c14$Period)
  periods.colors <- periods.colors.selected[periods.colors.selected$period %in% periods, "color"]
  tit <- paste0(plotname, " (", nrow(c14), " dates used)")
  neo_spdplot(spd.c14,
              type = 'stacked',
              calendar = "BCAD",
              cex.lab = .7,
              cex.axis = .7,
              legend.arg = list(cex = .7,
                                pt.cex = .7,
                                title = 'Periods'),
              spd.title = tit,
              # colpal = colpal,
              # periods = periods,
              # ref.period = ref.period,
              periods.colors = periods.colors,
              shown.per = shown.per,
              verbose = FALSE
  )
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


