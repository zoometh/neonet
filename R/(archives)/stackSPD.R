library(rcarbon)

shown.per <- c("EM", "MM", "LM", "EN", "MN", "LN")


source("R/plot_stackSPD.R") # adapted from rcarbon::plot.stackCalSPD.R to fetch the selected colors
# c14.db.url <- 'http://mappaproject.arch.unipi.it/mod/files/140_id00140_doc_elencoc14.tsv'
c14.db.url <- 'https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/140_140_id00140_doc_elencoc14.tsv'
c14 <- read.table(c14.db.url, sep = "\t", header = TRUE, stringsAsFactors = F, quote="")
ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv"
periods.colors <- read.csv(ref.period, sep = "\t")
periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
periods.colors.selected <- rbind(periods.colors.selected, c("others", "#808080"))
# fetch not listed periods (others)
df.other.periods <- df.c14[!(df.c14$Period %in% shown.per), ]
unshown.per <- unique(df.other.periods$Period)
df.c14.others <- within(df.c14, Period[Period %in% unshown.per] <- 'others')
df.c14.others <- merge(df.c14.others, periods.colors.selected, by.x = "Period", by.y = "period", all.x = T)
df.c14.others$colors <- NULL # rm previous colors


# shown.per.colors <- c("#0000CF", "#1D1DFF", "#3737FF", "#FF1B1B", "#FF8D1B", "#FFC04D", "#808080")
# unshown.per <- c("LMEN", "MNLN", "n/a", "UM", "UM ou EM ?", "EM ?", "UN")
# c14.grp <- within(c14, Period[Period %in% unshown.per] <- 'others')
c14.grp.reord <- dplyr::left_join(data.frame(Period = shown.per), df.c14.others, by = "Period")
c14.samp <- c14.grp.reord[sample(seq(1, nrow(c14.grp.reord), 20)), ]
bins <- rcarbon::binPrep(c14.samp$SiteName,
                         c14.samp$C14Age,
                         h = 50)
x <- rcarbon::calibrate(c14.samp$C14Age,
                        c14.samp$C14SD,
                        normalised = FALSE)
spd.c14 <- rcarbon::stackspd(x = x,
                             group = c14.samp$Period,
                             timeRange = c(9500, 5000),
                             bins = bins,
                             runm = 50)
if(export){
  png('SPDneonet.png', height = 11, width = 17, units="cm", res = 600)
}
plot.stackCalSPD(spd.c14,
                 type = 'stacked',
                 calendar = "BCAD",
                 cex.lab = .7,
                 cex.axis = .7,
                 legend.arg = list(cex = .7,
                                   pt.cex = .7,
                                   title = 'Periods'),
                 verbose = FALSE
)
if(export){
  dev.off()
}
