library(rcarbon)

source("R/plot_stackSPD.R") # adapted from radiocarbon::plot_stackSPD.R to fetch the selected colors
c14.db.url <- 'http://mappaproject.arch.unipi.it/mod/files/140_id00140_doc_elencoc14.tsv'
c14 <- read.table(c14.db.url, sep = "\t", header = TRUE, stringsAsFactors = F)
shown.per <- c("EM", "MM", "LM", "EN", "MN", "LN", "others")
shown.per.colors <- c("#0000CF", "#1D1DFF", "#3737FF", "#FF1B1B", "#FF8D1B", "#FFC04D", "#808080")
unshown.per <- c("LMEN", "MNLN", "n/a", "UM", "UM ou EM ?", "EM ?", "UN")
c14.grp <- within(c14, Period[Period %in% unshown.per] <- 'others')
c14.grp.reord <- dplyr::left_join(data.frame(Period = shown.per), c14.grp, by = "Period")
c14.samp <- c14.grp.reord
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
png('SPDneonet.png', height = 11, width = 17, units="cm", res = 600)
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
dev.off()