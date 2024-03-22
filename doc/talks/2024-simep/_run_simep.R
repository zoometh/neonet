library(c14bazAAR)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sf)
library(rnaturalearth)

getwd()
source("R/config.R")

## done
# l.dbs <- c("calpal", "medafricarbon", "agrichange", "neonet", "bda", "calpal", "radon", "katsianis")

root.path <- "C:/Rprojects/neonet/results"
where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
l.dbs <- c("calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis") 
# xronos
# l.dbs <- c("p3k14c")
# l.dbs <- c("bda")
# "neonet" gives a timeout
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read(where.roi,
                     quiet = TRUE)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
source("R/neo_dbs_parse.R")
df <- neo_dbs_parse(l.dbs = l.dbs, #what.db, # c("bda", "medafricarbon"),
                    col.c14baz = col.c14baz,
                    chr.interval.uncalBC = when,
                    roi = where)
source("R/neo_dbs_align.R")
df.c14 <- neo_dbs_align(df)

# remotes::install_github("people3k/p3k14c@2022.06")

# dbs
# DB not done: kiteeastafrica, nerd, aida,  (no culture)
# DB done: calpal, medafricarbon, agrichange, neonet, bda, calpal, radon, katsianis
# l.dbs <- c("bda", "medafricarbon")
# df <- neo_parse_db(l.dbs, chr.interval.uncalBC, present, roi)
# fspat(df.all.res, roi, outfile = "_db__all_class.png")
# fspat(df.all.res, roi, outfile = "_db__all.png")
# colnames(df.c14)
source("R/neo_calib.R")
df.c14 <- neo_calib(df.c14)
# write.csv(df.c14, samp_df, row.names = FALSE)

###################################################
##### shortcut: load the 'df14_simep.csv' file ####
# root.path <-"C:/Rprojects/neonet/results"
# samp_df <- paste0(root.path, "/df14_simep.csv")
# samp_df <- read.csv("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep.csv")
# samp_df <- read.csv(samp_df)
# df.c14 <- samp_df[sample(1:nrow(samp_df), 150), ]
# #           OR
# df.c14 <- samp_df
###################################################

df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
# unique(xxx$Period)
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
col.req <- gsub(pattern = ".tif", "", kcc.file)

source("R/neo_kcc_extract.R")
df_cc <- neo_kcc_extract(df.c14 = df.c14, kcc.file = kcc.file)

source("R/neo_kcc_sankey.R")
outDir <- neo_kcc_sankey(df_cc, col.req, )
ggsave(filename = paste0(outDir, period.names, "_kcc.png"),
       gout,
       width = 16, height = 12)

source("R/neo_kcc_plotbar.R")
Meso <- neo_kcc_plotbar(df_cc = df_cc, 
                        col.req = col.req,
                        selected.per = c("LM", "MM"),
                        title = "Mesolithic")
# Meso
Neo <- neo_kcc_plotbar(df_cc = df_cc, 
                       col.req = col.req,
                       selected.per = c("EN", "MN"),
                       title = "Neolithic")
Neo

source("R/neo_kcc_legend.R")
selected.kcc <- na.omit(unique(unlist(df.c14[, col.req]))) 
selected.kcc <- factor(selected.kcc, levels = unique(selected.kcc))
kcc.legend <- neo_kcc_legend(selected.kcc = selected.kcc, 
                             long.legend = TRUE)

# map
source("R/neo_map.R")
where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
g.neo.map <- neo_map(df.c14 = df.c14, 
                     plot.dates = TRUE,
                     ref.spat = where.roi, 
                     buff = 2.5,
                     title = "ROI")
g.neo.map

lay <- rbind(c(1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 3, 3, 3), 
             c(2, 2, 2, 2, 2, 4, 4, 4),
             c(2, 2, 2, 2, 2, 4, 4, 4), 
             c(2, 2, 2, 2, 2, 4, 4, 4))
g <- gridExtra::grid.arrange(Meso, Neo, kcc.legend, g.neo.map,
                             layout_matrix = lay,
                             top = paste0("Distribution of radiocarbon dates ", 
                                          "by Koppen classes in the ROI (n = ", 
                                          nrow(df.c14), ")")
                             
)
g.out <- paste0(root.path, "/kcc_meso_neo.png")
ggplot2::ggsave(file = g.out, g, width = 14, height = 10)

# neo_kcc_sankey(df_cc, col.req = col.req, 
#                selected.per = c("EN", "MM"), 
#                outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")
# df <- read.csv(paste0(root.path, "/medians.csv"))

library(rcarbon)

source("R/neo_spdplot.R")
source("R/neo_spd.R")

df_cc.meso <- df_cc[df_cc$Period %in% c("LM", "MM"), ]
# df_cc.neo <- head(df_cc.neo, 50)

neo_spd(df.c14 = df_cc.meso,
        title = "Mesolithic | LM, MM",
        # time.round = 1000,
        time.span = c(13000, 6500),
        calendar = 'BP',
        # shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
        color.on = "kcc",
        export = TRUE,
        outFile = "Mesolithic KCC",
        outDir = "C:/Rprojects/neonet/results/",
        width = 18, height = 13,
)
# 
# neo_spd(df.c14 = "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-24.geojson",
#         export = F)
# 
# samp <- head(df.c14, 10)

########### ISOCHRONES

library(rcarbon)

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")

frm <- function(df.c14 = NA, 
                c14.to.remove = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_to_remove2.tsv",
                selected.cols = c("sourcedb", "LabCode", "SiteName", "median", "db_period"),
                verbose = TRUE){
  # Remove unaccurate dates (optional)
  # escape the dates having a "-" as a prefix in their database (i.e. dates with lack of arguments to remove them)
  df.to.rm <- read.table(c14.to.remove, sep = "\t", header = TRUE)
  df.to.rm <- df.to.rm[!grepl("^-", df.to.rm$sourcedb), ]
  if(verbose){
    print(paste0(nrow(df.to.rm), " dates to be removed:"))
    print(df.to.rm[ , selected.cols])
  }
  df <- dplyr::anti_join(df.c14, df.to.rm, 
                         by = c("sourcedb", "LabCode"))
  return(df)
}

df_filtered <- frm(df.c14)

fdate <- function(LabCode = NA, columns = c("sourcedb", "LabCode", "SiteName", "median", "db_period", "db_culture")){
  # return info on a date from its LabCode
  a.date <- as.character(na.omit(
    sf::st_set_geometry(df.c14[df.c14$LabCode == LabCode, columns],
                        NULL)
  )[1,])
  cat(paste(a.date, collapse = "\t"), "\n")
}

fget.db <- function(db = NA, LabCode = NA){
  # Once the LabCode's database origin is sourced, print this LabCode infos
  df <- c14bazAAR::get_c14data(db)
  df <- as.data.frame(df[df$labnr == LabCode, ])
  print(df)
}

LabCode = "Arias_2009_x2"
fdate(LabCode = LabCode)
fget.db(db = "bda", LabCode = LabCode)


source("R/neo_find_dates.R")
neo_find_dates(df = map.iso$data, idf.dates = c(146))

# map.iso <- neo_isochr(df.c14 = df_filtered, 
#                       time.line.size = .5,
#                       calibrate = FALSE,
#                       shw.dates = TRUE,
#                       lbl.dates = TRUE,
#                       lbl.time.interv = FALSE)

#######################
#### Pioneer front ####
#######################
source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")
isochr.8k <- neo_isochr(df.c14 = df_filtered, 
                        isochr.subset = -8500,
                        kcc.file = "C:/Rprojects/neonet/doc/data/clim/koppen_7k.tif",
                        time.line.size = .5,
                        calibrate = FALSE,
                        shw.dates = TRUE,
                        lbl.dates = TRUE,
                        lbl.time.interv = TRUE)
isochr.8k$map

## check abberant dates
source("R/neo_find_dates.R")
abber.dates <- neo_find_dates(df = isochr.8k$data, idf.dates = c(365))
# LabCode = "UtC-1830"
fdate(LabCode = abber.dates$labcode)
abber.dates
fget.db(db = dates$sourcedb, LabCode = abber.dates$labcode)

source("R/neo_kcc_plotbar.R")
plotbar.neo <- neo_kcc_plotbar(df_cc = df_cc, 
                               kcc.file = c("koppen_8k.tif", "koppen_9k.tif"),
                               col.req = col.req,
                               selected.per = c("EN"),
                               title = "Neolithic: transition btw 7,000 and 6,000 BC",
                               legend.show = FALSE)

source("R/neo_kcc_legend.R")
df <- as.data.frame(df_cc)
selected.kcc <- na.omit(unique(unlist(df[, c("koppen_8k", "koppen_9k")]))) 
selected.kcc <- factor(selected.kcc, levels = unique(selected.kcc))
kcc.legend <- neo_kcc_legend(selected.kcc = selected.kcc, 
                             long.legend = TRUE)

lay <- rbind(c(1, 1, 1, 1, 1), 
             c(1, 1, 1, 1, 1), 
             c(1, 1, 1, 1, 1), 
             c(1, 1, 1, 1, 1),
             c(2, 2, 2, 3, 3), 
             c(2, 2, 2, 3, 3),
             c(2, 2, 2, 3, 3))
g <- gridExtra::grid.arrange(isochr.8k$map, plotbar.neo, kcc.legend,
                             layout_matrix = lay,
                             top = paste0("Spread of farming pionner front in 6,500 cal BC")
                             
)
g.out <- paste0(root.path, "/kcc_meso_neo3.png")
ggplot2::ggsave(file = g.out, g, width = 13, height = 13)




