# library(c14bazAAR)
# library(dplyr)
# library(ggplot2)
# library(grid)
# library(gridExtra)
# library(sf)
# library(rnaturalearth)

source("R/config.R")

## done
# l.dbs <- c("calpal", "medafricarbon", "agrichange", "neonet", "bda", "calpal", "radon", "katsianis")

root.path <- "C:/Rprojects/neonet/doc/talks/2024-simep/img/"
where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
# where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi_cyprus.geojson"

l.dbs <- c("neonet", "calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis") 
# l.dbs <- c("radonb") 
# xronos
# l.dbs <- c("p3k14c")
# l.dbs <- c("bda")
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read(where.roi,
                     quiet = TRUE)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")

## Read databases
source("R/neo_dbs_parse.R")
df <- neo_dbs_parse(l.dbs = l.dbs, #what.db, # c("bda", "medafricarbon"),
                    col.c14baz = col.c14baz,
                    # chr.interval.uncalBC = when,
                    roi = where)
source("R/neo_dbs_align.R")
df.c14 <- neo_dbs_align(df = df,
                        mapping.file = "C:/Rprojects/neonet/doc/ref_table_per_NM.xlsx")
# dbs
# DB not done: kiteeastafrica, nerd, aida,  (no culture)
# DB done: calpal, medafricarbon, agrichange, neonet, bda, calpal, radon, katsianis
# l.dbs <- c("bda", "medafricarbon")
# df <- neo_parse_db(l.dbs, chr.interval.uncalBC, present, roi)
# fspat(df.all.res, roi, outfile = "_db__all_class.png")
# fspat(df.all.res, roi, outfile = "_db__all.png")
# colnames(df.c14)

# write.csv(df.c14, samp_df, row.names = FALSE)
# 
# # to.plot <- df.c14[df.c14$sourcedb %in% c("neonet", "neonetatl"), ]
# to.plot <- df.c14[df.c14$Period %in% c("EN", "MN", "LN"), ]
# 
# 
# library(rcarbon)


######################## shortcut: load the 'df14_simep.csv' file
# df.c14 <- read.csv("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep_4.csv")
# ----------------------------------------------------------------

# df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
col.req <- gsub(pattern = ".tif", "", kcc.file)
where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi_cyprus.geojson"
# where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi-middle-east.geojson"
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read(where.roi,
                     quiet = TRUE)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
samp_df <- read.csv("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep_4.csv")
df.c14 <- samp_df
# correct coordinates (# Sabha)
source("R/neo_dbs_coord_dates.R")
df.c14 <- neo_dbs_coord_dates(df.c14, verbose = FALSE)

df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)

# remove aberrant dates listed in 'c14_aberrant_dates.tsv'
source("R/neo_dbs_rm_date.R")
df_filtered <- neo_dbs_rm_date(df.c14 = df.c14,
                               c14.to.remove = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_aberrant_dates.tsv")
# remove dates having a C14SD superior to..
df_filtered <- df_filtered[df_filtered$C14SD < 101, ]


source("R/neo_kcc_extract.R")
df_cc <- neo_kcc_extract(df.c14 = df_filtered, kcc.file = kcc.file)

# CA
source("R/neo_kcc_ca.R")
cas <- neo_kcc_ca(df_cc = df_cc, 
                  col.req = col.req)
g <- gridExtra::grid.arrange(grobs = cas, 
                             ncol = length(cas), 
                             # widths = c(10,10)
                             )
g.out <- "C:/Rprojects/neonet/doc/talks/2024-simep/img/ca.png"
ggsave(file = g.out, g, width=20, height=8)
# View(df_filtered[df_filtered$Period == "MN", ])


# KCC
source("R/neo_kcc_plotbar.R")
Meso <- neo_kcc_plotbar(df_cc = df_cc, 
                        col.req = col.req,
                        selected.per = c("LM"),
                        counts.show.size = 3,
                        title = "Late Mesolithic")
Meso
# ggplot2::ggsave(file = "C:/Rprojects/neonet/doc/talks/2024-simep/img/kcc_meso_lm-1.png", Meso, width = 10, height = 6)
Neo <- neo_kcc_plotbar(df_cc = df_cc, 
                       col.req = col.req,
                       selected.per = c("EN"),
                       counts.show.size = 3,
                       title = "Early Neolithic")
Neo
# ggplot2::ggsave(file = "C:/Rprojects/neonet/doc/talks/2024-simep/img/kcc_neo_en-.png", Neo, width = 10, height = 6)
source("R/neo_kcc_legend.R")
kcc.legend <- neo_kcc_legend(df_cc = df_cc, 
                             long.legend = TRUE)
kcc.legend
## Not Run / convert tableGrob to ggplot / TODO: add in neo_kcc_legend()
# # Create an empty ggplot object
# plot <- ggplot2::ggplot() +
#   ggplot2::geom_blank(ggplot2::aes(1, 1)) +
#   ggplot2::theme_void()  # Use theme_void() to create an empty canvas
# final_plot <- plot +
#   ggplot2::annotation_custom(
#     grob = kcc.legend, 
#     xmin = -Inf, xmax = Inf, 
#     ymin = -Inf, ymax = Inf
#   )
# print(final_plot)
# ggplot2::ggsave(file = "C:/Rprojects/neonet/doc/talks/2024-simep/img/kcc_meso_neo_legend.png", final_plot, width = 8, height = 10)

# map
source("R/neo_map.R")
g.neo.map <- neo_map(df.c14 = df_cc, 
                     plot.dates = TRUE,
                     roi = where, 
                     selected.per = c("LM", "EN"),
                     dates.within.roi = TRUE,
                     buff = 2.5,
                     title = "ROI")
# layout & export
lay <- rbind(c(1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 3, 3, 3), 
             c(2, 2, 2, 2, 2, 4, 4, 4),
             c(2, 2, 2, 2, 2, 4, 4, 4), 
             c(2, 2, 2, 2, 2, 4, 4, 4))
g <- gridExtra::grid.arrange(Meso, Neo, kcc.legend, g.neo.map,
                             # top = paste0("Distribution of radiocarbon dates ", 
                             #              "by Koppen classes in the ROI"),
                             layout_matrix = lay
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
# df_cc.meso <- df_cc[df_cc$Period %in% c("LM"), ]
# # df_cc.neo <- head(df_cc.neo, 50)
neo_spd(df.c14 = df_cc[df_cc$Period %in% c("EN"), ],
        title = "Early Neolithic",
        # time.round = 1000,
        time.span = c(13000, 6000),
        calendar = 'BP',
        # shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
        x.intercept = 11000,
        color.on = "kcc",
        export = TRUE,
        outFile = "EN_kcc_11k",
        outDir = root.path,
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

# frm <- function(df.c14 = NA, 
#                 c14.to.remove = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_to_remove2.tsv",
#                 selected.cols = c("sourcedb", "LabCode", "SiteName", "median", "db_period"),
#                 verbose = TRUE){
#   # Remove unaccurate dates (optional)
#   # escape the dates having a "-" as a prefix in their database (i.e. dates with lack of arguments to remove them)
#   df.to.rm <- read.table(c14.to.remove, sep = "\t", header = TRUE)
#   df.to.rm <- df.to.rm[!grepl("^-", df.to.rm$sourcedb), ]
#   if(verbose){
#     print(paste0(nrow(df.to.rm), " dates to be removed:"))
#     print(df.to.rm[ , selected.cols])
#   }
#   df <- dplyr::anti_join(df.c14, df.to.rm, 
#                          by = c("sourcedb", "LabCode"))
#   return(df)
# }


# df_filtered <- frm(df.c14)

# fdate <- function(LabCode = NA, 
#                   columns = c("sourcedb", "LabCode", "SiteName", "median", "db_period", "db_culture")){
#   # return info on a date from its LabCode
#   if(inherits(df.c14, "sf")){
#     a.date <- as.character(na.omit(
#       sf::st_set_geometry(df.c14[df.c14$LabCode == LabCode, columns],
#                           NULL)
#     )[1,])
#   }
#   if(is.data.frame(df.c14)){
#     a.date <- as.character(na.omit(
#       df.c14[df.c14$LabCode == LabCode, columns]
#     )[1,])
#   }
#   cat(paste(a.date, collapse = "\t"), "\n")
# }

# fget.db <- function(db = NA, LabCode = NA){
#   # Once the LabCode's database origin is sourced, print this LabCode infos
#   df <- c14bazAAR::get_c14data(db)
#   df <- as.data.frame(df[df$labnr == LabCode, ])
#   print(df)
# }






# df.c14[df.c14$db_period == "Džh 1", ]




# map.iso <- neo_isochr(df.c14 = df_filtered, 
#                       time.line.size = .5,
#                       calibrate = FALSE,
#                       shw.dates = TRUE,
#                       lbl.dates = TRUE,
#                       lbl.time.interv = FALSE)

########## Pioneer front ###########

source("R/neo_dbs_rm_date.R")
df_filtered <- neo_dbs_rm_date(df.c14)

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset = -7000,
                     # isochr.subset = c(-8000, -7500),
                     # where = where,
                     selected.per = "EN",
                     # kcc.file = "C:/Rprojects/neonet/doc/data/clim/koppen_11k.tif",
                     # kcc.file = NA, # "C:/Rprojects/neonet/doc/data/clim/koppen_10k.tif",
                     isochr.line.size = .5,
                     calibrate = FALSE,
                     shw.dates = TRUE,
                     lbl.dates = FALSE,
                     lbl.dates.size = 2,
                     lbl.time.interv = TRUE,
                     lbl.time.interv.size = 12)
isochr$map

## To save
ggplot2::ggsave(paste0(root.path, "EN_kcc_8k-iso-not.png"), isochr$map, width = 9, height = 6)
# ggplot2::ggsave(paste0(root.path, "EN_kcc_8k-iso-not.png"), isochr$map, width = 14, height = 10)

source("R/neo_find_date.R")
source("R/neo_dbs_info_date.R")
source("R/neo_dbs_info_date_src.R")

abber.date <- neo_find_date(df = isochr$data, idf.dates = 220)
abber.date <- neo_dbs_info_date(abber.date$labcode)
neo_dbs_info_date_src(db = abber.date$sourcedb, 
                      LabCode = abber.date$LabCode)

# ## check abberant dates
# source("R/neo_find_dates.R")
# abber.dates <- neo_find_dates(df = isochr$data, idf.dates = c(295))
# # LabCode = "UtC-1830"
# fdate(LabCode = abber.dates$labcode)
# abber.dates
# fget.db(db = dates$sourcedb, LabCode = abber.dates$labcode)

source("R/neo_kcc_plotbar.R")
plotbar.neo <- neo_kcc_plotbar(df_cc = df_cc, 
                               kcc.file = c("koppen_8k.tif", "koppen_9k.tif"),
                               col.req = col.req,
                               selected.per = c("EN"),
                               counts.show.size = 5,
                               title = "Neolithic: transition btw 7,000 and 6,000 BC",
                               legend.show = FALSE)
ggplot2::ggsave(paste0(root.path, "EN_kcc_stacked.png"), plotbar.neo, 
                width = 14, height = 10)


source("R/neo_dbs_info_date_count.R")
dbs.counts <- neo_dbs_info_date_count(df_filtered, where)
ggplot2::ggsave(paste0(root.path, "dbs_counts.png"), dbs.counts, 
                width = 14, height = 10)

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


## SPD

source("R/neo_spd.R")
source("R/neo_spdplot.R")
neo_spd(df.c14 = df.c14[c(1), ])

############# Plot one date ####

f3per <- function(df.c14 = NA){
  # find sites having different periods represented, for example "EN", "MN", "LN", to illustrate the different moments (respectively: early farmers, long-distance trade, copper industry) or "EM", "MM", "LM". For example "Baume de Montclus" has all the Mesolithic represented, while "Franchthi Cave" has all the Neolithic represented
  site.by.per <- df.c14 %>%
    dplyr::group_by(SiteName, Period) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = 'drop')
  site.by.per.count <- as.data.frame(table(site.by.per$SiteName))
  
  return(site.by.per.count)
  # to see the period, run, for example:
  # site.by.per[site.by.per$SiteName == "Franchthi Cave", ]
}

source("R/neo_calib.R")
source("R/neo_spd.R")
source("R/neo_spdplot.R")

site.by.per.count <- f3per(df.c14)
# selelct 1 site
# View(site.by.per.count)
# df.temp <- site.by.per[site.by.per$SiteName == "Baume de Montclus", ]
# df.temp.1 <- df.c14[df.c14$SiteName == "Baume de Montclus" & df.c14$Period %in% c("EM", "MM", "LM"), ]
df.temp <- site.by.per[site.by.per$SiteName == "Franchthi Cave", ]
df.temp.1 <- df.c14[df.c14$SiteName == "Franchthi Cave" & df.c14$Period %in% c("EN", "MN", "LN"), ]
df.temp.1 <- df.temp.1[df.temp.1$LabCode != "P-1921", ]
df.temp.1 <- df.temp.1[!is.na(df.temp.1$LabCode), ]
df.temp.1 <- neo_calib(df.temp.1, 
                       verbose.freq = 5)
df.temp.2 <- df.temp.1[4, ]

source("R/neo_spd.R")
source("R/neo_spdplot.R")
neo_spd(df.c14 = df.temp.2, outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/", 
        # outFile = "moments-dbs-neo-Franchti.png",
        # outFile = "moments-dbs-meso-Montclus.png",
        outFile = paste0("aDate-", df.temp.2$SiteName, "-", df.temp.2$LabCode, ".png"),
        # time.span = c(10000, 6500),
        time.span = c(8900, 8000),
        export = TRUE,
        title = paste(df.temp.2$SiteName, "-", df.temp.2$LabCode),
        show.median = TRUE,
        width = 14, height = 11)
# remotes::install_github("people3k/p3k14c@2022.06")

source("R/neo_kcc_sankey.R")
gout <- neo_kcc_sankey(df_cc, col.req)
ggsave(filename = paste0(outDir, period.names, "_kcc.png"),
       gout,
       width = 16, height = 12)

source("R/neo_kcc_map.R")
# kcc.per <- "koppen_9k"
kcc.file <- c("koppen_6k", "koppen_7k", "koppen_8k",
              "koppen_9k", "koppen_10k", "koppen_11k")
for(kcc.per in kcc.file){
  print(kcc.per)
  df_cc_per <- df_cc[!is.na(df_cc[[kcc.per]]), ]
  kcc <- paste0("C:/Rprojects/neonet/doc/data/clim/", kcc.per, ".tif")
  gout <- neo_kcc_map(kcc = kcc,
                      df.c14 = df_cc_per,
                      roi = where,
                      pt.size = .3,
                      sys.proj = 32633)
  ggplot2::ggsave(paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/", kcc.per, "_.png"),
                  gout,
                  width = 15,
                  height = 10,
                  units = "cm"
  )
}

############## ## Study ####
# bar plot + map by period and ky

source("R/neo_kcc_plotbar.R")
source("R/neo_kcc_map.R")
per <- "EN"
df_cc_per <- df_cc[!is.na(df_cc[["koppen_9k"]]), ]
df_cc_per <- df_cc_per[df_cc_per[["Period"]] == per, ]
neo_kcc_plotbar(df_cc = df_cc, 
                col.req = col.req,
                selected.per = per,
                counts.show.size = 3,
                title = "Early Neolithic")
gout <- neo_kcc_map(kcc = kcc,
                    df.c14 = df_cc_per,
                    roi = where,
                    pt.size = 1,
                    lbl.dates = TRUE,
                    sys.proj = 32633)
ggplot2::ggsave(paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/", kcc.per, "_", per, ".png"),
                gout,
                width = 15,
                height = 10,
                units = "cm"
)