# Browse different DB to collect LM / EN dates

# install.packages("c14bazAAR", repos = c(ropensci = "https://ropensci.r-universe.dev"))
library(c14bazAAR)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sf)
library(rnaturalearth)

source("R/neo_vars.R")

fspat <- function(df_selected, roi, outfile){
  ## distribution spat = map
  df_spat <- st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
  buff <- .5
  world <- ne_countries(scale = "medium", returnclass = "sf")
  distr_spat <- ggplot2::ggplot(world) +
    # TODO: color scale ramp on column 'c14age_uncalBC'
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = df_spat, inherit.aes = FALSE, size = 1) +
    ggplot2::coord_sf(xlim = c(sf::st_bbox(roi)[1] - buff, sf::st_bbox(roi)[3] + buff),
                      ylim = c(sf::st_bbox(roi)[2] - buff, sf::st_bbox(roi)[4] + buff)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 6)) 
  g.out <- paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/", outfile)
  ggsave(file = g.out, distr_spat, width = 14, height = 10)
}

fref <- function(df.all.res, outfile = "df_ref_per.xlsx"){
  # write a reference file used to map external db to periods (class) equal to: ..., LM, EN, ...
  print(unique(df.all.res$sourcedb))
  df.ref.per <- df.all.res[, c("period", "culture")]
  df.ref.per <- df.ref.per[!duplicated(df.ref.per), ]
  openxlsx::write.xlsx(df.ref.per, paste0(root.path, "/", outfile))
  
}

getwd()
source("R/config.R")

## done
# l.dbs <- c("calpal", "medafricarbon", "agrichange", "neonet", "bda", "calpal", "radon", "katsianis")

root.path <-"C:/Rprojects/neonet/results"
where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
what.db <- c("calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis") 
# "neonet" gives a timeout
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read(where.roi,
                     quiet = TRUE)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
source("R/neo_parse_db.R")
df <- neo_parse_dbs(l.dbs = what.db, #what.db, # c("bda", "medafricarbon"),
                    col.c14baz = col.c14baz,
                    chr.interval.uncalBC = when,
                    roi = where)
source("R/neo_align_dbs.R")
df.c14 <- neo_align_dbs(df)

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

#### shortcut: load the 'df14_simep.csv' file
# samp_df <- paste0(root.path, "/df14_simep.csv")
# write.csv(df.c14, samp_df, row.names = FALSE)
# samp_df <- read.csv(samp_df)
# df.c14 <- samp_df
# #############################

df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)

# unique(xxx$Period)
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
df_cc <- neo_kcc_extract(df.c14 = df.c14, kcc.file = kcc.file)
col.req <- gsub(pattern = ".tif", "", kcc.file)
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
# gridExtra::grid.arrange(Meso, Neo, ncol = 1)
source("R/neo_kcc_legend.R")

# to get only existing KCC
selected.kcc <- na.omit(unique(unlist(df[, col.req]))) 
selected.kcc <- factor(selected.kcc, levels = unique(selected.kcc))
kcc.legend <- neo_kcc_legend(selected.kcc = selected.kcc, 
                             long.legend = TRUE)

# map
source("R/neo_map.R")
g.neo.map <- neo_map(df.c14 = df.c14, 
                     # plot.dates = TRUE,
                     ref.spat = where.roi, 
                     buff = 0,
                     title = "ROI")

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
