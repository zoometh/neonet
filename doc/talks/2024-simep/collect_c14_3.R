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
source("R/neo_vars.R")

## done
# l.dbs <- c("calpal", "medafricarbon", "agrichange", "neonet", "bda", "calpal", "radon", "katsianis")

root.path <-"C:/Rprojects/neonet/results"
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson",
                     quiet = TRUE)
df <- neo_parse_db(l.dbs = c("bda", "medafricarbon"),
                   col.c14baz = c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat"),
                   chr.interval.uncalBC = when,
                   roi = where)
# dbs
# DB not done: kiteeastafrica, nerd, aida,  (no culture)
# DB done: calpal, medafricarbon, agrichange, neonet, bda, calpal, radon, katsianis
# l.dbs <- c("bda", "medafricarbon")
# df <- neo_parse_db(l.dbs, chr.interval.uncalBC, present, roi)
# fspat(df.all.res, roi, outfile = "_db__all_class.png")
# fspat(df.all.res, roi, outfile = "_db__all.png")
df.c14 <- neo_map_dbs(df)
# colnames(df.c14)
df.c14 <- neo_calib(df.c14)
df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)

# ...
xxx <- df.c14 %>% 
  sample_n(250)
# unique(xxx$Period)
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
df_cc <- neo_kcc_extract(df.c14 = df.c14, kcc.file = kcc.file)
col.req <- gsub(pattern = ".tif", "", kcc.file)
neo_kcc_plotbar(df_cc, 
                col.req = col.req,
                selected.per = c("LM", "MM"),
                outDir = "C:/Rprojects/neonet/results/")
# neo_kcc_sankey(df_cc, col.req = col.req, 
#                selected.per = c("EN", "MM"), 
#                outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")
# df <- read.csv(paste0(root.path, "/medians.csv"))
