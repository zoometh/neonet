## Spot outlier dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

# Download the R functions using https://download-directory.github.io/ and this URL https://github.com/zoometh/neonet/tree/main/R

# getwd()
root.path <- "C:/Rprojects/neonet/doc/talks/2024-simep/"
where.roi.path <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/"
where.roi <- paste0(where.roi.path, "roi_cyprus.geojson")
where.roi <- paste0(where.roi.path, "roi-midi-france.geojson") # = Perrin
where.roi <- paste0(where.roi.path, "roi-med-cw.geojson") # = Binder
# where <- sf::st_read(where.roi,
#                      quiet = TRUE)

# where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi-middle-east.geojson"
present <- 1950
when <- c(-9000, -4000)
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
# df_filtered <- df_filtered[df_filtered$C14SD < 101, ] # embedded into neo_isochr()

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
where.roi <- c(20, 30, 45, 40)
# where.roi <- c(2, 42, 7, 45) # Le Baratin
where.roi <- c(20, 30, 35, 42) # 6200 BC = 8.2 ky event
where.roi <- c(32, 30, 45, 40) # Near East
where.roi <- c(24, 25, 45, 40) # East Med
where.roi <- c(20, 25, 45, 40) # East Med2
where.roi <- c(10, 25, 45, 45) # East Med3
# where.roi <- c(-10, 30, 20, 45) # West Med3
# where.roi <- paste0(where.roi.path, "roi.geojson")
# where.roi <- paste0(where.roi.path, "roi_cyprus.geojson")
Balkans.where <- c(18, 35, 30, 43) ; Balkans.when <- c(-6200, -5800)
Italia.where <- c(5, 37, 18, 48) ; Italia.when <- c(-5600, -5400) # c(-5500, -5250, -5000)
Mediterranean.where <- paste0(where.roi.path, "roi.geojson")
source("R/neo_isochr.R")
source("R/neo_kcc_legend.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset =  c(-5100), #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
                     selected.per = "EN",
                     max.sd = 101,
                     where = Mediterranean.where, #Italia.where, # where.roi,
                     # kcc.file = NA,
                     kcc.file = "C:/Rprojects/neonet/doc/data/clim/koppen_7k.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/binder_et_al_22_fig11_5600-5450_AEC.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/perrin08_fig16_3_5800_BC.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/guilaine01_arythmic.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/jousse04_bos_domestic_afrique.tif",
                     is.other.geotiff = FALSE,
                     create.legend = FALSE,
                     isochr.line.color = NA, # "black", # NA to get colored isochrones (red, blue)
                     isochr.line.size = .5,
                     isochr.txt.size = 0,
                     calibrate = FALSE,
                     shw.dates = TRUE,
                     # show.all.dates = FALSE,
                     size.date = 1.5,
                     alpha.dates = .5,
                     lbl.dates = FALSE,
                     # lbl.all.dates = FALSE,
                     # lbl.date.field = "median",
                     lbl.dates.size = 3,
                     lbl.time.interv = TRUE)
isochr$map
isochr$data

source("R/neo_isochr_inter_map.R")
inter.map <- neo_isochr_inter_map(isochr$inter)
inter.map
ggplot2::ggsave(paste0(root.path, "img/", "isochrones-barriere-Italy-EN-inter-map.png"), inter.map, width = 7, height = 7)


# View(isochr$data)
ggplot2::ggsave(paste0(root.path, "img/", "isochrones-5100BC-EN-kcc.png"), isochr$map, width = 12, height = 7)
ggplot2::ggsave(paste0(root.path, "img/", "isochrones-barriere-Italy-EN-kcc-legend_1.png"), isochr$legend, width = 5, height = 5)
openxlsx::write.xlsx(x = isochr$data, paste0(root.path, "img/", "isochrones-barriere-Italy-EN-kcc_1.xlsx"))

# export data in TSV
df <- isochr$data[ , c("idf","site", "period", "median", "code", "lon", "lat", "sourcedb")]
write.table(df, paste0(root.path, "img/", "isochrones-barriere-Italy-EN-kcc.tsv"), sep = "\t", row.names = FALSE)

# source("R/neo_spd.R")
# source("R/neo_spdplot.R")
# 
# neo_spd(df.c14 = head(df_filtered, 200), time.span = c(-12000, -6000))
# neo_spd(df.c14 = df_filtered, width = 15, height = 11, outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")

# Create a stacked barplot of climates from sites

kcc_colors <- "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv"
kcc_colors <- read.csv(kcc_colors, sep = "\t")
kcc_colors <- kcc_colors[ , c("code", "color")]
isochr.1 <- merge(isochr$data, kcc_colors, by = "code", all.x = TRUE)
isochr.1 <- isochr.1[ , c("idf", "site", "median", "code", "color")]
isochr.1$all <- "All"
ggplot2::ggplot(isochr.1, ggplot2::aes(x = all, fill = code)) +
  ggplot2::geom_bar() +
  ggplot2::scale_fill_manual(values = unique(isochr.1$color[order(isochr.1$code)])) +
  ggplot2::theme_minimal()
# names(isochr$data)[names(isochr$data) == 'longitude'] <- 'lon'
# names(isochr$data)[names(isochr$data) == 'latitude'] <- 'lat'
# data.df <- sf::st_as_sf(isochr$data, coords = c("lon", "lat"), crs = 4326)
# kcc_geo <- terra::rast("C:/Rprojects/neonet/doc/data/clim/koppen_11k.tif")
# kcc.list <- terra::extract(kcc_geo, data.df)


source("R/neo_find_date.R")
source("R/neo_dbs_info_date.R")
abber.date <- neo_find_date(df = isochr$data, print.it = FALSE, idf.dates = 76)
ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)
# Do not add double quotes in the https://github.com/zoometh/neonet/blob/main/inst/extdata/c14_aberrant_dates.tsv file

## Not run
source("R/neo_dbs_info_date_src.R")
ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)
neo_dbs_info_date_src(db = ad$sourcedb,
                      LabCode = ad$LabCode)

# ## Not run
# source("R/neo_dbs_info_date_src.R")
# ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)
# a.db <- neo_dbs_info_date_src(db = "bda", print.res = FALSE)

# Add outlier dates in this dataframe: https://github.com/zoometh/neonet/blob/main/inst/extdata/c14_aberrant_dates.tsv 