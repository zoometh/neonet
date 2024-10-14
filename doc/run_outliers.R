## Spot outlier dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

# Download the R functions using https://download-directory.github.io/ and this URL https://github.com/zoometh/neonet/tree/main/R

# getwd()
root.path <- "C:/Rprojects/neonet/doc/talks/2024-simep/"
where.roi.path <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/"
where.roi <- paste0(where.roi.path, "roi_cyprus.geojson")
where.roi <- paste0(where.roi.path, "roi-midi-france.geojson") # = Perrin
where.roi <- paste0(where.roi.path, "roi.geojson")
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
df_filtered <- df_filtered[df_filtered$C14SD < 101, ]

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")
# where.roi <- c(30, 30, 45, 40)
where.roi <- c(2, 42, 7, 45)
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset =  "None", #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
                     selected.per = "EN",
                     where = where.roi,
                     kcc.file = "C:/Rprojects/neonet/doc/data/clim/koppen_7k.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/binder_et_al_22_fig11_5600-5450_AEC.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/perrin08_fig16_3_5800_BC.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/guilaine01_arythmic.tif",
                     # kcc.file = "C:/Rprojects/neonet/doc/references/jousse04_bos_domestic_afrique.tif",
                     # is.other.geotiff = TRUE,
                     isochr.line.color = "black", # NA to get colored isochrones (red, blue)
                     isochr.line.size = .5,
                     isochr.txt.size = 0,
                     calibrate = FALSE,
                     size.date = 1.5,
                     alpha.dates = 1,
                     lbl.dates = TRUE,
                     lbl.dates.size = 3,
                     lbl.time.interv = TRUE,
                     create.legend = TRUE)
isochr$map
# ggplot2::ggsave(paste0(root.path, "img/", "aDate-Le Baratin-Ly-4725-map.png"), isochr$map, width = 7, height = 7)
# ggplot2::ggsave(paste0(root.path, "img/", "aDate-Le Baratin-Ly-4725-map-legend.png"), isochr$legend, width = 5, height = 5)

# source("R/neo_spd.R")
# source("R/neo_spdplot.R")
# 
# neo_spd(df.c14 = head(df_filtered, 200), time.span = c(-12000, -6000))
# neo_spd(df.c14 = df_filtered, width = 15, height = 11, outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")




source("R/neo_find_date.R")
source("R/neo_dbs_info_date.R")
abber.date <- neo_find_date(df = isochr$data, print.it = FALSE, idf.dates = 52)
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