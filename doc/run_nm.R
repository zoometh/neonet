
# Download the R functions
# with: https://download-directory.github.io/
# and: https://github.com/zoometh/neonet/tree/main/R


root.path <- "C:/Rprojects/neonet/doc/talks/2024-simep/img/"
where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
# l.dbs <- c("neonet", "calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis") 
# l.dbs <- c("radonb") 
# xronos
# l.dbs <- c("p3k14c")
# l.dbs <- c("bda")
# "neonet" gives a timeout
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read(where.roi,
                     quiet = TRUE)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
samp_df <- read.csv("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep_3.csv")
df.c14 <- samp_df
df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
# unique(xxx$Period)
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
col.req <- gsub(pattern = ".tif", "", kcc.file)

source("R/neo_kcc_extract.R")
df_cc <- neo_kcc_extract(df.c14 = df.c14, kcc.file = kcc.file)

source("R/neo_dbs_rm_date.R")
df_filtered <- neo_dbs_rm_date(df.c14)

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset = -7000,
                     selected.per = "EN",
                     kcc.file = "C:/Rprojects/neonet/doc/data/clim/koppen_7k.tif",
                     time.line.size = .5,
                     calibrate = FALSE,
                     shw.dates = TRUE,
                     lbl.dates = TRUE,
                     lbl.time.interv = TRUE)
isochr$map