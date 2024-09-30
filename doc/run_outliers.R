## Spot outiler dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

# Download the R functions using https://download-directory.github.io/ and this URL https://github.com/zoometh/neonet/tree/main/R

# getwd()

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

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset = c(-5500), # c(-5500, -6000, -6500), # - 5500 TODO
                     selected.per = "EN",
                     # where = where,
                     kcc.file = NA,
                     # kcc.file = "C:/Rprojects/neonet/doc/data/clim/koppen_9k.tif",
                     isochr.line.size = 1,
                     calibrate = FALSE,
                     lbl.dates = TRUE,
                     lbl.dates.size = 2.5,
                     lbl.time.interv = TRUE)
isochr$map


source("R/neo_find_date.R")
source("R/neo_dbs_info_date.R")
abber.date <- neo_find_date(df = isochr$data, idf.dates = 758)
ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)
# Do not add double quotes in the https://github.com/zoometh/neonet/blob/main/inst/extdata/c14_aberrant_dates.tsv file

## Not run
source("R/neo_dbs_info_date_src.R")
ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)
neo_dbs_info_date_src(db = ad$sourcedb,
                      LabCode = ad$LabCode)

# Add outlier dates in this dataframe: https://github.com/zoometh/neonet/blob/main/inst/extdata/c14_aberrant_dates.tsv 