## Spot outiler dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

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

# Download the R functions using https://download-directory.github.io/
# and this URL https://github.com/zoometh/neonet/tree/main/R

getwd()

where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
present <- 1950
when <- c(-9000, -4000)
where <- sf::st_read(where.roi,
                     quiet = TRUE)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
samp_df <- read.csv("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep_4.csv")
df.c14 <- samp_df
df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)

source("R/neo_dbs_rm_date.R")
df_filtered <- neo_dbs_rm_date(df.c14 = df.c14,
                               c14.to.remove = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_aberrant_dates.tsv")

source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset = -6500,
                     selected.per = "EN",
                     kcc.file = NA, 
                     isochr.line.size = 1,
                     calibrate = FALSE,
                     shw.dates = TRUE,
                     lbl.dates = TRUE,
                     lbl.dates.size = 2.5,
                     lbl.time.interv = TRUE)
isochr$map

source("R/neo_find_date.R")
source("R/neo_dbs_info_date.R")
source("R/neo_dbs_info_date_src.R")
abber.date <- neo_find_date(df = isochr$data, idf.dates = 345)
ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)

## Not run
ad <- neo_dbs_info_date(df.c14 = df.c14, LabCode = abber.date$labcode)
neo_dbs_info_date_src(db = ad$sourcedb,
                      LabCode = ad$LabCode)

# Add outlier dates in this dataframe: https://github.com/zoometh/neonet/blob/main/inst/extdata/c14_aberrant_dates.tsv 