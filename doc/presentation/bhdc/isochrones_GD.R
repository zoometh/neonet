# Read GeoJSON stored in Google Drive and run isochrones

library(googledrive)
library(rcarbon)
source("R/neo_isochr.R")
source("R/neo_spd.R")

drive_find(n_max = 30)
td <- tempdir()
myc14data <- paste0(td, "/output.geojson")
drive_download("2_AOI_France_SE-W.geojson", path = myc14data, overwrite = T)

neo_isochr(df.c14 = myc14data, mapname = "France_SE-W")
neo_isochr(df.c14 = myc14data, mapname = "France_SE-W", selected.per = c("LM"))
