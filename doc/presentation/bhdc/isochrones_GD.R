# Read GeoJSON stored in Google Drive and run isochrones

library(googledrive)
library(rcarbon)
source("R/neo_isochr.R")
source("R/neo_spd.R")

drive_find(n_max = 30)

drive_ls(path = "BHDC")
td <- tempdir()
myc14data <- paste0(td, "/output.geojson")
AOI_France_E_W <- drive_download("1_AOI_France_E-W.geojson", path = myc14data, overwrite = T)

myc14data <- "https://raw.githubusercontent.com/zoometh/neonet/main/results/1_AOI_France_E-W.geojson"
neo_isochr(df.c14 = myc14data, mapname = "France_EW")
neo_isochr(df.c14 = myc14data, mapname = "France_EW", selected.per = c("LM"))

# example: C:\Users\Thomas Huet\AppData\Local\Temp\RtmpOq0fVQ


source("R/neo_subset.R")
source("R/neo_bib.R")
source("R/neo_matlife.R")
source("R/neo_calib.R")
source("R/neo_merge.R")
source("R/neo_html.R")
source("R/neo_datamiss.R")
source("R/neo_datasum.R")
source("R/neo_doi.R")



neo_isochr(df.c14 = "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-24.geojson", 
           show.lbl = FALSE)