## Spot outlier dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

# Download the R functions using https://download-directory.github.io/ and this URL https://github.com/zoometh/neonet/tree/main/R

root.path <- "C:/Rprojects/neonet/doc/talks/2024-asd/"
present <- 1950
when <- c(-9000, -4000)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
samp_df <- read.csv("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep_4.csv")
df.c14 <- samp_df

# third part dataset
if(dbs_3rdpart){
  # Brami15 
  source("R/neo_dbs_3rdpart_parse.R") # by default 
  df.brami  <- neo_dbs_3rdpart_parse() 
  # following 'Cyclops Cave'
  # subset(df.brami[,c(1:10)], SiteName == 'Cyclops Cave' & sourcedb == 'brami15')
  # View(df.brami)
}

df.c14 <- rbind(df.brami, samp_df)
# subset(df.14[,c(1:10)], sourcedb == 'brami15')
# subset(df.c14[,c(1:10)], SiteName == 'Cyclops Cave' & sourcedb == 'brami15')

# source("R/neo_dbs_info_dates_datatable.R")
# df.datatable <- neo_dbs_info_dates_datatable(df.c14) ; htmlwidgets::saveWidget(df.datatable, paste0("temp_filtered1", ".html"))

# correct sitenames
source("R/neo_dbs_sitename_dates.R")
df.c14 <- neo_dbs_sitename_dates(df.c14)
# correct labcodes
source("R/neo_dbs_labcode_dates.R")
df.c14 <- neo_dbs_labcode_dates(df.c14)
# remove duplicates
source("R/neo_dbs_rm_duplicated_dates.R")
df.c14 <- neo_dbs_rm_duplicated_dates(df.c14)
# correct coordinates (# Sabha)
source("R/neo_dbs_coord_dates.R")
df.c14 <- neo_dbs_coord_dates(df.c14)
# remove aberrant dates listed in 'c14_aberrant_dates.tsv'
source("R/neo_dbs_rm_date.R")
df_filtered <- neo_dbs_rm_date(df.c14 = df.c14,
                               c14.to.remove = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_aberrant_dates.tsv")

# subset(df.c14[,c(1:10)], SiteName == 'Cyclops Cave' & sourcedb == 'brami15')


# to sf
# df_filtered <- sf::st_as_sf(df_filtered, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
df_filtered <- sf::st_as_sf(df_filtered, coords = c("lon", "lat"), crs = 4326) # when GDAL/proj.db will be reinstalled



# isochrones
source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
WestMed3 <- c(-10, 30, 20, 45)
EastMed2.where <- c(24, 25, 45, 40) 
EastMed3.where <- c(10, 25, 45, 45)
source("R/neo_kcc_legend.R")
source("R/neo_dbs_info_dates_datatable.R")
### Choice, # !! some study area overlap different KCC
## title ; when (isochrones BC) ; where (xmin, ymin, xmax, ymax) use: https://geojson.io/#map=2/0/20 ;
obj.case <- list("Near East", c(-9000, -8000), c(32, 30, 45, 40), "koppen_11k.tif") # !!
obj.case <- list("Cyprus", c(-8000), c(30, 30, 40, 40),  "koppen_10k.tif")
obj.case <- list("Egee", c(-6600), c(18, 35, 34, 42), "koppen_9k.tif") # anct 
obj.case <- list("Egee", c(-6200), c(18, 35, 34, 42), "koppen_8k.tif") 
obj.case <- list("Egee", c(-5800), c(18, 35, 34, 42), "koppen_8k.tif") 
obj.case <- list("Balkans", c(-5800), c(18, 35, 30, 43), "koppen_8k.tif")
obj.case <- list("Italia", c(-5700, -5500, -5300), c(5, 37, 18, 48), "koppen_8k.tif")  # !!
obj.case <- list("Le Baratin", c(-5700, -5500), c(2, 42, 7, 45), "koppen_8k.tif") # !!
obj.case <- list("Mediterranean", c(-9000, -8000, -7000, -6000, -5000), c(-12, 27.5, 42.5, 47.5), "koppen_8k.tif") # !!
## by authors - - - - - - - - - - - - - - - - - - - - - - - -
obj.case <- list("Binder", c(-5500), c(0.07180157, 31.8513415, 23.73331373, 48.0342267), "koppen_8k.tif")
# kcc.file = "C:/Rprojects/neonet/doc/references/binder_et_al_22_fig11_5600-5450_AEC.tif",
obj.case <- list("Perrin", c(-5500), c(1.02768233, 40.83697718, 11.40383725, 46.44486459), "koppen_8k.tif")
# kcc.file = "C:/Rprojects/neonet/doc/references/perrin08_fig16_3_5800_BC.tif",
obj.case <- list("Brami", c(-6500), c(18, 35, 35, 43), "koppen_9k.tif")
# kcc.file = "C:/Rprojects/neonet/doc/references/perrin08_fig16_3_5800_BC.tif",
#################################################################
obj.case.name <- paste0("isochr", paste0(obj.case[[2]], "-", paste0("BC-", obj.case[[1]], collapse = "-")), "-", gsub(".tif", "", obj.case[[4]]))
obj.case.out <- paste0(root.path, "img/", obj.case.name)
kcc.file.path <- paste0("C:/Rprojects/neonet/doc/data/clim/", obj.case[[4]])
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset =  obj.case[[2]], #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
                     selected.per = "EN",
                     max.sd = 101,
                     where = obj.case[[3]], #Italia.where, # where.roi,
                     kcc.file = kcc.file.path, # NA, 
                     is.other.geotiff = FALSE,
                     create.legend = TRUE,
                     isochr.line.color = NA, # "black", # NA to get colored isochrones (red, blue)
                     isochr.line.size = .5,
                     isochr.txt.size = 0,
                     calibrate = FALSE,
                     shw.dates = TRUE,
                     # show.all.dates = FALSE,
                     size.date = 1,
                     # color.dates = "darkgrey",
                     alpha.dates = 1,
                     lbl.dates = TRUE,
                     # lbl.all.dates = FALSE,
                     # lbl.date.field = "median",
                     lbl.dates.size = 5,
                     lbl.time.interv = TRUE)
isochr$map
ggplot2::ggsave(paste0(obj.case.out, ".png"), isochr$map, width = 12, height = 10)
ggplot2::ggsave(paste0(obj.case.out, "-legend.png"), isochr$legend, width = 5)
write.table(isochr$data, paste0(obj.case.out, ".tsv"), sep = "\t", row.names = FALSE)
# df.datatable <- neo_dbs_info_dates_datatable(df.c14 = isochr$data) ; htmlwidgets::saveWidget(df.datatable, paste0(obj.case.out, ".html"))
# write.table(isochr$data, paste0(obj.case.out, ".tsv"), sep = "\t", row.names = FALSE)
# openxlsx::write.xlsx(x = isochr$data, paste0(obj.case.out, ".xlsx"))
# df <- isochr$data[ , c("idf","site", "period", "median", "code", "lon", "lat", "sourcedb")]
# write.table(df, paste0(root.path, "img/", "isochrones-barriere-Italy-EN-kcc.tsv"), sep = "\t", row.names = FALSE)
# ggplot2::ggsave(paste0(obj.case.out, ".png"), isochr$map, width = 14)
# ggplot2::ggsave(paste0(obj.case.out, "-legend.png"), isochr$legend, width = 5, height = 7)

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
abber.date <- neo_find_date(df = isochr$data, print.it = FALSE, idf.dates = 129)
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


##