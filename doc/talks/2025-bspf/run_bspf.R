## Spot outlier dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

# Download the R functions using https://download-directory.github.io/ and this URL https://github.com/zoometh/neonet/tree/main/R

# root.path <- "C:/Rprojects/neonet/doc/talks/2025-bspf/"
root <- "C:/Users/TH282424/Rprojects/neonet/"
root.path <- paste0(root, "doc/talks/2025-bspf/")
present <- 1950
when <- c(-9000, -3000)
col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
data.on.gh <- FALSE
if(data.on.gh){
  data.url <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/df14_simep_4.csv"} else{
    data.url <- paste0(root, "doc/talks/2024-simep/df14_simep_4.csv")
  }
samp_df <- read.csv(data.url)
df.c14 <- samp_df

dbs_3rdpart <- FALSE
# third part dataset
if(dbs_3rdpart){
  # Brami15 
  source("R/neo_dbs_3rdpart_parse.R") # by default 
  df.brami  <- neo_dbs_3rdpart_parse() 
  # following 'Cyclops Cave'
  # subset(df.brami[,c(1:10)], SiteName == 'Cyclops Cave' & sourcedb == 'brami15')
  # View(df.brami)
  df.c14 <- rbind(df.brami, samp_df)
}


# subset(df.c14[,c(1:10)], sourcedb == 'brami15')
# subset(df.c14[,c(1:10)], SiteName == 'Cyclops Cave' & sourcedb == 'brami15')

# source("R/neo_dbs_info_dates_datatable.R")
# df.datatable <- neo_dbs_info_dates_datatable(df.c14) ; htmlwidgets::saveWidget(df.datatable, paste0("temp_filtered1", ".html"))

# reference files
source("R/config.R")
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

# to sf
# subset(df_filtered[,c(1:10)], is.na(df_filtered$lon))
df_filtered <- sf::st_as_sf(df_filtered, coords = c("lon", "lat"), crs = 4326) # when GDAL/proj.db will be reinstalled

# isochrones
source("R/config.R")
source("R/neo_spd.R")
source("R/neo_calib.R")
source("R/neo_kcc_legend.R")
source("R/neo_dbs_info_dates_datatable.R")
### Choice, # !! some study area overlap different KCC
## title ; when (isochrones BC) ; where (xmin, ymin, xmax, ymax) use: https://geojson.io/#map=2/0/20 ;
my_list <- list(
  # EN
  list("MedWest", c(-5700), c(-10, 35, 19, 45), "koppen_8k.tif", "EN")
  # list("MedWestEst", c(-6000), c(-10, 35, 25, 45), "koppen_8k.tif", "EN")
)
i <- 1
obj.case.name <- paste0("isochr-", my_list[[i]][[5]], paste0(my_list[[i]][[2]], "-", paste0("BC-", my_list[[i]][[1]], collapse = "-")), "-", gsub(".tif", "", my_list[[i]][[4]]))
obj.case.out <- paste0(root.path, "img/")
kcc.file.path <- paste0(root, "doc/data/clim/", my_list[[i]][[4]])
# to reduce the number of displayed dates
lbl.dates.interval <- c(my_list[[i]][[2]], my_list[[i]][[2]]-99)
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset =  my_list[[i]][[2]], #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
                     isochr.subset.sup = my_list[[i]][[2]] - 100,
                     selected.per = my_list[[i]][[5]],
                     # largest.isochr = TRUE,
                     where = my_list[[i]][[3]], #Italia.where, # where.roi,
                     buff = 0,
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
                     segment.linetype = 1,
                     force = 2,
                     min.segment.length = 0,
                     lbl.dates.interval = lbl.dates.interval, # subset dates to be labeled
                     # lbl.all.dates = FALSE,
                     # lbl.date.field = "median",
                     lbl.dates.size = 4,
                     lbl.time.interv = TRUE)
isochr$map
ggplot2::ggsave("C:/Users/TH282424/Rprojects/neonet/doc/talks/2025-bspf/img/_temp_map.png", isochr$map, width = 10, height = 6)

# INterpolation map
source("R/neo_isochr_inter_map.R")
inter.map <- neo_isochr_inter_map(isochr$inter, title = "Interpolation des w-médianes des dates radiocarbones")
inter.map
ggplot2::ggsave(paste0(root.path, "img/", "_interpol_isochrones.png"), 
                inter.map, 
                width = 16, 
                height = 12, 
                dpi = 300,
                units = "cm")

# Sankey Diagram
source("R/neo_kcc_sankey.R")
gout <- neo_kcc_sankey(kcc_data =  c("https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_8k.tif", "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_7k.tif"),
                       roi = c(-10, 35, 19, 45),
                       label.size = 2)
ggplot2::ggsave(paste0(root.path, "img/", "_sankey_changes.png"), 
                gout, 
                width = 16, 
                height = 14, 
                dpi = 300,
                units = "cm")

# SPD climats
source("R/neo_spd.R")
source("R/neo_spdplot.R")
source("R/neo_kcc_extract.R")
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
df_cc <- neo_kcc_extract(df.c14 = df_filtered,
                         kcc.file = kcc.file)
df_filtered_EN <- df_cc[df_cc$Period == "EN", ]
df_filtered_EN_6000_5000BC <- df_filtered_EN[df_filtered_EN$median > -6001 & df_filtered_EN$median < -4999, ]
neo_spd(df.c14 = df_filtered_EN_6000_5000BC, 
        color.on = 'kcc',
        export = TRUE,
        legend.pos = "topright",
        cex.main = .7,
        width = 14, height = 12,
        outDir = paste0(root.path, "img/"), 
        outFile = "_SPD_kcc")



# # Med CentrOcc
# obj.case <- list("MedWest", c(-5600), c(-10, 35, 19, 45), "koppen_8k.tif")
# ## by sites - - - - - - - - - - - - - - - - - - - - - - - -
# obj.case <- list("Le Baratin", c(-5700, -5500), c(2, 42, 7, 45), "koppen_8k.tif") # !!
# ## by authors - - - - - - - - - - - - - - - - - - - - - - - -
# obj.case <- list("Binder", c(-5500), c(0.07180157, 31.8513415, 23.73331373, 48.0342267), "koppen_8k.tif")
# # kcc.file = "C:/Rprojects/neonet/doc/references/binder_et_al_22_fig11_5600-5450_AEC.tif",
# obj.case <- list("Perrin", c(-5500), c(1.02768233, 40.83697718, 11.40383725, 46.44486459), "koppen_8k.tif")
# # kcc.file = "C:/Rprojects/neonet/doc/references/perrin08_fig16_3_5800_BC.tif",
#################################################################
obj.case.name <- paste0("isochr", paste0(obj.case[[2]], "-", paste0("BC-", obj.case[[1]], collapse = "-")), "-", gsub(".tif", "", obj.case[[4]]))
obj.case.out <- paste0(root.path, "img/", obj.case.name)
lbl.dates.interval <- c(obj.case[[2]], obj.case[[2]]-99)
source("R/neo_isochr.R")
isochr <- neo_isochr(df.c14 = df_filtered, 
                     isochr.subset =  obj.case[[2]], #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
                     isochr.subset.sup = obj.case[[2]] - 100,
                     largest.isochr = FALSE,
                     selected.per = "EN",
                     where = obj.case[[3]], #Italia.where, # where.roi,
                     kcc.file = paste0(root, "doc/data/clim/", obj.case[[4]]), # NA, 
                     create.legend = TRUE,
                     isochr.line.size = .5,
                     isochr.txt.size = 0,
                     # show.all.dates = FALSE,
                     lbl.dates = TRUE,
                     lbl.dates.interval = c(obj.case[[2]], obj.case[[2]]-99),
                     size.date = 1.5,
                     # color.dates = "darkgrey",
                     alpha.dates = 1,
                     # lbl.all.dates = FALSE,
                     # lbl.date.field = "median",
                     lbl.dates.size = 4,
                     lbl.time.interv = TRUE)
isochr$map
# ggplot2::ggsave(paste0(obj.case.out, ".png"), isochr$map, width = 7, height = 6)
ggplot2::ggsave(paste0(obj.case.out, ".png"), isochr$map, width = 10, height = 8)
ggplot2::ggsave(paste0(obj.case.out, "-legend.png"), isochr$legend, width = 5)
write.table(isochr$data, paste0(obj.case.out, ".tsv"), sep = "\t", row.names = FALSE)
# subset(isochr$data[order(isochr$data$median),], code == 'Csb')


## Not Run
# ggplot2::ggsave(paste0(root.path, "img/", "isochrones-barriere-Italy-EN-inter-map.png"), inter.map, width = 7, height = 7)

# df.datatable <- neo_dbs_info_dates_datatable(df.c14 = isochr$data) ; htmlwidgets::saveWidget(df.datatable, paste0(obj.case.out, ".html"))
# df <- isochr$data[ , c("idf","site", "period", "median", "code", "lon", "lat", "sourcedb")]
# write.table(df, paste0(root.path, "img/", "isochrones-barriere-Italy-EN-kcc.tsv"), sep = "\t", row.names = FALSE)
# ggplot2::ggsave(paste0(obj.case.out, ".png"), isochr$map, width = 14)
# ggplot2::ggsave(paste0(obj.case.out, "-legend.png"), isochr$legend, width = 5, height = 7)

# source("R/neo_spd.R")
# source("R/neo_spdplot.R")
# 
# neo_spd(df.c14 = head(df_filtered, 200), time.span = c(-12000, -6000))
# neo_spd(df.c14 = df_filtered, width = 15, height = 11, outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")

source("R/neo_calib_plot.R")
neo_calib_plot(df.c14 = c(6930, 60))

source("R/neo_dbs_info_date.R")
neo_dbs_info_date(LabCode = "R-432", df.c14 = df_filtered)


source("R/neo_find_date.R")
source("R/neo_dbs_info_date.R")
abber.date <- neo_find_date(df = isochr$data, print.it = FALSE, idf.dates = 4)
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




# SPD
source("R/neo_spd.R")
source("R/neo_spdplot.R")
source("R/neo_kcc_extract.R")
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
df_cc <- neo_kcc_extract(df.c14 = df_filtered,
                         kcc.file = kcc.file)
per <- 'EN'
df_cc.per <- subset(df_cc, Period == per)
tit <- paste0("SPD on ", per)
neo_spd(df.c14 = df_cc.per, 
        title = tit,
        # color.on = "kcc",
        time.span = c(12000, 6500),
        x.intercept = 5200,
        outDir = paste0(root.path, 'img/'), outFile = 'dates',
        width = 17, height = 14)