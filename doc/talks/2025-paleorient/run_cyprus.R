## Spot outlier dates ##
## see: https://github.com/zoometh/neonet?tab=readme-ov-file#outlier-dates
########################

# Download the R functions using https://download-directory.github.io/ and this URL https://github.com/zoometh/neonet/tree/main/R

# root.path <- "C:/Rprojects/neonet/doc/talks/2025-bspf/"
root <- "C:/Users/TH282424/Rprojects/neonet/"
root.path <- paste0(root, "doc/talks/2025-paleorient/")
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
  list("Cyprus", c(-8000), c(26, 32, 37, 39), "koppen_8k.tif", "EN")
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

# general map
source("R/neo_map.R")
per <- 'EN'
tit <- paste0("Earliest radiocarbon dates (w-medians) for the ", "<span style='color: ", "red", ";'>", per, "</span>")
source("R/neo_subset_roi.R")
df_filtered_ww <- neo_subset_roi(df.c14 = df_filtered, where = c(-10, 35, 19, 45))
source("R/neo_subset_when.R")
df_filtered_ww <- neo_subset_when(df.c14 = df_filtered_ww, period = "EN",
                                  top.date = TRUE,
                                  col.median = "median",
                                  col.period = "Period",
                                  verbose = TRUE)
gg.map <- neo_map(df.c14 = df_filtered_ww,
                  selected.per = per,
                  breaks_values = c(-6000, -5900, -5800, -5700, -5600, -5500, -5400, -5300, -5200, -5100, -5000),
                  dates.size = 1,
                  title = tit,
                  roi = NA, dates.within.roi = FALSE)
ggplot2::ggsave(paste0(root.path, "img/_map_data_", per, ".png"), gg.map, width = 11, height = 6)


# seriation of date for one site
source("R/neo_calib_plot.R")
df_filtered_ww_pendimoun <- df_filtered[df_filtered$SiteName == 'Pendimoun', ]
df_filtered_ww_pendimoun$site <- df_filtered_ww_pendimoun$SiteName
df_filtered_ww_pendimoun$labcode <- df_filtered_ww_pendimoun$LabCode
fileOut <- paste0(root.path, "img/_date_pendimoun_", "EN", ".png")
png(fileOut, width = 30, height = 20, units = "cm",res = 150)
neo_calib_plot(df.c14 = df_filtered_ww_pendimoun, )
dev.off()
