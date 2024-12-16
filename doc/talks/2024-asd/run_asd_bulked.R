# to run after run_asd.R

my_list <- list(
  list("NearEast", c(-9000), c(30, 30, 45, 41), "koppen_11k.tif"),
  list("NearEast", c(-8500), c(30, 30, 45, 41), "koppen_11k.tif"),
  list("NearEast", c(-8000), c(30, 30, 45, 41), "koppen_10k.tif"),
  list("NearEast", c(-7500), c(30, 30, 45, 41), "koppen_10k.tif"),
  # CW barrier --------------------------------------------------------------
  list("CWAnatolie", c(-7300), c(23, 30, 45, 40), "koppen_9k.tif"),
  list("CWAnatolie", c(-7000), c(23, 30, 45, 40), "koppen_9k.tif"),
  list("CWAnatolie", c(-6700), c(23, 30, 45, 40), "koppen_9k.tif"),
  # --------------------------------------------------------------------------
  # Med Orient
  list("MedEast", c(-6600), c(18, 35, 34, 43), "koppen_9k.tif"),
  list("MedEast", c(-6200), c(18, 35, 34, 43), "koppen_8k.tif"), 
  # list("MedEast", c(-6300), c(19, 35, 32, 43), "koppen_8k.tif"), 
  list("MedEast", c(-5800), c(18, 35, 34, 43), "koppen_8k.tif"), 
  # Med Centr
  list("MedCentr", c(-6000), c(5, 35, 20, 47), "koppen_8k.tif"),
  list("MedCentr", c(-5700), c(5, 35, 20, 47), "koppen_8k.tif"),
  list("MedCentr", c(-5400), c(5, 35, 20, 47), "koppen_7k.tif"),
  list("MedCentr", c(-5100), c(5, 35, 20, 47), "koppen_7k.tif"),
  # Med Occ
  list("MedWest", c(-5600), c(-11, 35, 10, 44), "koppen_8k.tif"),
  list("MedWest", c(-5300), c(-11, 35, 10, 44), "koppen_7k.tif")
  # All Med
  # list("MedCompleted", c(-5200), c(-7, 30, 38, 46), "koppen_7k.tif")
)
for(i in seq(1, length(my_list))){
  obj.case.name <- paste0("isochr", paste0(my_list[[i]][[2]], "-", paste0("BC-", my_list[[i]][[1]], collapse = "-")), "-", gsub(".tif", "", my_list[[i]][[4]]))
  obj.case.out <- paste0(root.path, "img/img2/", obj.case.name)
  kcc.file.path <- paste0("C:/Rprojects/neonet/doc/data/clim/", my_list[[i]][[4]])
  source("R/neo_isochr.R")
  isochr <- neo_isochr(df.c14 = df_filtered, 
                       isochr.subset =  my_list[[i]][[2]], #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
                       selected.per = "EN",
                       max.sd = 101,
                       where = my_list[[i]][[3]], #Italia.where, # where.roi,
                       kcc.file = kcc.file.path, # NA, 
                       is.other.geotiff = FALSE,
                       create.legend = TRUE,
                       isochr.line.color = NA, # "black", # NA to get colored isochrones (red, blue)
                       isochr.line.size = .5,
                       isochr.txt.size = 0,
                       calibrate = FALSE,
                       shw.dates = TRUE,
                       # show.all.dates = FALSE,
                       size.date = 1.5,
                       # color.dates = "darkgrey",
                       alpha.dates = 1,
                       lbl.dates = TRUE,
                       # lbl.all.dates = FALSE,
                       # lbl.date.field = "median",
                       lbl.dates.size = 4,
                       lbl.time.interv = TRUE)
  ggplot2::ggsave(paste0(obj.case.out, ".png"), isochr$map, width = 10, height = 8)
  ggplot2::ggsave(paste0(obj.case.out, "-legend.png"), isochr$legend, width = 5)
  write.table(isochr$data, paste0(obj.case.out, ".tsv"), sep = "\t", row.names = FALSE)
}
