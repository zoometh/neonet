# to run after run_bspf.R

my_list <- list(
  # EN
  list("MedWest", c(-6000), c(-10, 35, 19, 45), "koppen_8k.tif", "EN"),
  list("MedWest", c(-5900), c(-10, 35, 19, 45), "koppen_8k.tif", "EN"),
  list("MedWest", c(-5800), c(-10, 35, 19, 45), "koppen_8k.tif", "EN"),
  list("MedWest", c(-5700), c(-10, 35, 19, 45), "koppen_8k.tif", "EN"),
  list("MedWest", c(-5600), c(-10, 35, 19, 45), "koppen_8k.tif", "EN"),
  list("MedWest", c(-5500), c(-10, 35, 19, 45), "koppen_8k.tif", "EN"),
  list("MedWest", c(-5400), c(-10, 35, 19, 45), "koppen_7k.tif", "EN"),
  list("MedWest", c(-5300), c(-10, 35, 19, 45), "koppen_7k.tif", "EN"),
  list("MedWest", c(-5200), c(-10, 35, 19, 45), "koppen_7k.tif", "EN"),
  list("MedWest", c(-5100), c(-10, 35, 19, 45), "koppen_7k.tif", "EN"),
  list("MedWest", c(-5000), c(-10, 35, 19, 45), "koppen_7k.tif", "EN"),
  # LM
  list("MedWest", c(-6000), c(-10, 35, 19, 45), "koppen_8k.tif", "LM"),
  list("MedWest", c(-5900), c(-10, 35, 19, 45), "koppen_8k.tif", "LM"),
  list("MedWest", c(-5800), c(-10, 35, 19, 45), "koppen_8k.tif", "LM"),
  list("MedWest", c(-5700), c(-10, 35, 19, 45), "koppen_8k.tif", "LM"),
  list("MedWest", c(-5600), c(-10, 35, 19, 45), "koppen_8k.tif", "LM"),
  list("MedWest", c(-5500), c(-10, 35, 19, 45), "koppen_8k.tif", "LM"),
  list("MedWest", c(-5400), c(-10, 35, 19, 45), "koppen_7k.tif", "LM"),
  list("MedWest", c(-5300), c(-10, 35, 19, 45), "koppen_7k.tif", "LM"),
  list("MedWest", c(-5200), c(-10, 35, 19, 45), "koppen_7k.tif", "LM"),
  list("MedWest", c(-5100), c(-10, 35, 19, 45), "koppen_7k.tif", "LM"),
  list("MedWest", c(-5000), c(-10, 35, 19, 45), "koppen_7k.tif", "LM")
)
for(i in seq(1, length(my_list))){
  # i <- 1
  obj.case.name <- paste0("isochr-", my_list[[i]][[5]], paste0(my_list[[i]][[2]], "-", paste0("BC-", my_list[[i]][[1]], collapse = "-")), "-", gsub(".tif", "", my_list[[i]][[4]]))
  obj.case.out <- paste0(root.path, "img/")
  kcc.file.path <- paste0("C:/Rprojects/neonet/doc/data/clim/", my_list[[i]][[4]])
  source("R/neo_isochr.R")
  isochr <- neo_isochr(df.c14 = df_filtered, 
                       isochr.subset =  my_list[[i]][[2]], #"None", #c(-5600), # , # c(-5600), # - 5500 TODO
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
                       # lbl.all.dates = FALSE,
                       # lbl.date.field = "median",
                       lbl.dates.size = 4,
                       lbl.time.interv = TRUE)
  ggplot2::ggsave(paste0(obj.case.out, obj.case.name, ".png"), isochr$map, width = 10, height = 6)
  ggplot2::ggsave(paste0(obj.case.out, "legend-", obj.case.name, ".png"), isochr$legend, width = 5)
  write.table(isochr$data, paste0(obj.case.out, "data-", obj.case.name, ".tsv"), sep = "\t", row.names = FALSE)
}
