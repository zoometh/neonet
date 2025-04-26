# to run after run_bspf.R

my_list <- list(
  # EN
  list("MedWestEst", c(-6000), c(-10, 35, 25, 45), "koppen_8k.tif", "EN"),
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
# 
for(i in seq(1, length(my_list))){
  # i <- 6
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
                       segment.linetype = 1,
                       force = 2,
                       min.segment.length = 0,
                       # show.all.dates = FALSE,
                       size.date = 1,
                       # color.dates = "darkgrey",
                       alpha.dates = 1,
                       lbl.dates = TRUE,
                       lbl.dates.interval = lbl.dates.interval, # subset dates to be labeled
                       # lbl.all.dates = FALSE,
                       # lbl.date.field = "median",
                       lbl.dates.size = 4,
                       lbl.time.interv = TRUE)
  ggplot2::ggsave(paste0(obj.case.out, obj.case.name, ".png"), isochr$map, width = 10, height = 6)
  ggplot2::ggsave(paste0(obj.case.out, "legend-", obj.case.name, ".png"), isochr$legend, width = 5)
  date.youngest <- lbl.dates.interval[1]
  date.oldest <- lbl.dates.interval[2]
  df.isochr.subset <- isochr$data[isochr$data$median < date.youngest & isochr$data$median > date.oldest, ]
  write.table(df.isochr.subset, paste0(obj.case.out, "data-", obj.case.name, ".tsv"), sep = "\t", row.names = FALSE)
}

## seriated dates + wmedians
df.c14 <- isochr$data.raw
chunks <- c(seq(from = 1, to = nrow(df.c14), by = 100), nrow(df.c14))
df.c14 <- df.c14[order(df.c14$median), ]
source("R/neo_calib_plot.R")
for(i in seq(1, length(chunks)-1)){
  achunk <- paste0(chunks[i],"_",chunks[i+1])
  gout <- paste0("C:/Users/TH282424/Rprojects/neonet/doc/talks/2025-bspf/img/_seriated_", achunk, ".png")
  png(file = gout, 
      width = 21, 
      height = 29.7, 
      units = "cm", 
      res = 300)
  df <- df.c14[seq(chunks[i], chunks[i+1]), ]
  neo_calib_plot(df,
                 col.wmedian = "blue",
                 cex.wmedian = .4,
                 cex.id = .5,
                 cex.lab = .5,
                 cex.axis = .5)
  dev.off()
}

# Stacked climates grouped by 100 years
when <- c(6100, 5000)
where <- c(-10, 35, 19, 45)
source("R/neo_subset_roi.R")
df_filtered_space <- neo_subset_roi(df_filtered, where = where)
source("R/neo_subset_when.R")
df_filtered_space_time <- neo_subset_when(df.c14 = df_filtered_space, 
                                          when = when, 
                                          period = "EN", 
                                          top.date = TRUE)
source("R/neo_kcc_extract.R")
# kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
#               "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
kcc.file <- c("koppen_7k.tif", "koppen_8k.tif")
df_cc <- neo_kcc_extract(df.c14 = df_filtered_space_time, kcc.file = kcc.file)
source("R/neo_kcc_plotbar_time_intervals.R")
gout <- neo_kcc_plotbar_time_intervals(df_cc, time.interval = when, time.bin = 100)
ggplot2::ggsave("C:/Users/TH282424/Rprojects/neonet/doc/talks/2025-bspf/img/_stacked_time_bin.png", gout, width = 10, height = 6)

# nrow(df.c14)/4

# cut the whole df in chunks before creating figures



