kcc.per <- "koppen_11k"
root.path <- "C:/Rprojects/neonet/doc/talks/2024-simep/"
where.roi.path <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/"
where.roi <- paste0(where.roi.path, "roi.geojson")
kcc <- paste0("C:/Rprojects/neonet/doc/data/clim/", kcc.per, ".tif")

source("C:/Rprojects/neonet/R/neo_kcc_map.R")
gout <- neo_kcc_map(kcc = kcc,
                    df.c14 = df_filtered,
                    roi = where.roi)
ggplot2::ggsave(paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/xxx", kcc.per, ".png"),
       gout,
       width = 8,
       height = 5,
      units = "cm"
)