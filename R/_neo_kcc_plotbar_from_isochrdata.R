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