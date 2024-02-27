neo_kcc_plot <- function(kcc = "C:/Rprojects/neonet/doc/data/clim/koeppen_7k.tif",
                         df.c14 = NA){
  # create a KCC map with dates (df.c14). The latter is a sf dataframe
  kcc_geo <- terra::rast(kcc)
  # plot(kcc_geo)
  ## colors
  koppen_codes <- c("Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", 
                    "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", 
                    "Cfb", "Cfc", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", 
                    "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF")
  hex_colors <- c("#0000FF", "#0078FF", "#46AAFF", "#FF0000", "#FF9696", "#F5A500", "#FFDC64", 
                  "#FFFF00", "#C8C800", "#969600", "#96FF96", "#64C864", "#329632", "#C8FF50", 
                  "#64FF50", "#32C800", "#FF00FF", "#C800C8", "#963296", "#966496", "#AAAFFF", 
                  "#5A78DC", "#4B50B4", "#320087", "#00FFFF", "#37C8FF", "#007D7D", "#00465F", 
                  "#B2B2B2", "#666666")
  # koppen_df <- data.frame(code = koppen_codes, hexColor = hex_colors)
  color_vector <- setNames(hex_colors, koppen_codes)
  # kcc_geo <- raster::raster(kcc)
  raster_df <- as.data.frame(kcc_geo, xy = TRUE)
  bbox <- sf::st_bbox(df.c14)
  ggplot2::ggplot() +
    ggplot2::geom_raster(data = raster_df, ggplot2::aes(x = x, y = y, fill = factor(code))) + 
    ggplot2::scale_fill_manual(values = color_vector) +  # Map fill colors using color_vector
    ggplot2::geom_sf(data = df.c14, color = "black", size = 0.5) +  # Add the sf object
    ggplot2::coord_sf() +  # Use coordinate system from sf object
    ggplot2::labs(fill = "Climate Code") + # Optional: add a legend title
    ggplot2::coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
    ggplot2::theme(legend.position = "none")
}

# neo_kcc_plot(df.c14 = head(df.c14, 30))
neo_kcc_plot(df.c14 = df.c14)

