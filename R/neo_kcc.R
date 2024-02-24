neo_kcc <- function(){
  kcc <- "C:/Rprojects/neonet/doc/data/clim/koeppen_7k.tif"
  kcc_geo <- terra::rast(kcc)
  # colors
  colors <- terra::coltab(kcc_geo)[[1]] # Get the color table for the first (or only) layer
  cat_df <- levels(kcc_geo)[[1]] # Get the levels/categories for the first (or only) layer
  final_df$hexColour <- rgb(final_df$colour.red, final_df$colour.green, final_df$colour.blue, maxColorValue = 255)
  color_vector <- setNames(final_df$hexColour, final_df$code)
  # kcc_geo <- raster::raster(kcc)
  raster_df <- as.data.frame(kcc_geo, xy = TRUE)
  bbox <- st_bbox(df.c14)
  ggplot2::ggplot() +
    ggplot2::geom_raster(data = raster_df, aes(x = x, y = y, fill = factor(code))) + 
    ggplot2::geom_sf(data = df.c14, color = "black", size = 0.5) +  # Add the sf object, assuming df.c14 is ready
    ggplot2::coord_sf() +  # Use coordinate system from sf object
    ggplot2::scale_fill_manual(values = color_vector) +  # Map fill colors using color_vector
    ggplot2::labs(fill = "Climate Code") + # Optional: add a legend title
    ggplot2::coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax))
}

ggplot() +
  ggplot2::geom_raster(data = raster_df, aes(x = x, y = y, fill = factor(code))) + 
  ggplot2::scale_fill_manual(values = color_vector) +  # Map fill colors using color_vector
  ggplot2::geom_sf(data = df.c14, color = "red", size = 0.5) +  # Add the sf object
  ggplot2::coord_sf() +  # Use coordinate system from sf object
  ggplot2::labs(fill = "Climate Code") + # Optional: add a legend title
  ggplot2::coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax))


