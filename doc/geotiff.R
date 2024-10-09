kcc.file = "C:/Rprojects/neonet/doc/references/perrin08_fig16_3_5800_BC.tif" # Grey
# kcc.file = "C:/Rprojects/neonet/doc/references/binder_et_al_22_fig11_5600-5450_AEC.tif" # RGB
kcc_geo <- terra::rast(kcc.file)
raster_df <- as.data.frame(kcc_geo, xy = TRUE)
if(length(names(kcc_geo)) == 3){
  # RGB
  colnames(raster_df) <- c("x", "y", "Red", "Green", "Blue")
  raster_df$color <- with(raster_df, rgb(Red/255, Green/255, Blue/255))
}
if(length(names(kcc_geo)) == 1){
  # Grey scale / not working
  colnames(raster_df) <- c("x", "y", "color")
  ggplot(raster_df) +
    geom_raster(aes(x = x, y = y, fill = color)) +
    scale_fill_gradient(low = "black", high = "white") +  # Blue for low values, red for high
    # scale_fill_viridis_c() +  # Use a continuous color scale (e.g., Viridis)
    coord_fixed() +  # Maintain aspect ratio for geospatial data
    labs(title = "Single-Channel Raster Plot", fill = "Value") +
    theme_minimal()
  # raster_df$color <- with(raster_df, rgb(Red/255, Green/255, Blue/255))
}
# ggplot(raster_df) +
#   geom_raster(aes(x = x, y = y, fill = color)) +
#   scale_fill_identity() +  # Use the color column as it is
#   coord_fixed() +  # Maintain the aspect ratio
#   labs(title = "GeoTIFF Raster Plot") +
#   theme_minimal()

# Plot the raster using ggplot2 and geom_raster

