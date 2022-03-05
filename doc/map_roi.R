library(sf)
library(ggplot2)

mapextent <- function(background, roi){
  bck_admin.shp <- st_read(dsn = "doc/data", layer = "admin_background")
  ws_roi.shp <- st_read(dsn = "doc/data", layer = "ws_med")
  # ws_roi.shp.mbr <- st_as_sf(st_as_sfc(st_bbox(ws_roi.shp)))
  # bck_admin.shp.roi <- st_intersection(bck_admin.shp, ws_roi.shp.mbr)
  gmap <- ggplot() + 
    geom_sf(data = bck_admin.shp, fill = 'grey70', color = NA) +
    geom_sf(data = ws_roi.shp, fill = 'grey40', color = NA) +
    theme_bw()+
    xlim(st_bbox(ws_roi.shp)[1], st_bbox(ws_roi.shp)[3]) +
    ylim(st_bbox(ws_roi.shp)[2], st_bbox(ws_roi.shp)[4])
  ggsave("spatialextension.jpg", gmap,width = 15, height = 11, dpi = 600, 
         units = "cm")
}