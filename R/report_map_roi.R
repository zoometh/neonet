#' Create a map with the ROI delimitation
#' @name report_map_roi
#' @description The shapefile 
#'
#' @param map.name the name of the output map and the name of the saved file (if export.plot is TRUE). By default "map_roi".
#' @param background the path to the SHP background.
#' @param roi the path to the SHP of the watershed
#' @param export.plot if TRUE export (by default), if FALSE display
#' @param dirOut name of the output folder. Only useful when export.plot is TRUE
#'
#' @return A PNG map with the extend of the ROI
#'
#' @examples
#'
#' report_map_roi(roi = "C:/Rprojects/neonet/doc/data/wsh_atl.shp")
#'
#' @export
library(sf)
library(ggplot2)

report_map_roi <- function(map.name = "map_roi",
                           background = "C:/Rprojects/neonet/doc/data/admin_background.shp",
                           roi = "C:/Rprojects/neonet/doc/data/wsh_med.shp",
                           export.plot = T,
                           dirOut = "C:/Rprojects/neonet/results/"){
  bck.layer <- DescTools::SplitPath(background)$filename
  roi.layer <- DescTools::SplitPath(roi)$filename
  bck_admin.shp <- sf::st_read(dsn = dirname(background), layer = bck.layer)
  ws_roi.shp <- sf::st_read(dsn = dirname(roi), layer = roi.layer)
  gmap <- ggplot() + 
    geom_sf(data = bck_admin.shp, fill = 'grey70', color = NA) +
    geom_sf(data = ws_roi.shp, fill = 'grey40', color = NA) +
    theme_bw()+
    xlim(st_bbox(ws_roi.shp)[1], st_bbox(ws_roi.shp)[3]) +
    ylim(st_bbox(ws_roi.shp)[2], st_bbox(ws_roi.shp)[4])
  if(export.plot){
  ggsave(paste0(dirOut, map.name, ".png"), 
         gmap,
         width = 15,
         height = 11,
         dpi = 600, 
         units = "cm")
  } else {
    gmap
  }
}