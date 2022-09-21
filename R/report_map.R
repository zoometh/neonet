#' Create a leaflet interactive distribution map 
#' @name report_map
#' @description To report the distribution of the radiocarbon dates, creates a leaflet interactive map from a Google Sheet sorted by BDs (MED, BDA, etc.)
#'
#' @param map.name the name of the output map and the name of the saved file (if export.plot is TRUE). By default "map".
#' @param data.path path to the dataset. By default, the shared Google Sheet
#' @param export.plot if TRUE export (by default), if FALSE display
#' @param dirOut name of the output folder. Only useful when export.plot is TRUE
#'
#' @return A leaflet interactive map for the radiocarbon dates
#'
#' @examples
#'
#' report_map(map.name = "neonet_atl", export.plot = T)
#'
#' @export
library(googlesheets4)
library(RColorBrewer)
library(leaflet)

# gs4_deauth();gs4_auth()

report_map <- function(map.name = "map",
                       data.path = "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing",
                       roi = "C:/Rprojects/neonet/doc/data/wsh_atl.shp",
                       export.plot = T,
                       dirOut = "C:/Rprojects/neonet/results/"){
  # gg.url <- "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing"
  db.atl <- googlesheets4::read_sheet(data.path)
  roi.layer <- DescTools::SplitPath(roi)$filename
  ws_roi.shp <- sf::st_read(dsn = dirname(roi), layer = roi.layer)
  ws_roi.shp.sp <- as(ws_roi.shp, "Spatial")
  
  n.BDs <- length(unique(db.atl$BD))
  # RColorBrewer::brewer.pal(n.BDs,"Set1")
  df.colors <- data.frame(BD = unique(db.atl$BD),
                          color = RColorBrewer::brewer.pal(n.BDs, "Set1"))
  db.atl <- merge(db.atl, df.colors, by = "BD", all.x = T)
  db.atl$lbl <- paste0("<b>", db.atl$SiteName," - ", db.atl$LabCode, "</b><br>",
                       db.atl$C14BP, " +/- ", db.atl$C14SD, "<br>",
                       db.atl$Period," - ",  db.atl$PhaseCode, "<br>",
                       "<b>", db.atl$BD,"</b>")
  neo.map <- leaflet(data = db.atl) %>%
    addProviderTiles(providers$"Esri.WorldImagery", group = "Ortho") %>%
    addProviderTiles(providers$"OpenStreetMap", group = "OSM") %>%
    addPolygons(data = ws_roi.shp.sp,
                color = "blue",
                fillOpacity = 0) %>%
    # addTiles(group = 'OSM') %>%
    addCircleMarkers(layerId = ~LabCode, 
                     lng = ~Longitude,
                     lat = ~Latitude,
                     weight = 1,
                     radius = 3,
                     popup = ~lbl,
                     label = ~SiteName,
                     fillColor = ~color,
                     fillOpacity = .2,
                     color = ~color,
                     opacity = .8) %>%
    addLegend("bottomright", 
              colors = df.colors$color, 
              labels = df.colors$BD,
              title = "BDs",
              opacity = 1) %>%
    addLayersControl(
      baseGroups = c("Ortho", "OSM"),
      position = "topleft"
    )
  if(export.plot){
    htmlwidgets::saveWidget(neo.map, paste0(dirOut, map.name, ".html"))
  } else {
    neo.map
  }
}
