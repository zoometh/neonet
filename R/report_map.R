#' Create a leaflet interactive distribution map 
#' 
#' @name report_map
#' 
#' @description distribution of the radiocarbon dates within a selected ROI. It creates a leaflet interactive map from a Google Sheet sorted by BDs (MED, BDA, etc.)
#'
#' @param map.name name of the output map and the name of the saved file (if export.plot is TRUE). Default "NeoNet_atl".
#' @param ggsheet if TRUE (Default), will read a Google Sheet. If FALSE, will read a standalone file (XLSX)
#' @param data.path path to the dataset. By default, the shared Google Sheet
#' @roi the region of interest, such as a river basin: Atlantic (Default) or Mediterranean
#' @param export.plot if TRUE export (by default), if FALSE display
#' @param dirOut name of the output folder. Only useful when `export.plot` is TRUE
#'
#' @return A leaflet interactive map for the radiocarbon dates
#'
#' @examples
#'
#' # Default map plot (Google Sheet)
#' report_map(map.name = "neonet_atl", export.plot = F)
#' 
#' # Export a map with data coming from an XLSX dataset
#' report_map(export.plot = T, 
#'            ggsheet = F,
#'            data.path = "C:/Users/Thomas Huet/Downloads/NeoNet_atl_ELR.xlsx",
#' )
#'
#' @export
report_map <- function(map.name = "neonet_atl",
                       ggsheet = TRUE,
                       data.path = "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing",
                       roi = "C:/Rprojects/neonet/doc/data/wsh_atl.shp",
                       title = "NeoNet atl",
                       export.plot = T,
                       dirOut = "C:/Rprojects/neonet/results/",
                       verbose = TRUE){
  # gs4_deauth();gs4_auth()
  # gg.url <- "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing"
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(ggsheet){
    df.dates <- googlesheets4::read_sheet(data.path)
  } else {
    df.dates <- openxlsx::read.xlsx(data.path)
  }
  roi.layer <- DescTools::SplitPath(roi)$filename
  ws_roi.shp <- sf::st_read(dsn = dirname(roi), layer = roi.layer)
  ws_roi.shp.sp <- as(ws_roi.shp, "Spatial")
  
  n.BDs <- length(unique(df.dates$BD))
  # RColorBrewer::brewer.pal(n.BDs,"Set1")
  df.colors <- data.frame(BD = unique(df.dates$BD),
                          color = c("blue", "red", "orange", "pink", "violet", "yellow")[1:n.BDs]
                          # color = RColorBrewer::brewer.pal(n.BDs, "Dark2")
                          )
  df.dates <- merge(df.dates, df.colors, by = "BD", all.x = T)
  df.dates$lbl <- paste0("<b>", df.dates$SiteName,"</b> [", df.dates$LabCode, "]<br>",
                       "C14Age: <b>", df.dates$C14Age, " +/- ", df.dates$C14SD, "</b><br>",
                       "Period: <b>", df.dates$Period,"</b> - PhaseCode:",  df.dates$PhaseCode, "<br>",
                       "source BD: <b>", df.dates$BD,"</b>")
  map.title <- paste0("<a href='https://github.com/zoometh/neonet'>", title,"</a><br>", 
                      "Nb of radiocarbon dates: ", nrow(df.dates))
  neo.map <- leaflet::leaflet(data = df.dates) %>%
    leaflet::addProviderTiles(leaflet::providers$"Esri.WorldImagery", group = "Ortho") %>%
    leaflet::addProviderTiles(leaflet::providers$"OpenStreetMap", group = "OSM") %>%
    leaflet::addProviderTiles(leaflet::providers$"Stamen.TerrainBackground", group = "Terrain") %>%
    # leaflet::addProviderTiles(leaflet::providers$"Thunderforest.Outdoors", group = "Default") %>%
    leaflet::addPolygons(data = ws_roi.shp.sp,
                         color = "blue",
                         fillOpacity = 0) %>%
    # addTiles(group = 'OSM') %>%
    leaflet::addCircleMarkers(layerId = ~LabCode, 
                              lng = ~Longitude,
                              lat = ~Latitude,
                              weight = 1,
                              radius = 3,
                              popup = ~lbl,
                              label = ~SiteName,
                              fillColor = ~color,
                              fillOpacity = .3,
                              color = ~color,
                              opacity = 1) %>%
    leaflet::addLegend("bottomright", 
                       colors = df.colors$color, 
                       labels = df.colors$BD,
                       title = "BDs",
                       opacity = 1) %>%
    leaflet::addLayersControl(
      baseGroups = c("Terrain", "OSM", "Ortho"),
      position = "topleft"
    ) %>%
    leaflet::addControl(map.title,
                        position = "topright")
  if(export.plot){
    htmlwidgets::saveWidget(neo.map, paste0(dirOut, map.name, ".html"))
    if(verbose){
      print(paste0(map.name, " has been saved into: ", dirOut))
    }
  } else {
    neo.map
  }
}

# Export a map with data coming from an XLSX dataset
report_map(export.plot = T,
           ggsheet = F,
           data.path = "C:/Users/Thomas Huet/Downloads/NeoNet_atl_ELR.xlsx",
)


