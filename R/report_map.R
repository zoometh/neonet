#' Create a plotly interactive distribution map 
#' @name plotly_surface_3d
#' @description To report the distribution of the radiocarbon dates, creates a Plotly interactive map from a Google Sheet sorted by BDs (MED, BDA, etc.)
#'
#' @param xyz a dataframe with X, Y, and Z columns for the surface
#' @param scale the scale of the axis (mm, cm, etc.)
#' @param marker a dataframe with X, Y, and Z columns of additional points. NA by default
#' @param size.markers,color.markers,opacity.markers grahical parameters
#' @param export.plot if TRUE export, if FALSE display
#' @param xyz.name name of the output plot. Only useful when export.plot is TRUE
#' @param dirOut name of the output folder. Only useful when export.plot is TRUE
#'
#' @return A dataframe
#'
#' @examples
#'
#' xyz <- read_dat()
#' plotly_surface_3d(xyz, export.plot = FALSE)
#'
#' @export

library(googlesheets4)
library(RColorBrewer)
library(leaflet)

# gs4_deauth();gs4_auth()

report_map <- function(map.name = "map",
                       data.path = "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing",
                       dirOut = paste0(system.file(package = "eamenaR"), "/results/")){
  # gg.url <- "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing"
  db.atl <- read_sheet(data.path)
  
  n.BDs <- length(unique(db.atl$BD))
  brewer.pal(n.BDs,"Set1")
  df.colors <- data.frame(BD = unique(db.atl$BD),
                          color = brewer.pal(n.BDs, "Set1"))
  db.atl <- merge(db.atl, df.colors, by = "BD", all.x = T)
  db.atl$lbl <- paste0("<b>", db.atl$SiteName," - ", db.atl$LabCode, "</b><br>",
                       db.atl$C14BP, " +/- ", db.atl$C14SD, "<br>",
                       db.atl$Period," - ",  db.atl$PhaseCode, "<br>",
                       "<b>", db.atl$BD,"</b>")
  neo.map <- leaflet(data = db.atl) %>%
    addProviderTiles(providers$"Esri.WorldImagery", group = "Ortho") %>%
    addProviderTiles(providers$"OpenStreetMap", group = "OSM") %>%
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
  htmlwidgets::saveWidget(neo.map, )
}
