#' Creates a map of the research area
#' 
#' @name neo_map
#' 
#' @description map 
#'
#' @param map.name name of the output map and the name of the saved file (if export.plot is TRUE). Default "NeoNet_atl".
#' @param ggsheet if TRUE (Default), will read a Google Sheet. If FALSE, will read a standalone file (XLSX)
#' @param df.c14 path to the dataset. By default, the shared Google Sheet
#' @roi the region of interest, such as a river basin: Atlantic (Default) or Mediterranean
#' @param export.plot if TRUE export (by default), if FALSE display
#' @param dirOut name of the output folder. Only useful when `export.plot` is TRUE
#' @param width,height dimension of the output map, if exported.
#'
#' @return A leaflet interactive map for the radiocarbon dates
#'
#' @examples
#'
#' # Plot current region and other river basins
#' neo_map(df.c14 = df.c14, plot.other.ws = T, export.plot = F)
#'
#' @export
neo_map <- function(map.name = "map_atl",
                    df.c14 = NA,
                    gg.url = "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing",
                    # ref.spat = "C:/Rprojects/neonet/doc/data/wsh_atl.shp",
                    ref.spat = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/wsh_atl.geojson",
                    buff = 0.5,
                    plot.other.ws = FALSE,
                    export.plot = T,
                    dirOut = "C:/Rprojects/neonet/results/",
                    width = 9,
                    height = 11,
                    verbose = TRUE){
  # gs4_deauth();gs4_auth()
  # 
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(is.na(df.c14)){
    df.dates <- googlesheets4::read_sheet(gg.url)
  } else {
    df.dates <- df.c14
  }
  roi.layer <- DescTools::SplitPath(ref.spat)$filename
  ws.roi <- sf::st_read(ref.spat, quiet = TRUE)
  if(plot.other.ws){
    ws.roi.other <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/wsh_med.geojson", quiet = TRUE)
  }
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  neo.map <- ggplot2::ggplot(world) +
    ggplot2::geom_sf(fill = '#f0f0f0') +
    ggplot2::geom_sf(data = ws.roi, fill = '#636363', inherit.aes = FALSE, color = NA)
  if(plot.other.ws){
    neo.map <- neo.map +
      ggplot2::geom_sf(data = ws.roi.other, fill = '#bdbdbd', inherit.aes = FALSE, color = NA)
  }
  neo.map <- neo.map +
    ggplot2::coord_sf(xlim = c(sf::st_bbox(ws.roi)[1] - buff, sf::st_bbox(ws.roi)[3] + buff),
                      ylim = c(sf::st_bbox(ws.roi)[2] - buff, sf::st_bbox(ws.roi)[4] + buff)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) 
    # 
  if(export.plot){
    # print(paste0(map.name, " has been saved to: ", dirOut))
    ggplot2::ggsave(paste0(dirOut, map.name, ".jpg"), neo.map, 
                    width = width, height = height, units = "cm")
    if(verbose){
      print(paste0(map.name, " has been saved to: ", dirOut))
    }
  } else {
    neo.map
  }
}