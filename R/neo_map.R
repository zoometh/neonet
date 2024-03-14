#' Creates a map of the research area
#' 
#' @name neo_map
#' 
#' @description map 
#'
#' @param map.name name of the output map and the name of the saved file (if export.plot is TRUE). Default "NeoNet_atl".
#' @param ggsheet if TRUE (Default), will read a Google Sheet. If FALSE, will read a standalone file (XLSX)
#' @param df.c14 path to the dataset. By default, the shared Google Sheet.
#' @param ref.spat The ROI.
#' @roi the region of interest, such as a river basin: Atlantic (Default) or Mediterranean
#' @param export.plot if TRUE export, else return a ggplot.
#' @param dirOut name of the output folder. Only useful when `export.plot` is TRUE
#' @param width,height dimension of the output map, if exported.
#' @param plot.other.ws Plot other watershed (TRUE or FALSE).
#'
#' @return A map
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
                    plot.dates = FALSE,
                    plot.other.ws = FALSE,
                    title = NA,
                    verbose = TRUE){
  # gs4_deauth();gs4_auth()
  # 
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(!inherits(df.c14, "sf")){
    df.dates <- googlesheets4::read_sheet(gg.url)
  } else {
    df.dates <- df.c14
  }
  roi.layer <- DescTools::SplitPath(ref.spat)$filename
  ws.roi <- sf::st_read(ref.spat, quiet = TRUE)
  if(plot.other.ws){
    ws.roi.other <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/wsh_med.geojson", quiet = TRUE)
  }
  world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  g.neo.map <- ggplot2::ggplot(world) +
    # ggplot2::geom_sf(fill = '#7d7d7d', color = '#7d7d7d') +
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = ws.roi, color = 'black', fill = NA, inherit.aes = FALSE)
  if(plot.dates){
    g.neo.map <- g.neo.map +
      ggplot2::geom_sf(data = df.dates, inherit.aes = FALSE)
  }
  if(plot.other.ws){
    g.neo.map <- g.neo.map +
      ggplot2::geom_sf(data = ws.roi.other, fill = '#bdbdbd', inherit.aes = FALSE, color = NA)
  }
  if(!is.na(title)){
    g.neo.map <- g.neo.map +
      ggplot2::ggtitle(title)
  }
  g.neo.map <- g.neo.map +
    ggplot2::coord_sf(xlim = c(sf::st_bbox(ws.roi)[1] - buff, sf::st_bbox(ws.roi)[3] + buff),
                      ylim = c(sf::st_bbox(ws.roi)[2] - buff, sf::st_bbox(ws.roi)[4] + buff)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) 
    # 
  return(g.neo.map)
}