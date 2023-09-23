#' @name neo_isochr
#'
#' @description create isochrones contours by interpolation of calibrated radiocarbon dates
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @param selected.neo the Period on which the isochrones will be calculated. Used to subset `df.c14`. the Default: EN.
#' @param calibrate if TRUE (default) will calibrate dates using the neo_calib() function.
#' @param time.interv time interval between two isochrones, in years. Default: 250.
#' @param coloramp the name of the coloramp to use on contour. For example: "Reds" (default), "Blues", etc. 
#' @param show.lbl show the sites identifiers (default: TRUE)
#' @param map.longest.size the longest size of the output map (height or width) in cm. The smallest size will be calculated from it. Only useful if if export = TRUE. Default: 15
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Export a file
#'
#' @examples
#' 
#' myc14data <- "C:/Rprojects/neonet/results/neonet_2023-09-23.geojson"
#' neo_isochr(df.c14 = myc14data)
#' shell.exec(myc14data)
#'
#'
#' @export
neo_isochr <- function(df.c14 = "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-23.geojson",
                       selected.neo = c("EN"),
                       # max.sd = 100,
                       calibrate = TRUE,
                       time.interv = 250,
                       mapname = NA,
                       coloramp = "Reds",
                       show.lbl = TRUE,
                       export = TRUE,
                       outDir = "C:/Rprojects/neonet/results/",
                       map.longest.size = 15,
                       verbose = TRUE){
  # library(tidyverse)
  # library(interp)
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  df.dates <- sf::st_read(df.c14, quiet = T)
  nb.dates.tot <- nrow(df.dates)
  if(verbose){
    print(paste0("Original GeoJSON file: ", nb.dates.tot, " dates"))
  }
  # subset on periods
  df.dates <- df.dates[df.dates$Period %in% selected.neo, ]
  if(verbose){
    print(paste0("After subset of Periods on '", 
                 paste0(selected.neo, collapse = ", "),"': ",
                 nrow(df.dates), " dates to model"))
  }
  # # subset on SD
  # if(!is.na(max.sd)){
  #   df.dates <- df.dates[df.dates$C14SD < max.sd, ]
  #   if(verbose){
  #     print(paste0("After subset of SD on < ", 
  #                  max.sd,": ",
  #                  nrow(df.dates), " dates to calibrate and model"))
  #   }
  # }
  if(calibrate){
    df.dates <- neo_calib(as.data.frame(df.dates))
  }
  # Select the row with the minimum median in each site
  df.dates.min <- df.dates %>% 
    dplyr::group_by(SiteName) %>% 
    dplyr::slice_min(median)
  if(verbose){
    print(paste0("Minimal medians by site selected: ",
                 nrow(df.dates.min), " dates"))
  }
  # to sf
  df.dates.min$geometry <- sf::st_as_text(df.dates.min$geometry)
  df.dates.min <- sf::st_as_sf(df.dates.min, wkt = "geometry")
  Xs <- sf::st_coordinates(df.dates.min$geometry)[, 1]
  Ys <- sf::st_coordinates(df.dates.min$geometry)[, 2]
  # TODO: create a IDF in NeoNet app
  if(!("idf_nn" %in% colnames(df.dates.min))){
    if(verbose){
      print(paste0("Creates the column new unique IDs"))
    }
    idf_nn <- 1:nrow(df.dates.min)
    df <- data.frame(site = df.dates.min$SiteName,
                     idf = idf_nn, # TODO: change once idf_nn exists
                     longitude = Xs,
                     latitude = Ys, 
                     median = df.dates.min$median)
  } else {
    df <- data.frame(site = df.dates.min$SiteName,
                     idf = df.dates.min$idf_nn, # TODO: change once idf_nn exists
                     longitude = Xs,
                     latitude = Ys, 
                     median = df.dates.min$median)
  }
  contour_levels <- seq(min(df$median), max(df$median), by = time.interv)
  # TODO: handle duplicated
  # duplicated(df[ , c("longitude", "latitude")])
  interpolated <- interp::interp(x = df$longitude, 
                                 y = df$latitude, 
                                 z = df$median, 
                                 duplicate = "mean",    #you have duplicated values
                                 output = "grid")
  #convert this to a long form dataframe
  interp_df <- tidyr::expand_grid(i = seq_along(interpolated$x), 
                                  j = seq_along(interpolated$y)) %>% 
    dplyr::mutate(lon = interpolated$x[i],
                  lat = interpolated$y[j],
                  date.med = purrr::map2_dbl(i, j, ~interpolated$z[.x, .y])) %>% 
    dplyr::select(-i, -j)
  # colors
  nb.contours <- length(contour_levels)
  myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp))(nb.contours)
  # title/filename
  if(is.na(mapname)){
    mapname <- DescTools::SplitPath(df.c14)$filename
  }
  tit <- paste0(mapname, " (", nrow(df), " dates used / ", nb.dates.tot, ")")
  # map
  buff <- .1
  bbox <- c(left = min(Xs) - buff, 
            bottom = min(Ys) - buff, 
            right = max(Xs) + buff, 
            top = max(Ys) + buff)
  # calculate autozoom
  autozoom <- rosm:::tile.raster.autozoom(
    rosm::extract_bbox(
      matrix(bbox, ncol = 2, byrow = TRUE)),
    epsg = 4326)
  # map
  stamenbck <- ggmap::get_stamenmap(bbox, 
                                    zoom = autozoom,
                                    maptype = "terrain-background")
  map <- ggmap::ggmap(stamenbck, darken = c(.2, "white")) + 
    # TODO: add bbox on map to show the studied area
    ggplot2::ggtitle(tit) +
    ggplot2::geom_contour(data = interp_df, 
                          ggplot2::aes(x = lon, y = lat, z = date.med, 
                                       # color = ..level..
                                       color = ggplot2::after_stat(level)
                          ),
                          breaks = contour_levels) +
    metR::geom_text_contour(data = interp_df,
                            binwidth = time.interv,
                            ggplot2::aes(x = lon, y = lat, z = date.med, colour = ..level..),
                            skip = 0,
                            rotate = TRUE,
                            stroke = .25,
                            size = 2.5) +
    ggplot2::geom_point(data = df, 
                        ggplot2::aes(x = longitude, y = latitude), 
                        col = "black",
                        size = 1) +
    ggplot2::scale_color_gradientn(colours = rev(myPalette),
                                   name = "Cal BC")
    # ggplot2::scale_color_gradient(low = "#000000", high = "#FFAAAA")
  if(show.lbl){
    map <- map +
      ggrepel::geom_text_repel(data = df, 
                               ggplot2::aes(x = longitude, y = latitude, label = idf),
                               size = 2,
                               segment.alpha = .3,
                               segment.size = .3,
                               max.overlaps = Inf)
  }
  if(export){
    # print(paste0("After subset on Periods ", paste0(select.periods, collapse = ", "),": ", nrow(df.dates), " dates"))
    outFile <- paste0(outDir, mapname, ".png")
    # calculate the right proportion of the output map by calculating the ration w/h of the bbox
    map.width.size <- bbox[["right"]] - bbox[["left"]]
    map.height.size <- bbox[["top"]] - bbox[["bottom"]]
    w_h.ratio <- round( map.width.size / map.height.size, 1)
    if(w_h.ratio > 1){
      # width larger than height
      ggplot2::ggsave(map, filename = outFile, device = "png",
                      width = map.longest.size,
                      height = map.longest.size / w_h.ratio)
    } else {
      ggplot2::ggsave(map, filename = outFile, device = "png",
                      width = map.longest.size / w_h.ratio,
                      height = map.longest.size)
    }
    
    if(verbose){
      print(paste0("Map '", mapname,"' has been exported to '", outDir, "'"))
    }
  } else {
    print(map)
  }
}