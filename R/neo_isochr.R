#' @name neo_isochr
#'
#' @description create isochron contours by interpolation of calibrated radiocarbon dates
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @param selected.neo the Period on which the isochrons will be calculated. Used to subset `df.c14`. the Default: EN.
#' @param max.sd the maximum of accepted SD. Dates with an higher value will be removed. Choose NA to not subset. Default: 100. 
#' @param calibrate if TRUE (default) will calibrate dates using the neo_calib() function.
#' @param map.longest.size the longest size of the output map (height or width) in cm. The smallest size will be calculated from it. Only useful if if export = TRUE. Default: 15
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Export a file
#'
#' @examples
#'
#'
#' @export
neo_isochr <- function(df.c14 = "C:/Rprojects/neonet/results/2023-09-15_neonet.geojson",
                       selected.neo = c("EN"),
                       max.sd = 100,
                       calibrate = TRUE,
                       export = TRUE,
                       outDir = "C:/Rprojects/neonet/results/",
                       mapname = NA,
                       map.longest.size = 15,
                       verbose = TRUE){
  
  # library(tidyverse)
  # library(interp)
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  df.dates <- sf::st_read(df.c14, quiet = T)
  if(verbose){
    print(paste0("Original GeoJSON file: ", nrow(df.dates), " dates"))
  }
  # subset on periods
  df.dates <- df.dates[df.dates$Period %in% selected.neo, ]
  if(verbose){
    print(paste0("After subset of Periods on '", 
                 paste0(selected.neo, collapse = ", "),"': ",
                 nrow(df.dates), " dates to model"))
  }
  # subset on SD
  if(!is.na(max.sd)){
    df.dates <- df.dates[df.dates$C14SD < max.sd, ]
    if(verbose){
      print(paste0("After subset of SD on < ", 
                   max.sd,": ",
                   nrow(df.dates), " dates to calibrate and model"))
    }
  }
  if(calibrate){
    df.dates <- neo_calib(as.data.frame(df.dates))
  }
  # Select the row with the minimum median in each site
  df.dates.min <- df.dates %>% 
    dplyr::group_by(SiteName) %>% 
    dplyr::slice_min(median)
  # to sf
  df.dates.min$geometry <- sf::st_as_text(df.dates.min$geometry)
  df.dates.min <- sf::st_as_sf(df.dates.min, wkt = "geometry")
  
  Xs <- sf::st_coordinates(df.dates.min$geometry)[, 1]
  Ys <- sf::st_coordinates(df.dates.min$geometry)[, 2]
  # TODO: create a IDF in NeoNet app
  if(!("idf_nn" %in% colnames(df.dates.min))){
    idf_nn <- 1:nrow(df.dates.min)
  }
  df <- data.frame(site = df.dates.min$SiteName,
                   idf = idf_nn, # TODO: change once idf_nn exists
                   longitude = Xs,
                   latitude = Ys, 
                   median = df.dates.min$median)
  contour_levels <- seq(min(df$median), max(df$median), by = 250)
  # TODO: handle duplicated
  # duplicated(df[ , c("longitude", "latitude")])
  interpolated <- interp::interp(df$longitude, 
                                 df$latitude, 
                                 df$median, 
                                 duplicate = "mean",    #you have duplicated values
                                 output = "grid")
  #convert this to a long form dataframe
  interp_df <- tidyr::expand_grid(i = seq_along(interpolated$x), 
                                  j = seq_along(interpolated$y)) %>% 
    dplyr::mutate(lon = interpolated$x[i],
                  lat = interpolated$y[j],
                  date.med = purrr::map2_dbl(i, j, ~interpolated$z[.x,.y])) %>% 
    dplyr::select(-i, -j)
  # map
  buff <- .1
  bbox <- c(left = min(Xs) - buff, 
            bottom = min(Ys) - buff, 
            right = max(Xs) + buff, 
            top = max(Ys) + buff)
  stamenbck <- ggmap::get_stamenmap(bbox, 
                                    zoom = 5,
                                    maptype = "terrain-background")
  map <- ggmap::ggmap(stamenbck, darken = c(.2, "white")) + 
    # TODO: add bbox on map to show the studied area
    ggplot2::geom_contour(data = interp_df, 
                          ggplot2::aes(x = lon, y = lat, z = date.med, 
                                       color = ..level..
                                       # colour = ggplot2::after_stat(level)
                                       ),
                          breaks = contour_levels) +
    metR::geom_text_contour(data = interp_df,
                            binwidth = 250,
                            ggplot2::aes(x = lon, y = lat, z = date.med),
                            size = 2) +
    ggplot2::geom_point(data = df, 
                        ggplot2::aes(x = longitude, y = latitude), col = "blue") +
    ggrepel::geom_text_repel(data = df, 
                             ggplot2::aes(x = longitude, y = latitude, label = idf),
                             size = 2,
                             max.overlaps = Inf) +
    ggplot2::scale_color_gradient(low = "#000000", high = "#FFAAAA")
  if(export){
    # print(paste0("After subset on Periods ", paste0(select.periods, collapse = ", "),": ", nrow(df.dates), " dates"))
    if(is.na(mapname)){
      mapname <- DescTools::SplitPath(df.c14)$filename
    }
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
  }
  shell.exec(outFile)
}

