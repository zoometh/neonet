#' @name neo_isochr
#'
#' @description creates isochrones contours by interpolation of calibrated radiocarbon dates. Select the date with the minimum median in each site.
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @selected.per the period selected. Default "EN".
#' @param calibrate if TRUE (default) will calibrate dates using the neo_calib() function.
#' @param time.interv time interval between two isochrones, in years. Default: 250.
#' @param coloramp the name of the coloramps to use on contour, for the Neolithic dates and Paleolithic dates. Default: c("Reds", "Blues"). 
#' @param lbl.dates show the sites identifiers (default: TRUE)
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
                       selected.per = c("EN"),
                       # max.sd = 100,
                       calibrate = TRUE,
                       time.interv = 250,
                       time.susbet = NA,
                       time.line.size = 1,
                       buff = .1,
                       shw.dates = TRUE,
                       lbl.dates = FALSE,
                       lbl.dates.size = 2,
                       lbl.time.interv = FALSE,
                       lbl.time.interv.size = 3,
                       coloramp = c("Reds", "Blues"),
                       mapname = "Isochrones",
                       export = FALSE,
                       outDir = "C:/Rprojects/neonet/results/",
                       map.longest.size = 15,
                       verbose = TRUE){
  # library(tidyverse)
  # library(interp)
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  # check which periods have been selected
  neolithic <- selected.per %in% c("EN", "EMN", "MN", "LN", "UN")
  paleolithic <- !neolithic
  if(is.character(df.c14)){
    df.dates <- sf::st_read(df.c14, quiet = T)
    # title/filename
    if(is.na(mapname)){
      mapname <- DescTools::SplitPath(df.c14)$filename
    }
  }
  if(inherits(df.c14, "sf")){
    df.dates <- df.c14
  }
  if(is.data.frame(df.c14)){
    df.dates <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
  }
  nb.dates.tot <- nrow(df.dates)
  if(verbose){
    print(paste0("Original file: ", nb.dates.tot, " dates"))
  }
  # subset on periods
  df.dates <- df.dates[df.dates$Period %in% selected.per, ]
  if(verbose){
    print(paste0("After subsetting Periods on '",
                 paste0(selected.per, collapse = ", "),"': ",
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
  # Early Neolithic (EN): Select the row with the minimum median in each site
  # not.EN <- rownames(df.dates[df.dates$Period != "EN", ])
  # rownames(df.dates[not.EN, ]) <- NULL
  # df.dates <- df.dates[df.dates$Period == "EN", ]
  if(calibrate){
    df.dates <- neo_calib(as.data.frame(df.dates))
  }
  if(neolithic){
    df.dates.min <- df.dates %>% 
      dplyr::group_by(SiteName) %>% 
      dplyr::slice_min(median)
  } else {
    df.dates.min <- df.dates %>% 
      dplyr::group_by(SiteName) %>% 
      dplyr::slice_max(median)
  }
  if(verbose){
    print(paste0("Medians of calibrated dates by site selected: ",
                 nrow(df.dates.min), " dates"))
  }
  # to sf
  # df.dates.min$geometry <- sf::st_as_text(df.dates.min$geometry)
  # df.dates.min <- sf::st_as_sf(df.dates.min, wkt = "geometry")
  
  Xs <- sf::st_coordinates(df.dates.min$geometry)[, 1]
  Ys <- sf::st_coordinates(df.dates.min$geometry)[, 2]
  
  # TODO: create a IDF in NeoNet app
  if(!("idf_nn" %in% colnames(df.dates.min))){
    if(verbose){
      print(paste0("Creates the column new unique IDs"))
    }
    idf_nn <- 1:nrow(df.dates.min)
    df <- data.frame(sourcedb = df.dates.min$sourcedb,
                     site = df.dates.min$SiteName,
                     idf = idf_nn,
                     labcode = df.dates.min$LabCode,
                     longitude = Xs,
                     latitude = Ys, 
                     median = df.dates.min$median)
  } else {
    df <- data.frame(sourcedb = df.dates.min$sourcedb,
                     site = df.dates.min$SiteName,
                     idf = df.dates.min$idf_nn,
                     labcode = df.dates.min$LabCode,
                     longitude = Xs,
                     latitude = Ys, 
                     median = df.dates.min$median)
  }
  if(is.na(time.susbet)){
    contour_levels <- seq(min(df$median), max(df$median), by = time.interv)
    print(contour_levels)
  }
  if(is.numeric(time.susbet)){
    contour_levels <- seq(min(df$median), max(df$median), by = time.interv)
  }
  # TODO: handle duplicated
  # duplicated(df[ , c("longitude", "latitude")])
  interpolated <- interp::interp(x = df$longitude, 
                                 y = df$latitude, 
                                 z = df$median, 
                                 duplicate = "mean", # duplicated values
                                 output = "grid")
  # convert this to a long form dataframe
  interp_df <- tidyr::expand_grid(i = seq_along(interpolated$x), 
                                  j = seq_along(interpolated$y)) %>% 
    dplyr::mutate(lon = interpolated$x[i],
                  lat = interpolated$y[j],
                  date.med = purrr::map2_dbl(i, j, ~interpolated$z[.x, .y])) %>% 
    dplyr::select(-i, -j)
  # colors. rm the lightest colors
  nb.contours <- length(contour_levels)
  if(neolithic){
    myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp[1]))(nb.contours + 5)
  } else {
    myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp[2]))(nb.contours + 5)
  }
  myPalette <- myPalette[-c(1:5)]
  # tit
  periods <- paste0(unique(df.dates$Period), collapse = " ")
  if(neolithic){
    tit <- paste("Neolithic")
    capt <- paste0(periods, " | isochrones on the earliest medians | ",
                   nrow(df), " medians from ", nb.dates.tot, " calibrated dates")
  } else {
    tit <- paste("Mesolithic")
    capt <- paste0(periods, " | isochrones on the latest medians | ",
                   nrow(df), " medians from ", nb.dates.tot, " calibrated dates")
  }
  # map
  bbox <- c(left = min(Xs) - buff, 
            bottom = min(Ys) - buff, 
            right = max(Xs) + buff, 
            top = max(Ys) + buff)
  world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  map <- ggplot2::ggplot(world) +
    ggplot2::geom_sf(color = 'black') + #fill = '#D3D3D3', 
    ggplot2::coord_sf(xlim = c(min(Xs) - buff, max(Xs) + buff),
                      ylim = c(min(Ys) - buff, max(Ys) + buff)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) +
    # ggplot2::ggtitle(tit) +
    ggplot2::geom_contour(data = interp_df, 
                          ggplot2::aes(x = lon, y = lat, z = date.med, 
                                       # color = ..level..
                                       color = ggplot2::after_stat(level)
                          ),
                          linewidth = time.line.size,
                          breaks = contour_levels) +
    ggplot2::scale_color_gradientn(colours = rev(myPalette),
                                   name = "Cal BC") +
    ggplot2::labs(title = tit,
                  caption = capt)
  # ggplot2::scale_color_gradient(low = "#000000", high = "#FFAAAA")
  if(lbl.dates){
    map <- map +
      ggrepel::geom_text_repel(data = df, 
                               ggplot2::aes(x = longitude, y = latitude, label = idf),
                               size = lbl.dates.size,
                               segment.alpha = .3,
                               segment.size = .3,
                               max.overlaps = Inf)
  }
  if(lbl.time.interv){
    map <- map +
      metR::geom_text_contour(data = interp_df,
                              binwidth = time.interv,
                              ggplot2::aes(x = lon, y = lat, z = date.med, colour = ..level..),
                              skip = 0,
                              rotate = TRUE,
                              stroke = .2,
                              size = lbl.time.interv.size)
  }
  if(shw.dates){
    map <- map +
      ggplot2::geom_point(data = df, 
                          ggplot2::aes(x = longitude, y = latitude), 
                          col = "darkgrey",
                          alpha = .5,
                          size = 1)
  }
  if(export){
    # print(paste0("After subset on Periods ", paste0(select.periods, collapse = ", "),": ", nrow(df.dates), " dates"))
    if(neolithic){
      outFile <- paste0(outDir, mapname, "-neolithic-isochr.png")
    } else {
      outFile <- paste0(outDir, mapname, "-paleolithic-isochr.png")
    }
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
      print(paste0("Map '", outFile,"' has been exported to '", outDir, "'"))
    }
  } else {
    outData <- list(data = df, map = map)
    return(outData)
  }
}

# library(rcarbon)
# source("R/neo_spd.R")
# source("R/neo_calib.R")
# myc14data <- "C:/Rprojects/neonet/doc/presentation/bhdc/data/neonet-data-2023-10-07.geojson"
# neo_isochr(df.c14 = myc14data, lbl.time.interv = FALSE, selected.per = ("LM"),
#            bck.alpha = .3, time.line.size = 1, export = T, outDir = myc14data <- "C:/Rprojects/neonet/doc/presentation/bhdc/data/")
