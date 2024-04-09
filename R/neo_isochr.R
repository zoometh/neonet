#' @name neo_isochr
#'
#' @description Creates isochrones contours by interpolation of calibrated radiocarbon dates. Select the date with the minimum median in each site. Need to read config.R to get the `kcc_colors` values
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @selected.per the period selected. Default "EN".
#' @param calibrate if TRUE (default) will calibrate dates using the neo_calib() function.
#' @param isochr.subset Default NA. Else: a unique date BC to plot only this isochrone (ex: -6000) in BC.
#' @param kcc.file a basemap KCC, ideally compliant with `isochr.subset`. If NA (default), will use a `rnaturalearth` basemap. Either a path to the GeoTiff, or a SpatRaster.
#' @param time.interv Time interval between two isochrones (bins), in years. Default: 250.
#' @param coloramp the name of the coloramps to use on contour, for the Neolithic dates and mesolithic dates. Default: c("Reds", "Blues"). 
#' @param lbl.dates show the sites identifiers (default: TRUE)
#' @param map.longest.size the longest size of the output map (height or width) in cm. The smallest size will be calculated from it. Only useful if if export = TRUE. Default: 15
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list with ggplot ($map) and a dataframe ($data)
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
                       isochr.subset = NA,
                       kcc.file = NA,
                       time.line.size = 1,
                       buff = .1,
                       shw.dates = TRUE,
                       lbl.dates = FALSE,
                       lbl.dates.size = 2,
                       lbl.time.interv = FALSE,
                       lbl.time.interv.size = 3,
                       coloramp = c("Reds", "Blues"),
                       mapname = "Isochrones",
                       verbose = TRUE){
  # TODO: median or mean
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  ## dates
  # check which periods have been selected
  neolithic <- selected.per %in% c("EN", "EMN", "MN", "LN", "UN")
  mesolithic <- !neolithic
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
                     period = df.dates.min$Period,
                     median = df.dates.min$median)
  } else {
    df <- data.frame(sourcedb = df.dates.min$sourcedb,
                     site = df.dates.min$SiteName,
                     idf = df.dates.min$idf_nn,
                     labcode = df.dates.min$LabCode,
                     longitude = Xs,
                     latitude = Ys, 
                     period = df.dates.min$Period,
                     median = df.dates.min$median)
  }
  # contour_levels <- seq(min(df$median), max(df$median), by = time.interv)
  # TODO: do the same on weighted medians
  if(is.na(isochr.subset)){
    contour_levels <- seq(min(df$median), max(df$median), by = time.interv)
    print(contour_levels)
  }
  if(is.numeric(isochr.subset)){
    contour_levels <- isochr.subset
  }
  #############################
  # TODO: handle duplicated
  #############################
  #
  # duplicated(df[ , c("longitude", "latitude")])
  # Assuming 'points' is your dataset
  # df.1 <- df[!duplicated(df), ]
  ## to avoid error #######
  # >   interpolated <- interp::interp(x = df$longitude,
  #                                    +                                  y = df$latitude,
  #                                    +                                  z = df$median,
  #                                    +                                  duplicate = "median", 
  #                                    +                                  output = "grid")
  # Error: shull: duplicate points found
  
  # df_unique <- df %>%
  #   dplyr::group_by(longitude, latitude) %>%
  #   dplyr::summarise(median = median(median), .groups = 'drop')
  # 
  # # Now use df_unique with interp
  # interpolated <- interp::interp(x = df_unique$longitude, 
  #                                y = df_unique$latitude, 
  #                                z = df_unique$median, 
  #                                output = "grid")
  # 
  
  # set.seed(123)  # for reproducibility
  # df$longitude <- jitter(df$longitude, .001)
  # df$latitude <- jitter(df$latitude, .001)
  # df <- df[!duplicated(df[c("longitude", "latitude")]),]
  ################################################################
  
  
  # interpolated <- interp::interp(x = df$longitude, 
  #                                y = df$latitude, 
  #                                z = df$median, 
  #                                duplicate = "mean", # 'strip' is also not working / duplicated values
  #                                output = "grid")
  # interpolated <- akima::interp(x = df$longitude, 
  #                               y = df$latitude, 
  #                               z = df$median, 
  #                               duplicate = "mean")
  
  source("R/neo_isochr_inter.R")
  interp_df <- neo_isochr_inter(df)
  
  
  # ## Export the interpolated grid to detect the edges ##########
  # # write.table(interpolated$z, 
  # #             "C:/Rprojects/neonet/doc/data/interpolated_isochr.csv",
  # #             row.names = F)
  # #############################################################
  # # convert this to a long form dataframe
  # interp_df <- tidyr::expand_grid(i = seq_along(interpolated$x), 
  #                                 j = seq_along(interpolated$y)) %>% 
  #   dplyr::mutate(lon = interpolated$x[i],
  #                 lat = interpolated$y[j],
  #                 date.med = purrr::map2_dbl(i, j, ~interpolated$z[.x, .y])) %>% 
  #   dplyr::select(-i, -j)
  # # rm interpolated results having NA
  # interp_df <- na.omit(interp_df)
  
  
  # colors. rm the lightest colors
  nb.contours <- length(contour_levels)
  if(nb.contours > 1){
    if(neolithic){
      myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp[1]))(nb.contours + 5)
    } else {
      myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp[2]))(nb.contours + 5)
    }
    myPalette <- myPalette[-c(1:5)]
  } else {
    myPalette <- "black"
  }
  # map
  bbox <- c(left = min(Xs) - buff, 
            bottom = min(Ys) - buff, 
            right = max(Xs) + buff, 
            top = max(Ys) + buff)
  ## basemap
  if(is.na(kcc.file)){
    if(verbose){
      print(paste0("Will use a neutral basemap (rnaturalearth)"))
      kcc.info <- "natural earth"
    }
    world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")}
  if(is(kcc.file, "SpatRaster")) {
    if(verbose){
      print(paste0("Will use a KCC basemap (SpatRaster)"))
    }
    # not tested
    world <- st_as_sf(kcc.file, coords = c("x", "y"), crs = st_crs(kcc.file))
    orig.file <- terra::sources(kcc.file, nlyr=FALSE, bands=FALSE)
    kcc.info <- DescTools::SplitPath(orig.file)$filename
    # world <- st_as_sf(kcc_geo, coords = c("x", "y"), crs = st_crs(kcc_geo))
  }
  if(is.character(kcc.file)){
    if(verbose){
      print(paste0("Will use a KCC basemap (reading a GeoTiff)"))
    }
    kcc_geo <- terra::rast(kcc.file)
    raster_df <- terra::as.data.frame(kcc_geo, xy = TRUE)
    kcc.info <- DescTools::SplitPath(kcc.file)$filename
    # 
    # raster_df <- terra::as.data.frame(raster, xy = TRUE, na.rm = TRUE)
    # world <- st_as_sf(raster_df, coords = c("x", "y"), crs = st_crs(raster))
  }
  # tit
  periods <- paste0(unique(df.dates$Period), collapse = " ")
  if(neolithic){
    tit <- paste("Neolithic")
    if(nb.contours < 3){
      subtit <- paste0("Isochrones: ", paste0(as.character(abs(contour_levels)), collapse = ", "), " BC")
    }
    capt <- paste0(periods, " | isochrones on the earliest medians | ",
                   nrow(df), " medians from ", nb.dates.tot, " calibrated dates BC\n",
                   "Map: ", kcc.info, " (BP)")
  } else {
    tit <- paste("Mesolithic")
    if(nb.contours < 3){
      subtit <- paste0("Isochrones: ", paste0(as.character(abs(contour_levels)), collapse = ", "), " BC")
    }
    capt <- paste0(periods, " | isochrones on the latest medians | ",
                   nrow(df), " medians from ", nb.dates.tot, " calibrated dates BC\n",
                   "Map: ", kcc.info, " (BP)")
  }
  # create map
  if(is.na(kcc.file)){
    map <- ggplot2::ggplot(world) +
      ggplot2::geom_sf(color = '#595959', fill = "white")
  } else {
    map <- ggplot2::ggplot() +
      ggplot2::geom_raster(data = raster_df, ggplot2::aes(x = x, y = y, fill = factor(code))) + 
      ggplot2::scale_fill_manual(values = kcc_colors)
  }
  # 
  # gout <- ggplot2::ggplot() +
  #   ggplot2::geom_raster(data = raster_df, ggplot2::aes(x = x, y = y, fill = factor(code))) + 
  #   ggplot2::scale_fill_manual(values = kcc_colors) +  # Map fill colors using color_vector
  #   # ggplot2::geom_sf(data = df.c14, color = "black", size = 0.5) +  # Add the sf object
  #   # ggplot2::coord_sf() +  # Use coordinate system from sf object
  #   ggplot2::labs(fill = "Climate Code") + # Optional: add a legend title
  #   ggplot2::coord_sf(xlim = c(min(Xs) - buff, max(Xs) + buff),
  #                     ylim = c(min(Ys) - buff, max(Ys) + buff)) +
  #   # ggplot2::coord_sf(xlim = c(roi$xmin, roi$xmax), ylim = c(roi$ymin, roi$ymax)) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(legend.position = "none")
  
  map <- map +
    ggplot2::coord_sf(xlim = c(min(Xs) - buff, max(Xs) + buff),
                      ylim = c(min(Ys) - buff, max(Ys) + buff)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) +
    ggplot2::geom_contour(data = interp_df, 
                          ggplot2::aes(x = lon, y = lat, z = date.med, 
                                       # color = ..level..
                                       color = ggplot2::after_stat(level)
                          ),
                          # breaks = contour_levels,
                          breaks = contour_levels,
                          linewidth = time.line.size) +
    # ggplot2::geom_contour(data = interp_df, 
    #                       ggplot2::aes(x = lon, y = lat, z = date.med, 
    #                                    # color = ..level..
    #                                    color = ggplot2::after_stat(level)
    #                       ),
    #                       linewidth = time.line.size,
    #                       breaks = contour_levels) +
    ggplot2::scale_color_gradientn(colours = rev(myPalette),
                                   name = "Cal BC") +
    ggplot2::labs(title = tit,
                  subtitle = subtit,
                  caption = capt)
  # ggplot2::scale_color_gradient(low = "#000000", high = "#FFAAAA")
  if(lbl.time.interv){
    if(is.na(isochr.subset)){
      # all contours
      map <- map +
        metR::geom_text_contour(data = interp_df,
                                binwidth = time.interv,
                                ggplot2::aes(x = lon, y = lat, z = date.med, colour = ..level..),
                                skip = 0,
                                rotate = TRUE,
                                stroke = .2,
                                size = lbl.time.interv.size)
    } else {
      # only one selected contour
      contour_data <- ggplot2::ggplot_build(ggplot2::ggplot(interp_df, ggplot2::aes(x = lon, y = lat, z = date.med)) + 
                                              ggplot2::geom_contour(breaks = isochr.subset))$data[[1]]
      # one label by countour
      contour_data_lbl <- contour_data %>%
        dplyr::group_by(group) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
      map <- map + 
        ggplot2::geom_contour(data = interp_df, ggplot2::aes(x = lon, y = lat, z = date.med), 
                              breaks = isochr.subset, colour = "black") +
        ggplot2::geom_text(data = contour_data_lbl, ggplot2::aes(x = x, y = y, label = sprintf("%.0f", level)),
                           size = 3, colour = "black")
    }
  }
  if(shw.dates){
    if(verbose){
      print(paste0("Add dates"))
    }
    if(is.na(isochr.subset)){
      map <- map +
        ggplot2::geom_point(data = df, 
                            ggplot2::aes(x = longitude, y = latitude), 
                            col = "black",
                            alpha = .5,
                            size = 1)
      if(lbl.dates){
        map <- map +
          ggrepel::geom_text_repel(data = df, 
                                   ggplot2::aes(x = longitude, y = latitude, label = idf),
                                   size = lbl.dates.size,
                                   segment.alpha = .3,
                                   segment.size = .3,
                                   max.overlaps = Inf)
      }
    } else {
      if(neolithic){
        # only plot medians older than 
        df.isochr.subset <- df[df[["median"]] < isochr.subset, ]
      }
      if(mesolithic){
        df.isochr.subset <- df[df[["median"]] > isochr.subset, ]
      }
      map <- map +
        ggplot2::geom_point(data = df.isochr.subset, 
                            ggplot2::aes(x = longitude, y = latitude), 
                            col = "black",
                            alpha = .5,
                            size = 1)
      if(lbl.dates){
        map <- map +
          ggrepel::geom_text_repel(data = df.isochr.subset, 
                                   ggplot2::aes(x = longitude, y = latitude, label = idf),
                                   size = lbl.dates.size,
                                   segment.alpha = .3,
                                   segment.size = .3,
                                   max.overlaps = Inf)
      }
    }
  }
  map <- map +
    ggplot2::theme(legend.position = "none")
  outData <- list(data = df, map = map)
  return(outData)
}

# library(rcarbon)
# source("R/neo_spd.R")
# source("R/neo_calib.R")
# myc14data <- "C:/Rprojects/neonet/doc/presentation/bhdc/data/neonet-data-2023-10-07.geojson"
# neo_isochr(df.c14 = myc14data, lbl.time.interv = FALSE, selected.per = ("LM"),
#            bck.alpha = .3, time.line.size = 1, export = T, outDir = myc14data <- "C:/Rprojects/neonet/doc/presentation/bhdc/data/")
