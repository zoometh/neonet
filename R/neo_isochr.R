#' @name neo_isochr
#'
#' @description Creates isochrones contours by interpolation of calibrated radiocarbon dates from one date (median) by site. If the selected period (`selected.per`) is EN (Early Neolithic) then the minimum median in each site is selected (i.e. the most ancien Early Neolithic date). At the opposite, in the selected period (`selected.per`) is LM (Late Mesolithic) then the maximum median in each site is selected (i.e. the most recent Late Mesolithic date). Need to read config.R to get the `kcc_colors` values
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @param selected.per the period selected. Default "EN".
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param where An area to limit the analysis. Can be an sf dataframe, a GeoJSON path, a bounding box (xmin, ymin, xmin, xmax). Default NA.
#' @param calibrate if TRUE (default: FALSE) will calibrate dates using the neo_calib() function.
#' @param isochr.subset Default NA. Else: a unique date BC to plot only this isochrone (ex: -6000) in BC.
#' @param largest.isochr If TRUE (Default: FALSE), will only show the largerst isochrone to avoid small closed lines.
#' @param kcc.file a basemap KCC, ideally compliant with `isochr.subset`. If NA (default), will use a `rnaturalearth` basemap. Either a path to the GeoTiff (using its path), or a SpatRaster object.
#' @param is.other.geotiff To display another Geotiff. Default: FALSE.
#' @param time.interv Time interval between two isochrones (bins), in years. Default: 250.
#' @param coloramp the name of the coloramps to use on contour, for the Neolithic dates and mesolithic dates. Default: c("Reds", "Blues"). 
#' @param lbl.dates show the sites identifiers (default: TRUE)
#' @param size.date size of the date label (default: 2)
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
neo_isochr <- function(df.c14 = NA, # "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-23.geojson",
                       selected.per = c("EN"),
                       ref.period = "https://raw.githubusercontent.com/zoometh/neonet/refs/heads/main/inst/extdata/periods.tsv",
                       where = NA,
                       max.sd = 101,
                       calibrate = FALSE,
                       time.interv = 250, # Useful?
                       isochr.subset = NA,
                       largest.isochr = FALSE,
                       kcc.file = NA,
                       is.other.geotiff = FALSE,
                       isochr.line.color = 'black',
                       isochr.line.size = 1,
                       isochr.txt.size = 3,
                       buff = .1,
                       size.date = 1,
                       shw.dates = TRUE,
                       show.all.dates = FALSE,
                       color.dates = "black",
                       alpha.dates = .5,
                       lbl.dates = FALSE,
                       lbl.date.field = "idf",
                       lbl.dates.size = 2,
                       lbl.time.interv = FALSE,
                       lbl.time.interv.size = 3,
                       coloramp = c("Reds", "Blues"),
                       mapname = "Isochrones",
                       create.legend = FALSE,
                       create.topomap = FALSE,
                       verbose = TRUE){
  # TODO: median or mean
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  ## dates
  # round to a year
  df.c14$median <- round(df.c14$median, 0)
  # check which periods have been selected
  neolithic <- selected.per %in% c("EN", "EMN", "MN", "LN", "UN")
  mesolithic <- !neolithic
  # TODO: periods colors
  # periods.colors <- read.csv(ref.period, sep = "\t")
  # periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
  isochr.txt.colour <- isochr.line.color
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
  ############ subset on spatial ############ 
  if(inherits(where, "character")){
    if(verbose){
      print(paste0("Spatial window on a new roi: '", 
                   paste0("[", paste0(obj.case[[3]], collapse = ", "),"]") ,
                   "', bounding box object"))
    }
    where <- sf::st_read(where,
                         quiet = TRUE)
    inside <- sf::st_within(df.dates, where, sparse = FALSE)
    df.dates <- df.dates[inside, ]
  }
  if(inherits(where, "sf")){
    if(verbose){
      print(paste0("Spatial subset on new ROI, sf object"))
    }
    inside <- sf::st_within(df.dates, where, sparse = FALSE)
    df.dates <- df.dates[inside, ]
  }
  if(inherits(where, "numeric")){
    if(verbose){
      print(paste0("Spatial subset on new ROI, bounding box"))
    }
    xmin <- where[1] ; ymin <- where[2] ; xmax <- where[3] ; ymax <- where[4]
    polygon <- sf::st_polygon(list(matrix(c(xmin, ymin,
                                            xmax, ymin,
                                            xmax, ymax,
                                            xmin, ymax,
                                            xmin, ymin), 
                                          ncol = 2, byrow = TRUE)))
    where <- sf::st_sf(geometry = sf::st_sfc(polygon, crs = 4326))
    inside <- sf::st_within(df.dates, where, sparse = FALSE)
    df.dates <- df.dates[inside, ]
  }
  # Load the sf package
  # library(sf)
  
  # Define the bounding coordinates
  # 
  nb.dates.tot <- nrow(df.dates)
  if(verbose){
    print(paste0("Original file: ", nb.dates.tot, " dates inside the ROI"))
  }
  ############ subset on Periods ############ 
  df.dates <- df.dates[df.dates$Period %in% selected.per, ]
  selected.per.lbl <- paste0(selected.per, collapse = ", ")
  if(verbose){
    print(paste0("After subsetting Periods on '",
                 #paste0(selected.per, collapse = ", "),"': ",
                 selected.per.lbl, "' there are ", 
                 nrow(df.dates), " dates to model"))
  }
  if(nrow(df.dates) == 0){
    stop(paste0("No ", selected.per.lbl, " dates in this geographical area"))
  }
  ############ subset on SD ############ 
  if(!is.na(max.sd)){
    df.dates <- df.dates[df.dates$C14SD < max.sd, ]
    # df_filtered <- df_filtered[df_filtered$C14SD < 101, ]
    if(verbose){
      print(paste0("After subset of SD on < ",
                   max.sd," years: ",
                   nrow(df.dates), " dates to model"))
    }
  }
  # not.EN <- rownames(df.dates[df.dates$Period != "EN", ])
  # rownames(df.dates[not.EN, ]) <- NULL
  # df.dates <- df.dates[df.dates$Period == "EN", ]
  if(calibrate){
    if(verbose){
      print(paste0("Will calibrate ", nrow(df.dates), " dates running 'neo_calib()'"))
    }
    df.dates <- neo_calib(as.data.frame(df.dates))
  }
  ############ EN vs LM median selection ############
  if(neolithic){
    df.dates.min <- df.dates %>% 
      dplyr::group_by(SiteName) %>% 
      dplyr::slice_min(median) # here is the most ancient
    n.dates.display <- nrow(df.dates.min[df.dates.min[["median"]] < isochr.subset, ])
  } else {
    df.dates.min <- df.dates %>% 
      dplyr::group_by(SiteName) %>% 
      dplyr::slice_max(median) # here is the most recent
    # 
    n.dates.display <- nrow(df.dates.min[df.dates.min[["median"]] > isochr.subset, ])
  }
  if(verbose){
    print(paste0("Medians of calibrated dates by site selected for interpolation: ",
                 nrow(df.dates.min), " dates"))
    print(paste0("Unique dates by site to display on the map: ",
                 n.dates.display, " dates"))
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
  # TODO: rm 'test_subset' and 'time.interv' variables. The latters aren't useful. Better the user select his own intervals.
  test_subset <- ifelse(all(is.na(isochr.subset)), FALSE, TRUE)
  if(verbose){
    print(paste0("'test_subset': ", as.character(test_subset)))
  }
  is.none.subset <- ifelse(isochr.subset == "None", TRUE, FALSE)
  # print(is.none.subset)
  # if(is.none.subset){
  if(any(is.none.subset)){
    contour_levels <- 0
  }
  # print(is.none.subset)
  if(!test_subset){
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
  # call Gridded Bivariate Interpolation for Irregular Data
  source("R/neo_isochr_inter.R")
  interp_df <- neo_isochr_inter(df)
  # print(class(interp_df))
  # print(head(interp_df))
  
  
  # ## Export the interpolated grid to detect the edges ##########
  # # write.table(interpolated$z, 
  # #             "C:/Rprojects/neonet/doc/data/interpolated_isochr.csv",
  # #             row.names = F)
  # ##
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
  # isochr.line.color <- isochr.line.color
  if(nb.contours == 0){
    isochr.line.color <- isochr.txt.colour <- NA
  }
  if(nb.contours > 1){
    if(verbose){
      print(paste0("Will plot ", nb.contours, " isochrones"))
    }
    if(neolithic){
      isochr.line.color <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp[1]))(nb.contours + 5)
    } else {
      isochr.line.color <- colorRampPalette(RColorBrewer::brewer.pal(9, coloramp[2]))(nb.contours + 5)
    }
    isochr.line.color <- isochr.line.color[-c(1:5)]
    isochr.txt.colour <- "black"
  } 
  # else {
  #   if(is.na(isochr.line.color)){
  #     if(neolithic){
  #       isochr.line.color <- isochr.txt.colour <- "red"
  #     }
  #     if(!neolithic){
  #       isochr.line.color <- isochr.txt.colour <-  "blue"
  #     }
  #   }
  # }
  ######## map bbox #############
  bbox <- c(left = min(Xs) - buff, 
            bottom = min(Ys) - buff, 
            right = max(Xs) + buff, 
            top = max(Ys) + buff)
  ######## map basemap #############
  if(is.na(kcc.file)){
    if(verbose){
      print(paste0("Basemap: Neutral (rnaturalearth)"))
      basemap.info <- "natural earth"
    }
    world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")}
  if(is(kcc.file, "SpatRaster")) {
    if(verbose){
      print(paste0("Basemap: will use a KCC or another raster (SpatRaster)"))
    }
    # not tested
    world <- st_as_sf(kcc.file, coords = c("x", "y"), crs = st_crs(kcc.file))
    orig.file <- terra::sources(kcc.file, nlyr = FALSE, bands = FALSE)
    basemap.info <- DescTools::SplitPath(orig.file)$filename
    # world <- st_as_sf(kcc_geo, coords = c("x", "y"), crs = st_crs(kcc_geo))
  }
  if(is.character(kcc.file)){
    if(verbose){
      print(paste0("Basemap: will use this GeoTiff: '", kcc.file, "'"))
    }
    kcc_geo <- terra::rast(kcc.file)
    raster_df <- terra::as.data.frame(kcc_geo, xy = TRUE)
    raster_df <- raster_df %>% 
      dplyr::mutate(dplyr::across(c(y, x), ~round(., digits = 4)))
    basemap.info <- DescTools::SplitPath(kcc.file)$filename
    # raster_df <- terra::as.data.frame(raster, xy = TRUE, na.rm = TRUE)
    # world <- st_as_sf(raster_df, coords = c("x", "y"), crs = st_crs(raster))
  }
  ######## map title #############
  periods <- paste0(unique(df.dates$Period), collapse = " ")
  isochrs.lbl <- paste0(as.character(abs(contour_levels)), collapse = ", ")
  subtit <- ""
  if(neolithic){
    tit <- paste("Neolithic (", periods, ")")
    if(nb.contours < 5 & nb.contours > 0){
      subtit <- paste0("Isochrones: ", isochrs.lbl, " BC")
    } else {subtit <- paste0("XXX") }
    capt <- paste0("max SD = ", max.sd," | ", "isochr. on earliest w-medians of ", nrow(df), " dates | ", n.dates.display, " dates older than isochr. ", isochrs.lbl, " BC", " (displayed)\n")
    capt <- paste0(capt, nb.dates.tot, " calibrated dates BC in total | ")
    capt <- paste0(capt, "basemap: ", basemap.info, "")
    
  } else {
    tit <- paste("Mesolithic")
    if(nb.contours < 5 & nb.contours > 0){
      subtit <- paste0("Isochrones: ", isochrs.lbl, " BC")
    }
    capt <- paste0(periods, " | isochrones on the latest w-medians | ")
    capt <- paste0(capt, nrow(df), " w-medians from ", nb.dates.tot, " calibrated dates BC\n",
                   "basemap: ", basemap.info, "")
  }
  ######## map creation #############
  if(is.na(kcc.file)){
    map <- ggplot2::ggplot(world) +
      ggplot2::geom_sf(color = '#7a7a7a', fill = "white")
  }
  if(!is.na(kcc.file)){
    if(!is.other.geotiff){
      world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
      map <- ggplot2::ggplot() +
        # ggplot2::geom_raster(data = raster_df, ggplot2::aes(x = x, y = y, fill = factor(code))) +
        ggplot2::geom_tile(data = raster_df, ggplot2::aes(x = x, y = y, fill = factor(code))) +
        ggplot2::geom_sf(data = world, color = '#7a7a7a', fill = "white") +
        ggplot2::scale_fill_manual(values = kcc_colors)
    }
    if(is.other.geotiff){
      # Create a dataframe from the raster layers
      world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
      raster_df2 <- raster_df
      if(length(names(kcc_geo)) == 1){
        if(verbose){
          print(paste0("   .. Grey scale"))
        }
        # Grey scale / not working
        colnames(raster_df2) <- c("x", "y", "color")
        map <- ggplot2::ggplot() +
          ggplot2::geom_raster(data= raster_df2, ggplot2::aes(x = x, y = y, fill = color)) +
          ggplot2::geom_sf(data = world, color = '#7a7a7a', fill = "white") +
          ggplot2::scale_fill_gradient(low = "black", high = "white")  # Blue for low values, red for high
        # scale_fill_viridis_c() +  # Use a continuous color scale (e.g., Viridis)
        # coord_fixed() +  # Maintain aspect ratio for geospatial data
        # labs(title = "Single-Channel Raster Plot", fill = "Value") +
        # theme_minimal()
      }
      if(length(names(kcc_geo)) == 3){
        if(verbose){
          print(paste0("   .. RGB scale"))
        }
        # RGB
        colnames(raster_df2) <- c("x", "y", "Red", "Green", "Blue")
        # Combine the RGB values into a single color
        raster_df2$color <- with(raster_df2, rgb(Red/255, Green/255, Blue/255))
        # Plot the raster using ggplot2 and geom_raster
        map <- ggplot2::ggplot() +
          # ggplot2::geom_raster(data = raster_df2, ggplot2::aes(x = x, y = y, fill = color)) +
          ggplot2::geom_tile(data = raster_df2, ggplot2::aes(x = x, y = y, fill = color)) +
          ggplot2::geom_sf(data = world, color = '#7a7a7a', fill = "white") +
          ggplot2::scale_fill_identity() #+  # Use the color column as it is
        # coord_fixed() +  # Maintain the aspect ratio
        # theme_minimal()
      }
    }
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
  ######## map crop + title #############
  map <- map +
    ggplot2::coord_sf(xlim = c(min(Xs) - buff, max(Xs) + buff),
                      ylim = c(min(Ys) - buff, max(Ys) + buff)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) +
    # ggplot2::geom_contour(data = interp_df, 
    #                       ggplot2::aes(x = lon, y = lat, z = date.med, 
    #                                    # color = ..level..
    #                                    color = ggplot2::after_stat(level)
    #                       ),
    #                       # breaks = contour_levels,
    #                       breaks = contour_levels,
    #                       linewidth = isochr.line.size) +
    # ggplot2::scale_color_gradientn(colours = rev(isochr.line.color),
    #                                name = "Cal BC") +
    ggplot2::labs(title = tit,
                  subtitle = subtit,
                  caption = capt)
  # ggplot2::scale_color_gradient(low = "#000000", high = "#FFAAAA")
  # if(!is.none.subset){
  #   
  # }
  if(!any(is.none.subset)){
    # print(isochr.line.color)
    # largest.isochr <- TRUE
    if(largest.isochr){
      if(verbose){
        print(paste0("Will only show the largest isochrone"))
      }
      initial_plot <- ggplot2::ggplot(interp_df, 
                                      ggplot2::aes(x = lon, y = lat, z = date.med)) +
        ggplot2::geom_contour(ggplot2::aes(color = ggplot2::after_stat(level)), 
                              breaks = contour_levels)
      # Extract contour lines
      plot_data <- ggplot2::ggplot_build(initial_plot)
      contour_lines <- plot_data$data[[1]]
      # Calculate the size of each contour group
      contour_sizes <- contour_lines %>%
        dplyr::group_by(group) %>%
        dplyr::summarize(size = dplyr::n(), level = dplyr::first(level)) %>%
        dplyr::arrange(desc(size))
      
      # Keep only the largest contour group
      largest_group <- contour_sizes$group[1]
      largest_contour <- contour_lines %>%
        dplyr::filter(group == largest_group)
      # Plot the largest contour
      map <- map +
        ggplot2::geom_path(data = largest_contour, 
                           ggplot2::aes(x = x, y = y, group = group, color = level),
                           linewidth = isochr.line.size,
                           show.legend = FALSE) +
        ggplot2::scale_color_gradientn(colours = rev(isochr.line.color),
                                       name = "Cal BC")
    } else {
      # map <- map +
      map <- map +
        ggplot2::geom_contour(data = interp_df, 
                              ggplot2::aes(x = lon, y = lat, z = date.med, 
                                           # color = ..level..
                                           color = ggplot2::after_stat(level)
                              ),
                              breaks = contour_levels,
                              linewidth = isochr.line.size) +
        ggplot2::scale_color_gradientn(colours = rev(isochr.line.color),
                                       name = "Cal BC")
    }
  }
  if(lbl.time.interv){
    if(verbose){
      print(paste0("Label isochrone lines"))
    }
    if(!test_subset & !any(is.none.subset)){
      # all contours
      map <- map +
        metR::geom_text_contour(data = interp_df,
                                binwidth = time.interv,
                                ggplot2::aes(x = lon, y = lat, z = date.med, colour = ..level..),
                                skip = 0,
                                rotate = TRUE,
                                stroke = 0.3, 
                                stroke.colour = "white",
                                # size = lbl.time.interv.size,
                                colour = isochr.txt.colour, 
                                size = isochr.txt.size, 
                                fontface = "bold"
        )
    } # else {
    if(test_subset & !any(is.none.subset)){
      if(verbose){
        print(paste0("Nb of isochrones: ", nrow(isochr.subset))) # Wrong, works with more than one contour
      }
      contour_data <- ggplot2::ggplot_build(ggplot2::ggplot(interp_df, 
                                                            ggplot2::aes(x = lon, y = lat, z = date.med)) + 
                                              ggplot2::geom_contour(breaks = isochr.subset))$data[[1]]
      # one label by countour
      
      # print(nrow(contour_data))
      # print(class(contour_data))
      if(nrow(contour_data) > 0){
        # add only if there are contours
        contour_data_lbl <- contour_data %>%
          dplyr::group_by(group) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup()
        map <- map + 
          # ggplot2::geom_contour(data = interp_df,
          #                       ggplot2::aes(x = lon, y = lat, z = date.med),
          #                       breaks = isochr.subset,
          #                       # colour = isochr.line.color) + #"black") +
          #                       colour = "black") +
          ggplot2::geom_text(data = contour_data_lbl, 
                             ggplot2::aes(x = x, y = y, label = sprintf("%.0f", level)),
                             size = isochr.txt.size, 
                             colour = isochr.txt.colour)
        # ggplot2::scale_color_gradientn(colours = rev(isochr.line.color),
        #                                name = "Cal BC")
      }
    }
  }
  if(shw.dates){
    if(verbose){
      print(paste0("Add dates (points) to the map"))
    }
    # print(test_subset)
    if(!test_subset){
      map <- map +
        ggplot2::geom_point(data = df, 
                            ggplot2::aes(x = longitude, y = latitude), 
                            col = color.dates,
                            alpha = alpha.dates,
                            stroke = NA,
                            size = size.date)
      if(lbl.dates){
        if(verbose){
          print(paste0("Add date (points) labels to the map"))
        }
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
      # TODO: show all dates to show case the interpolation method
      # show.all.dates <- TRUE
      if(show.all.dates){
        map <- map +
          ggplot2::geom_point(data = df, 
                              ggplot2::aes(x = longitude, y = latitude), 
                              col = "grey",
                              # alpha = alpha.dates,
                              stroke = NA,
                              size = size.date)
      }
      map <- map +
        ggplot2::geom_point(data = df.isochr.subset, 
                            ggplot2::aes(x = longitude, y = latitude), 
                            col = color.dates,
                            alpha = alpha.dates,
                            stroke = NA,
                            size = size.date)
      if(lbl.dates){
        if(verbose){
          print(paste0("Add date labels to the map"))
          # print(colnames(df))
        }
        # TODO: simplify by using !! in ggplot
        # label = !!label_sym), # 
        # my_label <- "idf"
        # label_sym <- sym(my_label)
        if(lbl.date.field == "idf"){
          map <- map +
            ggrepel::geom_text_repel(data = df.isochr.subset, 
                                     ggplot2::aes(x = longitude, y = latitude, label = idf),
                                     size = lbl.dates.size,
                                     segment.alpha = .3,
                                     segment.size = .3,
                                     max.overlaps = Inf)
        }
        if(lbl.date.field == "median"){
          map <- map +
            ggrepel::geom_text_repel(data = df.isochr.subset, 
                                     ggplot2::aes(x = longitude, y = latitude, label = median),
                                     size = lbl.dates.size,
                                     segment.alpha = .3,
                                     segment.size = .3,
                                     max.overlaps = Inf)
        }
      }
    }
  }
  map <- map +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()
    )
  if(create.legend & !is.na(kcc.file)){
    source("R/neo_kcc_legend.R")
    if(verbose){
      print(paste0("  + call `neo_kcc_legend()` to create a legend"))
    }
    legend <- neo_kcc_legend(df_cc = NA, 
                             kcc.file = kcc.file,
                             where = where,
                             long.legend = TRUE,
                             verbose = verbose)
  }
  if(!is.other.geotiff & !is.na(kcc.file)){
    # = KCC
    # source("R/neo_kcc_extract.R")
    if(verbose){
      print(paste0("Extract koppen classes"))
    }
    names(df)[names(df) == 'longitude'] <- 'lon'
    names(df)[names(df) == 'latitude'] <- 'lat'
    data.df <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
    # kcc_geo <- terra::rast("C:/Rprojects/neonet/doc/data/clim/koppen_11k.tif")
    kcc.list <- terra::extract(kcc_geo, data.df)
    # df <- data.frame(cbind(df, kcc.list))
    df <- data.frame(cbind(df, kcc.list))
    if(neolithic){
      # only plot medians older than
      df <- df[df[["median"]] < isochr.subset, ]
    }
    if(mesolithic){
      df <- df[df[["median"]] > isochr.subset, ]
    }
    # print(nrow(df))
    
    # names(df.isochr.subset)[names(df.isochr.subset) == 'longitude'] <- 'lon'
    # names(df.isochr.subset)[names(df.isochr.subset) == 'latitude'] <- 'lat'
    # print(head(df.isochr.subset))
    # 
    # kcc.file.full <- DescTools::SplitPath(kcc.file)$fullfilename
    # df_cc <- neo_kcc_extract(df.c14 = df.isochr.subset, labcode.col = "labcode", kcc.file = kcc.file.full)
    outData <- list(data = df, map = map, legend = legend, inter = interp_df)
  }
  if(!is.other.geotiff & is.na(kcc.file)){
    # = KCC
    # source("R/neo_kcc_extract.R")
    # if(verbose){
    #   print(paste0("Extract koppen classes"))
    # }
    names(df)[names(df) == 'longitude'] <- 'lon'
    names(df)[names(df) == 'latitude'] <- 'lat'
    data.df <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
    # kcc_geo <- terra::rast("C:/Rprojects/neonet/doc/data/clim/koppen_11k.tif")
    # kcc.list <- terra::extract(kcc_geo, data.df)
    # # df <- data.frame(cbind(df, kcc.list))
    # df <- data.frame(cbind(df, kcc.list))
    if(neolithic){
      # only plot medians older than
      df <- df[df[["median"]] < isochr.subset, ]
    }
    if(mesolithic){
      df <- df[df[["median"]] > isochr.subset, ]
    }
    # print(nrow(df))
    
    # names(df.isochr.subset)[names(df.isochr.subset) == 'longitude'] <- 'lon'
    # names(df.isochr.subset)[names(df.isochr.subset) == 'latitude'] <- 'lat'
    # print(head(df.isochr.subset))
    # 
    # kcc.file.full <- DescTools::SplitPath(kcc.file)$fullfilename
    # df_cc <- neo_kcc_extract(df.c14 = df.isochr.subset, labcode.col = "labcode", kcc.file = kcc.file.full)
    # filename.out <- paste0(deparse(substitute(Italia.when)), paste0(Italia.when, collapse = ""))
    outData <- list(data = df, map = map, inter = interp_df)
  }
  if(is.other.geotiff){
    outData <- list(data = df, map = map)
  }
  if(create.topomap){
    # TODO: create a TOPO map ()
    # Load necessary libraries
    library(basemaps)
    library(ggplot2)
    library(sf)
    
    # Set the map defaults for the basemap
    set_defaults(map_service = "osm", map_type = "topographic")
    
    # Define the new bounding box using xmin, ymin, xmax, ymax
    xmin <- 10
    ymin <- 37
    xmax <- 18
    ymax <- 45
    
    # Create a bounding box object with the defined coordinates (WGS84 - EPSG:4326)
    bbox <- st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = st_crs(4326))
    
    # Convert the bbox to an 'sf' object (sfc) so that it's compatible with ggplot
    ext <- st_as_sfc(bbox)
    
    # Create the ggplot map using the new extent and basemap_gglayer
    ggplot() +
      basemap_gglayer(ext) +     # Use the new extent for the basemap
      scale_fill_identity() +    # Keep the fill scale
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)  # Set the coordinate limits
    
    
    xmin <- 10
    ymin <- 37
    xmax <- 18
    ymax <- 45
    # bbox <- c(xmin, ymin, xmax, ymax)
    bbox <- sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = sf::st_crs(4326))
    ext2 <- sf::st_as_sfc(bbox)
    ext2 <- sf::st_sf(ext2)
    sf::st_geometry(ext2) = "geometry" # rename
    
    library(basemaps)
    library(basemaps)
    data(ext)
    set_defaults(map_service = "osm", map_type = "topographic")
    basemaps::get_maptypes()
    library(ggplot2)
    ggplot() +
      basemap_gglayer(ext2) +
      scale_fill_identity() +
      coord_sf()
    
    # data(ext)
    set_defaults(map_service = "esri", map_type = "world_shaded_relief")
    basemaps::get_maptypes()
    library(ggplot2)
    ggplot() +
      basemap_gglayer(ext) +
      scale_fill_identity() +
      coord_sf()
    
    # world_topo_map, world_shaded_relief, world_imagery
    library(basemaps)
    library(ggplot2)
    data(ext)
    for (esri in basemaps::get_maptypes()$esri){
      set_defaults(map_service = "esri", map_type = esri)
      gmap <- ggplot() +
        ggplot2::ggtitle(paste0("esri-", esri)) +
        basemap_gglayer(ext) +
        scale_fill_identity() +
        coord_sf()
      print(gmap)
    }
    
    
    
    
    library(ggmap) 
    
    nc <- st_read(system.file("shape/nc.shp", package="sf"))
    #> Reading layer `nc' from data source `/home/gilles/R/x86_64-pc-linux-gnu-library/3.4/sf/shape/nc.shp' using driver `ESRI Shapefile'
    #> Simple feature collection with 100 features and 14 fields
    #> geometry type:  MULTIPOLYGON
    #> dimension:      XY
    #> bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
    #> epsg (SRID):    4267
    #> proj4string:    +proj=longlat +datum=NAD27 +no_defs
    nc_map <- get_map(location = "North Carolina, NC", zoom = 7)
    #> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=North+Carolina,+NC&zoom=7&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    #> Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=North%20Carolina,%20NC&sensor=false
    nc_centers <- st_centroid(nc)
    #> Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon =
    #> of_largest_polygon): st_centroid does not give correct centroids for
    #> longitude/latitude data
    
    ggmap::ggmap(nc_map) +
      # ggplot2::geom_sf(data = nc_centers, 
      #                  ggplot2::aes(color = SID79, size = BIR74),
      #         show.legend = "point", inherit.aes = FALSE) +
      # coord_sf(datum = NA) +
      ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      theme_minimal()
    
    
    map <- ggplot2::ggplot() +
      # ggspatial::annotation_map_tile("esri_topo") +
      gg +
      ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      ggplot2::labs(title = "ESRI Topographic Map",
                    subtitle = paste("Bounding box from", xmin, "to", xmax, "Longitude and", ymin, "to", ymax, "Latitude"))
    library(ggplot2)
    library(basemaps)
    
    data(ext)
    set_defaults(map_service = "osm", map_type = "topographic")
    
    gg <- basemaps::basemap_ggplot(ext)
    
    #default ggplot2 behavior
    gg
    
    # # Print the map
    # print(map)
    # library(tidyverse)
    # library(sf)
    # library(ggspatial)
    # 
    # site <- data.frame(longitude = -75.144353, latitude = 39.917631) %>% 
    #   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    # 
    # ggplot2::ggplot(site) +
    #   ggspatial::annotation_map_tile("cartolight") +
    #   ggplot2::geom_sf(size = 5)
  }
  return(outData)
}

# library(rcarbon)
# source("R/neo_spd.R")
# source("R/neo_calib.R")
# myc14data <- "C:/Rprojects/neonet/doc/presentation/bhdc/data/neonet-data-2023-10-07.geojson"
# neo_isochr(df.c14 = myc14data, lbl.time.interv = FALSE, selected.per = ("LM"),
#            bck.alpha = .3, isochr.line.size = 1, export = T, outDir = myc14data <- "C:/Rprojects/neonet/doc/presentation/bhdc/data/")
