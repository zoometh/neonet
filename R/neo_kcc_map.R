#' Creates a KCC map of the research area
#' 
#' @name neo_kcc_map
#' 
#' @description Creates a KCC map of the research area. Need to read config.R to get the `kcc_colors` values
#'
#' @param kcc A KCC GeoTiff.
#' @param df.c14 A sf object. If NA, will create the KCC map without dates
#' @param roi A sf object (optional).
#' @param sys.proj A new EPSG for a reprojection. Example: `32633` (= UTM 33N, Central Mediterranean). Default NA
#'
#' @return A ggplot
#'
#' @examples
#' 
#' source("R/neo_kcc_map.R")
#' kcc.per <- "koppen_11k"
#' kcc <- paste0("C:/Rprojects/neonet/doc/data/clim/", kcc.per, ".tif")
#' gout <- neo_kcc_map(kcc = kcc,
#'                     df.c14 = NA,
#'                     roi = where)
#' ggsave(paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/", kcc.per, ".png"),
#'        gout,
#'        width = 8,
#'        height = 5,
#'       units = "cm"
#' )
#'
#' @export
neo_kcc_map <- function(kcc = "C:/Rprojects/neonet/doc/data/clim/koppen_7k.tif",
                        df.c14 = NA,
                        roi = NA,
                        sys.proj = NA,
                        pt.size = .5,
                        lbl.dates = FALSE,
                        lbl.dates.size = 2,
                        verbose = TRUE){
  # create a KCC map with dates (df.c14). The latter is a sf dataframe
  cc.ky <- DescTools::SplitPath(kcc)$filename 
  kcc_geo <- terra::rast(kcc)
  if(is.numeric(sys.proj)){
    if(verbose){
      print(paste0("Map: Reproject to ", as.character(sys.proj)))
    }
    kcc_geo <- terra::project(kcc_geo, paste0("EPSG:", as.character(sys.proj)))
    roi <- sf::st_transform(roi, sys.proj)
  }
  roi <- sf::st_bbox(roi)
  # plot(kcc_geo)
  # kcc_geo <- raster::raster(kcc)
  raster_df <- terra::as.data.frame(kcc_geo, xy = TRUE)
  tit <- paste0("Dates belonging to ", cc.ky, " (KCC)")
  gout <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = raster_df, ggplot2::aes(x = x, y = y, fill = factor(code))) + 
    ggplot2::scale_fill_manual(values = kcc_colors) +  # Map fill colors using color_vector
    # ggplot2::geom_sf(data = df.c14, color = "black", size = 0.5) +  # Add the sf object
    # ggplot2::coord_sf() +  # Use coordinate system from sf object
    ggplot2::labs(fill = "Climate Code", 
                  x = "lon",
                  y = "lat") + # Optional: add a legend title
    ggplot2::coord_sf(xlim = c(roi$xmin, roi$xmax), ylim = c(roi$ymin, roi$ymax)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  if(inherits(df.c14, "sf")){
    if(verbose){
      print(paste0("Dates: Reads a 'sf' dataframe"))
    }
    if(is.numeric(sys.proj)){
      if(verbose){
        print(paste0("Dates:convert the EPSG"))
      }
      df.c14 <- sf::st_transform(df.c14, sys.proj)
    }
    # tit <- paste0("Dates on the KCC ", cc.ky)
    capt <- paste0("Number of dates: ", nrow(df.c14), " | ")
    capt <- paste0(capt, "weighted median intervals: ",
                   abs(round(min(df.c14$median), 0)), " / ",
                   abs(round(max(df.c14$median), 0)), " BC")
    gout <- gout +
      ggplot2::ggtitle(tit) +
      ggplot2::labs(caption = capt) +
      ggplot2::geom_sf(data = df.c14, color = "black", size = pt.size) + # +  # Add the sf object
      # ggplot2::coord_sf(crs = sf::st_crs(sys.proj))  # Use coordinate system from sf object
      ggplot2::coord_sf(xlim = c(roi$xmin, roi$xmax), ylim = c(roi$ymin, roi$ymax))
    
    
    # if(!inherits(roi, "sf")){
    #   roi <- sf::st_bbox(df.c14)
    # }
    # if(is.data.frame(df.c14)){
    #   roi <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
    #   roi <- sf::st_bbox(roi)
    # }
    if(lbl.dates){
      if(!("idf_nn" %in% colnames(df.c14))){
        if(verbose){
          print(paste0("Creates the column new unique IDs"))
        }
        df.c14$idf <- 1:nrow(df.c14)
      }
      # df.c14.wgs84 <- sf::st_transform(df.c14, 4326)
      coords <- sf::st_coordinates(df.c14)
      df.c14$longitude <- coords[ , 1]
      df.c14$latitude <- coords[ , 2]
      gout <- gout +
        ggrepel::geom_text_repel(data = df.c14, 
                                 ggplot2::aes(x = longitude, y = latitude, label = idf),
                                 size = lbl.dates.size,
                                 point.padding = grid::unit(5, "pt"),
                                 segment.alpha = .3,
                                 segment.size = .3,
                                 max.overlaps = Inf)
    }
  }
  if(!inherits(df.c14, "sf")){
    tit <- paste0(cc.ky)
    gout <- gout +
      ggplot2::ggtitle(tit)
  }
  return(gout)
}


# where.roi <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
# what.db <- c("calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis") 
# where <- sf::st_read(where.roi,
#                      quiet = TRUE)
# df.c14.7k <- df.c14[df.c14$median < -4500 & df.c14$median > -5500, ]
# neo_kcc_map(df.c14 = df.c14.7k,
#             kcc = "C:/Rprojects/neonet/doc/data/clim/koppen_7k.tif",
#             roi = where,
#             export = TRUE,
#             fileOut = "neonet_kcc_7k.png" )

