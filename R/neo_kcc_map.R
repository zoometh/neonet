#' Creates a KCC map of the research area
#' 
#' @name neo_kcc_map
#' 
#' @description Creates a KCC map of the research area. Need to read config.R to get the `kcc_colors` values
#'
#' @param kcc A KCC GeoTiff.
#' @param df.c14 A sf object. If NA, will create the KCC map without dates
#' @param roi A sf object (optional.
#'
#' @return 
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
                        roi = NA){
  # create a KCC map with dates (df.c14). The latter is a sf dataframe
  cc.ky <- DescTools::SplitPath(kcc)$filename
  kcc_geo <- terra::rast(kcc)
  # plot(kcc_geo)
  # kcc_geo <- raster::raster(kcc)
  raster_df <- terra::as.data.frame(kcc_geo, xy = TRUE)
  roi <- sf::st_bbox(roi)
  tit <- paste0("Dates on the KCC ", cc.ky)
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
  if(!is.na(df.c14)){
    tit <- paste0("Dates on the KCC ", cc.ky)
    gout <- gout +
      ggplot2::ggtitle(tit) +
      ggplot2::geom_sf(data = df.c14, color = "black", size = 0.5) +  # Add the sf object
      ggplot2::coord_sf()  # Use coordinate system from sf object
    # if(!inherits(roi, "sf")){
    #   roi <- sf::st_bbox(df.c14)
    # }
    # if(is.data.frame(df.c14)){
    #   roi <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
    #   roi <- sf::st_bbox(roi)
    # }
  }
  if(is.na(df.c14)){
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

