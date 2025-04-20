#' Crops a KCC map using a ROI
#' 
#' @name neo_kcc_crop
#' 
#' @description Crops KCC map using a ROI
#'
#' @param kcc A KCC GeoTiff.
#' @param roi The path to a GeoJSON object or a xmin, xmax, ymin, ymax MBR
#'
#' @return A terra object
#'
#' @examples
#' 
#' # Calculate with a GeoJSON ROI
#' map.select <- neo_kcc_crop(kcc = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_7k.tif",
#'                            roi = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi-midi-france.geojson")
#'                            
#' # Calculate with a MBR ROI
#' map.select <- neo_kcc_crop(kcc = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_7k.tif",
#'                            roi = c(-10, 35, 19, 45))
#' 
#' # Plot
#' map.select <- neo_kcc_crop()
#' terra::plot(map.select)
#' 
#' # and Save
#' map.select <- neo_kcc_crop()
#' terra::writeRaster(map.select,
#'                    "kcc_crop.tif", 
#'                    datatype = "INT1U",
#'                    overwrite = TRUE)
#' @export
neo_kcc_crop <- function(kcc = NA,
                         roi = NA,
                         verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  if(inherits(roi, "character")){
    if(verbose){
      print("Reads an sf object")
    }
    roi <- sf::st_read(roi, quiet = TRUE)
  }
  if(inherits(roi, "numeric")){
    if(verbose){
      print(paste0("Reads a bounding box"))
    }
    xmin <- roi[1] ; ymin <- roi[2] ; xmax <- roi[3] ; ymax <- roi[4]
    polygon <- sf::st_polygon(list(matrix(c(xmin, ymin,
                                            xmax, ymin,
                                            xmax, ymax,
                                            xmin, ymax,
                                            xmin, ymin), 
                                          ncol = 2, byrow = TRUE)))
    roi <- sf::st_sf(geometry = sf::st_sfc(polygon, crs = 4326))
  }
  map <- terra::rast(kcc)
  map.select <- terra::crop(map, roi)
  return(map.select)
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

