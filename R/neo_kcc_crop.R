#' Crops a KCC map using a ROI
#' 
#' @name neo_kcc_crop
#' 
#' @description Crops KCC map using a ROI
#'
#' @param kcc A KCC GeoTiff.
#' @param roi A sf object (optional).
#'
#' @return A terra object
#'
#' @examples
#' 
#' # Calculate
#' map.select <- neo_kcc_crop(kcc = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_7k.tif",
#'                            roi = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi-midi-france.geojson")
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
  roi <- sf::st_read(roi, quiet = TRUE)
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

