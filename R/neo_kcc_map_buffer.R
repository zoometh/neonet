#' Creates a KCC map from a polygon (ex: Mediterranean sea) and a buffer (ex: 200 km)
#' 
#' 
#' @name neo_kcc_map_buffer
#' 
#' @description Use the buffer and the polygon to 'clip' the original KCC maps. For example: 300 km from the Mediterranean coastline
#'
#' @param buffer_km A path toA buffer in km. 
#' @param med.sea A path to a GeoJSON file
#' @param root.path A root path.
#' @param kcc.file A list of KCC maps
#'
#' @return A ggplot
#'
#' @examples
#' 
#' # export the maps
#' kcc_clipped_list <- neo_kcc_map_buffer(buffer_km = 300)
#' for(i in seq(1, length(kcc_clipped_list))){
#'   plot(kcc_clipped_list[[i]])
#'   print(names(kcc_clipped_list[i]))
#'   output_file <- paste0("C:/Rprojects/neonet/doc/talks/2024-simep/data/", names(kcc_clipped_list[i]))
#'   terra::writeRaster(kcc_clipped_list[[i]], output_file, overwrite=TRUE)
#' }
#'
#' @export
neo_kcc_map_buffer <- function(buffer_km = 500, 
                               med.sea = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/med_sea.geojson",
                               root.path = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/",
                               kcc.file = c("koppen_11k.tif", "koppen_10k.tif", "koppen_9k.tif",
                                            "koppen_8k.tif", "koppen_7k.tif", "koppen_6k.tif")){
  med.coast <- sf::st_read(med.sea,
                           quiet = TRUE)
  ll <- list()
  # Define the raw GitHub URL to the .tif file
  # koppen_8k <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_8k.tif"
  for (kcc in kcc.file){
    temp_file <- tempfile(fileext = ".tif")
    koppen_k <- paste0(root.path, kcc)
    download.file(koppen_k, destfile = temp_file, mode = "wb")
    what <- terra::rast(temp_file)
    buffer <- sf::st_buffer(med.coast, dist = buffer_km*1000)  # in meters to km
    buffer_vect <- terra::vect(buffer)
    what_clipped <- terra::mask(what, buffer_vect)
    what_clipped <- terra::crop(what_clipped, buffer_vect)
    ll[[length(ll)+1]] <- what_clipped
    # output_file <- paste0("koeppen_", ky, ".tif")
    names(ll)[length(ll)] <- kcc
  }
  # plot(what_clipped)
  unlink(temp_file)
  return(ll)
}


