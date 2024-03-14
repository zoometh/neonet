#' @name neo_kcc_create
#'
#' @description Creates a serie of KCC maps with the `pastclim` package.
#'
#' @param from_to A numerical vector with a serie of calendar years in BP (ex: -7000 is equal to 5,000 BC) used to create as many KCC maps.
#' @param present The present date. Default: 2000.
#' @param dataset A dataset to create the KCC maps. Default: Beyer2020. 
#' @param outDir The path to the folder where the GeoTiffs will be exported. 
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Creates one to many GeoTiffs.
#'
#' @examples
#' 
#' outDir <- "C:/Rprojects/neonet/doc/data/clim/"
#' pastclim::set_data_path(path_to_nc = outDir)
#' neo_kcc_create()
#'
#' @export
neo_kcc_create <- function(from_to = c(-11000, -10000, -9000, -8000, -7000, -6000),
                           present = 2000,
                           dataset = "Beyer2020",
                           outDir = "C:/Rprojects/neonet/doc/data/clim/",
                           verbose = TRUE){
  pastclim::download_dataset(dataset = dataset, annual = FALSE, monthly = TRUE)
  for(i in 1:length(from_to)){
    when <- from_to[i]
    ky.bp <- paste0(abs(when) / 1000, "k")
    ky.bc <- abs(when) - present
    if(verbose){
      print(paste0("*read: ", ky.bp, " BP (", ky.bc," BC)"))
    }
    # extract monthly temperature and precipitation
    tavg <- pastclim::region_slice(
      time_bp = when,
      bio_variables = c(paste0("temperature_0", 1:9),
                        paste0("temperature_", 10:12)),
      dataset = dataset
    )
    prec <- pastclim::region_slice(
      time_bp = when,
      bio_variables = c(paste0("precipitation_0", 1:9),
                        paste0("precipitation_", 10:12)),
      dataset = dataset
    )
    # create the koeppen classification
    koeppen <- pastclim::koeppen_geiger(
      prec = prec,
      tavg = tavg
    )
    output_file <- paste0(outDir, "koppen_", ky.bp, ".tif")
    terra::writeRaster(koeppen, output_file, overwrite = TRUE)
    if(verbose){
      print(paste0("  exported: ", output_file))
    }
  }
}