# Creates a serie of Koeppen CC
# from_to is in BP

# devtools::install_github("EvolEcolGroup/pastclim", ref="dev")
# library(pastclim)
# library(terra)

neo_kcc_create <- function(from_to = c(-11000, -10000, -9000, -8000, -7000, -6000),
                           present = 2000,
                           dataset = "Beyer2020",
                           outDir = "C:/Rprojects/neonet/doc/data/clim/"){
  # outDir <- "C:/Rprojects/neonet/doc/clim/data/"
   #?
  pastclim::download_dataset(dataset = dataset, annual = FALSE, monthly = TRUE)
  for(i in 1:length(from_to)){
    when <- from_to[i]
    ky.bp <- paste0(abs(when) / 1000, "k")
    ky.bc <- abs(when) - present
    print(paste0("*read: ", ky.bp, " BP (", ky.bc," BC)"))
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
    print(paste0("  exported: ", output_file))
  }
}

# outDir <- "C:/Rprojects/neonet/doc/data/clim/"
# pastclim::set_data_path(path_to_nc = outDir)
# neo_kcc_create()