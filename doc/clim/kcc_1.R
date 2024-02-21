# Creates a serie of Koeppen CC

# devtools::install_github("EvolEcolGroup/pastclim", ref="dev")
library(pastclim)
library(terra)


from_to <- c(-9000, -8000, -7000, -6000)
dataset <- "Beyer2020"
# outDir <- "C:/Rprojects/neonet/doc/clim/data/"
outDir <- "C:/Rprojects/neonet/doc/data/clim/" #?
set_data_path(path_to_nc = outDir)
download_dataset(dataset = dataset, annual = FALSE, monthly = TRUE)

for(i in 1:length(from_to)){
  when <- from_to[i]
  ky <- paste0(abs(when) / 1000, "k")
  print(paste("* read:", ky))
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
  koeppen <- koeppen_geiger(
    prec = prec,
    tavg = tavg
  )
  output_file <- paste0(outDir, "koeppen_", ky, ".tif")
  writeRaster(koeppen, output_file, overwrite=TRUE)
}


## dim (ex: width = 3000, height = 1200)
# long.dim = 6000
# lat.dim = long.dim / 2.5

## plot it
# plotOut = paste0(outDir, "7k_koeppen.jpg")
# jpeg(filename = plotOut, width = long.dim, height = lat.dim)
# plot(koeppen_7k)
# dev.off()

# outFile = paste0(outDir, "koppen_", as.character(when), ".tif")
# writeRaster(koeppen_7k, outFile)