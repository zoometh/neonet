# devtools::install_github("EvolEcolGroup/pastclim", ref="dev")

library(pastclim)

when <- -7000
# outDir <- "C:/Rprojects/neonet/doc/clim/data/"
outDir <- "C:/Rprojects/neonet/doc/data/clim/" #?
set_data_path(path_to_nc = outDir)
# list_available_datasets()
# get_vars_for_dataset(dataset = "Beyer2020", annual = FALSE, monthly = TRUE)
download_dataset(dataset = "Beyer2020", annual = FALSE, monthly = TRUE)
# extract monthly temperature and precipitation
tavg_7k <- pastclim::region_slice(
  time_bp = when,
  bio_variables = c(paste0("temperature_0", 1:9),
                    paste0("temperature_", 10:12)),
  dataset = "Beyer2020"
)
prec_7k <- pastclim::region_slice(
  time_bp = when,
  bio_variables = c(paste0("precipitation_0", 1:9),
                    paste0("precipitation_", 10:12)),
  dataset = "Beyer2020"
)
# create the koeppen classification
koeppen_7k <- koeppen_geiger(
  prec = prec_7k,
  tavg = tavg_7k
)
# > object.size(koeppen_7k)
# 1304 bytes

library(terra)

# Assuming 'r' is your SpatRaster object
# Create a SpatRaster object for demonstration; replace this with your actual SpatRaster
r <- rast(nrows = 100, ncols = 100)
values(r) <- 1:ncell(r)

# Define the path and name of the output GeoTIFF file
# output_file <- "path/to/your_output_file.tif"
output_file <- paste0(outDir, "7k_koeppen.tif")

# Export the SpatRaster to a GeoTIFF file
writeRaster(koeppen_7k, output_file, overwrite=TRUE)

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