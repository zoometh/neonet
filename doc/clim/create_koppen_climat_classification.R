library(pastclim)

when <- -7000
outDir <- "C:/Rprojects/neonet/doc/data/clim/"
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
# plot it
plot(koeppen_7k)
outFile = paste0(outDir, "koppen_", as.character(when), ".tif")
writeRaster(koeppen_7k, outFile)