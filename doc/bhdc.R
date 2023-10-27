######################################################
# Basic management of radiocarbon dates for the BHDC #
######################################################

# Not run:
# install.packages("Bchron")
# install.packages("rcarbon")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("interp")
# install.packages("DescTools")
# install.packages("sf")
# install.packages("ggmap")
# install.packages("rosm")
# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("metR")


df.c14 <- paste0(getwd(), "/doc/presentation/bhdc/img/neonet-data-2023-10-22.geojson")
neo_isochr(df.c14 = df.c14, lbl.dates = TRUE, lbl.time.interv = TRUE)


# change the paths when needed 

# source needed funtion
source("R/neo_calib.R")
source("R/neo_isochr.R")
source("R/neo_spd.R")
source("R/neo_spdplot.R")

# the GeoJSON file
c14data <- "C:/Users/Thomas Huet/Downloads/neonet-data-2023-09-25.geojson"
# create a map with isochrones
neo_isochr(df.c14 = c14data, 
           time.interv = 100,
           outDir = "C:/Rprojects/neonet/results/",
           show.lbl = FALSE)
# create a SPD
neo_spd(df.c14 = c14data,
        outDir = "C:/Rprojects/neonet/results/")