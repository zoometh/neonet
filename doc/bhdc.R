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


# change the paths when needed 

# source needed funtion
source("R/neo_calib.R")
source("R/neo_isochr.R")
source("R/neo_spd.R")
source("R/neo_spdplot.R")

# the GeoJSON file
c14data <- "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-24.geojson"
# create a map with isochrones
neo_isochr(df.c14 = c14data, 
           outDir = "C:/Rprojects/neonet/results/",
           show.lbl = FALSE)
# create a SPD
neo_spd(df.c14 = c14data,
        outDir = "C:/Rprojects/neonet/results/")