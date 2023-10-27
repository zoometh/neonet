# load

fileOut <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", "selectionShapes.rds")
# load(fileOut)
selectionShapes <- readRDS(fileOut)


lon = c(selectionShapes$east, selectionShapes$west)
lat = c(selectionShapes$south,selectionShapes$north)
Poly_Coord_df <- data.frame(lon, lat)

poly <- Poly_Coord_df %>% 
  sf::st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc()
poly

lselections <- list()
for(ashape in 1:length(selectionShapes[[2]])){
  # ashape <- 1
  selectionShape <- selectionShapes[[2]][[ashape]]
  coords <- selectionShape$geometry$coordinates[[1]]
  ageom <- c()
  # get coordinates
  for(i in 1:length(coords)){
    x <- coords[[i]][[1]]
    y <- coords[[i]][[2]]
    ageom <- c(ageom, x, y)
  }
  ageom.matrix <- matrix(ageom, ncol = 2, byrow = TRUE)
  polygon_sf <- sf::st_polygon(list(ageom.matrix))
  polygon_sf <- sf::st_sfc(polygon_sf, crs = 4326)
  polygon_sf <- sf::st_as_sf(polygon_sf)
  # add selection ID
  polygon_sf <- merge(polygon_sf, data.frame(ID = selectionShape$properties$"_leaflet_id"))
  lselections[[length(lselections) + 1]] <- polygon_sf
}
selections <- do.call(rbind, lselections)
plot(selections)
# plot(lselections[[1]])
# plot(lselections[[2]])
