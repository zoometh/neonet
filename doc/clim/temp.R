library(leaflet)
library(leaflegend)
data("quakes")
symbols <- makeSymbolsSize(
  values = quakes$depth,
  shape = 'diamond',
  color = 'red',
  fillColor = 'red',
  opacity = .5,
  baseSize = 10
)
leaflet() %>%
  addTiles() %>%
  addMarkers(data = quakes,
             icon = symbols,
             lat = ~lat, lng = ~long) %>%
  addLegendSize(
    values = quakes$depth,
    color = 'red',
    fillColor = 'red',
    opacity = .5,
    title = 'Depth',
    shape = 'circle',
    orientation = 'horizontal',
    breaks = 5)