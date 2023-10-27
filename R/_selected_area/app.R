library(shiny)
library(leaflet)
library(leaflet.extras)

lngs <- c(-1, 4, 8)
lats <- c(36, 44, 44)

# shinyApp(
ui <- 
  fluidPage(
    leafletOutput("map"), 
    absolutePanel(top = 50, left = 80,
                  downloadButton('dwnld_selectshape', 
                                 label = "selection area")
    ))
server <- function(input, output, session){
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(lng = lngs, lat = lats, layerId = 1:length(lats)) %>% 
      addDrawToolbar(targetGroup = "test", 
                     # rectangleOptions = F, 
                     polylineOptions = F, 
                     markerOptions = F,
                     rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,
                                                                                             color = 'white',
                                                                                             weight = 3)),
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(clickable = T)))
  })  # END RENDER LEAFLET 
  
  observeEvent(input$map_marker_click, {
    print(input$map_marker_click)
  })  
  
  # my_df <- eventReactive(input$map_draw_new_feature, {
  #   selectionShape <- input$map_draw_new_feature
  #   coords <- selectionShape$geometry$coordinates[[1]]
  #   ageom <- c()
  #   # get coordinates
  #   for(i in 1:length(coords)){
  #     x <- coords[[i]][[1]]
  #     y <- coords[[i]][[2]]
  #     ageom <- c(ageom, x, y)
  #   }
  #   ageom.matrix <- matrix(ageom, ncol = 2, byrow = TRUE)
  #   polygon_sf <- sf::st_polygon(list(ageom.matrix))
  #   polygon_sf <- sf::st_sfc(polygon_sf, crs = 4326)
  #   polygon_sf <- sf::st_as_sf(polygon_sf)
  #   # add selection ID
  #   polygon_sf <- merge(polygon_sf, data.frame(ID = selectionShape$properties$"_leaflet_id"))
  #   polygon_sf
  # })
  
  output$dwnld_selectshape <- downloadHandler(
    filename = function(){paste0("neonet-data-", Sys.Date(), "-select-aera.geojson")}, 
    content = function(fname){
      # selectionShape <- input$map_draw_new_feature
      # selectionShape <- input$draw_all_features
      selectionShape <- input$map_draw_all_features
      print(length(selectionShape))
      if(length(selectionShape) == 0){
        selectionShape <- input$map_bounds
        print("YO")
      } 
      # print(str(selectionShape))
      fileOut <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", "selectionShapes.rds")
      saveRDS(selectionShape, fileOut)
      # 
      # coords <- selectionShape$geometry$coordinates[[1]]
      # ageom <- c()
      # # get coordinates
      # for(i in 1:length(coords)){
      #   x <- coords[[i]][[1]]
      #   y <- coords[[i]][[2]]
      #   ageom <- c(ageom, x, y)
      # }
      # ageom.matrix <- matrix(ageom, ncol = 2, byrow = TRUE)
      # polygon_sf <- sf::st_polygon(list(ageom.matrix))
      # polygon_sf <- sf::st_sfc(polygon_sf, crs = 4326)
      # polygon_sf <- sf::st_as_sf(polygon_sf)
      # # add selection ID
      # polygon_sf <- merge(polygon_sf, data.frame(ID = selectionShape$properties$"_leaflet_id"))
      # # print(nrow(data.out))
      # st_write(polygon_sf, fname)
    }
  )
  # observeEvent(input$map_draw_new_feature, {
  #   selectionShape <- input$map_draw_new_feature
  #   # print(class(selectionShape))
  #   # print(selectionShape)
  #   fileOut <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/", "selectionShape.Rdata")
  #   save(selectionShape, file = fileOut)
  #   # print(input$map_draw_new_feature)
  # })
  # 
}
# run
shinyApp(ui, server)