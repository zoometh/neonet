
df <- data.frame(arr_lon = sf::st_coordinates(df.dates.min$geometry)[,1],
                 arr_lat = sf::st_coordinates(df.dates.min$geometry)[,2], 
                 EMISSIONS_KGCO2EQ = df.dates.min$median)

library(tidyverse)
library(interp)

interpolated <- interp(df$arr_lon, 
                       df$arr_lat, 
                       df$EMISSIONS_KGCO2EQ, 
                       duplicate = "mean",    #you have duplicated values
                       output = "grid")

#convert this to a long form dataframe
interp_df <- expand_grid(i = seq_along(interpolated$x), 
                         j = seq_along(interpolated$y)) %>% 
  mutate(lon = interpolated$x[i],
         lat = interpolated$y[j],
         emissions = map2_dbl(i, j, ~interpolated$z[.x,.y])) %>% 
  select(-i, -j)

#then you can use this in your plot
ggplot()+
  # geom_point(data = df, aes(x = dep_lon, y = dep_lat), col = "red") +
  geom_point(data = df, aes(x = arr_lon, y = arr_lat), col = "blue") +
  geom_contour(data = interp_df, aes(x = lon, y = lat, z = emissions)) 

# install necessary packages
# install.packages( c( "shiny", "leaflet", "mapview" ) )

# load necessary packages
library( shiny )
library( leaflet )
library( mapview )

ui <- fluidPage(
  leafletOutput( outputId = "map"),
  downloadButton( outputId = "dwnld_map")
)

server <- function(input, output, session) {
  
  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map <- reactive({
    
    leaflet() %>% # create a leaflet map widget
      addTiles()
      # addTiles( urlTemplate = "https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png" ) # specify provider tile and type
    
  }) # end of foundational.map()
  
  # render foundational leaflet map
  output$map <- leaflet::renderLeaflet({
    
    # call reactive map
    foundational.map()
    
  }) # end of render leaflet
  
  # store the current user-created version
  # of the Leaflet map for download in 
  # a reactive expression
  user.created.map <- reactive({
    
    # call the foundational Leaflet map
    foundational.map() %>%
      
      # store the view based on UI
      setView( lng = input$map_center$lng
               ,  lat = input$map_center$lat
               , zoom = input$map_zoom
      )
    
  }) # end of creating user.created.map()
  
  
  
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$dwnld_map <- downloadHandler(
    filename = paste0( Sys.Date()
                       , "_customLeafletmap"
                       , ".pdf"
    )
    
    , content = function(file) {
      mapshot( x = user.created.map()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
  
} # end of server

# run the Shiny app
shinyApp(ui = ui, server = server)

# end of script #