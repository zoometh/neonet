


##
source("R/neo_calib.R")
source("R/neo_isochr.R")
source("R/neo_spd.R")
source("R/neo_spdplot.R")

c14data <- "https://raw.githubusercontent.com/zoometh/neonet/main/results/neonet-data-2023-09-24.geojson"
neo_isochr(df.c14 = c14data, 
           outDir = "C:/Rprojects/neonet/results/",
           show.lbl = FALSE)
neo_spd(df.c14 = c14data,
        outDir = "C:/Rprojects/neonet/results/")

##

source("R/neo_subset.R")
source("R/neo_bib.R")
source("R/neo_matlife.R")
source("R/neo_calib.R")
source("R/neo_merge.R")
source("R/neo_html.R")
source("R/neo_datamiss.R")
source("R/neo_datasum.R")
source("R/neo_doi.R")


c14.file <- "NeoNet_Med_v2.tsv"
df.c14 <- read.csv2(paste0("C:/Rprojects/neonet/R/app-dev-neonet/", c14.file), sep = "\t")
neo_datamiss(df.c14, main = c14.file)
df.c14 <- neo_subset(df.c14, rm.Spatial = T,
                     ref.spat = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/wsh_med.geojson")

source("R/neo_spd.R")
source("R/neo_spdplot.R")




## Editable datatable

# site.to.edit <- "Pokrovnik"

d1 <- df.site.to.edit # this df has been created by neo_strat_edit.R

cwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
# rstudioapi::jobRunScript(path = paste0(cwd, "/editabledata_test.R"), importEnv = TRUE)
source(paste0(cwd, "/neo_strat.R"))

ui <- shiny::fluidPage(
  shiny::titlePanel("NeoNet - stratigraphical relationships"),
  h3(site.to.edit),
  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("data", #"Sample Bank",
               
               DT::dataTableOutput("site.data"),
               # br(),
               # actionButton("viewBtn","View"),
               # br(),
               # actionButton("saveBtn","Save"),
               br(),
               plotOutput("gg.strati")
      ))))

server <- function(input, output) {
  
  
  output$site.data <- DT::renderDataTable(
    DT::datatable(
      d1,
      selection = 'none',
      editable = TRUE,
      rownames = TRUE,
      extensions = 'Buttons',
      width = "100%",
      options = list(
        searching = TRUE,
        # fixedColumns = TRUE,
        # autoWidth = TRUE,
        # ordering = TRUE,
        lengthMenu = list(c('50', '100', '200', -1),
                          c('50', '100', '200', 'All')),
        paging = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", title = site.to.edit)
        )
        # buttons = c('csv')
      ),
      class = "display"
    )
  )
  
  
  observeEvent(input$site.data_cell_edit, {
    d1[input$site.data_cell_edit$row,input$site.data_cell_edit$col] <<- input$site.data_cell_edit$value
  })
  
  
  # view_fun <- eventReactive(input$viewBtn,{
  #   if(is.null(input$saveBtn)||input$saveBtn==0)
  #   {
  #     returnValue()
  #   }
  #   else
  #   {
  #     DT::datatable(d1, selection = 'none')
  #   }
  #
  # })
  # observeEvent(input$saveBtn,{
  #   outData <- paste0(site.to.edit, ".csv")
  #   write.csv(d1, outData)
  # })
  
  output$gg.strati <- renderPlot({
    neo_strat(smp.sitename = site.to.edit)
  })
  
  # output$updated.df <- DT::renderDataTable({
  #   view_fun()
  # }
  # )
}

shinyApp(ui, server)


#######################

library(dplyr)


# df <- read.table("http://mappaproject.arch.unipi.it/mod/files/140_140_id00140_doc_elencoc14.tsv", sep = "\t", header = T, quote = "")
df <- read.table("140_140_id00140_doc_elencoc14.tsv", sep = "\t", header = T, quote = "")


mysite <- "Pokrovnik"

df.sample <- df[df$SiteName == mysite, ]

col.names <- c("SiteName", "Period", "PhaseCode", "LabCode", "C14Age", "C14SD", "Material", "MaterialSpecies")

df.sample <- df.sample[ , col.names]

df.sample$After <- NA


###############################


# reset if TRUE
from.scratch <- T

if(from.scratch){
  
  lcul_col <- list(# colors
    EM = "#0000CF", # BLUE
    MM = "#1D1DFF", #
    LM = "#3737FF", #
    LMEN = "#6A6AFF", #
    UM = "#8484FF", #
    EN = "#FF1B1B", # RED
    EMN = "#FF541B", #
    MN = "#FF8D1B", #
    LN = "#FFC04D", #
    UN = "#E7E700" # NEO UNDEF.
  )
  
  df.colors.per <- as.data.frame((stack(lcul_col)))
  df.colors.per$name <- c("Early Mesolithic",
                          "Middle Mesolithic",
                          "Late Mesolithic",
                          "Late Mesolithic/Early Neolithic",
                          "Undefined Mesolithic",
                          "Early Neolithic",
                          "Early/Middle Neolithic",
                          "Middle Neolithic",
                          "Late Neolithic",
                          "Undefined Neolithic")
  names(df.colors.per)[names(df.colors.per) == 'ind'] <- 'period'
  names(df.colors.per)[names(df.colors.per) == 'values'] <- 'color'
  names(df.colors.per)[names(df.colors.per) == 'name'] <- 'period_full_name'
  df.colors.per <- df.colors.per[ , c(2, 3, 1)]
  write.table(df.colors.per, "C:/Rprojects/neonet/doc/img/periods.tsv", sep = "\t", row.names = F)
} else {
  # TODO: read a TSV dataframe
  # pass
  df.colors.per <- read.table("https://raw.githubusercontent.com/zoometh/neonet/main/doc/img/periods.tsv", header = T)
}


df.colors.per[ , c(3)] <- kableExtra::cell_spec(df.colors.per[, c(3)], color = df.colors.per$color)
dt <- knitr::kable(df.colors.per, format = "html",
                   row.names = F,
                   booktabs = T,
                   escape = F,
                   align = "l") %>%
  kableExtra::kable_styling(full_width = FALSE,
                            position = "center",
                            font_size = 20)

readr::write_file(dt, "C:/Rprojects/neonet/doc/img/periods.html")

################

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