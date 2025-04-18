library(dplyr)

####################################################
## App NeoNet-strati
## Editable table to record stratigraphic relations
####################################################


# df <- read.table("http://mappaproject.arch.unipi.it/mod/files/140_140_id00140_doc_elencoc14.tsv", sep = "\t", header = T, quote = "")

serv <- FALSE

if(serv){
  df <- readr::read_tsv("140_140_id00140_doc_elencoc14.tsv", quote = "")
} else {
  df <- readr::read_tsv("C:/Rprojects/neonet/R/app-strati/c14_dataset_med_x_atl_2.tsv")
  nn_strati_logo <- "logo_nn_strati.png" # "C:/Rprojects/neonet/R/app-strati/logo_nn_strati.png"
}

mysite <- "Pokrovnik"
df.sample <- df[df$SiteName == mysite, ]
col.names <- c("SiteName", "Period", "PhaseCode", "LabCode", "C14Age", "C14SD", "Material", "MaterialSpecies")

df.sample <- df.sample[ , col.names]

df.sample$After <- NA

library(shiny)
library(DT)
library(dplyr)

ui <- fluidPage(
  # titlePanel("NeoNet stratigraphy"),
  titlePanel(
    tags$div(
      tags$h1("NeoNet stratigraphy", style = "display:inline; vertical-align:middle;"),
      tags$img(src = nn_strati_logo, height = '70px', style = "vertical-align:middle; margin-right: 10px;")
    )
  ),
  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Site stratigraphy",
               textInput("mysite", "select a site name", value = "Pokrovnik"),
               div(
                 DT::dataTableOutput("neonet.df_data"), #, width = 1200
                 style = "font-size: 90%"
               ),
               # DT::dataTableOutput("neonet.df_data", width = "90%"),
               # br(),
      ),
      tabPanel("All sites",
               div(
                 DT::dataTableOutput("alldata", width = 1200),
                 style = "font-size: 80%"
               )
               # DT::dataTableOutput("alldata")
      ))))

# neonet.df <- df.sample

server <- function(input, output) {
  # output$neonet_url <- renderText({
  #   # ??
  #   "http://mappaproject.arch.unipi.it/mod/files/140_140_id00140_doc_elencoc14.tsv"
  # })
  output$neonet.df_data <- renderDataTable(
    df.sample,
    selection = 'none',
    editable = TRUE,
    rownames = FALSE,
    extensions = 'Buttons',
    # width = "90%",
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = FALSE,
      autoWidth = TRUE,
      responsive = TRUE,
      ordering = TRUE,
      # dom = 'Bfrtip',
      dom = "Bft",
      pageLength = 10000,
      # scrollX = T,
      buttons = c('csv')
    ),
    class = "display"
  )
  observeEvent(input$neonet.df_data_cell_edit, {
    df.sample[input$neonet.df_data_cell_edit$row, input$neonet.df_data_cell_edit$col] <<- input$neonet.df_data_cell_edit$value
  })
  observeEvent(input$mysite, ignoreInit = TRUE, {
    mysite <- input$mysite
    mysite <- trimws(mysite)
    outFile <- paste0(mysite, "_", Sys.Date())
    # print(mysite)
    df.sample <- df[df$SiteName == mysite, ]
    df.sample <- df.sample[ , col.names]
    df.sample$After <- NA
    output$neonet.df_data <- renderDataTable(
      df.sample,
      selection = 'none',
      editable = TRUE,
      rownames = FALSE,
      extensions = 'Buttons',
      # width = "90%",
      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        # dom = 'Bfrtip',
        dom = "Bft",
        pageLength = 10000,
        # scrollX = T,
        buttons = list(
          list(extend = 'csv', filename = outFile)
        )
      ),
      class = "display"
    )
  })
  output$alldata <- renderDT(DT::datatable(
    df,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      lengthMenu = list(c(100, 200, -1), c('100', '200', 'All')),
      pageLength = 100,
      # width = "90%",
      autoWidth = TRUE,
      # scrollX = T,
      buttons = list(
        list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")))
    ))
  )
}

shinyApp(ui, server)
