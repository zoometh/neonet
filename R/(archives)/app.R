library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(raster)
library(rgeos)
library(sp)
library(DT)
library(htmlwidgets)
library(dplyr)
library(ggplot2)
library(grDevices)
library(Bchron)
library(rcarbon)
library(bibtex)

print(getwd())

source(paste0(getwd(), "/functions.R"))
# note: intCal is 'intcal20'

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

## graphical param
gcalib.w <- 1500 # abs, px
gcalib.h <- 200 # rel, px
gcalib.lbl.sz <- 1.5 # text
gcalib.strip.text.sz <- 2.5 # facet label
gcalib.axis.title.sz <- 4
gcalib.xaxis.sz <- 3
gcalib.yaxis.sz <- 2
gcalib.gline.sz <- .3 # geom_line size
gcalib.bin <- 100 # the chronological bin

nsites.14C.cal <- 1000 # max of sites calibrated at the same time, panel calib

c14bibtex.url <- paste0(dirname(getwd()), "/inst/extdata/references.bib")
# print(c14bibtex.url)
bib <- read.bib(c14bibtex.url)
bib <- sort(bib) # sort
bibrefs.md <- capture.output(print(bib)) # to Markdown layout
bibrefs.md <- replace(bibrefs.md, bibrefs.md == "", "<br><br>") 
bibrefs.md <- paste0(bibrefs.md, collapse = '') # separate references
bibrefs.html <- shiny::markdown(bibrefs.md) # to HTML layout

mat.life.url <- paste0(dirname(getwd()), "/inst/extdata/c14_material_life.tsv")
material.life.duration <- read.csv(mat.life.url, sep = "\t")
short.life <- subset(material.life.duration, life.duration == 'short.life')
long.life <- subset(material.life.duration, life.duration == 'long.life')
other.life <- material.life.duration[is.na(material.life.duration$life.duration),]
family.life <- c(rep("short.life",nrow(short.life)),
                 rep("long.life",nrow(long.life)),
                 rep("other.life",nrow(other.life)))
type.life <- c(short.life$material.type,
               long.life$material.type,
               other.life$material.type)
material.life <- data.frame(family.life=family.life,
                            type.life=type.life)
short.life <- as.character(material.life[material.life$family.life == "short.life", "type.life"])
long.life <- as.character(material.life[material.life$family.life == "long.life", "type.life"])
other.life <- as.character(material.life[material.life$family.life == "other.life", "type.life"])

df.tot <- read.csv(paste0(dirname(getwd()), "/inst/extdata/c14_dataset.tsv"),
                   sep = "\t",
                   encoding="UTF-8")
df.tot <- df.tot[df.tot$Period %in% names(lcul_col), ] # only selected periods
df.tot <- df.tot[!is.na(df.tot$Period), ]
df.tot$tpq <- as.numeric(df.tot$tpq)
df.tot$taq <- as.numeric(df.tot$taq)
df.tot$Longitude <- as.numeric(df.tot$Longitude)
df.tot$Latitude <- as.numeric(df.tot$Latitude)
df.tot <- subset(df.tot, Longitude != 'NA') # without missing coords
df.tot <- subset(df.tot, Latitude != 'NA')
df.tot <- df.tot[!(is.na(df.tot$Latitude)) & !(is.na(df.tot$Longitude)),] # rm NA
df.tot <- df.tot[df.tot$Latitude != 'NA' & df.tot$Longitude != 'NA',] # rm NA
out.png.name <- "neonet.png"

df.tot$locationID <- df.tot$LabCode
df.tot$secondLocationID <- paste(rownames(df.tot), "_selectedLayer", sep = "")

wgs84 <- '+init=EPSG:4326'
df.tot$lbl <- NA
df.tot[is.na(df.tot)] <- 'NA'
df.tot$tpq[df.tot$tpq == 'NA'] <- Inf
df.tot$taq[df.tot$taq == 'NA'] <- -Inf
df.tot$tpq <- as.numeric(df.tot$tpq)
df.tot$taq <- as.numeric(df.tot$taq)

# material type
mat.type.life <- c("short life","long life","others")
df.tot$mat.life <- ifelse(df.tot$Material %in%  short.life, "short life",
                          ifelse(df.tot$Material %in%  long.life,"long life","others"))
hotcols <- c("Country", "SiteName", "Period", "PhaseCode", # "Culture",
             "Longitude","Latitude",
             "tpq", "taq",
             "LabCode", "C14Age", "C14SD", "Material", "mat.life",
             "bib", "bib_url")
refcols <- c(hotcols, c("locationID", "secondLocationID"))
df.tot <- df.tot[ , c(refcols, setdiff(names(df.tot), refcols))]
df.tot <- df.tot[ , refcols] # exclude other columns
# replace values
df.tot[df.tot==""] <- "unknown"
df.tot$lbl <- NA
# labels
for (i in seq(1, nrow(df.tot))){
  # popup notification
  desc <- paste(sep = "<br/>",
                paste0("<b>", df.tot[i,"SiteName"],"</b> / ",
                       df.tot[i,"Material"]," (", df.tot[i,"mat.life"],")"),
                paste0("date: ", df.tot[i,"C14Age"], " +/- ", df.tot[i,"C14SD"],
                       " BP [", df.tot[i,"LabCode"],"]"),
                paste0("tpq/taq: ", df.tot[i,"tpq"], " to ", df.tot[i,"taq"],
                       " cal BC"),
                paste0("<span style='color: ", df.tot[i,"colors"],";'><b>", df.tot[i,"Period"], "</b></span>  ",
                #paste0("period: ", df.tot[i,"Period"],
                       " <b>|</b> PhaseCode: <i>", df.tot[i,"PhaseCode"],
                       "</i> <br/>"))
  if(grepl("^http", df.tot[i,"bib_url"])){
    # for href, if exist
    desc <- paste0(desc, 'ref: <a href=', shQuote(paste0(df.tot[i, 'bib_url'])),
                   "\ target=\"_blank\"", ">", df.tot[i, 'bib'], "</a>")
  } else {desc <- paste0(desc, "ref: ", df.tot[i, "bib"])}
  df.tot[i, "lbl"]  <- desc
}
df.tot$idf <- seq(1, nrow(df.tot))
# colors
Periods <- as.factor(unique(df.tot$Period))
# restrict on recorded period
lcul_col <- lcul_col[names(lcul_col) %in% unique(df.tot$Period)]
myColors <- c()
for (i in names(lcul_col)){
  myColors <- c(myColors, as.character(lcul_col[i]))
}
# df.tot$colors <- NA
for (i in seq(1:nrow(df.tot))){
  df.tot[i,"colors"] <- toupper(as.character(lcul_col[df.tot[i, "Period"]]))
}
# labels
for (i in seq(1, nrow(df.tot))){
  # popup notification
  desc <- paste(sep = "<br/>",
                paste0("<b>", df.tot[i,"SiteName"],"</b> / ",
                       df.tot[i,"Material"]," (", df.tot[i,"mat.life"],")"),
                paste0("date: ", df.tot[i,"C14Age"], " +/- ", df.tot[i,"C14SD"],
                       " BP [", df.tot[i,"LabCode"],"]"),
                paste0("tpq/taq: ", df.tot[i,"tpq"], " to ", df.tot[i,"taq"],
                       " cal BC"),
                paste0("<span style='color: ", df.tot[i,"colors"],";'><b>", df.tot[i,"Period"], "</b></span>  ",
                       #paste0("period: ", df.tot[i,"Period"],
                       " <b>|</b> PhaseCode: <i>", df.tot[i,"PhaseCode"],
                       "</i> <br/>"))
  if(grepl("^http", df.tot[i,"bib_url"])){
    # for href, if exist
    desc <- paste0(desc, 'ref: <a href=', shQuote(paste0(df.tot[i, 'bib_url'])),
                   "\ target=\"_blank\"", ">", df.tot[i, 'bib'], "</a>")
  } else {desc <- paste0(desc, "ref: ", df.tot[i, "bib"])}
  df.tot[i, "lbl"]  <- desc
}
# sp
xy <- list(longitude = c(as.numeric(df.tot$Longitude)),
           latitude = c(as.numeric(df.tot$Latitude)))
df.tot.sp <- SpatialPointsDataFrame(coords = xy,
                                    data = df.tot,
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
tit <- HTML(paste0('NEONET ',
                   'Radiocarbon dates by Location, Chronology and Material Life Duration (dev. vers.)'))
neonet.logo.path <- paste0(dirname(getwd()), "/doc/img/neonet.png")
# print(neonet.logo.path)
b64 <- base64enc::dataURI(file = neonet.logo.path) # load image
data.credits <- HTML(paste0(' <b> Data gathering: </b>',
                            '<ul>',
                            '<li> <a href=', shQuote(paste0("https://orcid.org/0000-0002-9315-3625")), "\ target=\"_blank\"",
                            '> Niccolo Mazzucco </a>: niccolo.mazzucco@unipi.it </li>',
                            '<li> <a href=', shQuote(paste0("https://orcid.org/0000-0002-1112-6122")), "\ target=\"_blank\"",
                            '> Thomas Huet </a>: thomashuet7@gmail.com </li>',
                            '<li> <a href=', shQuote(paste0("https://orcid.org/0000-0002-2386-8473")), "\ target=\"_blank\"",
                            '> Miriam Cubas Morera </a>: mcubas.morera@gmail.com, </li>',
                            '<li> <a href=', shQuote(paste0("https://orcid.org/0000-0002-0830-3570")), "\ target=\"_blank\"",
                            '> Juan Gibaja </a>: jfgibaja@gmail.com, </li>',
                            '<li> <a href=', shQuote(paste0("https://orcid.org/0000-0002-1642-548X")), "\ target=\"_blank\"",
                            '> F. Xavier Oms</a>: oms@ub.edu, </li>',
                            '</ul>'))
print("neonet.logo.path")
# website for documentation
webpage.app <- "http://shinyserver.cfs.unipi.it:3838/neonet/index.html"
app.page <- paste0('<a href=',shQuote(webpage.app),"\ target=\"_blank\"",'>http://shinyserver.cfs.unipi.it:3838/neonet/index.html</a>')
# GitHub repo
devpage.app <- "https://github.com/zoometh/neonet#readme"
app.dev <- paste0('<a href=',shQuote(devpage.app),"\ target=\"_blank\"",'>https://github.com/zoometh/C14/tree/main/neonet</a>')
# credits
app.redneo <- HTML(paste0('<a href=', shQuote(paste0("https://redneonet.com")), "\ target=\"_blank\"",
                             '><b> Red NeoNet group website </b></a><br>'))
app.credits <- HTML(paste0(' <b> App developments </b> ', app.dev,' <b>:</b>',
                           '<ul>',
                           '<li> <a href=',shQuote(paste0("https://orcid.org/0000-0002-1112-6122")),"\ target=\"_blank\"",
                           '> Thomas Huet </a>: thomashuet7@gmail.com </li>',
                           '<li> <a href=',shQuote(paste0("https://orcid.org/0000-0002-9315-3625")),"\ target=\"_blank\"",
                           '> Niccolo Mazzucco </a>: niccolo.mazzucco@unipi.it </li>',
                           '</ul>'))
app.website <- HTML(paste0(' <b> Documentation: </b> ',
                           '<ul>',
                           '<li>', app.page,
                           '</li>',
                           '</ul>'))
all.credits <- paste0(app.website, "<br>",
                      app.credits, "<br>",
                      data.credits, "<br>",
                      app.redneo
)
# for the chrono slider
Mx <- max(df.tot$taq)
Mn <- min(df.tot$tpq)
if(!is.na(Mx %% gcalib.bin)){Mx <- Mx - (Mx %% gcalib.bin)} # round to next xx'
if(!is.na(Mn %% gcalib.bin)){Mn <- Mn - (Mn %% gcalib.bin)} # round to next xx'

f.hgg <- function(some_14C, C14.grouped){
  # group or ungroup dates by PhaseCode, SiteName, etc.
  # height, add 1 by default
  if(C14.grouped == "C14ungroup"){
    h.gg <- nrow(some_14C)
  }
  if(C14.grouped == "C14groupsl"){
    h.gg <- nrow(some_14C[!duplicated(some_14C[c("SiteName", "PhaseCode")]),])
  }
  if(C14.grouped == "C14groupsp"){
    h.gg <- nrow(some_14C[!duplicated(some_14C[c("SiteName", "Period")]),])
  }
  if(C14.grouped == "C14groupp"){
    h.gg <- nrow(some_14C[!duplicated(some_14C[c("Period")]),])
  }
  if(C14.grouped == "C14all"){
    h.gg <- 1
  }
  return(h.gg+1)
}

# color
my.colors <- rep('purple', length(mat.type.life))
fun.color.labels <- function() {
  # color labels
  res <- list()
  for (o in mat.type.life) {
    res[[length(res)+1]] <- tags$span(tags$b(o),
                                      style = paste0('color: ', 
                                                     my.colors[which(mat.type.life == o)],
                                                     ';'))
  }
  return(res)
}

############################################### ui #################################################
ui <- navbarPage(tit,
                 tabPanel(title = "map",
                          fluidPage(
                            tags$style(".checkbox { /* checkbox is a div class*/
                                       line-height: 12px;font-size:12px;
                                        background-color: #f2f2f2;
                                       }
                                       input[type='checkbox']{ /* style for checkboxes */
                                       width: 12px; /*Desired width*/
                                       height: 12px; /*Desired height*/
                                       line-height: 12px;
                                       }
                                       "),
                            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: purple}")),
                            fluidRow(htmlOutput("presentation")),
                            fluidRow(
                              column(12,
                                     leafletOutput("map", width = "100%", height = 700),
                                     # tpq taq slider
                                     absolutePanel(bottom = 10, left = 150,
                                                   sliderInput("range", 
                                                               width = '600px',
                                                               label = "tpq/taq (BC)", Mn, Mx,
                                                               value = range(Mn, Mx),
                                                               step = gcalib.bin)),
                                     # a txt output
                                     fluidRow(verbatimTextOutput("Click_text")),
                                     # a radio button
                                     absolutePanel(top = 50, left = 80,
                                                   materialSwitch(inputId = "hover",
                                                                  label = "group C14 on map",
                                                                  status = "default",
                                                                  width = "120px")
                                     ),
                                     ## selection on material type and sd
                                     # type of material (short,...,long life)
                                     absolutePanel(bottom = 70, right = 40,
                                                   checkboxGroupInput("mater",
                                                                      label = "material life duration and max accepted SD",
                                                                      choiceNames = fun.color.labels(),
                                                                      choiceValues = mat.type.life,
                                                                      selected = mat.type.life,
                                                                      inline = TRUE,
                                                                      width = "300px")
                                     ),
                                     # slider, filter on sd
                                     absolutePanel(bottom = 10, right = 40,
                                                   sliderInput("sds",
                                                               width = '300px',
                                                               label = NULL,
                                                               min = 0,
                                                               max = max(df.tot$C14SD),
                                                               value = max(df.tot$C14SD),
                                                               step = 10)
                                     ),
                                     # periods
                                     absolutePanel(top = 70, right = 40,
                                                   checkboxGroupInput("cults", 
                                                                      label = "periods",
                                                                      choices = names(lcul_col),
                                                                      selected = names(lcul_col),
                                                                      width = "100px")),
                                     # the logo
                                     absolutePanel(top = 15, left = 80,
                                                   img(src = b64, width = "10%", align = 'left')),
                                     # the clicked coordinates
                                     absolutePanel(bottom = 10, left = 250,
                                                   textOutput("out"),
                                                   tags$head(tags$style(HTML("#out {font-size: 14px;}")))
                                     ))),
                            fluidRow(
                              htmlOutput("nb.dat"),
                              column(12,
                                     div(DTOutput("tbl"), style = "font-size:70%"))
                            )
                          )),
                 tabPanel("calib",
                          fluidRow(
                            column(5,
                                   htmlOutput("calib.presentation")
                            ),
                            column(5,
                                   # a radio button to un/group
                                   radioButtons(
                                     inputId = "C14group",
                                     label = "C14 grouped by site and/ or period",
                                     choices = c("by dates" = "C14ungroup",
                                                 "by PhaseCode" =  "C14groupsl",
                                                 "by site and period" = "C14groupsp",
                                                 "by period" = "C14groupp",
                                                 "all C14" = "C14all"),
                                     selected = "C14ungroup",
                                     inline = TRUE,width = NULL,choiceNames = NULL,
                                     choiceValues = NULL
                                   )
                            ),
                            column(1,
                                   downloadButton('dwnld_calib'))
                          ),
                          fluidRow(imageOutput("rdpd")
                          )
                 ),
                 tabPanel("data",
                          fluidPage(
                            fluidRow(
                              fluidRow(div(DTOutput("hot"), style = "font-size:70%"))
                            )
                          )
                 ),
                 tabPanel("biblio",
                          fluidPage(
                            htmlOutput("biblio")
                          )
                 ),
                 tabPanel("infos",
                          fluidPage(
                            tags$head(
                              tags$style(HTML("
                    li {
                    font-size: 14px;
                    }
                    li span {
                    font-size: 18px;
                    }
                    ul {
                    list-style-type: square;
                    }
                    "))
                            ),
                            HTML(all.credits)
                          )
                 )
)

############################################### server #################################################
server <- function(input, output, session) {
  output$presentation <- renderUI({
    HTML(paste0("move the <b> window map </b>  to select dates by location |", 
                " move the <b> tpq/taq slider </b> to selected sites within ‘standard’ cal BC duration ",
                "(calibration with the <a href='https://rdrr.io/cran/Bchron/man/BchronCalibrate.html'>BchronCalibrate</a> ", 
                "function and the '", intCal, "' calibration curve) <br/>",
                " draw <b> polygon or rectangle </b> to subset dates by a smaller area than the ROI |",
                " check/uncheck <b> material buttons </b> to show/hide sites by type of life duration material |",
                " check/uncheck <b> periods buttons </b> to show/hide sites by Periods |",
                " <b> click </b> on sites to get informations | <b> click </b> on the map to get long/lat coordinates <br/>"))
  })
  output$tbl <- DT::renderDataTable({
    # small filtered table on the 'map' tab
    datatable(
      data_map(),
      rownames = FALSE,
      extensions = c("Scroller","Buttons"),
      style = "bootstrap",
      class = "compact",
      width = "100%",
      editable = F,
      callback = JS("table.on('click.dt', 'td', function() {
                               Shiny.onInputChange('click', Math.random());
                });"),
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 300,
        scroller = TRUE,
        dom = 'tp'
      )
    )
  })
  output$calib.presentation <- renderUI({
    # TODO: loading message
    HTML(paste0(" in the <b>map panel</b>, click on a site to get its C14 calibration ",
                "and those of all sites within the region of interest (", intCal," calibration curve, limited to <b><font color= green >",
                nsites.14C.cal," dates</font></b>) | ",
                "<b>check/uncheck</b> by dates, by site and/or stratigraphical layer and/or period, all C14 to (un)group dates | ",
                "<b>download</b> the plot with the button"))
  })
  output$hot <- DT::renderDataTable({
    # the big table in 'data' 
    datatable(
      df.tot[ , hotcols],
      rownames = FALSE,
      width = "100%",
      editable = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 100
      )
    )
  })
  # dynamic table below the map, 'map' tab
  in_bounding_box <- function(data.f, lat, long, tpq, taq, sds, bounds) {
    # filter sites on coordinates, chrono & type of material
    if(is.null(inside.geometry$LabCode.selected)){
      # there is no selection shape
      data.f %>%
        dplyr::filter(
          # conditional select on map and chrono extent
          lat >= bounds$south &
            lat <= bounds$north &
            long <= bounds$east & 
            long >= bounds$west &
            sds <= input$sds &
            ((taq > input$range[1] & tpq < input$range[1]) |
               (tpq < input$range[2] & taq > input$range[2]) |
               (tpq >= input$range[1] & taq <= input$range[2]) |
               (tpq <= input$range[1] & taq >= input$range[2])) &
            Period %in% input$cults &
            mat.life %in% input$mater
        )
    } else {
      # there is a selection shape
      data.f %>%
        dplyr::filter(
          LabCode %in% inside.geometry$LabCode.selected &
            lat >= bounds$south &
            lat <= bounds$north &
            long <= bounds$east & 
            long >= bounds$west &
            sds <= input$sds &
            ((taq > input$range[1] & tpq < input$range[1]) |
               (tpq < input$range[2] & taq > input$range[2]) |
               (tpq >= input$range[1] & taq <= input$range[2]) |
               (tpq <= input$range[1] & taq >= input$range[2])) &
            Period %in% input$cults &
            mat.life %in% input$mater
        )
    }
  }
  # compute map extent dynamically
  data_map <- reactive({
    data.f <- df.tot
    # remove some col from dynamic table
    data.f <- data.f[ , !(colnames(data.f) %in% c("lbl", "idf", "colors"))] 
    if (is.null(input$map_bounds)) {data.f} else {
      bounds <- input$map_bounds
      in_bounding_box(data.f, df.tot$Latitude, df.tot$Longitude,
                      df.tot$tpq, df.tot$taq, df.tot$C14SD, bounds)
    }
  })
  data_count <- reactive({
    # count number of selected dates
    data.f <- df.tot
    # remove some col from dynamic table
    data.f <- data.f[ ,!(colnames(data.f) %in% c("lbl", "idf", "colors"))]
    if (is.null(input$map_bounds)) {as.character(c(0, 0))} else {
      bounds <- input$map_bounds
      sel.data <- in_bounding_box(data.f,
                                  df.tot$Latitude, df.tot$Longitude,
                                  df.tot$tpq, df.tot$taq, df.tot$C14SD,
                                  bounds)
      ndat <- nrow(sel.data)
      if(ndat <= nsites.14C.cal){
        ndat <- paste0("<font color= green >", as.character(ndat), "</font>")
      }
      nsite <- length(unique(sel.data$SiteName))
      as.character(c(ndat,nsite))
    }
  })
  # filtered data on ROI, tpq/taq, periods, etc.
  # reactive, return a SpatialPointsDataFrame
  filteredData <- reactive({
    tpq.taq.val <- (df.tot.sp$taq > input$range[1] & df.tot.sp$tpq < input$range[1]) | 
      (df.tot.sp$tpq < input$range[2] & df.tot.sp$taq > input$range[2]) | 
      (df.tot.sp$tpq >= input$range[1] & df.tot.sp$taq <= input$range[2]) |
      (df.tot.sp$tpq <= input$range[1] & df.tot.sp$taq >= input$range[2])
    cult.select <- df.tot.sp$Period %in% input$cults
    mat.select <- df.tot$mat.life %in% input$mater
    dat.sds <- df.tot$C14SD <= input$sds
    dyna.df_ <- df.tot.sp[tpq.taq.val & cult.select & mat.select & dat.sds,]
    dyna.df_
  })
  output$map <- renderLeaflet({
    # non dynamic
    leaflet(df.tot.sp) %>%
      addTiles() %>%
      addMapPane("sites_", zIndex = 420) %>%
      fitBounds(~min(Longitude),
                ~min(Latitude),
                ~max(Longitude),
                ~max(Latitude)) %>%
      # get coords
      onRender(
        "function(el,x){
                    this.on('click', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lng, lat];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                }"
      )
  })
  # clicked coords
  output$out <- renderText({
    if(!is.null(input$hover_coordinates)) {
      paste0(round(input$hover_coordinates[1], 4), ",", round(input$hover_coordinates[2], 4))
    }
  })
  output$nb.dat <- renderUI({
    HTML(paste0("Dataset within the region of interest (ROI) and the selected parameters: ",
                "<b>", data_count()[2], "</b> sites, ",
                "<b>", data_count()[1], "</b> dates "))
  })
  observe({
    legende.per <- unique(filteredData()@data[c("Period")])[, 1]
    # legende.per <- legende.per[order(match(legende.per, names(lcul_col)))]
    legende.per <- names(lcul_col)[names(lcul_col) %in% legende.per] # existing periods, ordered
    legende <- lcul_col[legende.per] # filter
    if (nrow(filteredData()) > 0) {
      df_colors <-  data.frame(color = filteredData()@data$colors,
                               material = filteredData()@data$Material,
                               stringsAsFactors = FALSE)
      nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"
      proxy.sites <- leafletProxy("map", data = filteredData()) %>%
        addTiles(group = 'OSM') %>%
        addProviderTiles(providers$Esri.WorldImagery, group='Ortho')
      proxy.sites %>% 
        clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addLayersControl(
          baseGroups = c('OSM', 'Ortho')) %>%
        addLegend("bottomleft",
                  colors = as.character(legende),
                  labels = names(legende),
                  title = "Periods",
                  opacity = 1) %>%
        addScaleBar("map", position = "bottomleft")
      # clustered - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(input$hover == TRUE){
        proxy.sites <- proxy.sites %>% 
          removeMarkerFromCluster(layerId = ~LabCode,
                                  clusterId = "grouped")
        proxy.sites <- proxy.sites %>%
          addCircleMarkers(layerId = ~LabCode, 
                           lng = ~Longitude,
                           lat = ~Latitude,
                           weight = 1,
                           radius = 3,
                           popup = ~lbl,
                           clusterId  = "grouped",
                           fillColor = ~colors,
                           color = "black",
                           opacity = 0.7,
                           fillOpacity = 0.7,
                           group = df.tot.sp$Period,
                           clusterOptions = markerClusterOptions(showCoverageOnHover = T,
                                                                 zoomToBoundsOnClick = T),
                           options = pathOptions(pane = "sites_", markerOptions(riseOnHover = TRUE))) %>%
          addDrawToolbar(
            targetGroup = 'Selected',
            polylineOptions = FALSE,
            circleOptions = FALSE,
            circleMarkerOptions = FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,
                                                                              color = 'white',
                                                                              weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,
                                                                                  color = 'white',
                                                                                  weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, 
                                             selectedPathOptions = selectedPathOptions()))
      }
      # non clustered - - - - - - - - - - - - - - - - - - - - -  - - - - - - - -
      if(input$hover == FALSE){
        proxy.sites <- proxy.sites %>% 
          removeMarkerFromCluster(layerId = df.tot$LabCode,
                                  clusterId  = "grouped")
        proxy.sites <- proxy.sites  %>%
          addCircleMarkers(layerId = ~LabCode, # to get the info on df.tot
                           lng = ~Longitude,
                           lat = ~Latitude,
                           weight = 1,
                           radius = 3,
                           popup = ~lbl,
                           fillColor = ~colors,
                           # fillColor = df_colors$color,
                           color = "black",
                           opacity = 0.7,
                           fillOpacity = 0.7,
                           label = ~lapply(paste0("<b>",as.character(SiteName), "</b><br>",
                                                  as.character(Period)," - <i>",
                                                  as.character(PhaseCode), "</i><br>",
                                                  "[",as.character(LabCode), "]"),
                                           htmltools::HTML),
                           group = df.tot.sp$Period,
                           options = pathOptions(pane = "sites_")) %>%
          addDrawToolbar(
            targetGroup = 'Selected',
            polylineOptions = FALSE,
            circleOptions = FALSE,
            circleMarkerOptions = FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,
                                                                                color = 'white',
                                                                                weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,
                                                                                    color = 'white',
                                                                                    weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
      }
    }
  })
  observeEvent(input$map_marker_click, { 
    # on click
    p <- input$map_marker_click
    sel.r <- subset(df.tot, LabCode == p$id)
    # select other sites in bounding box
    bounds <- input$map_bounds
    some_14C <- in_bounding_box(df.tot, 
                                df.tot$Latitude,df.tot$Longitude,
                                df.tot$tpq,df.tot$taq,df.tot$C14SD,
                                bounds)
    # calibrate QQQ
    C14.grouped <- input$C14group
    clicked_site <- c(sel.r$SiteName,sel.r$C14Age,sel.r$LabCode) # to identify the clicked site
    # threshold of n
    if(nrow(some_14C) < nsites.14C.cal){
      output$rdpd <- renderImage({
        # TODO: loading message
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')
        # # Generate the PNG
        out <- paste0(df.tot[i,"LabCode"], ".png") # useful ?
        gcalib <- f.gcalib(some_14C,clicked_site,C14.grouped)
        h.gg <- f.hgg(some_14C,C14.grouped)
        ggsave(outfile, gcalib, 
               width=gcalib.w/300,
               height = (gcalib.h*h.gg/300)+1,
               dpi=300,
               units="in",
               limitsize = FALSE)
        list(src = outfile,
             contentType = 'image/png',
             width = gcalib.w,
             height = (gcalib.h*h.gg)+1,
             alt = "This is alternate text")
      }, deleteFile = TRUE)
    }
    observeEvent(input$C14group, {
      p <- input$map_marker_click  # typo was on this line
      sel.r <- subset(df.tot, LabCode == p$id)
      # select other sites in bounding box
      bounds <- input$map_bounds
      some_14C <- in_bounding_box(df.tot, 
                                  df.tot$Latitude,
                                  df.tot$Longitude,
                                  df.tot$tpq,
                                  df.tot$taq,
                                  df.tot$C14SD,
                                  bounds)
      C14.grouped <- input$C14group
      clicked_site <- c(sel.r$SiteName,sel.r$C14Age,sel.r$LabCode) # to
      if(nrow(some_14C) < nsites.14C.cal){
        output$rdpd <- renderImage({
          # A temp file to save the output.
          # This file will be removed later by renderImage
          outfile <- tempfile(fileext = '.png')
          # Generate the PNG
          out <- paste0(df.tot[i, "LabCode"], ".png")
          gcalib <- f.gcalib(some_14C,clicked_site,C14.grouped)
          h.gg <- f.hgg(some_14C,C14.grouped)
          ggsave(outfile, 
                 gcalib, 
                 width = gcalib.w/300,
                 height = gcalib.h*h.gg/300,
                 dpi = 300,
                 units = "in",
                 limitsize = FALSE)
          # Return a list containing the filename
          list(src = outfile,
               contentType = 'image/png',
               width = gcalib.w,
               height = gcalib.h*h.gg,
               alt = "This is alternate text")}, 
          deleteFile = TRUE)
      }
    })
    output$dwnld_calib <- downloadHandler(
      filename = out.png.name,
      content = function(file) {
        p <- input$map_marker_click  # typo was on this line
        sel.r <- subset(df.tot, LabCode == p$id)
        bounds <- input$map_bounds
        some_14C <- in_bounding_box(df.tot, 
                                    df.tot$Latitude, df.tot$Longitude, 
                                    df.tot$tpq, df.tot$taq, df.tot$C14SD, 
                                    bounds)
        C14.grouped <- input$C14group
        h.gg <- f.hgg(some_14C,C14.grouped)
        device <- function(..., width, height) {
          grDevices::png(..., width = gcalib.w/300,
                         height = gcalib.h*h.gg/300,
                         res = 300, units = "in")
        }
        ggsave(file, plot = f.gcalib(some_14C,
                                     clicked_site,
                                     C14.grouped), 
               device = device,
               limitsize = FALSE)
      })
  })
  # - - - - - - - - - - - - - - - - - - - - - - -
  # list to store the selections for tracking
  inside.geometry <- reactiveValues(LabCode.selected = NULL)
  data_of_click <- reactiveValues(clickedMarker = list())
  observeEvent(input$map_draw_new_feature,{
    # A selection shape is created
    coordinates <- as.data.frame(filteredData())
    coordinates <- SpatialPointsDataFrame(coordinates[, c('Longitude', 'Latitude')], coordinates)
    found_in_bounds <- findLocations(shape = input$map_draw_new_feature,
                                     location_coordinates = coordinates,
                                     location_id_colname = "locationID")
    # usefull ?
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0)
      }
    }
    selected <- subset(filteredData(), locationID %in% data_of_click$clickedMarker)
    LabCode.selected <- selected@data$LabCode
    inside.geometry$LabCode.selected <- LabCode.selected
  })
  observeEvent(input$map_draw_deleted_features,{
    # A selection shape is deleted -> all current selected dates
    selected <- filteredData()
    LabCode.selected <- selected@data$LabCode
    inside.geometry$LabCode.selected <- LabCode.selected
  })
  output$biblio <- renderText({ 
    # bibliographical references
    bibrefs.html
  })
}

# run
shinyApp(ui, server)