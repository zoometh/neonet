#' Creates a map of the research area
#' 
#' @name neo_map
#' 
#' @description Create a distribution map of the region of interest (roi) of selected radiocarbon dates.
#'
#' @param map.name name of the output map and the name of the saved file (if export.plot is TRUE). Default "NeoNet_atl".
#' @param df.c14 Path to the dataset (Google Sheet) or the dataset itself (dataframe, sf). 
#' @param gg.url The URL to a NeoNet structured Google Sheet dataset, ex: "https://docs.google.com/spreadsheets/d/1q6VdxS_1Pi0fVWfyQzW6VBhjuBY58hymtSLWg4JyLEA/edit?usp=sharing".
#' @param roi The region of interest, such as a river basin: Atlantic (Default) or Mediterranean
#' @param selected.per Subset on selected periods.
#' @param plot.dates Will plot dates. Default: TRUE.
#' @param dates.size Size of the points (dates)
#' @param dates.within.roi Plot only dates located inside the ROI. Default: TRUE.
#' @param width,height dimension of the output map, if exported.
#'
#' @return A ggplot
#'
#' @examples
#' 
#' # Plot a curated dataset
#' neo_map(df.c14 = df_filtered, 
#'         selected.per = 'EN', 
#'         breaks_values = c(-10000, -9000, -8000, -7000, -6500, -6000, -5500, -5000, -4500),
#'         roi = NA, dates.within.roi = FALSE)
#'
#' # Plot current region and other river basins
#' neo_map(df.c14 = df.c14, plot.other.ws = T, export.plot = F)
#'
#' @export
neo_map <- function(map.name = "map_atl",
                    df.c14 = NA,
                    gg.url = NA,
                    # ref.spat = "C:/Rprojects/neonet/doc/data/wsh_atl.shp",
                    roi = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/wsh_atl.geojson",
                    buff = 0.5,
                    plot.dates = TRUE,
                    dates.size = 1,
                    ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                    breaks_values = NA,
                    # breaks_labels <- c("-8000", "-7000", "-6000", "-5000", "-4000")
                    selected.per = NA,
                    dates.within.roi = TRUE,
                    title = NA,
                    verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  breaks_labels <- as.character(abs(breaks_values))
  breaks_labels <- paste0(breaks_labels, "-", breaks_labels[-1])
  breaks_labels <- breaks_labels[-length(breaks_labels)]
  if(is.character(roi)){
    if(verbose){
      print(paste0("ROI: Reads a GeoJSON path"))
    }
    where.roi <- sf::st_read(roi, quiet = TRUE)
  }
  if(inherits(roi, "sf")){
    if(verbose){
      print(paste0("ROI: Reads a 'sf' dataframe"))
    }
    where.roi <- roi
  }
  if(!is.na(gg.url)){
    # gs4_deauth();gs4_auth()
    if(verbose){
      print(paste0("C14: Reads a Google Sheet"))
    }
    df.dates <- googlesheets4::read_sheet(gg.url)
  } 
  if(inherits(df.c14, "sf")){
    if(verbose){
      print(paste0("C14: Reads a 'sf' dataframe"))
    }
    df.dates <- df.c14
  } 
  if(!inherits(df.c14, "sf")){
    if(verbose){
      print(paste0("C14: Reads a dataframe"))
    }
    df.dates <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
  } 
  df.dates$median <- round(df.dates$median, 0)
  if(dates.within.roi){
    df.dates <- sf::st_make_valid(df.dates)
    where.roi <- sf::st_make_valid(where.roi)
    if(verbose){
      print(paste0("Spatial intersection with the ROI"))
    }
    # nrow(df.dates)
    if (sf::st_crs(df.dates) != sf::st_crs(where.roi)) {
      df.dates <- sf::st_transform(df.dates, sf::st_crs(where.roi))
    }
    ## Workout, sf::st_within() doesn't work
    roi_bbox <- sf::st_bbox(where.roi)
    points_within_roi <- df.dates %>%
      dplyr::filter(
        sf::st_coordinates(.)[,1] >= roi_bbox$xmin & 
          sf::st_coordinates(.)[,1] <= roi_bbox$xmax & 
          sf::st_coordinates(.)[,2] >= roi_bbox$ymin & 
          sf::st_coordinates(.)[,2] <= roi_bbox$ymax
      )
    df.dates <- points_within_roi
  }
  if(is.character(selected.per)){
    df.dates <- df.dates[df.dates$Period %in% selected.per, ]
    # caption
    periods.colors <- read.csv(ref.period, sep = "\t")
    periods.colors <- periods.colors[periods.colors$period %in% selected.per, ]
    capt <- c()
    for(i in 1:nrow(periods.colors)){
      period <- periods.colors[i, "period"]
      color <- periods.colors[i, "color"]
      capt <- c(capt, paste0("<span style='color: ", color, ";'><b>", period, "</b></span>"))
    }
    capt <- paste0(capt, collapse = ", ")
    caption <- paste("n =", nrow(df.dates), "dates | periods:", capt)
  }
  world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  g.neo.map <- ggplot2::ggplot(world) +
    # ggplot2::geom_sf(fill = '#7d7d7d', color = '#7d7d7d') +
    ggplot2::geom_sf() +
    ggplot2::theme_bw()
  if(is.character(roi)){
    g.neo.map + 
      ggplot2::geom_sf(data = where.roi, color = 'black', fill = NA, inherit.aes = FALSE)
  }
  if(plot.dates){
    # g.neo.map <- g.neo.map +
    #   ggplot2::geom_sf(data = df.dates, inherit.aes = FALSE, size = dates.size)
    df.dates$median_bin <- cut(
      df.dates$median,  # Convert to absolute and numeric
      breaks = breaks_values,           # Define custom breaks
      labels = breaks_labels,       # Labels correspond to intervals (excluding the first edge)
      include.lowest = TRUE
    )
    nb.na.bin <- sum(is.na(df.dates$median_bin))
    caption <- paste0("n = ", nrow(df.dates), " dates (", nb.na.bin, " out of range & not displayed) | ", "periods: ", capt)
    # df.dates$median <- abs(df.dates$median)
    g.neo.map <- g.neo.map +
      ggplot2::geom_sf(data = df.dates, ggplot2::aes(color = median_bin), inherit.aes = FALSE, size = dates.size) + 
      # ggplot2::scale_color_gradient(low = "lightpink", high = "darkred", name = "Median Age") # +
      ggplot2::scale_color_viridis_d(option = "C", 
                                     # breaks = breaks_values,  # Custom breaks for the color scale
                                     # labels = breaks_labels,   # Custom labels for the breaks
                                     name = "wmedian BC",
                                     guide = ggplot2::guide_legend(
                                       keyheight = ggplot2::unit(1, "cm"),  # Increase marker height
                                       keywidth = ggplot2::unit(1, "cm")     # Increase marker width
                                     )) +
      ggplot2::theme(legend.position = "right")
    # labs(title = "Map of Sites", subtitle = "Colored by Median Age") +  # Add titles and labels
    # theme_minimal()  # Use a minimal theme
  }
  if(!is.na(title)){
    # caption <- paste0("n = ", nrow(df.dates), " dates")
    g.neo.map <- g.neo.map +
      ggplot2::labs(
        title = title,
        caption = caption) +
      # ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.caption = ggtext::element_markdown())
  }
  if(is.character(roi)){
    g.neo.map <- g.neo.map +
      ggplot2::coord_sf(xlim = c(sf::st_bbox(roi)[1] - buff, sf::st_bbox(roi)[3] + buff),
                        ylim = c(sf::st_bbox(roi)[2] - buff, sf::st_bbox(roi)[4] + buff))
  } else {
    g.neo.map <- g.neo.map +
      ggplot2::coord_sf(xlim = c(sf::st_bbox(df.dates)[1] - buff, sf::st_bbox(df.dates)[3] + buff),
                        ylim = c(sf::st_bbox(df.dates)[2] - buff, sf::st_bbox(df.dates)[4] + buff))
  }
  g.neo.map <- g.neo.map + 
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) 
  return(g.neo.map)
}
