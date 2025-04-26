#' @name neo_isochr_inter_map
#'
#' @description Create an interpolation map. This function is run after `neo_isochr()`
#'
#' @param df a tibble
#'
#' @return A ggplot
#'
#' @examples
#' 
#' neo_isochr_inter_map(df)
#'
#' @export
neo_isochr_inter_map <- function(df = NA,
                                 title = "Interpolation map",
                                 color = "w-median (BC)",
                                 plot.mode = "heatmap",
                                 point.color.ramp = c("deeppink4", "lightpink"),
                                 barwidth = 20){
  lon_min <- min(df$lon, na.rm = TRUE)
  lon_max <- max(df$lon, na.rm = TRUE)
  lat_min <- min(df$lat, na.rm = TRUE)
  lat_max <- max(df$lat, na.rm = TRUE)
  world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  # Plot with ggplot2
  # TODO: adapt to the neo_isochr() layout
  if(plot.mode == "heatmap"){
    map <-  ggplot2::ggplot(df, ggplot2::aes(lon, lat, fill= abs(date.med))) + 
      ggplot2::geom_tile() +
      ggplot2::geom_sf(data = world, color = 'black', fill = "white", inherit.aes = FALSE) +
      ggplot2::scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))
    # ggplot2::scale_fill_viridis(discrete=FALSE)
  }
  if(plot.mode == "points"){
    map <- ggplot2::ggplot() +
      ggplot2::geom_point(data = df, size = 1,
                          ggplot2::aes(x = lon, y = lat, color = abs(date.med))) +
      ggplot2::geom_sf(data = world, color = 'black', fill = "white") +
      ggplot2::scale_color_gradient(low = point.color.ramp[2], 
                                    high = point.color.ramp[1])
  }
  map <- map +
    ggplot2::labs(title = title,
                  color = color) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::guides(fill = guide_colourbar(barwidth = barwidth,
                                           barheight = .5)) +
    ggplot2::coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))  
  
  return(map)
}

