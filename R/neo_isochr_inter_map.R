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
neo_isochr_inter_map <- function(df = NA){
  lon_min <- min(df$lon, na.rm = TRUE)
  lon_max <- max(df$lon, na.rm = TRUE)
  lat_min <- min(df$lat, na.rm = TRUE)
  lat_max <- max(df$lat, na.rm = TRUE)
  # Plot with ggplot2
  map <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df, 
                        ggplot2::aes(x = lon, y = lat, color = date.med)) +  # Plot points with aes()
    ggplot2::geom_sf(data = world, color = 'black', fill = "white") +  # Use geom_sf for the world map
    ggplot2::scale_color_gradient(low = "deeppink4", high = "lightpink") +  # Color gradient for date.med
    ggplot2::labs(title = "Interpolation map",
                  color = "w-median (BC)") +
    # TODO: adapt to the neo_isochr() layout
    # ggplot2::theme_minimal() +
    # ggplot2::labs(title = tit,
    #               subtitle = subtit,
    #               caption = capt,
    #               color = "w-median (BC)") 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))  # Set map limits to bounding box
  return(map)
}