#' @name neo_isochr_inter
#'
#' @description Interpolate dates to create isochrones. convert this to a long form dataframe. Creates edge detection to identify isochrone barriers.
#'
#' @param df a dataframe
#' @param edge.detection If TRUE will create an edge detection. Default FALSE. 
#' @param span.time The threshold to detect edges. Default 200 (years). Only useful if `edge.detection` is True.
#' @param n.connectivity N-connectivity: 4 or 8.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A matrix for `neo_isochr()`
#'
#' @examples
#' 
#'
#'
#' @export
neo_isochr_inter <- function(df = NA,
                             span.time = 200,
                             n.connectivity = 8,
                             edge.detection = FALSE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  interpolated <- akima::interp(x = df$longitude, 
                                y = df$latitude, 
                                z = df$median, 
                                duplicate = "mean")
  
  interp_df <- tidyr::expand_grid(i = seq_along(interpolated$x), 
                                  j = seq_along(interpolated$y)) %>% 
    dplyr::mutate(lon = interpolated$x[i],
                  lat = interpolated$y[j],
                  date.med = purrr::map2_dbl(i, j, ~interpolated$z[.x, .y])) %>% 
    dplyr::select(-i, -j)
  # rm interpolated results having NA
  interp_df <- na.omit(interp_df)
  if(!edge.detection){
    return(interp_df)
  } else {
    ###########
    # Not working
    mat <- raster::as.matrix(interpolated$z)
    result <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
    # Loop through each cell in the matrix (excluding borders)
    for (i in 2:(nrow(mat) - 1)) {
      for (j in 2:(ncol(mat) - 1)) {
        # Get the value of the current cell
        # mat[2, 2]
        center_value <- mat[i, j]
        
        # Extract the neighborhood of the current cell
        neighborhood <- mat[(i-1):(i+1), (j-1):(j+1)]
        
        # Check if any absolute difference with neighbors exceeds the threshold
        if (any(abs(neighborhood - center_value) > span.time)) {
          result[i, j] <- 1
        }}}
  }
  
}