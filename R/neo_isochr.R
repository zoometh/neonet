#' @name neo_isochr
#'
#' @description create isochron contours
#'
#' @param df.c14 a dataset of dates
#' @param select.periods will select only these periods. Default: LM and EN. 
#' @param calibrate if TRUE (default) will calibrate dates using the neo_calib() function.
#' @param out.df.c14.topub the name of the conformed dataset to output
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Export a file
#'
#' @examples
#'
#'
#' @export
neo_isochr <- function(df.c14 = "C:/Rprojects/neonet/results/2023-09-13_neonet.geojson",
                       select.periods = c("LM", "EN"),
                       calibrate = TRUE,
                       export = TRUE,
                       outDir = "C:/Rprojects/neonet/inst/extdata/",
                       verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  df.dates <- sf::st_read(df.c14)
  if(verbose){
    print(paste0("Original GeoJSON file: ", nrow(df.dates), " dates"))
  }
  # subset on periods
  df.dates <- df.dates[df.dates$Period %in% select.periods, ]
  if(verbose){
    print(paste0("After subset on Periods ", paste0(select.periods, collapse = ", "),": ", nrow(df.dates), " dates"))
  }
  if(calibrate){
    df.dates <- neo_calib(as.data.frame(df.dates))
  }
  # Select the row with the minimum median in each site
  df.dates.min <- df.dates %>% 
    dplyr::group_by(SiteName) %>% 
    dplyr::slice_min(median)
  # to sf
  df.dates.min$geometry <- sf::st_as_text(df.dates.min$geometry)
  df.dates.min <- sf::st_as_sf(df.dates.min, wkt = "geometry")
  
  
  # Load required libraries
  library(sf)
  library(gstat)
  
  # Load your spatial data (replace 'your_data.sf' with your actual data file)
  # your_data <- st_read("your_data.shp")
  
  # Create a gstat object
  formula <- attribute_to_interpolate ~ 1
  your_gstat <- gstat::gstat(formula = median~1, id = "median", data = df.dates.min)
  
  # Define the interpolation grid (replace with your desired extent and resolution)
  Xs <- sf::st_coordinates(df.dates.min$geometry)[,1]
  Ys <- sf::st_coordinates(df.dates.min$geometry)[,2]
  interp_grid <- expand.grid(
    x = seq(min(Xs), max(Xs), by = 100),
    y = seq(min(Ys), max(Ys), by = 100)
  )
  
  # Interpolate the data using IDW
  interpolated_values <- predict(your_gstat, newdata = interp_grid)
  
  # Add interpolated values to the grid
  interp_grid$interpolated_values <- interpolated_values
  
  # Convert the result to an sf object
  interp_sf <- st_as_sf(interp_grid, coords = c("x", "y"))
  
  # Create contour lines
  contour_lines <- st_contour(interp_sf, z = "interpolated_values")
  
  # Plot the contour lines
  plot(your_data)
  plot(contour_lines, add = TRUE, lwd = 2, col = "red")
  
  
  
  df.dates.min[df.dates.min$Period = ]
  
  contour(df.dates.min$geom, y, z,
          nlevels = 20) 
  
  table(df.dates$SiteName)
  # old dataset
  df.c14.pub <- read.csv(df.c14.pub, sep = "\t")
  if(verbose){
    in.original.only <- setdiff(colnames(df.c14), colnames(df.c14.pub)) # OK
    print(paste0("Columns in the original dataset but not in the publicated one:"))
    cat(in.original.only, sep = ", ")
    print("\n")
    in.publicated.only <- setdiff(colnames(df.c14.pub), colnames(df.c14)) # OK
    print(paste0("Columns in the publicated dataset but not in the original one:"))
    cat(in.publicated.only, sep = ", ")
    print("\n")
  }
  # rm non usefull col
  df.c14[ , in.original.only] <- NULL
  if(verbose){
    print("Order columns")
  }
  df.c14 <- df.c14[ , colnames(df.c14.pub)]
  if(export){
    write.table(df.c14, paste0(outDir, out.df.c14.topub),
                sep = "\t", 
                row.names = FALSE)
    if(verbose){
      print(paste0(out.df.c14.topub, " has been exported in: ", outDir))
    }
  } else {
    return(df.c14)
  }
  
}