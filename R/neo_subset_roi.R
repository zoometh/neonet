#' @name neo_subset_roi
#'
#' @description Subset an sf object (dataframe of dates) with a ROI
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @param where An area to limit the analysis. Can be an sf dataframe, a GeoJSON path, a bounding box (xmin, ymin, xmin, xmax). Default NA.
#'
#' @return A dataset
#'
#' @examples
#' 
#'
#'
#' @export
neo_subset_roi <- function(df.c14 = NA, 
                           where = NA,
                           verbose = TRUE){
  if(inherits(where, "character")){
    if(verbose){
      print(paste0("Spatial subset on new ROI, path to a sf object"))
    }
    where <- sf::st_read(where,
                         quiet = TRUE)
    inside <- sf::st_within(df.c14, where, sparse = FALSE)
    df.dates <- df.c14[inside, ]
  }
  if(inherits(where, "sf")){
    if(verbose){
      print(paste0("Spatial subset on new ROI, sf object"))
    }
    inside <- sf::st_within(df.c14, where, sparse = FALSE)
    df.dates <- df.c14[inside, ]
  }
  if(inherits(where, "numeric")){
    if(verbose){
      print(paste0("Spatial subset on new ROI, bounding box"))
    }
    xmin <- where[1] ; ymin <- where[2] ; xmax <- where[3] ; ymax <- where[4]
    polygon <- sf::st_polygon(list(matrix(c(xmin, ymin,
                                            xmax, ymin,
                                            xmax, ymax,
                                            xmin, ymax,
                                            xmin, ymin), 
                                          ncol = 2, byrow = TRUE)))
    where <- sf::st_sf(geometry = sf::st_sfc(polygon, crs = 4326))
    inside <- sf::st_within(df.c14, where, sparse = FALSE)
    df.dates <- df.c14[inside, ]
  }
  nb.dates.tot <- nrow(df.dates)
  if(verbose){
    print(paste0("Original dataset: ", nrow(df.c14)))
  }
  if(verbose){
    print(paste0("Filtered dataset: ", nb.dates.tot, " (dates inside the ROI)"))
  }
  return(df.dates)
}