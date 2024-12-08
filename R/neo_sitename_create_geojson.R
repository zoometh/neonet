#' @name neo_sitename_create_geojson
#'
#' @description Creates an sf object of all unique sites from a NeoNet compliant layout
#'
#' @param df.c14 a dataset of dates
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return An sf object that can be exported into a GeoJSON file
#'
#' @examples
#' 
#' df <- neo_sitename_create_geojson(df.c14)
#' sf::st_write(df, "C:/Rprojects/neonet/inst/extdata/c14_corrected_sitenames.geojson", driver = "GeoJSON")
#'
#'
#' @export
neo_sitename_create_geojson <- function(df.c14 = NA,
                                   verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  df <- df.c14 %>% 
    dplyr::group_by(SiteName) %>% 
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()
  sf::st_crs(df) <- 4326
  return(df)
}