#' @name neo_kcc_extract_longformat
#'
#' @description Convert a dataframe having different KCC columns ("koppen_6k", "koppen_7k", "koppen_8k", "koppen_9k", etc.) into a long dataframe having one KCC column. 
#'
#' @param df.c14 a dataset of dates
#' @param koppen_columns Names of the columns where the KCC values are recorderd by the `neo_kcc_extract()` function.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of dates
#'
#' @examples
#' 
#' df_kcc_long <- neo_kcc_extract_longformat(df.c14)
#'
#'
#' @export
neo_kcc_extract_longformat <- function(df.c14 = NA,
                                       koppen_columns = c("koppen_6k", "koppen_7k", "koppen_8k", "koppen_9k", "koppen_10k", "koppen_11k"),
                                       verbose = TRUE){
  
  `%>%` <- dplyr::`%>%` # used to not load dplyr 
  if(inherits(df.c14, "sf")){
    df <- df.c14
    df$X <- sf::st_coordinates(df$geometry)[, 1]
    df$Y <- sf::st_coordinates(df$geometry)[, 2]
    df_kcc <- sf::st_drop_geometry(df)
  }
  df_kcc <- df_kcc
  df <- df_kcc %>%
    tidyr::pivot_longer(cols = all_of(koppen_columns), names_to = "map", values_to = "code") %>%
    dplyr::filter(!is.na(code))
  df <- data.frame(df)
  return(df)
}